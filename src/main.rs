use chrono::NaiveDate;
use ego_tree::iter::Edge;
use indexmap::indexmap;
use itertools::Itertools;
use openapiv3::{
    ArrayType, IntegerType, MediaType, NumberType, ObjectType, OpenAPI, Operation, PathItem,
    ReferenceOr, RequestBody, Response, Responses, Schema, SchemaData, SchemaKind, StatusCode,
    StringType, Type as SchemaType,
};
use scraper::{ElementRef, Html, Node, Selector};
use std::ops::Range;

trait ElementRefExt {
    fn full_text(&self) -> String;
}

impl ElementRefExt for ElementRef<'_> {
    fn full_text(&self) -> String {
        self.traverse()
            .filter_map(|edge| {
                if let Edge::Open(node) = edge {
                    match node.value() {
                        Node::Text(text) => Some(text.as_ref()),
                        Node::Element(elem) if elem.name() == "br" => Some("\n"),
                        _ => None,
                    }
                } else {
                    None
                }
            })
            .collect()
    }
}

trait StrExt {
    fn is_first_letter_lowercase(self) -> bool;
}

impl<'a> StrExt for &'a str {
    fn is_first_letter_lowercase(self) -> bool {
        self.chars().next().map(|c| c.is_lowercase()).unwrap()
    }
}

#[derive(Debug, thiserror::Error)]
enum Error {
    #[error("Invalid Required: {0}")]
    InvalidRequired(String),
    #[error("Failed to extract type from description: {0}")]
    TypeExtractionFailed(String),
}

#[derive(Debug)]
enum State {
    SearchRecentChanges,
    GetRecentChange,
    SearchGettingUpdates,
    GetName,
    GetDescription {
        name: String,
    },
    GetObjectFields {
        name: String,
        description: String,
    },
    GetMethodFields {
        name: String,
        description: String,
        return_type: Type,
    },
}

enum SchemaKindOrRef {
    Kind(SchemaKind),
    Ref(ReferenceOr<Box<Schema>>),
}

impl SchemaKindOrRef {
    fn into_ref_or_schema(self) -> ReferenceOr<Box<Schema>> {
        match self {
            SchemaKindOrRef::Kind(schema_kind) => ReferenceOr::Item(Box::new(Schema {
                schema_data: SchemaData::default(),
                schema_kind,
            })),
            SchemaKindOrRef::Ref(reference) => reference,
        }
    }
}

#[derive(Debug, Clone, Eq, PartialEq)]
enum Type {
    Integer,
    String,
    Bool,
    BoolTrue,
    Float,
    Or(Vec<Type>),
    Array(Box<Type>),
    Object(String),
}

impl Type {
    // this function parses types from `Type` column in docs
    fn new(s: &str) -> Self {
        match s {
            "Integer" | "Int" => Self::Integer,
            "String" => Self::String,
            "Boolean" => Self::Bool,
            "True" => Self::BoolTrue,
            "Float" | "Float number" => Self::Float,
            _ if s.contains(" or ") => {
                let types = s.split(" or ").map(Self::new).collect();
                Self::Or(types)
            }
            _ if s.starts_with("Array of ") => {
                let types: Vec<&str> = s
                    .trim_start_matches("Array of ")
                    .split(" and ")
                    .flat_map(|s| s.split(','))
                    .map(str::trim)
                    .collect();
                if types.len() == 1 {
                    Self::Array(Box::new(Self::new(types[0])))
                } else {
                    Self::Array(Box::new(Self::Or(
                        types.into_iter().map(Self::new).collect(),
                    )))
                }
            }
            _ => Self::Object(s.to_string()),
        }
    }

    fn has(&self, other: &Type) -> bool {
        match self {
            Type::Integer | Type::String | Type::Bool | Type::BoolTrue | Type::Float => {
                self == other
            }
            Type::Or(types) => types.iter().any(|ty| ty.has(other)),
            Type::Array(ty) => ty.has(other),
            Type::Object(object) => {
                if let Type::Object(other_object) = other {
                    object == other_object
                } else {
                    false
                }
            }
        }
    }

    fn maybe_file_to_send(&self) -> bool {
        self.has(&Type::Object("InputMedia".to_string()))
            || self.has(&Type::Object("InputMediaPhoto".to_string()))
            || self.has(&Type::Object("InputMediaVideo".to_string()))
            || self.has(&Type::Object("InputMediaAnimation".to_string()))
            || self.has(&Type::Object("InputMediaAudio".to_string()))
            || self.has(&Type::Object("InputMediaDocument".to_string()))
            || self.has(&Type::Object("InputFile".to_string()))
    }

    fn match_type(self) -> SchemaKindOrRef {
        match self {
            this @ Type::Integer
            | this @ Type::String
            | this @ Type::Bool
            | this @ Type::BoolTrue
            | this @ Type::Float
            | this @ Type::Array(_) => {
                let schema_type = match this {
                    Type::Integer => SchemaType::Integer(IntegerType::default()),
                    Type::String => SchemaType::String(StringType::default()),
                    Type::Bool | Type::BoolTrue => SchemaType::Boolean {},
                    Type::Float => SchemaType::Number(NumberType::default()),
                    Type::Array(array) => SchemaType::Array(ArrayType {
                        items: array.match_type().into_ref_or_schema(),
                        min_items: None,
                        max_items: None,
                        unique_items: false,
                    }),
                    _ => unreachable!(),
                };
                SchemaKindOrRef::Kind(SchemaKind::Type(schema_type))
            }
            Type::Or(types) => SchemaKindOrRef::Kind(SchemaKind::AnyOf {
                any_of: types
                    .into_iter()
                    .map(Type::match_type)
                    .map(SchemaKindOrRef::into_ref_or_schema)
                    .map(ReferenceOr::unbox)
                    .collect(),
            }),
            Type::Object(reference) => SchemaKindOrRef::Ref(ReferenceOr::Reference {
                reference: format!("#/components/schemas/{}", reference),
            }),
        }
    }
}

#[derive(Debug, Clone, Eq, PartialEq)]
enum Required {
    Yes,
    Optional,
}

#[derive(Debug, Clone)]
struct Field {
    name: String,
    kind: Type,
    required: Required,
    description: String,
}

impl Field {
    fn into_ref_or_schema(self) -> (String, ReferenceOr<Box<Schema>>) {
        let default = match self.kind {
            Type::BoolTrue => Some(true.into()),
            _ => None,
        };

        let kind_or_ref = self.kind.match_type();
        let ref_or_schema = match kind_or_ref {
            SchemaKindOrRef::Kind(schema_kind) => ReferenceOr::Item(Box::new(Schema {
                schema_data: SchemaData {
                    default,
                    description: Some(self.description),
                    ..SchemaData::default()
                },
                schema_kind,
            })),
            SchemaKindOrRef::Ref(reference) => reference,
        };

        (self.name, ref_or_schema)
    }
}

#[derive(Debug)]
struct Object {
    name: String,
    description: String,
    fields: Vec<Field>,
}

impl Required {
    fn new(s: String) -> Result<Self, Error> {
        match s.as_str() {
            "Yes" => Ok(Self::Yes),
            "Optional" => Ok(Self::Optional),
            _ => Err(Error::InvalidRequired(s)),
        }
    }
}

#[derive(Debug, Clone)]
struct Argument {
    name: String,
    kind: Type,
    required: Required,
    description: String,
}

#[derive(Debug)]
enum MethodArgs {
    No,
    Yes(Vec<Argument>),
    WithMultipart(Vec<Argument>),
}

impl MethodArgs {
    fn new(args: Vec<Argument>) -> Self {
        if args.iter().any(|arg| arg.kind.maybe_file_to_send()) {
            Self::WithMultipart(args)
        } else if args.is_empty() {
            Self::No
        } else {
            Self::Yes(args)
        }
    }

    fn is_empty(&self) -> bool {
        match self {
            MethodArgs::No => true,
            MethodArgs::Yes(_) | MethodArgs::WithMultipart(_) => false,
        }
    }
}

#[derive(Debug)]
struct Method {
    name: String,
    description: String,
    args: MethodArgs,
    return_type: Type,
}

const BASE_SCHEMA: &str = include_str!("../base-schema.yml");

#[tokio::main]
async fn main() -> anyhow::Result<()> {
    let api = reqwest::get("https://core.telegram.org/bots/api/")
        .await?
        .text()
        .await?;

    let h3 = Selector::parse("h3").unwrap();
    let h4 = Selector::parse("h4").unwrap();
    let table = Selector::parse("table").unwrap();
    let td = Selector::parse("td").unwrap();
    let p = Selector::parse("p").unwrap();
    let any = Selector::parse("h3, h4, p, table").unwrap();
    let doc = Html::parse_document(&api);

    let mut api: OpenAPI = serde_yaml::from_str(BASE_SCHEMA).unwrap();

    let mut recent_change = None;
    let mut objects = Vec::new();
    let mut methods = Vec::new();
    let success = api
        .components
        .as_mut()
        .unwrap()
        .schemas
        .remove("Success")
        .unwrap();

    let mut state = State::SearchRecentChanges;
    let mut select_any = doc.select(&any).peekable();
    while let Some(elem) = select_any.next() {
        let new_state = match state {
            State::SearchRecentChanges
                if h3.matches(&elem) && elem.full_text() == "Recent changes" =>
            {
                State::GetRecentChange
            }
            State::GetRecentChange if h4.matches(&elem) => {
                let date = elem.full_text();
                recent_change = Some(NaiveDate::parse_from_str(&date, "%B %e, %Y")?);
                State::SearchGettingUpdates
            }
            State::SearchGettingUpdates
                if h3.matches(&elem) && elem.full_text() == "Getting updates" =>
            {
                State::GetName
            }
            State::GetName if h4.matches(&elem) => {
                let name = elem.full_text();
                // get rid of elements like `Formatting options`, `Sending files` that are not objects or methods
                if name.chars().any(char::is_whitespace) {
                    State::GetName
                } else {
                    State::GetDescription { name }
                }
            }
            State::GetDescription { name } if p.matches(&elem) => {
                fn get_range(text: &str, start: &str, end: &str) -> Option<Range<usize>> {
                    let start = text.find(start)? + start.len();
                    let end = text[start..].find(end)? + start;
                    Some(start..end)
                }

                fn extract_type(text: &str) -> Option<Type> {
                    const ARRAY: &str = "Array";
                    const AN_ARRAY_OF: &str = "an array of ";
                    const OTHERWISE: &str = "otherwise";

                    if text.contains(OTHERWISE) {
                        let types = text
                            .split(OTHERWISE)
                            .map(extract_type)
                            .collect::<Option<_>>()?;
                        Some(Type::Or(types))
                    } else {
                        let (pos, ty) = text
                            .split_whitespace()
                            .find_position(|word| !word.is_first_letter_lowercase())?;
                        let mut ty = ty.trim_matches(',');

                        // get rid of plural ending
                        if ty.ends_with("es") {
                            ty = ty.trim_end_matches('s');
                        }

                        if ty == ARRAY
                            || text[pos.saturating_sub(AN_ARRAY_OF.len())..]
                                .starts_with(AN_ARRAY_OF)
                        {
                            let text = &text[pos + ARRAY.len()..];
                            let ty = extract_type(text)?;
                            Some(Type::Array(Box::new(ty)))
                        } else {
                            Some(Type::new(ty))
                        }
                    }
                }

                fn search_type(text: &str, start: &str, end: &str) -> Option<Type> {
                    let range = get_range(text, start, end)?;
                    let text = &text[range];
                    extract_type(text)
                }

                fn search(text: &str, patterns: &[(&str, &str)]) -> Result<Type, Error> {
                    patterns
                        .iter()
                        .find_map(|(start, end)| search_type(text, start, end))
                        .ok_or_else(|| Error::TypeExtractionFailed(text.to_string()))
                }

                const PATTERNS: &[(&str, &str)] = &[
                    ("Returns ", " on success"),
                    ("Returns ", " object."),
                    ("An ", " is returned."),
                    ("On success, ", " is returned."),
                    ("On success, ", " object."),
                    ("returns ", "."),
                ];

                let description = elem.full_text();

                let no_table = select_any
                    .peek()
                    .map(|next_elem| !table.matches(next_elem))
                    .unwrap_or(true);
                let is_method = name.is_first_letter_lowercase();
                match (no_table, is_method) {
                    (false, true) => State::GetMethodFields {
                        name,
                        return_type: search(&description, PATTERNS)?,
                        description,
                    },
                    (false, false) => State::GetObjectFields { name, description },
                    (true, true) => {
                        methods.push(Method {
                            name,
                            return_type: search(&description, PATTERNS)?,
                            description,
                            args: MethodArgs::No,
                        });
                        State::GetName
                    }
                    (true, false) => {
                        objects.push(Object {
                            name,
                            description,
                            fields: vec![],
                        });
                        State::GetName
                    }
                }
            }
            State::GetObjectFields { name, description } if table.matches(&elem) => {
                let fields: Vec<Field> = elem
                    .select(&td)
                    .map(|td| td.full_text())
                    .chunks(3)
                    .into_iter()
                    .filter_map(|mut tds| {
                        let name = tds.next()?;
                        let kind = tds.next()?;
                        let description = tds.next()?;
                        Some(Field {
                            name,
                            kind: Type::new(&kind),
                            required: if description.starts_with("Optional.") {
                                Required::Optional
                            } else {
                                Required::Yes
                            },
                            description: description.trim_start_matches("Optional. ").to_string(),
                        })
                    })
                    .collect();

                objects.push(Object {
                    name: name.clone(),
                    description: description.clone(),
                    fields,
                });

                State::GetName
            }
            State::GetMethodFields {
                name,
                description,
                return_type,
            } if table.matches(&elem) => {
                let args: Vec<Argument> = elem
                    .select(&td)
                    .map(|td| td.full_text())
                    .chunks(4)
                    .into_iter()
                    .filter_map(|mut tds| {
                        let name = tds.next()?;
                        let kind = tds.next()?;
                        let required = Required::new(tds.next()?);
                        let description = tds.next()?;

                        let arg = required.map(|required| Argument {
                            name,
                            kind: Type::new(&kind),
                            required,
                            description,
                        });

                        Some(arg)
                    })
                    .collect::<Result<_, _>>()?;

                methods.push(Method {
                    name,
                    description,
                    args: MethodArgs::new(args),
                    return_type,
                });

                State::GetName
            }
            x => x,
        };
        state = new_state;
    }

    let mut schemas = indexmap![];
    for object in objects {
        let mut properties = indexmap![];
        let mut required = vec![];
        for field in object.fields {
            if let Required::Yes = field.required {
                required.push(field.name.clone());
            }

            let (name, ref_or_schema) = field.into_ref_or_schema();
            properties.insert(name, ref_or_schema);
        }

        schemas.insert(
            object.name,
            ReferenceOr::Item(Schema {
                schema_data: SchemaData {
                    description: Some(object.description),
                    ..SchemaData::default()
                },
                schema_kind: SchemaKind::Type(SchemaType::Object(ObjectType {
                    properties,
                    required,
                    ..ObjectType::default()
                })),
            }),
        );
    }

    let mut paths = indexmap![];
    for method in methods {
        let mut file_uploading = false;
        let has_args = !method.args.is_empty();

        let mut required = vec![];
        let mut properties = indexmap![];
        let mut content = indexmap![];

        match method.args {
            MethodArgs::No => {}
            MethodArgs::Yes(args) | MethodArgs::WithMultipart(args) => {
                for arg in args {
                    if arg.kind.maybe_file_to_send() {
                        file_uploading = true;
                    }

                    if let Required::Yes = arg.required {
                        required.push(arg.name.clone());
                    }

                    let ref_or_schema = match arg.kind.match_type() {
                        SchemaKindOrRef::Kind(schema_kind) => ReferenceOr::Item(Box::new(Schema {
                            schema_data: SchemaData {
                                description: Some(arg.description),
                                ..SchemaData::default()
                            },
                            schema_kind,
                        })),
                        SchemaKindOrRef::Ref(reference) => reference,
                    };
                    properties.insert(arg.name, ref_or_schema);
                }
            }
        }

        const FORM_URL_ENCODED: &str = "application/x-www-form-urlencoded";
        const JSON: &str = "application/json";
        const FORM_DATA: &str = "multipart/form-data";

        for content_type in [
            Some(FORM_URL_ENCODED),
            Some(FORM_DATA),
            Some(JSON).filter(|_| !file_uploading),
        ]
        .iter()
        .flatten()
        {
            content.insert(
                content_type.to_string(),
                MediaType {
                    schema: Some(ReferenceOr::Item(Schema {
                        schema_data: Default::default(),
                        schema_kind: SchemaKind::Type(SchemaType::Object(ObjectType {
                            properties: properties.clone(),
                            required: required.clone(),
                            ..ObjectType::default()
                        })),
                    })),
                    ..MediaType::default()
                },
            );
        }

        let mut success = success.clone();
        if let ReferenceOr::Item(item) = &mut success {
            if let SchemaKind::Type(SchemaType::Object(object)) = &mut item.schema_kind {
                object.properties.insert(
                    "result".to_string(),
                    method.return_type.match_type().into_ref_or_schema(),
                );
            }
        }

        let operation = Operation {
            description: Some(method.description),
            request_body: if has_args {
                Some(ReferenceOr::Item(RequestBody {
                    description: None,
                    content,
                    required: true,
                }))
            } else {
                None
            },
            responses: Responses {
                default: Some(ReferenceOr::Item(Response {
                    content: indexmap! {
                        JSON.to_string() => MediaType {
                            schema: Some(ReferenceOr::Reference { reference: "#/components/schemas/Error".to_string() }),
                            ..MediaType::default()
                        }
                    },
                    ..Response::default()
                })),
                responses: indexmap! {
                    StatusCode::Code(200) => ReferenceOr::Item(Response {
                        content: indexmap! {
                            JSON.to_string() => MediaType {
                                schema: Some(success),
                                ..MediaType::default()
                            }
                        },
                        ..Response::default()
                    }),
                },
            },
            ..Operation::default()
        };

        let item = PathItem {
            post: Some(operation),
            ..PathItem::default()
        };

        paths.insert(format!("/{}", method.name), ReferenceOr::Item(item));
    }

    api.info.version = recent_change.unwrap().format(&api.info.version).to_string();
    api.paths = paths;
    if let Some(components) = &mut api.components {
        components.schemas.extend(schemas)
    }

    let api = serde_yaml::to_string(&api)?;
    println!("{}", api);

    Ok(())
}
