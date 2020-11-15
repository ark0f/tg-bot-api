use crate::parser::{Argument, Field, MethodArgs, ObjectData, Parsed, Type as ParserType};
use indexmap::indexmap;
use openapiv3::{
    ArrayType, ExternalDocumentation, IntegerType, MediaType, NumberType, ObjectType, OpenAPI,
    Operation, PathItem, ReferenceOr, RequestBody, Response, Responses, Schema, SchemaData,
    SchemaKind, StatusCode, StringType, Type,
};
use serde_json::Value;

const BASE_SCHEMA: &str = include_str!("../base-schema.yml");
const FORM_URL_ENCODED: &str = "application/x-www-form-urlencoded";
const JSON: &str = "application/json";
const FORM_DATA: &str = "multipart/form-data";

#[derive(Debug, thiserror::Error)]
pub enum OpenApiError {
    #[error("YAML: {0}")]
    Yaml(
        #[from]
        #[source]
        serde_yaml::Error,
    ),
}

pub fn generate(parsed: Parsed) -> Result<OpenAPI, OpenApiError> {
    let mut api: OpenAPI = serde_yaml::from_str(BASE_SCHEMA)?;

    let success = api
        .components
        .as_mut()
        .unwrap()
        .schemas
        .remove("Success")
        .unwrap();

    let mut schemas = indexmap![];
    for object in parsed.objects {
        match object.data {
            ObjectData::Fields(fields) => {
                let mut properties = indexmap![];
                let mut required = vec![];
                for field in fields {
                    if field.required {
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
                            external_docs: Some(ExternalDocumentation {
                                description: None,
                                url: object.docs_link,
                            }),
                            ..SchemaData::default()
                        },
                        schema_kind: SchemaKind::Type(Type::Object(ObjectType {
                            properties,
                            required,
                            ..ObjectType::default()
                        })),
                    }),
                );
            }
            ObjectData::Elements(elements) => {
                let any_of = elements
                    .into_iter()
                    .map(|ty| ty.into_ref_or_schema(None))
                    .map(ReferenceOr::unbox)
                    .collect();

                schemas.insert(
                    object.name,
                    ReferenceOr::Item(Schema {
                        schema_data: SchemaData {
                            description: Some(object.description),
                            external_docs: Some(ExternalDocumentation {
                                description: None,
                                url: object.docs_link,
                            }),
                            ..SchemaData::default()
                        },
                        schema_kind: SchemaKind::AnyOf { any_of },
                    }),
                );
            }
        }
    }

    let mut paths = indexmap![];
    for method in parsed.methods {
        let mut file_uploading = false;
        let has_args;

        let mut required = vec![];
        let mut properties = indexmap![];
        let mut content = indexmap![];

        match method.args {
            MethodArgs::No => has_args = false,
            MethodArgs::Yes(args) | MethodArgs::WithMultipart(args) => {
                has_args = true;

                for arg in args {
                    if arg.kind.maybe_file_to_send() {
                        file_uploading = true;
                    }

                    if arg.required {
                        required.push(arg.name.clone());
                    }

                    let (name, ref_or_schema) = arg.into_ref_or_schema();
                    properties.insert(name, ref_or_schema);
                }
            }
        }

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
                        schema_kind: SchemaKind::Type(Type::Object(ObjectType {
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
            if let SchemaKind::Type(Type::Object(object)) = &mut item.schema_kind {
                object.properties.insert(
                    "result".to_string(),
                    method.return_type.into_ref_or_schema(None),
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
            external_documentation: Some(ExternalDocumentation {
                description: None,
                url: method.docs_link,
            }),
            ..Operation::default()
        };

        let item = PathItem {
            post: Some(operation),
            ..PathItem::default()
        };

        paths.insert(format!("/{}", method.name), ReferenceOr::Item(item));
    }

    api.info.version = parsed.version.to_string();
    api.paths = paths;
    if let Some(components) = &mut api.components {
        components.schemas.extend(schemas)
    }

    Ok(api)
}

trait TypeExt: Sized {
    fn into_ref_or_schema(self, description: Option<String>) -> ReferenceOr<Box<Schema>>;

    fn default(&self) -> Option<serde_json::Value>;
}

impl TypeExt for ParserType {
    fn into_ref_or_schema(self, description: Option<String>) -> ReferenceOr<Box<Schema>> {
        let default = self.default();
        let schema_kind = match self {
            this @ ParserType::Integer { .. }
            | this @ ParserType::String { .. }
            | this @ ParserType::Bool { .. }
            | this @ ParserType::Float
            | this @ ParserType::Array(_) => {
                let schema_type = match this {
                    ParserType::Integer { min, max, .. } => Type::Integer(IntegerType {
                        minimum: min,
                        maximum: max,
                        ..IntegerType::default()
                    }),
                    ParserType::String { one_of, .. } => Type::String(StringType {
                        enumeration: one_of,
                        ..StringType::default()
                    }),
                    ParserType::Bool { .. } => Type::Boolean {},
                    ParserType::Float => Type::Number(NumberType::default()),
                    ParserType::Array(array) => Type::Array(ArrayType {
                        items: array.into_ref_or_schema(None),
                        min_items: None,
                        max_items: None,
                        unique_items: false,
                    }),
                    _ => unreachable!(),
                };
                SchemaKind::Type(schema_type)
            }
            ParserType::Or(types) => SchemaKind::AnyOf {
                any_of: types
                    .into_iter()
                    .map(|ty| ty.into_ref_or_schema(None))
                    .map(ReferenceOr::unbox)
                    .collect(),
            },
            ParserType::Object(reference) => {
                return ReferenceOr::Reference {
                    reference: format!("#/components/schemas/{}", reference),
                }
            }
        };
        ReferenceOr::Item(Box::new(Schema {
            schema_data: SchemaData {
                description,
                default,
                ..SchemaData::default()
            },
            schema_kind,
        }))
    }

    fn default(&self) -> Option<Value> {
        match self {
            ParserType::Bool { default } => default.map(Into::into),
            ParserType::Integer { default, .. } => default.map(Into::into),
            ParserType::String { default, .. } => default.clone().map(Into::into),
            _ => None,
        }
    }
}

trait FieldExt: Sized {
    fn into_ref_or_schema(self) -> (String, ReferenceOr<Box<Schema>>);
}

impl FieldExt for Field {
    fn into_ref_or_schema(self) -> (String, ReferenceOr<Box<Schema>>) {
        let ref_or_schema = self.kind.into_ref_or_schema(Some(self.description));
        (self.name, ref_or_schema)
    }
}

trait ArgumentExt: Sized {
    fn into_ref_or_schema(self) -> (String, ReferenceOr<Box<Schema>>);
}

impl ArgumentExt for Argument {
    fn into_ref_or_schema(self) -> (String, ReferenceOr<Box<Schema>>) {
        let ref_or_schema = self.kind.into_ref_or_schema(Some(self.description));
        (self.name, ref_or_schema)
    }
}
