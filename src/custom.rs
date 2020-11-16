use crate::parser;
use crate::parser::{MethodArgs, Parsed, Type};
use chrono::Datelike;
use schemars::schema::RootSchema;
use schemars::schema_for;
use schemars::JsonSchema;
use serde::Serialize;

pub fn generate(parsed: Parsed) -> (Schema, RootSchema) {
    let methods = parsed.methods.into_iter().map(Method::from).collect();
    let objects = parsed.objects.into_iter().map(Object::from).collect();

    (
        Schema {
            version: Version {
                major: parsed.version.major,
                minor: parsed.version.minor,
                patch: parsed.version.patch,
            },
            recent_changes: Date {
                year: parsed.recent_changes.year(),
                month: parsed.recent_changes.month(),
                day: parsed.recent_changes.day(),
            },
            methods,
            objects,
        },
        schema_for!(Schema),
    )
}

#[derive(Serialize, JsonSchema)]
pub struct Schema {
    version: Version,
    recent_changes: Date,
    methods: Vec<Method>,
    objects: Vec<Object>,
}

#[derive(Serialize, JsonSchema)]
struct Version {
    major: u64,
    minor: u64,
    patch: u64,
}

#[derive(Serialize, JsonSchema)]
struct Date {
    year: i32,
    month: u32,
    day: u32,
}

#[derive(Debug, Serialize, JsonSchema)]
#[serde(tag = "type")]
#[serde(rename_all = "snake_case")]
enum BaseKind {
    Integer {
        #[serde(skip_serializing_if = "Option::is_none")]
        default: Option<i64>,
        #[serde(skip_serializing_if = "Option::is_none")]
        min: Option<i64>,
        #[serde(skip_serializing_if = "Option::is_none")]
        max: Option<i64>,
    },
    String {
        #[serde(skip_serializing_if = "Option::is_none")]
        default: Option<String>,
        #[schemars(with = "Option<Vec<Argument>>")]
        #[serde(skip_serializing_if = "Vec::is_empty")]
        enumeration: Vec<String>,
    },
    Bool {
        #[serde(skip_serializing_if = "Option::is_none")]
        default: Option<bool>,
    },
    Float,
    AnyOf {
        #[schemars(with = "Option<Vec<Argument>>")]
        #[serde(skip_serializing_if = "Vec::is_empty")]
        any_of: Vec<Kind>,
    },
    Reference {
        reference: String,
    },
}

#[derive(Debug, Serialize, JsonSchema)]
#[serde(untagged)]
enum Kind {
    Base(BaseKind),
    Array(Box<BaseKind>),
}

impl From<parser::Type> for Kind {
    fn from(ty: parser::Type) -> Self {
        let base = match ty {
            Type::Integer { default, min, max } => BaseKind::Integer { default, min, max },
            Type::String { default, one_of } => BaseKind::String {
                default,
                enumeration: one_of,
            },
            Type::Bool { default } => BaseKind::Bool { default },
            Type::Float => BaseKind::Float,
            Type::Or(types) => BaseKind::AnyOf {
                any_of: types.into_iter().map(Kind::from).collect(),
            },
            Type::Object(object) => BaseKind::Reference { reference: object },
            Type::Array(ty) => {
                let kind = Kind::from(*ty);
                let base = match kind {
                    Kind::Base(base) => base,
                    Kind::Array(base) => panic!("Recursion type detected: {:?}", base),
                };
                return Kind::Array(Box::new(base));
            }
        };
        Kind::Base(base)
    }
}

#[derive(Serialize, JsonSchema)]
struct Method {
    name: String,
    description: String,
    #[schemars(with = "Option<Vec<Argument>>")]
    #[serde(skip_serializing_if = "Vec::is_empty")]
    arguments: Vec<Argument>,
    multipart_only: bool,
    return_type: Kind,
    documentation_link: String,
}

impl From<parser::Method> for Method {
    fn from(method: parser::Method) -> Self {
        let (multipart_only, args) = match method.args {
            MethodArgs::No => (false, vec![]),
            MethodArgs::Yes(args) => (false, args),
            MethodArgs::WithMultipart(args) => (true, args),
        };
        Self {
            name: method.name,
            description: method.description,
            arguments: args.into_iter().map(Argument::from).collect(),
            multipart_only,
            return_type: Kind::from(method.return_type),
            documentation_link: method.docs_link,
        }
    }
}

#[derive(Serialize, JsonSchema)]
struct Argument {
    name: String,
    description: String,
    required: bool,
    #[serde(flatten)]
    kind: Kind,
}

impl From<parser::Argument> for Argument {
    fn from(arg: parser::Argument) -> Self {
        Self {
            name: arg.name,
            description: arg.description,
            required: arg.required,
            kind: Kind::from(arg.kind),
        }
    }
}

#[derive(Serialize, JsonSchema)]
struct Object {
    name: String,
    description: String,
    #[serde(flatten)]
    data: ObjectData,
    documentation_link: String,
}

impl From<parser::Object> for Object {
    fn from(object: parser::Object) -> Self {
        Self {
            name: object.name,
            description: object.description,
            data: ObjectData::from(object.data),
            documentation_link: object.docs_link,
        }
    }
}

#[derive(Serialize, JsonSchema)]
#[serde(untagged)]
enum ObjectData {
    Known(KnownObjectData),
    Unknown {},
}

impl From<parser::ObjectData> for ObjectData {
    fn from(object_data: parser::ObjectData) -> Self {
        match object_data {
            parser::ObjectData::Fields(fields) if !fields.is_empty() => {
                ObjectData::Known(KnownObjectData::Properties {
                    properties: fields.into_iter().map(Property::from).collect(),
                })
            }
            parser::ObjectData::Fields(_) => ObjectData::Unknown {},
            parser::ObjectData::Elements(types) => ObjectData::Known(KnownObjectData::AnyOf {
                any_of: types.into_iter().map(Kind::from).collect(),
            }),
        }
    }
}

#[derive(Serialize, JsonSchema)]
#[serde(rename_all = "snake_case")]
#[serde(tag = "type")]
enum KnownObjectData {
    Properties { properties: Vec<Property> },
    AnyOf { any_of: Vec<Kind> },
}

#[derive(Serialize, JsonSchema)]
struct Property {
    name: String,
    description: String,
    required: bool,
    #[serde(flatten)]
    kind: Kind,
}

impl From<parser::Field> for Property {
    fn from(field: parser::Field) -> Self {
        Self {
            name: field.name,
            description: field.description,
            required: field.required,
            kind: Kind::from(field.kind),
        }
    }
}
