use chrono::Datelike;
use schemars::{gen::SchemaGenerator, schema::RootSchema, schema_for, JsonSchema};
use serde::{Serialize, Serializer};
use tg_bot_api::{MethodArgs, Parsed, Type};

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
enum Kind {
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
        any_of: Vec<KindWrapper>,
    },
    Reference {
        reference: String,
    },
    Array {
        array: Box<KindWrapper>,
    },
}

// this type used to avoid recursion type
// because serde and schemars don't support such types
#[derive(Debug)]
struct KindWrapper(Kind);

impl Serialize for KindWrapper {
    fn serialize<S>(&self, serializer: S) -> Result<S::Ok, S::Error>
    where
        S: Serializer,
    {
        self.0.serialize(serializer)
    }
}

impl JsonSchema for KindWrapper {
    fn schema_name() -> String {
        Kind::schema_name()
    }

    fn json_schema(gen: &mut SchemaGenerator) -> schemars::schema::Schema {
        Kind::json_schema(gen)
    }
}

impl From<tg_bot_api::Type> for KindWrapper {
    fn from(ty: tg_bot_api::Type) -> Self {
        let base = match ty {
            Type::Integer { default, min, max } => Kind::Integer { default, min, max },
            Type::String { default, one_of } => Kind::String {
                default,
                enumeration: one_of,
            },
            Type::Bool { default } => Kind::Bool { default },
            Type::Float => Kind::Float,
            Type::Or(types) => Kind::AnyOf {
                any_of: types.into_iter().map(KindWrapper::from).collect(),
            },
            Type::Object(object) => Kind::Reference { reference: object },
            Type::Array(ty) => Kind::Array {
                array: Box::new(KindWrapper::from(*ty)),
            },
        };
        KindWrapper(base)
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
    return_type: KindWrapper,
    documentation_link: String,
}

impl From<tg_bot_api::Method> for Method {
    fn from(method: tg_bot_api::Method) -> Self {
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
            return_type: KindWrapper::from(method.return_type),
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
    kind: KindWrapper,
}

impl From<tg_bot_api::Argument> for Argument {
    fn from(arg: tg_bot_api::Argument) -> Self {
        Self {
            name: arg.name,
            description: arg.description,
            required: arg.required,
            kind: KindWrapper::from(arg.kind),
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

impl From<tg_bot_api::Object> for Object {
    fn from(object: tg_bot_api::Object) -> Self {
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

impl From<tg_bot_api::ObjectData> for ObjectData {
    fn from(object_data: tg_bot_api::ObjectData) -> Self {
        match object_data {
            tg_bot_api::ObjectData::Fields(fields) if !fields.is_empty() => {
                ObjectData::Known(KnownObjectData::Properties {
                    properties: fields.into_iter().map(Property::from).collect(),
                })
            }
            tg_bot_api::ObjectData::Fields(_) => ObjectData::Unknown {},
            tg_bot_api::ObjectData::Elements(types) => ObjectData::Known(KnownObjectData::AnyOf {
                any_of: types.into_iter().map(KindWrapper::from).collect(),
            }),
        }
    }
}

#[derive(Serialize, JsonSchema)]
#[serde(rename_all = "snake_case")]
#[serde(tag = "type")]
enum KnownObjectData {
    Properties { properties: Vec<Property> },
    AnyOf { any_of: Vec<KindWrapper> },
}

#[derive(Serialize, JsonSchema)]
struct Property {
    name: String,
    description: String,
    required: bool,
    #[serde(flatten)]
    kind: KindWrapper,
}

impl From<tg_bot_api::Field> for Property {
    fn from(field: tg_bot_api::Field) -> Self {
        Self {
            name: field.name,
            description: field.description,
            required: field.required,
            kind: KindWrapper::from(field.kind),
        }
    }
}
