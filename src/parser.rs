mod sentence;
mod tags;

use crate::{
    extractor::{
        Extracted, RawArgument, RawDescription, RawField, RawMethod, RawObject, RawObjectData,
    },
    parser::sentence::Sentence,
    util::{ElementRefExt, StrExt},
    BOT_API_DOCS_URL,
};
use chrono::NaiveDate;
use ego_tree::iter::Edge;
use itertools::Itertools;
use logos::Span;
use scraper::{node::Element, ElementRef, Node};
use semver::Version;
use sentence::{Pattern, SentenceRef, Sentences};
use std::{num::ParseIntError, ops::Deref, str::ParseBoolError};
use tags::TagsHandlerFactory;

type Result<T, E = ParseError> = std::result::Result<T, E>;

#[derive(Debug, thiserror::Error)]
pub enum ParseError {
    #[error("Invalid Required: {0}")]
    InvalidRequired(String),
    #[error("Failed to extract type from description: {0:?}")]
    TypeExtractionFailed(String),
    #[error("chrono: {0}")]
    ChronoParse(
        #[from]
        #[source]
        chrono::ParseError,
    ),
    #[error("Missing `href` attribute")]
    MissingHref,
    #[error("Missing `alt` attribute")]
    MissingAlt,
    #[error("SemVer: {0}")]
    SemVer(
        #[from]
        #[source]
        semver::Error,
    ),
    #[error("Integer parsing: {0}")]
    ParseInt(
        #[from]
        #[source]
        ParseIntError,
    ),
    #[error("Boolean parsing: {0}")]
    ParseBool(
        #[from]
        #[source]
        ParseBoolError,
    ),
    #[error("Lexer error: {lexeme:?} ({span:?}) in {input:?}")]
    Lexer {
        input: String,
        lexeme: String,
        span: Span,
    },
}

pub fn parse(raw: Extracted) -> Result<Parsed> {
    let recent_changes = NaiveDate::parse_from_str(&raw.recent_changes, "%B %e, %Y")?;
    let version = parse_version(raw.version)?;
    let objects = raw
        .objects
        .into_iter()
        .map(parse_object)
        .collect::<Result<_>>()?;
    let methods = raw
        .methods
        .into_iter()
        .map(parse_method)
        .collect::<Result<_>>()?;

    Ok(Parsed {
        recent_changes,
        version,
        methods,
        objects,
    })
}

fn parse_version(version: ElementRef) -> Result<Version> {
    let version = version
        .plain_text()
        .chars()
        .skip_while(|c| !c.is_ascii_digit())
        .collect::<String>()
        .trim_end_matches('.')
        .to_string()
        + ".0";
    Ok(Version::parse(&version)?)
}

fn parse_object(raw_object: RawObject) -> Result<Object> {
    let name = raw_object.name.plain_text();
    let description = raw_object.description.markdown();
    let data = match raw_object.data {
        RawObjectData::Fields(fields) if !fields.is_empty() => {
            ObjectData::Fields(fields.into_iter().map(parse_field).collect::<Result<_>>()?)
        }
        RawObjectData::Fields(_) => ObjectData::Unknown,
        RawObjectData::Elements(elements) => ObjectData::Elements(
            elements
                .into_iter()
                .map(|elem| elem.plain_text())
                .map(|s| Type::new(&s))
                .collect(),
        ),
    };
    let docs_link = raw_object.name.a_href().map(make_url_from_fragment)?;
    Ok(Object {
        name,
        description,
        data,
        docs_link,
    })
}

fn parse_field(raw_field: RawField) -> Result<Field> {
    let plain_description = raw_field.description.plain_text();
    let required = !plain_description.starts_with("Optional.");
    let kind = Type::new_with_description(
        &raw_field.kind,
        TypeParsingUnit::Element(&raw_field.description),
    )?;

    Ok(Field {
        name: raw_field.name,
        kind,
        required,
        description: raw_field.description.markdown(),
    })
}

fn parse_method(raw_method: RawMethod) -> Result<Method> {
    let name = raw_method.name.plain_text();
    let docs_link = raw_method.name.a_href().map(make_url_from_fragment)?;
    let return_type =
        Type::extract_from_text(TypeParsingUnit::Description(&raw_method.description))?;
    let args = raw_method
        .args
        .into_iter()
        .map(parse_argument)
        .collect::<Result<_>>()?;
    Ok(Method {
        name,
        description: raw_method.description.markdown(),
        args: MethodArgs::new(args),
        return_type,
        docs_link,
    })
}

fn parse_argument(raw_arg: RawArgument) -> Result<Argument> {
    let kind = Type::new_with_description(
        &raw_arg.kind,
        TypeParsingUnit::Element(&raw_arg.description),
    )?;
    let required = parse_required(raw_arg.required)?;
    Ok(Argument {
        name: raw_arg.name,
        kind,
        required,
        description: raw_arg.description.markdown(),
    })
}

fn parse_required(s: String) -> Result<bool> {
    match s.as_str() {
        "Yes" => Ok(true),
        "Optional" => Ok(false),
        _ => Err(ParseError::InvalidRequired(s)),
    }
}

#[derive(Debug, Clone)]
pub struct Parsed {
    pub recent_changes: NaiveDate,
    pub version: Version,
    pub methods: Vec<Method>,
    pub objects: Vec<Object>,
}

#[derive(Debug, Clone, Eq, PartialEq)]
pub enum Type {
    Integer {
        default: Option<i64>,
        min: Option<i64>,
        max: Option<i64>,
        one_of: Vec<i64>,
    },
    String {
        default: Option<String>,
        min_len: Option<u64>,
        max_len: Option<u64>,
        one_of: Vec<String>,
    },
    Bool {
        default: Option<bool>,
    },
    Float,
    Or(Vec<Type>),
    Array(Box<Type>),
    Object(String),
}

impl Type {
    // this function parses types from `Type` column in docs
    fn new(s: &str) -> Self {
        const ARRAY_OF: &[&str] = &["Array", "of"];

        fn types_from_sentence_ref(sentence: &SentenceRef) -> Vec<Type> {
            sentence
                .parts()
                .iter()
                .filter(|part| !part.as_inner().is_first_letter_lowercase())
                .map(|part| part.as_inner().as_str())
                .map(Type::new)
                .collect()
        }

        match s {
            "Integer" | "Int" => Self::Integer {
                default: None,
                min: None,
                max: None,
                one_of: vec![],
            },
            "String" => Self::String {
                default: None,
                min_len: None,
                max_len: None,
                one_of: vec![],
            },
            "Boolean" => Self::Bool { default: None },
            "True" => Self::Bool {
                default: Some(true),
            },
            "Float" | "Float number" => Self::Float,
            _ => {
                let parser = Sentences::parse(s);
                if let Some(sentence) = parser.find(&["or"]) {
                    let types = types_from_sentence_ref(sentence);
                    Self::Or(types)
                } else if let Some(sentence) = parser.find_and_crop(ARRAY_OF) {
                    let sentence = &sentence[2..];
                    let ty = if sentence.len() == 1 {
                        Self::new(sentence.parts()[0].as_inner())
                    } else if sentence.starts_with(ARRAY_OF) {
                        Self::new(
                            &sentence
                                .parts()
                                .iter()
                                .map(|part| part.as_inner())
                                .join(" "),
                        )
                    } else {
                        Self::Or(types_from_sentence_ref(sentence))
                    };
                    Self::Array(Box::new(ty))
                } else {
                    Self::Object(s.to_string())
                }
            }
        }
    }

    fn new_with_description(s: &str, description: TypeParsingUnit) -> Result<Self> {
        let default = sentence::parse_type_custom(Pattern::Default, description, |sentence| {
            sentence.parts().first().map(|part| part.as_inner().clone())
        })?;
        let min_max = sentence::parse_type_custom(Pattern::MinMax, description, |sentence| {
            let values = sentence.parts().first()?.as_inner();
            let mut split = values.split('-');
            let min = split.next()?.to_string();
            let max = split.next()?.to_string();
            Some((min, max))
        })?;
        let one_of = sentence::parse_type_custom(Pattern::OneOf, description, |sentence| {
            Some(
                sentence
                    .parts()
                    .iter()
                    .filter(|part| {
                        part.has_quotes()
                            || part.is_italic()
                            || part.as_inner().chars().all(|c| c.is_ascii_digit())
                    })
                    .map(|part| part.as_inner())
                    .cloned()
                    .dedup()
                    .collect::<Vec<_>>(),
            )
        })?;

        let (min, max) = if let Some((min, max)) = min_max {
            (Some(min), Some(max))
        } else {
            (None, None)
        };

        let ty = match Type::new(s) {
            Type::Integer {
                default: type_default,
                min: type_min,
                max: type_max,
                one_of: type_one_of,
            } => {
                let one_of = if let Some(one_of) = one_of {
                    one_of
                        .into_iter()
                        .map(|x| x.parse::<i64>())
                        .collect::<Result<_, ParseIntError>>()?
                } else {
                    type_one_of
                };

                Type::Integer {
                    default: default
                        .as_deref()
                        .map(str::parse)
                        .transpose()?
                        .or(type_default),
                    min: min.as_deref().map(str::parse).transpose()?.or(type_min),
                    max: max.as_deref().map(str::parse).transpose()?.or(type_max),
                    one_of,
                }
            }
            Type::Bool {
                default: type_default,
            } => Type::Bool {
                default: default
                    .as_deref()
                    .map(str::to_lowercase)
                    .as_deref()
                    .map(str::parse)
                    .transpose()?
                    .or(type_default),
            },
            Type::String {
                default: type_default,
                min_len: type_min_len,
                max_len: type_max_len,
                one_of: type_one_if,
            } if default.is_some() || min.is_some() || max.is_some() || one_of.is_some() => {
                Type::String {
                    default: default.or(type_default),
                    min_len: min.as_deref().map(str::parse).transpose()?.or(type_min_len),
                    max_len: max.as_deref().map(str::parse).transpose()?.or(type_max_len),
                    one_of: one_of.unwrap_or(type_one_if),
                }
            }
            x => x,
        };

        Ok(ty)
    }

    pub fn extract_from_text(text: TypeParsingUnit) -> Result<Self> {
        fn strip_plural_ending(mut s: &str) -> &str {
            if s.ends_with("es") {
                s = s.strip_suffix('s').unwrap_or(s);
            }

            s
        }

        fn extract_type(sentence: &SentenceRef) -> Option<Type> {
            const ARRAY: &str = "Array";
            const AN_ARRAY_OF: &[&str] = &["an", "array", "of"];
            const OTHERWISE: &[&str] = &["otherwise"];

            if sentence.contains(OTHERWISE) {
                let types = sentence
                    .parts()
                    .iter()
                    .filter(|part| !part.as_inner().is_first_letter_lowercase())
                    .map(SentenceRef::from_part)
                    .map(extract_type)
                    .collect::<Option<_>>()?;
                Some(Type::Or(types))
            } else {
                let (pos, part) = sentence
                    .parts()
                    .iter()
                    .find_position(|part| !part.as_inner().is_first_letter_lowercase())?;
                let ty = part.as_inner();
                let ty = strip_plural_ending(ty);

                if ty == ARRAY {
                    let sentence = &sentence[pos + 1..];
                    let ty = extract_type(sentence)?;
                    Some(Type::Array(Box::new(ty)))
                } else if sentence[pos.saturating_sub(AN_ARRAY_OF.len())..].starts_with(AN_ARRAY_OF)
                {
                    let sentence = &sentence[pos..];
                    let ty = extract_type(sentence)?;
                    Some(Type::Array(Box::new(ty)))
                } else {
                    Some(Type::new(ty))
                }
            }
        }

        sentence::parse_type_custom(Pattern::ReturnType, text, extract_type)
            .transpose()
            .ok_or_else(|| ParseError::TypeExtractionFailed(text.plain_text()))?
    }

    pub fn maybe_file_to_send(&self) -> bool {
        match self {
            Type::Integer { .. } | Type::String { .. } | Type::Bool { .. } | Type::Float => false,
            Type::Or(types) => types.iter().any(Self::maybe_file_to_send),
            Type::Array(ty) => ty.maybe_file_to_send(),
            // Kinda bad, but the alternative is hardcoding every value
            Type::Object(object) => object.starts_with("Input") && object != "InputPollOption",
        }
    }
}

#[derive(Debug, Copy, Clone)]
pub enum TypeParsingUnit<'a> {
    Element(&'a ElementRef<'a>),
    Description(&'a RawDescription<'a>),
}

impl TypeParsingUnit<'_> {
    fn sentences(self) -> Result<Vec<Sentence>> {
        match self {
            TypeParsingUnit::Element(elem) => elem.sentences(),
            TypeParsingUnit::Description(description) => description.sentences(),
        }
    }

    fn plain_text(self) -> String {
        match self {
            TypeParsingUnit::Element(elem) => elem.plain_text(),
            TypeParsingUnit::Description(description) => description.plain_text(),
        }
    }
}

#[derive(Debug, Clone)]
pub struct Object {
    pub name: String,
    pub description: String,
    pub data: ObjectData,
    pub docs_link: String,
}

#[derive(Debug, Clone)]
pub enum ObjectData {
    Fields(Vec<Field>),
    Elements(Vec<Type>),
    /// Object without fields or elements
    /// So we don't know what it will be in the future
    Unknown,
}

#[derive(Debug, Clone)]
pub struct Field {
    pub name: String,
    pub kind: Type,
    pub required: bool,
    pub description: String,
}

#[derive(Debug, Clone)]
pub struct Method {
    pub name: String,
    pub description: String,
    pub args: MethodArgs,
    pub return_type: Type,
    pub docs_link: String,
}

#[derive(Debug, Clone)]
pub enum MethodArgs {
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
}

#[derive(Debug, Clone)]
pub struct Argument {
    pub name: String,
    pub kind: Type,
    pub required: bool,
    pub description: String,
}

fn make_url_from_fragment(fragment: String) -> String {
    assert!(fragment.starts_with('#'));
    format!("{}{}", BOT_API_DOCS_URL, fragment)
}

trait RawDescriptionExt {
    fn sentences(&self) -> Result<Vec<Sentence>>;

    fn markdown(&self) -> String;

    fn plain_text(&self) -> String;
}

impl RawDescriptionExt for RawDescription<'_> {
    fn sentences(&self) -> Result<Vec<Sentence>> {
        self.0
            .iter()
            .map(ElementRef::sentences)
            .try_fold(Vec::new(), |mut acc, x| {
                acc.extend(x?);
                Ok(acc)
            })
    }

    fn markdown(&self) -> String {
        html2md::parse_html_custom(
            &self.0.iter().map(ElementRef::html).join("\n"),
            &TagsHandlerFactory::new_in_map(),
        )
    }

    fn plain_text(&self) -> String {
        self.0.iter().map(ElementRef::plain_text).join("\n")
    }
}

trait ElementRefParserExt {
    fn sentences(&self) -> Result<Vec<Sentence>>;

    fn markdown(&self) -> String;

    fn a_href(&self) -> Result<String>;
}

impl ElementRefParserExt for ElementRef<'_> {
    fn sentences(&self) -> Result<Vec<Sentence>> {
        sentence::parse_node(*self.deref())
    }

    fn markdown(&self) -> String {
        html2md::parse_html_custom(&self.html(), &TagsHandlerFactory::new_in_map())
    }

    fn a_href(&self) -> Result<String> {
        for edge in self.traverse() {
            if let Edge::Open(node) = edge {
                if let Node::Element(elem) = node.value() {
                    if elem.name() == "a" {
                        return elem.a_href();
                    }
                }
            }
        }

        Err(ParseError::MissingHref)
    }
}

trait ElementExt {
    fn a_href(&self) -> Result<String>;
}

impl ElementExt for Element {
    fn a_href(&self) -> Result<String> {
        self.attr("href")
            .map(str::to_string)
            .ok_or(ParseError::MissingHref)
    }
}

#[cfg(test)]
mod tests {
    use super::*;

    #[test]
    fn or_type() {
        let ty = Type::new("Integer or String");
        assert_eq!(
            ty,
            Type::Or(vec![
                Type::Integer {
                    default: None,
                    min: None,
                    max: None,
                    one_of: vec![],
                },
                Type::String {
                    default: None,
                    min_len: None,
                    max_len: None,
                    one_of: vec![]
                }
            ])
        )
    }

    #[test]
    fn array_of_type() {
        let ty = Type::new("Array of PhotoSize");
        assert_eq!(
            ty,
            Type::Array(Box::new(Type::Object("PhotoSize".to_string())))
        );
    }

    #[test]
    fn array_of_array_type() {
        let ty = Type::new("Array of Array of PhotoSize");
        assert_eq!(
            ty,
            Type::Array(Box::new(Type::Array(Box::new(Type::Object(
                "PhotoSize".to_string()
            )))))
        );
    }
}
