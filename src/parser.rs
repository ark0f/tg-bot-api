use crate::extractor::{Extracted, RawArgument, RawField, RawMethod, RawObject};
use crate::util::ElementRefExt;
use crate::util::StrExt;
use crate::BOT_API_DOCS;
use chrono::NaiveDate;
use ego_tree::iter::Edge;
use html2md::{Handle, NodeData, StructuredPrinter, TagHandler, TagHandlerFactory};
use itertools::Itertools;
use scraper::{ElementRef, Node};
use semver::Version;
use std::collections::HashMap;
use std::mem;
use std::num::ParseIntError;
use std::ops::Index;
use std::slice::SliceIndex;
use std::str::ParseBoolError;

/*const RETURN_TYPE_PATTERNS: &[(&str, &str)] = &[
    ("Returns ", " on success"),
    ("Returns ", " object."),
    ("An ", " is returned."),
    ("On success, ", " is returned."),
    ("On success, ", " object."),
    ("returns ", "."),
];
const DEFAULTS_PATTERNS: &[(&str, &str)] = &[
    ("Defaults to ", ","),
    ("Defaults to ", "."),
    ("Defaults to ", "”"),
];
const MIN_MAX_PATTERNS: &[(&str, &str)] = &[("Values between ", " are accepted.")];
const ONE_OF_PATTERNS: &[(&str, &str)] = &[("One of", "."), ("one of", ".")];*/
const RETURN_TYPE_PATTERNS: &[&[&str]] = &[&["On", "success"], &["Returns"], &["returns"], &["An"]];
const DEFAULTS_PATTERNS: &[&[&str]] = &[&["Defaults", "to"]];
const MIN_MAX_PATTERNS: &[&[&str]] = &[&["Values", "between"]];
const ONE_OF_PATTERNS: &[&[&str]] = &[&["One", "of"], &["one", "of"]];

type Result<T> = std::result::Result<T, ParseError>;

#[derive(Debug, thiserror::Error)]
pub enum ParseError {
    #[error("Invalid Required: {0}")]
    InvalidRequired(String),
    #[error("Failed to extract type from description: {0}")]
    TypeExtractionFailed(String),
    #[error("chrono: {0}")]
    ChronoParse(
        #[from]
        #[source]
        chrono::ParseError,
    ),
    #[error("Missing `href` attribute")]
    MissingHref,
    #[error("SemVer: {0}")]
    SemVer(
        #[from]
        #[source]
        semver::SemVerError,
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
        objects,
        methods,
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
    let fields = raw_object
        .fields
        .into_iter()
        .map(parse_field)
        .collect::<Result<_>>()?;
    let docs_link = raw_object.name.a_href().map(make_url_from_fragment)?;
    Ok(Object {
        name,
        description,
        fields,
        docs_link,
    })
}

fn parse_field(raw_field: RawField) -> Result<Field> {
    let plain_description = raw_field.description.plain_text();
    let required = !plain_description.starts_with("Optional.");
    let kind = Type::new_with_description(&raw_field.kind, &plain_description)?;

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
    let plain_description = raw_method.description.plain_text();
    let return_type = Type::extract_from_text(&plain_description)?;
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
    let plain_description = raw_arg.description.plain_text();
    let kind = Type::new_with_description(&raw_arg.kind, &plain_description)?;
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

#[derive(Debug)]
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
    },
    String {
        default: Option<String>,
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
        match s {
            "Integer" | "Int" => Self::Integer {
                default: None,
                min: None,
                max: None,
            },
            "String" => Self::String {
                default: None,
                one_of: vec![],
            },
            "Boolean" => Self::Bool { default: None },
            "True" => Self::Bool {
                default: Some(true),
            },
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

    fn new_with_description(s: &str, description: &str) -> Result<Self> {
        let default = Self::custom_parse(
            DEFAULTS_PATTERNS,
            // |sentence| Some(text.trim_quotes().to_string()),
            |sentence| Some(sentence.parts.get(0)?.inner.clone()),
            description,
        );

        let min_max = Self::custom_parse(
            MIN_MAX_PATTERNS,
            /*|text| {
                let mut split = text.split('-');
                let min = split.next()?.to_string();
                let max = split.next()?.to_string();
                Some((min, max))
            }*/
            |sentence| {
                let values = &sentence.parts.get(0)?.inner;
                let mut split = values.split('-');
                let min = split.next()?.to_string();
                let max = split.next()?.to_string();
                Some((min, max))
            },
            description,
        );
        let (min, max) = if let Some((min, max)) = min_max {
            (Some(min), Some(max))
        } else {
            (None, None)
        };

        let one_of = Self::custom_parse(
            ONE_OF_PATTERNS,
            /*|text| {
                Some(
                    SentenceParser::new(text).sentences[0]
                        .parts
                        .iter()
                        .filter(|part| part.has_quotes)
                        .map(|part| &part.inner)
                        .cloned()
                        .collect(),
                )
            },*/
            |sentence| {
                Some(
                    sentence
                        .parts
                        .iter()
                        .filter(|part| part.has_quotes)
                        .map(|part| &part.inner)
                        .cloned()
                        .collect(),
                )
            },
            description,
        );

        let ty = match Type::new(s) {
            Type::Integer { .. } => Type::Integer {
                default: default.as_deref().map(str::parse).transpose()?,
                min: min.as_deref().map(str::parse).transpose()?,
                max: max.as_deref().map(str::parse).transpose()?,
            },
            Type::Bool { .. } => Type::Bool {
                default: default.as_deref().map(str::parse).transpose()?,
            },
            Type::String { .. } if one_of.is_some() => Type::String {
                default,
                one_of: one_of.unwrap(),
            },
            x => x,
        };

        Ok(ty)
    }

    fn custom_parse<F, T>(patterns: &[&[&str]], extractor: F, text: &str) -> Option<T>
    where
        F: Fn(&SentenceRef) -> Option<T>,
    {
        patterns.iter().find_map(|&pattern| {
            let parser = SentenceParser::new(text);
            let sentence = parser.find(pattern)?;
            let sentence = &sentence[pattern.len()..];
            extractor(sentence)
        })

        /*fn get_range(text: &str, start: &str, end: &str) -> Option<Range<usize>> {
            let start = text.find(start)? + start.len();
            let end = text[start..].find(end)? + start;
            Some(start..end)
        }

        patterns.iter().find_map(|(start, end)| {
            let range = get_range(text, start, end)?;
            let text = &text[range];
            extractor(text)
        })*/
    }

    pub fn extract_from_text(text: &str) -> Result<Self> {
        /*fn extract_type(text: &str) -> Option<Type> {
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
                    || text[pos.saturating_sub(AN_ARRAY_OF.len())..].starts_with(AN_ARRAY_OF)
                {
                    let text = &text[pos + ARRAY.len()..];
                    let ty = extract_type(text)?;
                    Some(Type::Array(Box::new(ty)))
                } else {
                    Some(Type::new(ty))
                }
            }
        }*/

        fn extract_type(sentence: &SentenceRef) -> Option<Type> {
            const ARRAY: &str = "Array";
            const AN_ARRAY_OF: &[&str] = &["an", "array", "of"];
            const OTHERWISE: &[&str] = &["otherwise"];

            if sentence.contains(OTHERWISE) {
                /*let types = sentence
                .parts
                .iter()
                .map(|part| part.inner.as_str())
                .map(SentenceParser::new)
                .filter(|parser| parser.sentences.is_empty())
                .flat_map(|parser| parser.sentences)
                .map(|sentence| extract_type(sentence.as_ref()))
                .collect::<Option<_>>()?;*/
                let types = sentence
                    .parts
                    .iter()
                    .map(|part| &part.inner)
                    .filter(|s| !s.is_first_letter_lowercase())
                    .map(String::as_str)
                    .map(|s| dbg!(s))
                    .map(Type::new)
                    .collect();
                Some(Type::Or(types))
            } else {
                let (pos, part) = sentence
                    .parts
                    .iter()
                    .find_position(|part| !part.inner.is_first_letter_lowercase())?;
                let mut ty = part.inner.as_str();

                // get rid of plural ending
                if ty.ends_with("es") {
                    ty = ty.trim_end_matches('s');
                }

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

        Self::custom_parse(RETURN_TYPE_PATTERNS, extract_type, text)
            .ok_or_else(|| ParseError::TypeExtractionFailed(text.to_string()))
    }

    pub fn maybe_file_to_send(&self) -> bool {
        match self {
            Type::Integer { .. } | Type::String { .. } | Type::Bool { .. } | Type::Float => false,
            Type::Or(types) => types.iter().any(Self::maybe_file_to_send),
            Type::Array(ty) => ty.maybe_file_to_send(),
            Type::Object(object) => object.starts_with("Input"),
        }
    }
}

#[derive(Debug, Clone)]
pub struct Field {
    pub name: String,
    pub kind: Type,
    pub required: bool,
    pub description: String,
}

#[derive(Debug)]
pub struct Object {
    pub name: String,
    pub description: String,
    pub fields: Vec<Field>,
    pub docs_link: String,
}

#[derive(Debug, Clone)]
pub struct Argument {
    pub name: String,
    pub kind: Type,
    pub required: bool,
    pub description: String,
}

#[derive(Debug)]
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

#[derive(Debug)]
pub struct Method {
    pub name: String,
    pub description: String,
    pub args: MethodArgs,
    pub return_type: Type,
    pub docs_link: String,
}

#[derive(Debug)]
struct SentenceParser {
    sentences: Vec<Sentence>,
}

impl SentenceParser {
    fn new(sentence: &str) -> Self {
        const QUOTES: &[(char, char)] = &[('“', '”'), ('"', '"')];

        let mut sentences = vec![];
        let mut parts = vec![];
        let mut part = Part::default();

        let mut last_quote = None;

        for c in sentence.chars() {
            if let Some(&(start_quote, _)) = QUOTES.iter().find(|&&(l, r)| l == c || r == c) {
                if last_quote == Some(start_quote) {
                    part.has_quotes = true;
                    parts.push(mem::take(&mut part));
                    last_quote = None;
                } else {
                    last_quote = Some(start_quote);
                }
            } else if c.is_whitespace() && last_quote.is_none() {
                if !part.inner.is_empty() {
                    parts.push(mem::take(&mut part));
                }
            } else if c == '.' {
                if !part.inner.is_empty() {
                    parts.push(mem::take(&mut part));
                }
                sentences.push(Sentence {
                    parts: mem::take(&mut parts),
                });
            } else if c == ',' {
                if !part.inner.is_empty() {
                    parts.push(mem::take(&mut part));
                }
            } else {
                part.inner.push(c);
            }
        }

        sentences.push(Sentence { parts });

        Self { sentences }
    }

    fn find(&self, words: &[&str]) -> Option<&SentenceRef> {
        self.sentences.iter().find_map(|sentence| {
            if sentence
                .parts
                .windows(words.len())
                .any(|window| window == words)
            {
                Some(sentence.as_ref())
            } else {
                None
            }
        })
    }
}

#[derive(Debug, Clone, Default)]
struct Part {
    inner: String,
    has_quotes: bool,
}

impl PartialEq<&str> for Part {
    fn eq(&self, other: &&str) -> bool {
        self.inner == *other
    }
}

#[derive(Debug)]
struct Sentence {
    parts: Vec<Part>,
}

impl<I> Index<I> for Sentence
where
    I: SliceIndex<[Part], Output = [Part]>,
{
    type Output = SentenceRef;

    fn index(&self, index: I) -> &Self::Output {
        &self.as_ref()[index]
    }
}

impl AsRef<SentenceRef> for Sentence {
    fn as_ref(&self) -> &SentenceRef {
        unsafe { &*(self.parts.as_slice() as *const [Part] as *const SentenceRef) }
    }
}

#[derive(Debug)]
struct SentenceRef {
    parts: [Part],
}

impl SentenceRef {
    fn starts_with(&self, words: &[&str]) -> bool {
        self.parts[..words.len()]
            .iter()
            .zip(words)
            .all(|(l, &r)| l.inner == r)
    }

    fn contains(&self, words: &[&str]) -> bool {
        self.parts.windows(words.len()).any(|parts| parts == words)
    }
}

impl<I> Index<I> for SentenceRef
where
    I: SliceIndex<[Part], Output = [Part]>,
{
    type Output = SentenceRef;

    fn index(&self, index: I) -> &Self::Output {
        unsafe { &*(&self.parts[index] as *const [Part] as *const SentenceRef) }
    }
}

trait ElementRefParserExt {
    fn markdown(&self) -> String;

    fn a_href(&self) -> Result<String>;
}

impl ElementRefParserExt for ElementRef<'_> {
    fn markdown(&self) -> String {
        html2md::parse_html_custom(&self.html(), &AnchorHandlerFactory::new_in_map())
    }

    fn a_href(&self) -> Result<String> {
        for edge in self.traverse() {
            if let Edge::Open(node) = edge {
                if let Node::Element(elem) = node.value() {
                    if elem.name() == "a" {
                        return elem
                            .attr("href")
                            .map(str::to_string)
                            .ok_or(ParseError::MissingHref);
                    }
                }
            }
        }

        Err(ParseError::MissingHref)
    }
}

struct AnchorHandlerFactory;

impl AnchorHandlerFactory {
    fn new_in_map() -> HashMap<String, Box<dyn TagHandlerFactory>> {
        let mut map = HashMap::new();
        map.insert("a".to_string(), Box::new(AnchorHandlerFactory) as _);
        map
    }
}

impl TagHandlerFactory for AnchorHandlerFactory {
    fn instantiate(&self) -> Box<dyn TagHandler> {
        Box::new(AnchorHandler::default())
    }
}

fn make_url_from_fragment(fragment: String) -> String {
    assert!(fragment.starts_with('#'));
    format!("{}{}", BOT_API_DOCS, fragment)
}

#[derive(Default)]
struct AnchorHandler {
    inner: Option<(usize, String)>,
}

impl TagHandler for AnchorHandler {
    fn handle(&mut self, tag: &Handle, printer: &mut StructuredPrinter) {
        if let NodeData::Element { attrs, .. } = &tag.data {
            if let Some(attr) = attrs
                .borrow()
                .iter()
                .find(|attr| attr.name.local.as_ref() == "href")
            {
                let pos = printer.data.len();
                let value = attr.value.to_string();
                let value = if value.starts_with('#') {
                    make_url_from_fragment(value)
                } else {
                    value
                };
                self.inner = Some((pos, value));
            }
        }
    }

    fn after_handle(&mut self, printer: &mut StructuredPrinter) {
        let (pos, value) = self.inner.as_ref().unwrap();
        printer.insert_str(*pos, "[");
        printer.append_str(&format!("]({})", value));
    }
}

trait StrParserExt<'a> {
    fn trim_quotes(self) -> &'a str;
}

impl<'a> StrParserExt<'a> for &'a str {
    fn trim_quotes(self) -> &'a str {
        const QUOTES: &[char] = &['“', '”', '"'];
        self.trim_matches(QUOTES)
    }
}

#[cfg(test)]
mod tests {
    use super::*;

    #[test]
    fn make_absolute_a_href() {
        let map = AnchorHandlerFactory::new_in_map();
        let md = html2md::parse_html_custom(r##"<a href="#fragment">This is a link</a>"##, &map);
        assert_eq!(md, format!("[This is a link]({}/#fragment)", BOT_API_DOCS))
    }

    #[test]
    fn sentence_parser() {
        let parser = dbg!(SentenceParser::new(
            r#"Emoji on which the dice throw animation is based.Currently, must be one of “🎲”, “🎯”, “🏀”, “⚽”, or “🎰”. Dice can have values 1-6 for “🎲” and “🎯”, values 1-5 for “🏀” and “⚽”, and values 1-64 for “🎰”. Defaults to “🎲”. The section of the user's Telegram Passport which has the issue, one of “passport”, “driver_license”, “identity_card”, “internal_passport”"#,
        ));
        let sentence = parser.find(&["one", "of"]).unwrap();
        let emojis = sentence.remove_words(&[",", "or"])[5..]
            .parts
            .iter()
            .map(|part| &part.inner)
            .cloned()
            .collect::<Vec<String>>();
        assert_eq!(emojis, vec!["🎲", "🎯", "🏀", "⚽", "🎰"]);
    }
}
