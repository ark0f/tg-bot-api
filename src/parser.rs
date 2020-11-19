use crate::{
    extractor::{
        Extracted, RawArgument, RawDescription, RawField, RawMethod, RawObject, RawObjectData,
    },
    util::{ElementRefExt, StrExt},
    BOT_API_DOCS_URL,
};
use chrono::NaiveDate;
use ego_tree::iter::Edge;
use html2md::{Handle, NodeData, StructuredPrinter, TagHandler, TagHandlerFactory};
use itertools::Itertools;
use scraper::{ElementRef, Node};
use semver::Version;
use std::{
    collections::HashMap, mem, num::ParseIntError, ops::Index, slice, slice::SliceIndex,
    str::ParseBoolError,
};

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
    let data = match raw_object.data {
        RawObjectData::Fields(fields) => {
            ObjectData::Fields(fields.into_iter().map(parse_field).collect::<Result<_>>()?)
        }
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
                    .strip_suffix("Array of ")
                    .unwrap_or(s)
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
        let parser = SentenceParser::new(description);
        let default = Self::custom_parse(
            DEFAULTS_PATTERNS,
            |sentence| Some(sentence.parts.get(0)?.inner.clone()),
            &parser,
        );

        let min_max = Self::custom_parse(
            MIN_MAX_PATTERNS,
            |sentence| {
                let values = &sentence.parts.get(0)?.inner;
                let mut split = values.split('-');
                let min = split.next()?.to_string();
                let max = split.next()?.to_string();
                Some((min, max))
            },
            &parser,
        );
        let (min, max) = if let Some((min, max)) = min_max {
            (Some(min), Some(max))
        } else {
            (None, None)
        };

        let one_of = Self::custom_parse(
            ONE_OF_PATTERNS,
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
            &parser,
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

    fn custom_parse<F, T>(patterns: &[&[&str]], extractor: F, parser: &SentenceParser) -> Option<T>
    where
        F: Fn(&SentenceRef) -> Option<T>,
    {
        patterns.iter().find_map(|&pattern| {
            let sentence = parser.find(pattern)?;
            let sentence = &sentence[pattern.len()..];
            extractor(sentence)
        })
    }

    pub fn extract_from_text(text: &str) -> Result<Self> {
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
                    .parts
                    .iter()
                    .filter(|part| !part.inner.is_first_letter_lowercase())
                    .map(SentenceRef::from_part)
                    .map(extract_type)
                    .collect::<Option<_>>()?;
                Some(Type::Or(types))
            } else {
                let (pos, part) = sentence
                    .parts
                    .iter()
                    .find_position(|part| !part.inner.is_first_letter_lowercase())?;
                let ty = part.inner.as_str();
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

        let parser = SentenceParser::new(text);
        Self::custom_parse(RETURN_TYPE_PATTERNS, extract_type, &parser)
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

#[derive(Debug)]
struct SentenceParser {
    sentences: Vec<Sentence>,
}

impl SentenceParser {
    fn new(text: &str) -> Self {
        const QUOTES: &[(char, char)] = &[('“', '”'), ('"', '"')];

        let mut sentences = vec![];
        let mut parts = vec![];
        let mut part = Part::default();

        let mut last_quote = None;
        let mut c = '\0';
        let mut chars = text.chars();

        enum State {
            GetNextChar,
            CheckQuotes,
            CheckWhitespace,
            CheckDot,
            CheckComma,
            PushChar,
            PushPart { and_sentence: bool },
            PushSentence,
            Break,
        }

        let mut state = State::GetNextChar;
        loop {
            let new_state = match state {
                State::GetNextChar => {
                    if let Some(new_c) = chars.next() {
                        c = new_c;
                        State::CheckQuotes
                    } else {
                        State::Break
                    }
                }
                State::CheckQuotes => {
                    if let Some(&(start_quote, _)) = QUOTES.iter().find(|&&(l, r)| l == c || r == c)
                    {
                        if last_quote == Some(start_quote) {
                            part.has_quotes = true;
                            last_quote = None;
                            State::PushPart {
                                and_sentence: false,
                            }
                        } else {
                            last_quote = Some(start_quote);
                            State::GetNextChar
                        }
                    } else {
                        State::CheckWhitespace
                    }
                }
                State::CheckWhitespace => {
                    if c.is_whitespace() && last_quote.is_none() {
                        State::PushPart {
                            and_sentence: false,
                        }
                    } else {
                        State::CheckDot
                    }
                }
                State::CheckDot => {
                    if c == '.' {
                        State::PushPart { and_sentence: true }
                    } else {
                        State::CheckComma
                    }
                }
                State::CheckComma => {
                    if c == ',' {
                        State::PushPart {
                            and_sentence: false,
                        }
                    } else {
                        State::PushChar
                    }
                }
                State::PushChar => {
                    part.inner.push(c);
                    State::GetNextChar
                }
                State::PushPart { and_sentence } => {
                    if !part.inner.is_empty() {
                        parts.push(mem::take(&mut part));
                    }

                    if and_sentence {
                        State::PushSentence
                    } else {
                        State::GetNextChar
                    }
                }
                State::PushSentence => {
                    sentences.push(Sentence {
                        parts: mem::take(&mut parts),
                    });
                    State::GetNextChar
                }
                State::Break => {
                    if !parts.is_empty() {
                        sentences.push(Sentence { parts });
                    }
                    break;
                }
            };
            state = new_state;
        }

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
    fn from_part(part: &Part) -> &Self {
        unsafe {
            &*(slice::from_raw_parts(part as *const Part, 1) as *const [Part] as *const SentenceRef)
        }
    }

    fn starts_with(&self, words: &[&str]) -> bool {
        self.parts
            .get(..words.len())
            .map(|slice| slice.iter().zip(words).all(|(l, &r)| l.inner == r))
            .unwrap_or(false)
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

trait RawDescriptionExt {
    fn markdown(&self) -> String;

    fn plain_text(&self) -> String;
}

impl RawDescriptionExt for RawDescription<'_> {
    fn markdown(&self) -> String {
        html2md::parse_html_custom(
            &self.0.iter().map(ElementRef::html).join("\n"),
            &AnchorHandlerFactory::new_in_map(),
        )
    }

    fn plain_text(&self) -> String {
        self.0.iter().map(ElementRef::plain_text).join("\n")
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
    format!("{}{}", BOT_API_DOCS_URL, fragment)
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

#[cfg(test)]
mod tests {
    use super::*;

    #[test]
    fn make_absolute_a_href() {
        let map = AnchorHandlerFactory::new_in_map();
        let md = html2md::parse_html_custom(r##"<a href="#fragment">This is a link</a>"##, &map);
        assert_eq!(
            md,
            format!("[This is a link]({}/#fragment)", BOT_API_DOCS_URL)
        )
    }

    #[test]
    fn sentence_parser_parts() {
        let parser = SentenceParser::new(
            r#"Emoji on which the dice throw animation is based.Currently, must be one of “🎲”, “🎯”, “🏀”, “⚽”, or “🎰”. Dice can have values 1-6 for “🎲” and “🎯”, values 1-5 for “🏀” and “⚽”, and values 1-64 for “🎰”. Defaults to “🎲”."#,
        );
        assert_eq!(parser.sentences.len(), 4);
        assert_eq!(parser.sentences[0].parts.len(), 9);
        assert_eq!(parser.sentences[1].parts.len(), 11);
        assert_eq!(parser.sentences[2].parts.len(), 20);
        assert_eq!(parser.sentences[3].parts.len(), 3);
    }

    #[test]
    fn sentence_parser_quotes() {
        let parser = SentenceParser::new(
            r#"The section of the user's Telegram Passport which has the issue, one of “passport”, “driver_license”, “identity_card”, “internal_passport”."#,
        );
        assert_eq!(
            parser
                .sentences
                .into_iter()
                .next()
                .unwrap()
                .parts
                .into_iter()
                .filter(|part| part.has_quotes)
                .map(|part| part.inner)
                .collect::<Vec<String>>(),
            vec![
                "passport",
                "driver_license",
                "identity_card",
                "internal_passport"
            ]
        );
    }
}
