use crate::{
    extractor::{
        Extracted, RawArgument, RawDescription, RawField, RawMethod, RawObject, RawObjectData,
    },
    util::{ElementRefExt, StrExt},
    BOT_API_DOCS_URL,
};
use chrono::NaiveDate;
use ego_tree::iter::Edge;
use html2md::{common::get_tag_attr, Handle, StructuredPrinter, TagHandler, TagHandlerFactory};
use itertools::Itertools;
use logos::Logos;
use percent_encoding::{utf8_percent_encode, AsciiSet, CONTROLS};
use scraper::{ElementRef, Node};
use semver::Version;
use std::{
    collections::HashMap, mem, num::ParseIntError, ops::Index, slice, slice::SliceIndex,
    str::ParseBoolError,
};

type Result<T> = std::result::Result<T, ParseError>;

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
        ElementOrDescription::Element(&raw_field.description),
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
        Type::extract_from_text(ElementOrDescription::Description(&raw_method.description))?;
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
        ElementOrDescription::Element(&raw_arg.description),
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
        const ARRAY_OF: &[&str] = &["Array", "of"];

        fn types_from_sentence_ref(sentence: &SentenceRef) -> Vec<Type> {
            sentence
                .parts
                .iter()
                .filter(|part| !part.inner.is_first_letter_lowercase())
                .map(|part| part.inner.as_str())
                .map(Type::new)
                .collect()
        }

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
            _ => {
                let parser = Sentences::parse(s);
                if let Some(sentence) = parser.find(&["or"]) {
                    let types = types_from_sentence_ref(sentence);
                    Self::Or(types)
                } else if let Some(sentence) = parser.find(ARRAY_OF) {
                    let sentence = &sentence[2..];
                    let ty = if sentence.len() == 1 {
                        Self::new(&sentence.parts[0].inner)
                    } else if sentence.starts_with(ARRAY_OF) {
                        Self::new(
                            &sentence
                                .parts
                                .iter()
                                .map(|part| part.inner.as_str())
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

    fn new_with_description(s: &str, description: ElementOrDescription) -> Result<Self> {
        let default = Self::custom_parse(Pattern::Default, description, |sentence| {
            sentence.parts.get(0).map(|part| part.inner.clone())
        })?;
        let min_max = Self::custom_parse(Pattern::MinMax, description, |sentence| {
            let values = &sentence.parts.get(0)?.inner;
            let mut split = values.split('-');
            let min = split.next()?.to_string();
            let max = split.next()?.to_string();
            Some((min, max))
        })?;
        let one_of = Self::custom_parse(Pattern::OneOf, description, |sentence| {
            Some(
                sentence
                    .parts
                    .iter()
                    .filter(|part| part.has_quotes)
                    .map(|part| &part.inner)
                    .cloned()
                    .collect(),
            )
        })?;

        let (min, max) = if let Some((min, max)) = min_max {
            (Some(min), Some(max))
        } else {
            (None, None)
        };

        let ty = match Type::new(s) {
            Type::Integer { .. } => Type::Integer {
                default: default.as_deref().map(str::parse).transpose()?,
                min: min.as_deref().map(str::parse).transpose()?,
                max: max.as_deref().map(str::parse).transpose()?,
            },
            Type::Bool { .. } => Type::Bool {
                default: default
                    .as_deref()
                    .map(str::to_lowercase)
                    .as_deref()
                    .map(str::parse)
                    .transpose()?,
            },
            Type::String { .. } if default.is_some() || one_of.is_some() => Type::String {
                default,
                one_of: one_of.unwrap_or_default(),
            },
            x => x,
        };

        Ok(ty)
    }

    fn custom_parse<E, T>(
        pattern: Pattern,
        text: ElementOrDescription,
        extractor: E,
    ) -> Result<Option<T>>
    where
        E: Fn(&SentenceRef) -> Option<T>,
    {
        let sentences = text.sentences()?;
        let mut result = None;
        let patterns = pattern.parts();
        'patterns: for pattern in patterns {
            for sentence in &sentences {
                for (word_idx, words) in sentence.parts.windows(pattern.parts.len()).enumerate() {
                    if pattern.parts == words {
                        let offset = (word_idx as isize
                            + pattern.parts.len() as isize
                            + pattern.offset) as usize;

                        let sentence = &sentence[offset..];
                        result = Some(sentence);
                        break 'patterns;
                    }
                }
            }
        }

        Ok(result.and_then(extractor))
    }

    pub fn extract_from_text(text: ElementOrDescription) -> Result<Self> {
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

        Self::custom_parse(Pattern::ReturnType, text, extract_type)
            .transpose()
            .ok_or_else(|| ParseError::TypeExtractionFailed(text.plain_text()))?
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

#[derive(Debug, Copy, Clone)]
pub enum ElementOrDescription<'a> {
    Element(&'a ElementRef<'a>),
    Description(&'a RawDescription<'a>),
}

impl ElementOrDescription<'_> {
    fn sentences(self) -> Result<Vec<Sentence>> {
        match self {
            ElementOrDescription::Element(elem) => elem.sentences(),
            ElementOrDescription::Description(description) => description.sentences(),
        }
    }

    fn plain_text(self) -> String {
        match self {
            ElementOrDescription::Element(elem) => elem.plain_text(),
            ElementOrDescription::Description(description) => description.plain_text(),
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

#[derive(Debug, PartialEq, Copy, Clone)]
enum Pattern {
    ReturnType,
    Default,
    MinMax,
    OneOf,
}

impl Pattern {
    fn parts(self) -> Vec<SearcherPattern> {
        match self {
            Pattern::ReturnType => vec![
                SearcherPattern::default().by_word("On").by_word("success"),
                SearcherPattern::default().by_word("Returns"),
                SearcherPattern::default().by_word("returns"),
                SearcherPattern::default().by_word("An"),
            ],
            Pattern::Default => vec![
                SearcherPattern::default().by_word("Defaults").by_word("to"),
                SearcherPattern::default().by_word("defaults").by_word("to"),
                SearcherPattern::default()
                    .by_word("must")
                    .by_word("be")
                    .by_kind(PartKind::Italic)
                    .with_offset(-1),
            ],
            Pattern::MinMax => vec![SearcherPattern::default()
                .by_word("Values")
                .by_word("between")],
            Pattern::OneOf => {
                vec![
                    SearcherPattern::default().by_word("One").by_word("of"),
                    SearcherPattern::default().by_word("one").by_word("of"),
                    SearcherPattern::default().by_word("Can").by_word("be"),
                ]
            }
        }
    }
}

#[derive(Debug, Default)]
struct SearcherPattern {
    parts: Vec<SearcherPart>,
    offset: isize,
}

impl SearcherPattern {
    fn by_word<T: Into<String>>(mut self, inner: T) -> Self {
        self.parts.push(SearcherPart::by_word(inner));
        self
    }

    fn by_kind(mut self, kind: PartKind) -> Self {
        self.parts.push(SearcherPart::by_kind(kind));
        self
    }

    /// Useful for partial matching  
    fn with_offset(mut self, offset: isize) -> Self {
        self.offset = offset;
        self
    }
}

#[derive(Debug, Clone, Logos)]
enum SentenceLexer {
    #[error]
    #[token(",", logos::skip)]
    #[token(" ", logos::skip)]
    #[token("(", logos::skip)]
    #[token(")", logos::skip)]
    Error,
    #[regex(r#"([^“., ]|\\.)+"#, |_| false)] // just word
    #[regex(r#""(?:[^"']|\\["'])*["']"#, |_| true)] // words in "", '", '' or "' quotes
    #[regex(r#"“(?:[^”]|\\”)*”"#, |_| true)] // words in unicode quotes
    Word(bool), // does word has quotes?
    #[token(".")]
    Dot,
    #[token("\"")]
    #[token("“")]
    #[token("”")]
    /// In case line break between text and tag
    Quote,
}

#[derive(Debug)]
struct Sentences {
    inner: Vec<Sentence>,
}

impl Sentences {
    fn parse(text: &str) -> Self {
        let mut sentences = vec![];
        let mut parts = vec![];

        let lexer = SentenceLexer::lexer(&text);
        for (token, span) in lexer.spanned() {
            let lexeme = &text[span.start..span.end];
            match token {
                SentenceLexer::Error => {
                    dbg!(text);
                    dbg!(lexeme);
                    unreachable!()
                }
                SentenceLexer::Word(has_quotes) => {
                    let inner = lexeme
                        .trim_matches('"')
                        .trim_matches('\'')
                        .trim_start_matches('“')
                        .trim_end_matches('”')
                        .to_string();
                    parts.push(Part::new(inner).with_quotes(has_quotes));
                }
                SentenceLexer::Dot => {
                    sentences.push(Sentence {
                        parts: mem::take(&mut parts),
                    });
                }
                SentenceLexer::Quote => unreachable!(),
            }
        }

        if !parts.is_empty() {
            sentences.push(Sentence { parts });
        }

        Self { inner: sentences }
    }

    fn find(&self, words: &[&str]) -> Option<&SentenceRef> {
        self.inner.iter().find_map(|sentence| {
            sentence
                .parts
                .windows(words.len())
                .position(|window| window == words)
                .map(|pos| &sentence[pos..])
        })
    }
}

#[derive(Debug, Clone, Default, PartialEq)]
struct Part {
    inner: String,
    has_quotes: bool,
    kind: PartKind,
}

impl Part {
    fn new(inner: String) -> Self {
        Self {
            inner,
            ..Self::default()
        }
    }

    fn link(inner: String, link: String) -> Self {
        Self {
            inner,
            kind: PartKind::Link(link),
            ..Self::default()
        }
    }

    fn italic(inner: String) -> Self {
        Self {
            inner,
            kind: PartKind::Italic,
            ..Self::default()
        }
    }

    fn code(inner: String) -> Self {
        Self {
            inner,
            kind: PartKind::Code,
            ..Self::default()
        }
    }

    fn bold(inner: String) -> Self {
        Self {
            inner,
            kind: PartKind::Bold,
            ..Self::default()
        }
    }

    fn with_quotes(mut self, has_quotes: bool) -> Self {
        self.has_quotes = has_quotes;
        self
    }
}

impl PartialEq<&str> for Part {
    fn eq(&self, other: &&str) -> bool {
        self.inner == *other
    }
}

#[derive(Debug, Clone, PartialEq)]
enum PartKind {
    Word,
    Link(String),
    Bold,
    Italic,
    Code,
}

impl Default for PartKind {
    fn default() -> Self {
        Self::Word
    }
}

#[derive(Debug, Clone, PartialEq)]
enum SearcherPart {
    ByWord(String),
    ByKind(PartKind),
}

impl SearcherPart {
    fn by_word<T: Into<String>>(inner: T) -> Self {
        Self::ByWord(inner.into())
    }

    fn by_kind(kind: PartKind) -> Self {
        Self::ByKind(kind)
    }
}

impl PartialEq<Part> for SearcherPart {
    fn eq(&self, other: &Part) -> bool {
        match self {
            SearcherPart::ByWord(inner) => other.inner == *inner,
            SearcherPart::ByKind(kind) => other.kind == *kind,
        }
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

    fn len(&self) -> usize {
        self.parts.len()
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
        let mut sentences = vec![];
        let mut parts = vec![];
        let mut quote_part = false;

        for node in self.children() {
            match node.value() {
                Node::Text(text) => {
                    let lexer = SentenceLexer::lexer(&text);
                    for (token, span) in lexer.spanned() {
                        let lexeme = &text[span.start..span.end];
                        match token {
                            SentenceLexer::Error => {
                                dbg!(text);
                                dbg!(lexeme);
                                unreachable!()
                            }
                            SentenceLexer::Word(has_quotes) => {
                                let inner = lexeme
                                    .trim_matches('"')
                                    .trim_matches('\'')
                                    .trim_start_matches('“')
                                    .trim_end_matches('”')
                                    .to_string();
                                parts.push(Part::new(inner).with_quotes(has_quotes));
                            }
                            SentenceLexer::Dot => {
                                sentences.push(Sentence {
                                    parts: mem::take(&mut parts),
                                });
                            }
                            SentenceLexer::Quote => {
                                quote_part = !quote_part;
                            }
                        }
                    }
                }
                Node::Element(elem) => {
                    let inner = node.first_child();
                    let text = inner
                        .as_ref()
                        .and_then(|node| node.value().as_text())
                        .map(|text| text.to_string());

                    let part = match (elem.name(), text) {
                        ("a", Some(text)) => {
                            // TODO: Element::a_href()
                            let link = elem.attr("href").ok_or(ParseError::MissingHref)?;
                            Some(Part::link(text, link.to_string()))
                        }
                        ("a", None) => {
                            let link = elem.attr("href").ok_or(ParseError::MissingHref)?;
                            Some(Part::new(link.to_string()))
                        }
                        ("em", Some(text)) => Some(Part::italic(text)),
                        ("code", Some(text)) => Some(Part::code(text)),
                        ("strong", Some(text)) => Some(Part::bold(text)),
                        ("img", _) => {
                            let alt = elem.attr("alt").ok_or(ParseError::MissingAlt)?;
                            Some(Part::new(alt.to_string()).with_quotes(quote_part))
                        }
                        ("br", _) => None,
                        _ => {
                            // TODO: log skipped tags
                            dbg!(elem);
                            None
                        }
                    };
                    parts.extend(part);
                }
                _ => continue,
            }
        }

        if !parts.is_empty() {
            sentences.push(Sentence { parts });
        }

        Ok(sentences)
    }

    fn markdown(&self) -> String {
        html2md::parse_html_custom(&self.html(), &TagsHandlerFactory::new_in_map())
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

enum TagsHandlerFactory {
    Anchor,
    Image,
}

impl TagsHandlerFactory {
    fn new_in_map() -> HashMap<String, Box<dyn TagHandlerFactory>> {
        let mut map = HashMap::new();
        map.insert("a".to_string(), Box::new(TagsHandlerFactory::Anchor) as _);
        map.insert("img".to_string(), Box::new(TagsHandlerFactory::Image) as _);
        map
    }
}

impl TagHandlerFactory for TagsHandlerFactory {
    fn instantiate(&self) -> Box<dyn TagHandler> {
        match self {
            TagsHandlerFactory::Anchor => Box::new(AnchorHandler::default()),
            TagsHandlerFactory::Image => Box::new(ImageHandler),
        }
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
        self.inner = get_tag_attr(tag, "href")
            .map(|value| {
                if value.starts_with('#') {
                    make_url_from_fragment(value)
                } else {
                    value
                }
            })
            .map(|value| (printer.data.len(), value))
    }

    fn after_handle(&mut self, printer: &mut StructuredPrinter) {
        let (pos, value) = self.inner.as_ref().unwrap();
        if *pos != printer.data.len() {
            printer.insert_str(*pos, "[");
            printer.append_str(&format!("]({})", value));
        }
    }
}

struct ImageHandler;

impl TagHandler for ImageHandler {
    fn handle(&mut self, tag: &Handle, printer: &mut StructuredPrinter) {
        // almost all of code taken from html2md source code
        // because html2md::ImgHandler is not public

        const FRAGMENT: &AsciiSet = &CONTROLS.add(b' ').add(b'"').add(b'<').add(b'>').add(b'`');

        // try to extract attrs
        let src = get_tag_attr(tag, "src");
        let alt = get_tag_attr(tag, "alt");
        let title = get_tag_attr(tag, "title");
        let height = get_tag_attr(tag, "height");
        let width = get_tag_attr(tag, "width");
        let align = get_tag_attr(tag, "align");

        if let Some(alt) = alt {
            printer.append_str(&alt);
            return;
        }

        if height.is_some() || width.is_some() || align.is_some() {
            // need to handle it as inline html to preserve attributes we support
            printer.append_str(&format!(
                "<img{} />",
                alt.map(|value| format!(" alt=\"{}\"", value))
                    .unwrap_or_default()
                    + &src
                        .map(|value| format!(" src=\"{}\"", value))
                        .unwrap_or_default()
                    + &title
                        .map(|value| format!(" title=\"{}\"", value))
                        .unwrap_or_default()
                    + &height
                        .map(|value| format!(" height=\"{}\"", value))
                        .unwrap_or_default()
                    + &width
                        .map(|value| format!(" width=\"{}\"", value))
                        .unwrap_or_default()
                    + &align
                        .map(|value| format!(" align=\"{}\"", value))
                        .unwrap_or_default()
            ));
        } else {
            // need to escape URL if it contains spaces
            // don't have any geometry-controlling attrs, post markdown natively
            let mut img_url = src.unwrap_or_default();
            if img_url.contains(' ') {
                img_url = utf8_percent_encode(&img_url, FRAGMENT).to_string();
            }

            printer.append_str(&format!(
                "![{}]({}{})",
                alt.unwrap_or_default(),
                &img_url,
                title
                    .map(|value| format!(" \"{}\"", value))
                    .unwrap_or_default()
            ));
        }
    }

    fn after_handle(&mut self, _printer: &mut StructuredPrinter) {}
}

#[cfg(test)]
mod tests {
    use super::*;

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

    #[test]
    fn empty_link_skipped() {
        let map = TagsHandlerFactory::new_in_map();
        let md = html2md::parse_html_custom(r#"<a href=""></a>"#, &map);
        assert_eq!(md, "");
    }

    #[test]
    fn make_absolute_a_href() {
        let map = TagsHandlerFactory::new_in_map();
        let md = html2md::parse_html_custom(r##"<a href="#fragment">This is a link</a>"##, &map);
        assert_eq!(
            md,
            format!("[This is a link]({}#fragment)", BOT_API_DOCS_URL)
        )
    }

    #[test]
    fn extract_img_alt() {
        let map = TagsHandlerFactory::new_in_map();
        let md = html2md::parse_html_custom(
            r#"<img alt="🎲" src="//telegram.org/img/emoji/40/F09F8EB2.png" height="20" width="20" />, <img alt="🎯" src="//telegram.org/img/emoji/40/F09F8EAF.png" height="20" width="20" />"#,
            &map,
        );
        assert_eq!(md, "🎲, 🎯");

        const TAGS: &str = r#"<img src="//telegram.org/img/emoji/40/F09F8EB2.png" height="20" width="20" />, <img src="//telegram.org/img/emoji/40/F09F8EAF.png" height="20" width="20" />"#;
        let md = html2md::parse_html_custom(TAGS, &map);
        assert_eq!(md, TAGS);
    }

    #[test]
    fn sentence_parser_one_word() {
        let sentences = Sentences::parse("One");
        assert_eq!(sentences.inner.len(), 1);
        assert_eq!(sentences.inner[0].parts.len(), 1);
        assert_eq!(sentences.inner[0].parts[0], "One");
    }

    #[test]
    fn sentence_parser_parts() {
        let sentences = Sentences::parse(
            r#"Emoji on which the dice throw animation is based. Currently, must be one of “🎲”, “🎯”, “🏀”, “⚽”, or “🎰”. Dice can have values 1-6 for “🎲” and “🎯”, values 1-5 for “🏀” and “⚽”, and values 1-64 for “🎰”. Defaults to “🎲”."#,
        );
        assert_eq!(sentences.inner.len(), 4);
        assert_eq!(sentences.inner[0].parts.len(), 9);
        assert_eq!(sentences.inner[1].parts.len(), 11);
        assert_eq!(sentences.inner[2].parts.len(), 20);
        assert_eq!(sentences.inner[3].parts.len(), 3);
    }

    #[test]
    fn sentence_parser_quotes() {
        let sentences = Sentences::parse(
            r#"The section of the user's Telegram Passport which has the issue, one of “passport”, “driver_license”, “identity_card”, “internal_passport”."#,
        );
        assert_eq!(
            sentences
                .inner
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
