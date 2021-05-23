use super::{ParseError, TypeParsingUnit};
use crate::parser::ElementExt;
use ego_tree::{NodeRef, Tree};
use logos::Logos;
use scraper::{node::Text, Node};
use std::{mem, ops::Index, slice, slice::SliceIndex};
use tendril::StrTendril;

#[derive(Debug, PartialEq, Copy, Clone)]
pub enum Pattern {
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
pub(crate) struct Sentences {
    inner: Vec<Sentence>,
}

impl Sentences {
    pub(crate) fn parse(text: &str) -> Self {
        let text = Node::Text(Text {
            text: StrTendril::from_slice(text),
        });
        let tree = Tree::new(text);

        let sentences = parse_node(tree.root()).unwrap();
        Self { inner: sentences }
    }

    pub(crate) fn find(&self, words: &[&str]) -> Option<&SentenceRef> {
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
pub struct Part {
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

    pub fn has_quotes(&self) -> bool {
        self.has_quotes
    }

    pub fn as_inner(&self) -> &String {
        &self.inner
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
pub struct Sentence {
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
pub struct SentenceRef {
    parts: [Part],
}

impl SentenceRef {
    pub(crate) fn from_part(part: &Part) -> &Self {
        unsafe {
            &*(slice::from_raw_parts(part as *const Part, 1) as *const [Part] as *const SentenceRef)
        }
    }

    pub(crate) fn len(&self) -> usize {
        self.parts.len()
    }

    pub(crate) fn starts_with(&self, words: &[&str]) -> bool {
        self.parts
            .get(..words.len())
            .map(|slice| slice.iter().zip(words).all(|(l, &r)| l.inner == r))
            .unwrap_or(false)
    }

    pub(crate) fn contains(&self, words: &[&str]) -> bool {
        self.parts.windows(words.len()).any(|parts| parts == words)
    }

    pub fn parts(&self) -> &[Part] {
        &self.parts
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

pub(crate) fn parse_node(elem: NodeRef<Node>) -> Result<Vec<Sentence>, ParseError> {
    let mut sentences = vec![];
    let mut parts = vec![];
    let mut quote_part = false;

    for node in elem.children() {
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
                        let link = elem.a_href()?;
                        Some(Part::link(text, link.to_string()))
                    }
                    ("a", None) => {
                        let link = elem.a_href()?;
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
                        log::warn!("Tag {} skipped", elem.name());
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

pub fn parse_type_custom<E, T>(
    pattern: Pattern,
    text: TypeParsingUnit,
    extractor: E,
) -> Result<Option<T>, ParseError>
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
                    let offset = (word_idx as isize + pattern.parts.len() as isize + pattern.offset)
                        as usize;

                    let sentence = &sentence[offset..];
                    result = Some(sentence);
                    break 'patterns;
                }
            }
        }
    }

    Ok(result.and_then(extractor))
}

#[cfg(test)]
mod tests {
    use super::*;

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
