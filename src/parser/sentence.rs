use super::{ParseError, TypeParsingUnit};
use crate::parser::ElementExt;
use ego_tree::NodeRef;
use itertools::Itertools;
use logos::Logos;
use scraper::{node::Text, Node};
use std::{mem, ops::Index, ptr, slice::SliceIndex};
use tendril::StrTendril;

#[derive(Debug, PartialEq, Eq, Copy, Clone)]
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
                SearcherPattern::default()
                    .by_word("Returns")
                    .by_word("the")
                    .by_word("bot's")
                    .by_word("Telegram")
                    .exclude(),
                SearcherPattern::default()
                    .by_word("Returns")
                    .by_word("the")
                    .by_word("list")
                    .by_word("of")
                    .exclude(),
                SearcherPattern::default().by_word("On").by_word("success"),
                SearcherPattern::default().by_word("Returns"),
                SearcherPattern::default().by_word("returns"),
                SearcherPattern::default().by_word("An"),
            ],
            Pattern::Default => vec![
                SearcherPattern::default().by_word("Defaults").by_word("to"),
                SearcherPattern::default()
                    .by_word("defaults")
                    .by_word("to")
                    .exclude(),
                SearcherPattern::default().by_word("defaults").by_word("to"),
                SearcherPattern::default()
                    .by_word("must")
                    .by_word("be")
                    .by_kind(PartKind::Italic)
                    .with_offset(-1),
                SearcherPattern::default()
                    .by_word("always")
                    .by_quotes()
                    .with_offset(-1),
            ],
            Pattern::MinMax => vec![
                SearcherPattern::default()
                    .by_word("Values")
                    .by_word("between"),
                SearcherPattern::default()
                    .by_word("characters")
                    .with_offset(-2),
            ],
            Pattern::OneOf => {
                vec![
                    SearcherPattern::default().by_word("either"),
                    SearcherPattern::default().by_word("One").by_word("of"),
                    SearcherPattern::default().by_word("one").by_word("of"),
                    SearcherPattern::default().by_word("Can").by_word("be"),
                    SearcherPattern::default()
                        .by_word("can")
                        .by_word("be")
                        .by_quotes()
                        .with_offset(-1),
                    SearcherPattern::default()
                        .by_quotes()
                        .by_word("or")
                        .by_quotes()
                        .with_offset(-3),
                    SearcherPattern::default().by_word("Choose").by_word("one"),
                ]
            }
        }
    }
}

#[derive(Debug, Default)]
struct SearcherPattern {
    parts: Vec<SearchBy>,
    offset: isize,
    exclude: bool,
}

impl SearcherPattern {
    fn by_word<T: Into<String>>(mut self, inner: T) -> Self {
        self.parts.push(SearchBy::word(inner));
        self
    }

    fn by_kind(mut self, kind: PartKind) -> Self {
        self.parts.push(SearchBy::kind(kind));
        self
    }

    fn by_quotes(mut self) -> Self {
        self.parts.push(SearchBy::quotes());
        self
    }

    /// Useful for partial matching
    fn with_offset(mut self, offset: isize) -> Self {
        self.offset = offset;
        self
    }

    fn exclude(mut self) -> Self {
        self.exclude = true;
        self
    }
}

impl PartialEq<&[Part]> for SearcherPattern {
    fn eq(&self, other: &&[Part]) -> bool {
        self.parts == *other
    }
}

#[derive(Debug, Clone, Logos, Eq, PartialEq)]
#[logos(skip r"[, ]")]
#[logos(skip "\n")]
enum SentenceLexer {
    #[regex(r#"[^, "“”\(\)\.\n]+"#)]
    Word,
    #[token(".")]
    Dot,
    #[token("\"")]
    #[token("“")]
    #[token("”")]
    /// In case line break between text and tag
    Quote,
    #[token("(")]
    LParen,
    #[token(")")]
    RParen,
}

#[derive(Debug)]
pub(crate) struct Sentences {
    inner: Vec<Sentence>,
}

impl Sentences {
    pub(crate) fn parse(text: &str) -> Self {
        let tree = ego_tree::tree! {
            Node::Document => {
                Node::Text(Text {
                    text: StrTendril::from_slice(text),
                })
            }
        };

        let sentences = parse_node(tree.root()).unwrap();
        Self { inner: sentences }
    }

    pub fn find(&self, words: &[&str]) -> Option<&SentenceRef> {
        self.inner.iter().find_map(|sentence| {
            sentence.parts.windows(words.len()).find_map(|window| {
                if window == words {
                    Some(sentence.as_ref())
                } else {
                    None
                }
            })
        })
    }

    pub(crate) fn find_and_crop(&self, words: &[&str]) -> Option<&SentenceRef> {
        self.inner.iter().find_map(|sentence| {
            sentence
                .parts
                .windows(words.len())
                .position(|window| window == words)
                .map(|pos| &sentence[pos..])
        })
    }
}

#[derive(Debug, Clone, Hash, Default, PartialEq, Eq)]
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

    pub fn is_italic(&self) -> bool {
        matches!(self.kind, PartKind::Italic)
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

#[derive(Debug, Clone, Hash, PartialEq, Eq)]
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
enum SearchBy {
    Word(String),
    Kind(PartKind),
    Quotes,
}

impl SearchBy {
    fn word<T: Into<String>>(inner: T) -> Self {
        Self::Word(inner.into())
    }

    fn kind(kind: PartKind) -> Self {
        Self::Kind(kind)
    }

    fn quotes() -> Self {
        Self::Quotes
    }
}

impl PartialEq<Part> for SearchBy {
    fn eq(&self, other: &Part) -> bool {
        match self {
            SearchBy::Word(inner) => other.inner == *inner,
            SearchBy::Kind(kind) => other.kind == *kind,
            SearchBy::Quotes => other.has_quotes,
        }
    }
}

impl PartialEq<&str> for SearchBy {
    fn eq(&self, other: &&str) -> bool {
        match self {
            SearchBy::Word(s) => s == other,
            _ => false,
        }
    }
}

#[derive(Debug, Hash, PartialEq, Eq)]
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
        unsafe { &*(ptr::slice_from_raw_parts(part as *const Part, 1) as *const SentenceRef) }
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

#[derive(Debug, Copy, Clone, Eq, PartialEq)]
enum QuoteState {
    Left,
    Right,
    None,
}

impl QuoteState {
    fn next_state(self) -> Self {
        match self {
            QuoteState::Left => QuoteState::Right,
            QuoteState::Right => QuoteState::None,
            QuoteState::None => QuoteState::Left,
        }
    }
}

pub(crate) fn parse_node(elem: NodeRef<Node>) -> Result<Vec<Sentence>, ParseError> {
    let mut sentences = vec![];
    let mut parts = vec![];
    let mut quote = QuoteState::None;
    let mut quote_part_start = 0;
    let mut paren = false;

    for node in elem.children() {
        match node.value() {
            Node::Text(text) => {
                let lexer = SentenceLexer::lexer(text);
                for (token, span) in lexer.spanned() {
                    let lexeme = &text[span.start..span.end];

                    let token = match token {
                        Ok(token) => token,
                        Err(()) => {
                            return Err(ParseError::Lexer {
                                lexeme: lexeme.to_string(),
                                input: text.to_string(),
                                span,
                            });
                        }
                    };

                    match token {
                        SentenceLexer::Word if !paren => {
                            let part = Part::new(lexeme.to_string());
                            parts.push(part);
                        }
                        SentenceLexer::Dot if !paren && quote != QuoteState::Left => {
                            sentences.push(Sentence {
                                parts: mem::take(&mut parts),
                            });
                        }
                        SentenceLexer::Quote if !paren => {
                            quote = quote.next_state();

                            match quote {
                                QuoteState::Left => {
                                    quote_part_start = parts.len();
                                }
                                QuoteState::Right => {
                                    let part = parts
                                        .drain(quote_part_start..)
                                        .map(|part| part.inner)
                                        .join(" ");
                                    let part = Part::new(part).with_quotes(true);
                                    parts.push(part);

                                    quote_part_start = 0;
                                    quote = QuoteState::None;
                                }
                                QuoteState::None => unreachable!(),
                            }
                        }
                        SentenceLexer::LParen => paren = true,
                        SentenceLexer::RParen => paren = false,
                        _ => continue,
                    }
                }
            }
            Node::Element(elem) if !paren => {
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
                        Some(Part::new(alt.to_string()).with_quotes(quote == QuoteState::Left))
                    }
                    ("br", _) => None,
                    ("li", _) => {
                        if !parts.is_empty() {
                            sentences.push(Sentence {
                                parts: mem::take(&mut parts),
                            });
                        }

                        sentences.extend(parse_node(node)?);
                        None
                    }
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

    'sentences: for sentence in &sentences {
        for pattern in &patterns {
            for (word_idx, words) in sentence.parts.windows(pattern.parts.len()).enumerate() {
                if *pattern == words {
                    if pattern.exclude {
                        continue 'sentences;
                    }

                    let offset = (word_idx as isize + pattern.parts.len() as isize + pattern.offset)
                        as usize;

                    let sentence = &sentence[offset..];
                    result = Some(sentence);
                    break 'sentences;
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
    fn sentence_lexer_quote_and_words() {
        let mut sentence = SentenceLexer::lexer("\" base quote");
        assert_eq!(sentence.next(), Some(Ok(SentenceLexer::Quote)));
        assert_eq!(sentence.next(), Some(Ok(SentenceLexer::Word)));
        assert_eq!(sentence.next(), Some(Ok(SentenceLexer::Word)));
    }

    #[test]
    fn sentence_parser_parentheses_ignored() {
        let sentences = Sentences::parse("Hello (really?), world!");
        itertools::assert_equal(
            sentences.inner[0].parts.iter().map(|part| part.as_inner()),
            vec!["Hello", "world!"],
        );
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
