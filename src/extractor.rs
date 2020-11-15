use crate::util::{ElementRefExt, StrExt};
use itertools::Itertools;
use scraper::{ElementRef, Html, Selector};

#[derive(Debug, thiserror::Error)]
pub enum ExtractorError {
    #[error("No `Recent changes` found in document")]
    NoRecentChanges,
    #[error("No version string found in document")]
    NoVersion,
}

pub struct Extractor {
    doc: Html,
}

impl Extractor {
    pub fn from_str(s: &str) -> Self {
        Self {
            doc: Html::parse_document(s),
        }
    }

    pub fn extract(&self) -> Result<Extracted<'_>, ExtractorError> {
        let mut recent_changes = None;
        let mut version = None;
        let mut objects = Vec::new();
        let mut methods = Vec::new();

        let h3 = Selector::parse("h3").unwrap();
        let h4 = Selector::parse("h4").unwrap();
        let table = Selector::parse("table").unwrap();
        let td = Selector::parse("td").unwrap();
        let p = Selector::parse("p").unwrap();
        let any = Selector::parse("h3, h4, p, table").unwrap();

        let mut state = State::SearchRecentChanges;
        let mut select_any = self.doc.select(&any).peekable();
        while let Some(elem) = select_any.next() {
            let new_state = match state {
                State::SearchRecentChanges
                    if h3.matches(&elem) && elem.plain_text() == "Recent changes" =>
                {
                    State::GetRecentChange
                }
                State::GetRecentChange if h4.matches(&elem) => {
                    recent_changes = Some(elem.plain_text());
                    State::GetVersion
                }
                State::GetVersion if p.matches(&elem) => {
                    version = Some(elem);
                    State::SearchGettingUpdates
                }
                State::SearchGettingUpdates
                    if h3.matches(&elem) && elem.plain_text() == "Getting updates" =>
                {
                    State::GetName
                }
                State::GetName if h4.matches(&elem) => {
                    let name = elem.plain_text();
                    // get rid of elements like `Formatting options`, `Sending files` that are not objects or methods
                    if name.chars().any(char::is_whitespace) {
                        State::GetName
                    } else {
                        State::GetDescription { name: elem }
                    }
                }
                State::GetDescription { name } if p.matches(&elem) => {
                    let description = elem;

                    let no_table = select_any
                        .peek()
                        .map(|next_elem| !table.matches(next_elem))
                        .unwrap_or(true);
                    let is_method = name.plain_text().is_first_letter_lowercase();
                    match (no_table, is_method) {
                        (false, true) => State::GetMethodFields { name, description },
                        (false, false) => State::GetObjectFields { name, description },
                        (true, true) => {
                            methods.push(RawMethod {
                                name,
                                description,
                                args: vec![],
                            });
                            State::GetName
                        }
                        (true, false) => {
                            objects.push(RawObject {
                                name,
                                description,
                                fields: vec![],
                            });
                            State::GetName
                        }
                    }
                }
                State::GetObjectFields { name, description } if table.matches(&elem) => {
                    objects.push(RawObject {
                        name,
                        description,
                        fields: extract_fields(&td, elem),
                    });

                    State::GetName
                }
                State::GetMethodFields { name, description } if table.matches(&elem) => {
                    methods.push(RawMethod {
                        name,
                        description,
                        args: extract_args(&td, elem),
                    });

                    State::GetName
                }
                x => x,
            };
            state = new_state;
        }

        Ok(Extracted {
            recent_changes: recent_changes.ok_or(ExtractorError::NoRecentChanges)?,
            version: version.ok_or(ExtractorError::NoVersion)?,
            methods,
            objects,
        })
    }
}

fn extract_fields<'a>(td: &Selector, elem: ElementRef<'a>) -> Vec<RawField<'a>> {
    elem.select(td)
        .chunks(3)
        .into_iter()
        .filter_map(|mut tds| {
            let name = tds.next()?.plain_text();
            let kind = tds.next()?.plain_text();
            let description = tds.next()?;
            Some(RawField {
                name,
                kind,
                description,
            })
        })
        .collect()
}

fn extract_args<'a>(td: &Selector, elem: ElementRef<'a>) -> Vec<RawArgument<'a>> {
    elem.select(td)
        .chunks(4)
        .into_iter()
        .filter_map(|mut tds| {
            let name = tds.next()?.plain_text();
            let kind = tds.next()?.plain_text();
            let required = tds.next()?.plain_text();
            let description = tds.next()?;

            Some(RawArgument {
                name,
                kind,
                required,
                description,
            })
        })
        .collect()
}

pub struct Extracted<'a> {
    pub recent_changes: String,
    pub version: ElementRef<'a>,
    pub methods: Vec<RawMethod<'a>>,
    pub objects: Vec<RawObject<'a>>,
}

#[derive(Debug)]
enum State<'a> {
    SearchRecentChanges,
    GetRecentChange,
    GetVersion,
    SearchGettingUpdates,
    GetName,
    GetDescription {
        name: ElementRef<'a>,
    },
    GetObjectFields {
        name: ElementRef<'a>,
        description: ElementRef<'a>,
    },
    GetMethodFields {
        name: ElementRef<'a>,
        description: ElementRef<'a>,
    },
}

pub struct RawMethod<'a> {
    pub name: ElementRef<'a>,
    pub description: ElementRef<'a>,
    pub args: Vec<RawArgument<'a>>,
}

pub struct RawArgument<'a> {
    pub name: String,
    pub kind: String,
    pub required: String,
    pub description: ElementRef<'a>,
}

pub struct RawObject<'a> {
    pub name: ElementRef<'a>,
    pub description: ElementRef<'a>,
    pub fields: Vec<RawField<'a>>,
}

pub struct RawField<'a> {
    pub name: String,
    pub kind: String,
    pub description: ElementRef<'a>,
}
