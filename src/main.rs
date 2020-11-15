mod extractor;
mod openapi;
mod parser;
mod util;

use crate::extractor::Extractor;

const BOT_API_DOCS: &str = "https://core.telegram.org/bots/api/";

#[tokio::main]
async fn main() -> anyhow::Result<()> {
    let api = reqwest::get(BOT_API_DOCS).await?.text().await?;

    let extractor = Extractor::from_str(&api);
    let extracted = extractor.extract()?;
    let parsed = parser::parse(extracted)?;

    /*let mut state = State::SearchRecentChanges;
    let mut select_any = doc.select(&any).peekable();
    while let Some(elem) = select_any.next() {
        let new_state = match state {
            State::SearchRecentChanges
                if h3.matches(&elem) && elem.full_text() == "Recent changes" =>
            {
                State::GetRecentChange
            }
            State::GetRecentChange if h4.matches(&elem) => {
                let date = elem.full_text();
                recent_change = Some(NaiveDate::parse_from_str(&date, "%B %e, %Y")?);
                State::SearchGettingUpdates
            }
            State::SearchGettingUpdates
                if h3.matches(&elem) && elem.full_text() == "Getting updates" =>
            {
                State::GetName
            }
            State::GetName if h4.matches(&elem) => {
                let name = elem.full_text();
                // get rid of elements like `Formatting options`, `Sending files` that are not objects or methods
                if name.chars().any(char::is_whitespace) {
                    State::GetName
                } else {
                    State::GetDescription { name }
                }
            }
            State::GetDescription { name } if p.matches(&elem) => {
                fn get_range(text: &str, start: &str, end: &str) -> Option<Range<usize>> {
                    let start = text.find(start)? + start.len();
                    let end = text[start..].find(end)? + start;
                    Some(start..end)
                }

                fn extract_type(text: &str) -> Option<Type> {
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
                            || text[pos.saturating_sub(AN_ARRAY_OF.len())..]
                                .starts_with(AN_ARRAY_OF)
                        {
                            let text = &text[pos + ARRAY.len()..];
                            let ty = extract_type(text)?;
                            Some(Type::Array(Box::new(ty)))
                        } else {
                            Some(Type::new(ty))
                        }
                    }
                }

                fn search_type(text: &str, start: &str, end: &str) -> Option<Type> {
                    let range = get_range(text, start, end)?;
                    let text = &text[range];
                    extract_type(text)
                }

                fn search(text: &str, patterns: &[(&str, &str)]) -> Result<Type, Error> {
                    patterns
                        .iter()
                        .find_map(|(start, end)| search_type(text, start, end))
                        .ok_or_else(|| Error::TypeExtractionFailed(text.to_string()))
                }

                const PATTERNS: &[(&str, &str)] = &[
                    ("Returns ", " on success"),
                    ("Returns ", " object."),
                    ("An ", " is returned."),
                    ("On success, ", " is returned."),
                    ("On success, ", " object."),
                    ("returns ", "."),
                ];

                let description = elem.full_text();

                let no_table = select_any
                    .peek()
                    .map(|next_elem| !table.matches(next_elem))
                    .unwrap_or(true);
                let is_method = name.is_first_letter_lowercase();
                match (no_table, is_method) {
                    (false, true) => State::GetMethodFields {
                        name,
                        return_type: search(&description, PATTERNS)?,
                        description,
                    },
                    (false, false) => State::GetObjectFields { name, description },
                    (true, true) => {
                        methods.push(Method {
                            name,
                            return_type: search(&description, PATTERNS)?,
                            description,
                            args: MethodArgs::No,
                        });
                        State::GetName
                    }
                    (true, false) => {
                        objects.push(Object {
                            name,
                            description,
                            fields: vec![],
                        });
                        State::GetName
                    }
                }
            }
            State::GetObjectFields { name, description } if table.matches(&elem) => {
                let fields: Vec<Field> = elem
                    .select(&td)
                    .map(|td| td.full_text())
                    .chunks(3)
                    .into_iter()
                    .filter_map(|mut tds| {
                        let name = tds.next()?;
                        let kind = tds.next()?;
                        let description = tds.next()?;
                        Some(Field {
                            name,
                            kind: Type::new(&kind),
                            required: if description.starts_with("Optional.") {
                                Required::Optional
                            } else {
                                Required::Yes
                            },
                            description: description.trim_start_matches("Optional. ").to_string(),
                        })
                    })
                    .collect();

                objects.push(Object {
                    name: name.clone(),
                    description: description.clone(),
                    fields,
                });

                State::GetName
            }
            State::GetMethodFields {
                name,
                description,
                return_type,
            } if table.matches(&elem) => {
                let args: Vec<Argument> = elem
                    .select(&td)
                    .map(|td| td.full_text())
                    .chunks(4)
                    .into_iter()
                    .filter_map(|mut tds| {
                        let name = tds.next()?;
                        let kind = tds.next()?;
                        let required = Required::new(tds.next()?);
                        let description = tds.next()?;

                        let arg = required.map(|required| Argument {
                            name,
                            kind: Type::new(&kind),
                            required,
                            description,
                        });

                        Some(arg)
                    })
                    .collect::<Result<_, _>>()?;

                methods.push(Method {
                    name,
                    description,
                    args: MethodArgs::new(args),
                    return_type,
                });

                State::GetName
            }
            x => x,
        };
        state = new_state;
    }*/

    let api = openapi::generate(parsed)?;
    let api = serde_yaml::to_string(&api)?;
    println!("{}", api);

    Ok(())
}
