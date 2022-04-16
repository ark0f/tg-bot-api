#[macro_use]
extern crate ego_tree;

mod extractor;
mod parser;
mod util;

pub use extractor::ExtractorError;
pub use parser::{
    Argument, Field, Method, MethodArgs, Object, ObjectData, ParseError, Parsed, Type,
};

pub const CORE_TELEGRAM_URL: &str = "https://core.telegram.org";
pub const BOT_API_DOCS_URL: &str = "https://core.telegram.org/bots/api/";

use extractor::Extractor;

#[derive(Debug, thiserror::Error)]
pub enum Error {
    #[error("Extractor: {0}")]
    Extractor(
        #[from]
        #[source]
        ExtractorError,
    ),
    #[error("Parser: {0}")]
    Parse(
        #[from]
        #[source]
        ParseError,
    ),
}

pub fn get(html_doc: &str) -> Result<Parsed, Error> {
    let extractor = Extractor::from_str(html_doc);
    let extracted = extractor.extract()?;
    let parsed = parser::parse(extracted)?;
    Ok(parsed)
}
