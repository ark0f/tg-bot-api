mod custom;
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

    let api = openapi::generate(parsed)?;
    let api = serde_yaml::to_string(&api)?;
    println!("{}", api);

    Ok(())
}
