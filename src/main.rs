mod custom;
mod extractor;
mod openapi;
mod parser;
mod util;

use crate::extractor::Extractor;
use std::fs;

const BOT_API_DOCS: &str = "https://core.telegram.org/bots/api/";

#[tokio::main]
async fn main() -> anyhow::Result<()> {
    let api = reqwest::get(BOT_API_DOCS).await?.text().await?;

    let extractor = Extractor::from_str(&api);
    let extracted = extractor.extract()?;
    let parsed = parser::parse(extracted)?;

    let api = openapi::generate(parsed.clone());
    let api_yml = serde_yaml::to_string(&api)?;
    fs::write("openapi.yml", api_yml)?;
    let api_json = serde_json::to_string_pretty(&api)?;
    fs::write("openapi.json", api_json)?;
    let api_min_json = serde_json::to_string(&api)?;
    fs::write("openapi.min.json", api_min_json)?;

    let (custom_schema, json_schema) = custom::generate(parsed);
    let json_schema = serde_json::to_string_pretty(&json_schema)?;
    fs::write("custom.schema.json", json_schema)?;
    let custom_schema = serde_json::to_string_pretty(&custom_schema)?;
    fs::write("custom.json", custom_schema)?;

    Ok(())
}
