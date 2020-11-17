mod custom;
mod openapi;

use std::{fs, path::PathBuf};
use tg_bot_api::BOT_API_DOCS_URL;

fn main() -> anyhow::Result<()> {
    let api = reqwest::blocking::get(BOT_API_DOCS_URL)?.text()?;
    let parsed = tg_bot_api::get(&api)?;

    let publish_dir = PathBuf::from("public");
    fs::create_dir(&publish_dir)?;

    let api = openapi::generate(parsed.clone());
    let api_yml = serde_yaml::to_string(&api)?;
    fs::write(publish_dir.join("openapi.yml"), api_yml)?;
    let api_json = serde_json::to_string_pretty(&api)?;
    fs::write(publish_dir.join("openapi.json"), api_json)?;

    let (custom_schema, json_schema) = custom::generate(parsed);
    let custom_schema = serde_json::to_string_pretty(&custom_schema)?;
    fs::write(publish_dir.join("custom.json"), custom_schema)?;
    let json_schema = serde_json::to_string_pretty(&json_schema)?;
    fs::write(publish_dir.join("custom.schema.json"), json_schema)?;

    Ok(())
}
