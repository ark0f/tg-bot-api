# tg-bot-api

[![Actions Status](https://github.com/ark0f/tg-bot-api/workflows/CI/badge.svg)](https://github.com/ark0f/tg-bot-api/actions)
![License](https://img.shields.io/crates/l/tg_bot_api.svg)
[![crates.io](https://img.shields.io/crates/v/tg-bot-api.svg)](https://crates.io/crates/tg-bot-api)
[![Documentation](https://docs.rs/tg-bot-api/badge.svg)](https://docs.rs/tg-bot-api)

Telegram Bot API parser in Rust

Generated schemas can be found at `ark0f.github.io/tg-bot-api`:

* [`/openapi.yml`](https://ark0f.github.io/tg-bot-api/openapi.yml) or
  [`/openapi.json`](https://ark0f.github.io/tg-bot-api/openapi.json) - OpenAPI 3.0.0
* [`/custom.json`](https://ark0f.github.io/tg-bot-api/custom.json) - custom schema that more convenient to work with
* [`/custom.schema.json`](https://ark0f.github.io/tg-bot-api/custom.schema.json) - JSON Schema Draft #7
  for `/custom.json`

`.min.json` suffix can be used to fetch minimized JSON. For example: `openapi.min.json`, etc.

## Automatic deploy

Schemas are deployed automatically every midnight at UTC+0 and when there is a new commit in tdlib/telegram-bot-api.
