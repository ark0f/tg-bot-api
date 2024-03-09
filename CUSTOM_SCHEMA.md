# Custom schema description

## Root

```json5
{
  // semver of bot API
  "version": {
    "major": 5,
    "minor": 6,
    "patch": 0
  },
  // when recent changes was made to bot API
  "recent_changes": {
    "year": 2021,
    "month": 12,
    "day": 30
  },
  // contains list of Method objects
  "methods": [
    ...
  ],
  // contains list of Object
  "objects": [
    ...
  ]
}
```

## Type

Basic structure:

```json5
{
  "type": "string",
  // and some additional fields if applicable
}
```

### "type": "integer"

```json5
{
  "type": "integer",
  // optional
  "default": 123,
  // optional
  "min": 0,
  // optional
  "max": 255,
  // there can be variants or list is empty
  "enumeration": [
    123,
    23,
    1423423
  ]
}
```

### "type": "string"

```json5
{
  "type": "string",
  // optional
  "default": "apple",
  // optional
  "min_len": 0,
  // optional
  "max_len": 100,
  // there can be variants or list is empty
  "enumeration": [
    "apple",
    "orange"
  ]
}
```

### "type": "bool"

```json5
{
  "type": "bool",
  // optional
  "default": "false",
}
```

### "type": "float"

No additional fields.

### "type": "any_of"

```json5
{
  "type": "any_of",
  // list of Type objects
  "any_of": [
    ...
  ]
}
```

### "type": "reference"

```json5
{
  "type": "reference",
  // refers to some Object
  "reference": "Update"
}
```

### "type": "array"

```json5
{
  "type": "bool",
  // refers to Type object
  "array": {
    ...
  }
}
```

## Method

```json5
{
  "name": "getUpdates",
  "description": "Use this method to receive incoming updates using long polling ([wiki](https://en.wikipedia.org/wiki/Push_technology#Long_polling)). An Array of [Update](https://core.telegram.org/bots/api/#update) objects is returned.",
  // see Argument heading
  "arguments": [
    ...
  ],
  // arguments may contain some Input* types (like InputMedia, InputFile, etc) 
  // so it is recommended to send request using multipart/form-data 
  "maybe_multipart": false,
  // see Type heading
  "return_type": {
    ...
  },
  "documentation_link": "https://core.telegram.org/bots/api/#getupdates"
}
```

## Argument

```json5
{
  "name": "allowed_updates",
  "description": "A JSON-serialized list of the update types you want your bot to receive. For example, specify [“message”, “edited\\_channel\\_post”, “callback\\_query”] to only receive updates of these types. See [Update](https://core.telegram.org/bots/api/#update) for a complete list of available update types. Specify an empty list to receive all update types except *chat\\_member* (default). If not specified, the previous setting will be used.  \n\nPlease note that this parameter doesn't affect updates created before the call to the getUpdates, so unwanted updates may be received for a short period of time.",
  // is argument required to be presented?
  "required": false,
  // see Type heading
  "type_info": {
    ...
  }
}
```

## Object

```json5
{
  "name": "GameHighScore",
  "description": "This object represents one row of the high scores table for a game.",
  // type of Object. Can be "properties", "any_of" or "unknown"
  "type": "properties",
  "documentation_link": "https://core.telegram.org/bots/api/#gamehighscore"
}
```

Explanation of Object's `type` field:

### "type": "properties"

```json5
{
  "name": "Update",
  "description": "This [object](https://core.telegram.org/bots/api/#available-types) represents an incoming update.  \nAt most **one** of the optional parameters can be present in any given update.",
  "type": "properties",
  // see Property heading
  "properties": [
    ...
  ],
  "documentation_link": "https://core.telegram.org/bots/api/#update"
}
```

### "type": "any_of"

This field means that Object is consists of other Type objects.

```json5
{
  "name": "PassportElementError",
  "description": "This object represents an error in the Telegram Passport element which was submitted that should be resolved by the user. It should be one of:",
  "type": "any_of",
  // list of Type objects
  "any_of": [
    {
      "type": "reference",
      "reference": "PassportElementErrorDataField"
    },
    {
      "type": "reference",
      "reference": "PassportElementErrorFrontSide"
    },
    ...
  ],
  "documentation_link": "https://core.telegram.org/bots/api/#passportelementerror"
}
```

### "type": "unknown"

This field means that Object nor has properties neither consists of other types

```json5
{
  "name": "CallbackGame",
  "description": "A placeholder, currently holds no information. Use [BotFather](https://t.me/botfather) to set up your game.",
  "type": "unknown",
  "documentation_link": "https://core.telegram.org/bots/api/#callbackgame"
}
```

## Property

```json5
{
  "name": "position",
  "description": "Position in high score table for the game",
  // is property required to be presented?
  "required": true,
  // see Type heading
  "type_info": {
    ...
  }
}
```
