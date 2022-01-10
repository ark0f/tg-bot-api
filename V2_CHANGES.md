# The 2nd version changes

## Lists

Lists are not skipped during serialization anymore, so, for example, instead of

```json
{
  "type": "string"
}
```

you will see

```json
{
  "type": "string",
  "enumeration": []
}
```

because empty list and absent field are semantically same.

## Field/argument types

Type objects are separated everywhere (not flattened anymore).

Was:

```json
{
  "name": "reply_to_message_id",
  "description": "If the message is a reply, ID of the original message",
  "required": false,
  "type": "integer"
}
```

Become:

```json
{
  "name": "reply_to_message_id",
  "description": "If the message is a reply, ID of the original message",
  "required": false,
  "type_info": {
    "type": "integer"
  }
}
```

Because it's hard to deserialize such structures without powerful libraries like Rust's serde.

## Object types

Here you can see `type` is `properties`:

```json
{
  "name": "VoiceChatScheduled",
  "description": "This object represents a service message about a voice chat scheduled in the chat.",
  "type": "properties",
  "properties": [
    ...
  ],
  "documentation_link": "https://core.telegram.org/bots/api/#voicechatscheduled"
}
```

But here `type` is not exists because it nor has properties neither consists of other types:

```json
{
  "name": "VoiceChatStarted",
  "description": "This object represents a service message about a voice chat started in the chat. Currently holds no information.",
  "documentation_link": "https://core.telegram.org/bots/api/#voicechatstarted"
}
```

So I added `unknown` value for consistency:

```json
{
  "name": "VoiceChatStarted",
  "description": "This object represents a service message about a voice chat started in the chat. Currently holds no information.",
  "type": "unknown",
  "documentation_link": "https://core.telegram.org/bots/api/#voicechatstarted"
}
```

## "maybe_multipart"

Field `only_multipart` from `Method` object was renamed to `maybe_multipart` to be clearer about its meaning.
