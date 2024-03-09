Format: vX.Y.Z (DD-MM-YYYY)

# v0.6.0 (09.03.2024)

Parse `icon_color` field enumeration

# v0.5.0 (26.02.2024)

* Update dependencies
* Resolve #16

# v0.4.4 (20.11.2022)

Fix library tries to parse `default to the` as boolean

# v0.4.3 (15.05.2022)

Fix `Choose one` "one of" pattern is not recognized

# v0.4.2 (17.04.2022)

Fix links like `/bots/webapps` are not handled

# v0.4.1 (16.03.2022)

* Fix `deleteMessage` cannot be extracted
* Fix `" base quote` sentence cannot be parsed
* Fix description is not full for types like `PassportElementError`

# v0.4.0 (05.07.2021)

Yank v0.3.1 because of breaking changes

# v0.3.1 (05.07.2021)

* Add `min-max characters` min-max pattern for strings
* Add `always "something"` default pattern

# v0.3.0 (26.05.2021)

* Yank v0.2.6 because of breaking changes
* Return lexer error instead of panicking

# v0.2.6 (24.05.2021)

* Fix issue #11
* Fix issue #9

# v0.2.5 (09.05.2021)

* Fix issue #7
* Fix issue #8

# v0.2.4 (25.04.2021)

Update README.md (new domain for links)

# v0.2.3 (01.12.2020)

Add `Can be` pattern for enumeration detection
Fix `</br>` tag ignored when converting HTML to plain text

# v0.2.2 (30.11.2020)

Fix type parsing like `Array of Array of PhotoSize`

# v0.2.1 (28.11.2020)

Add `either` pattern for enumeration detection

# v0.2.0 (20.11.2020)

Add `Unknown` variant for `ObjectData` to cope better with future changes
