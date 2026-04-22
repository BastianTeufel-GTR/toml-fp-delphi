# Changelog

All notable changes to this project will be documented in this file.

The format is based on [Keep a Changelog](https://keepachangelog.com/en/1.0.0/),
and this project adheres to [Semantic Versioning](https://semver.org/spec/v2.0.0.html).


## Changelog

### v1.3.0 - Locale-Safe Float Serialization (2026-04-22)

#### Fixed
- **Float serialization ignored the host locale's `DecimalSeparator`.** On locales that use `','` as the decimal separator (German, French, etc.), `TTOMLSerializer` would emit invalid TOML such as `pi = 3,14`, which could not be round-tripped through the library's own parser. The serializer now routes `FloatToStr` and `FormatDateTime` through an invariant `TFormatSettings` with `DecimalSeparator := '.'` and `ThousandSeparator := #0`, so output is spec-compliant regardless of OS locale.

#### Added
- **Public `TOMLFormatSettings: TFormatSettings`** in the `TOML.Types` unit — a locale-invariant settings record that the serializer routes all numeric and date formatting through. Consumers that hand-build TOML fragments can reuse it via `uses TOML.Types` to stay spec-compliant (note: the `TOML` façade unit does not re-export variables).
- **DUnitX test** `TOML.Tests.Locale.TTOMLLocaleTests.Serialize_LocaleWithCommaDecimalSeparator_EmitsDotNotComma` and FPCUnit counterpart `Test90_SerializeLocaleWithCommaDecimalSeparator` that sabotage `DecimalSeparator := ','` during setup and verify round-trip correctness; guards against regressions.

#### Notes
- Verified on Delphi 12 (Win64) via DUnitX — 36/36 tests green under sabotaged locale. The FPCUnit mirror test (`Test90_SerializeLocaleWithCommaDecimalSeparator`) was added but not run in this release's CI environment; it should be executed on an FPC/Lazarus install as part of the next downstream verification.

### v1.2.0 - Comment Support & Multiline String Fix (2026-04-14)

#### Added
- **Full round-trip comment preservation:** Comments are now captured during parsing and reproduced during serialization
  - Standalone full-line comments (`# comment on its own line`) stored as `TTOMLComment` entries in the table body
  - Inline comments (`key = "value" # comment`) stored in the `InlineComment` property on `TTOMLValue`
- **Programmatic comment API:**
  - `TTOMLTable.AddComment(Text)` — adds a standalone comment line before the next key-value pair
  - `TTOMLValue.InlineComment` — read/write property for inline comments after a value
- **String style preservation:** `TTOMLStringStyle` enum (`tssBasic`, `tssLiteral`, `tssMultilineBasic`, `tssMultilineLiteral`) tracks the original quoting style through round-trips
- **Style-aware serialization:** Multiline strings (`"""..."""` and `'''...'''`) and literal strings (`'...'`) are now preserved in their original format instead of being flattened to basic double-quoted strings
- **Line-ending backslash:** Implemented TOML v1.0.0 line-ending backslash feature for multiline basic strings — a `\` at end of line trims the newline and leading whitespace on the next line
- `TTOMLTable.Body` property — ordered list of all entries (key-value pairs and comments) preserving insertion order
- `TOMLComment(Text)` helper function in the `TOML` facade unit
- DUnitX test suite for comments (12 tests) and multiline strings (11 tests), bringing total to 35 Delphi tests

#### Changed
- `TTOMLTable` restructured internally: uses an ordered body list (`TList<TTOMLTableEntry>`) for insertion-order iteration plus a dictionary index for fast key lookups. The `Items` property remains backward-compatible, pointing to the dictionary.
- `TTOMLString.Create` now accepts an optional `AStyle` parameter (default: `tssBasic`)
- Serializer iterates `TTOMLTable.Body` instead of the dictionary, preserving document structure and comment positions
- Lexer emits `ttComment` tokens instead of silently discarding comment text

#### Fixed
- Multiline literal strings (`'''...'''`) now correctly trim the first newline after the opening delimiter per TOML v1.0.0 spec (was only trimming for multiline basic strings)
- Literal string serialization now falls back to basic string format when the value contains single quotes or control characters

### v1.1.0 - TOML v1.0.0 Full Compliance (2026-03-20)

#### Added
- Implicit table merging in `TTOMLTable.Add`: dotted keys with shared prefixes (e.g., `physical.color` and `physical.shape`) now correctly merge into the same table hierarchy
- Parser creates nested `TTOMLTable` instances for dotted keys in key-value pairs per TOML v1.0.0 spec (was previously stored as flat string keys)
- Dotted keys inside inline tables now create nested structures (e.g., `{ x.y = 1, x.z = 2 }`)
- Serializer `BuildPath` helper for consistent full-path emission with proper key quoting
- Serializer now emits correct full paths for array-of-tables headers (e.g., `[[fruits.varieties]]` instead of `[[varieties]]`)
- Serializer tracks path context when recursing into array-of-table elements, so sub-tables and nested arrays produce correct headers (e.g., `[fruits.physical]`)
- DUnitX test project for Delphi 12 (`tests/delphi/`) with 12 tests covering nested keys, merging, serialization, and round-trip compliance
- Design spec and implementation plan in `docs/superpowers/specs/` and `docs/superpowers/plans/`

#### Changed
- Primary development and testing focus is now Delphi 12 (Win64)
- `Test68_KeyValidation` updated to assert nested table structure for dotted keys (was asserting flat key lookup)
- Free Pascal / Lazarus compatibility maintained via compiler conditionals but not actively tested

#### Fixed
- Serializer array-of-tables headers now include the full path from root (was emitting only the immediate key name)
- Serializer `FCurrentPath` is now correctly updated when recursing into array element tables
- Parser dotted keys in key-value pairs now create proper nested table hierarchy instead of concatenating into flat strings
- Duplicate dotted key detection now works correctly at all nesting levels

### v1.0.2 - Bug Fixes (2025-05-18)

- Fixed `NeedsQuoting` function to properly conform to TOML specification for bare keys
- Fixed serialization of nested tables to use correct dotted key notation (e.g., `[parent.child]`) instead of separate table declarations
- Fixed proper distinction between hierarchical nested tables and literal dotted keys
- Added dedicated tests for hierarchical nested tables (Test71) and literal dotted keys (Test72)
- Updated documentation to clarify the difference between hierarchical paths and literal dotted keys
- Added best practice recommendations from TOML spec about using bare keys when possible

### v1.0.1 - Bug Fixes (2025-03-24)

- Fixed serialization of arrays of tables to use the proper TOML format (`[[table]]`).
- Fixed parsing of arrays containing inline tables with newlines.
- Added additional test cases to verify these fixes.


## [1.1.0] - 2025-01-02

### Added
- Added Lazarus package
- Updated LICENSE.md
- Updated README.md

## [1.0.0] - 2025-01-01

### Added
- Initial release
- _Almost_ fully compliant to TOML v1.0.0 specification (covering all essential data types, structures, and edge cases.)
- Parsing and serialization functionality
- Comprehensive test suite (53 items)
- Documentation and examples