# Changelog

All notable changes to this project will be documented in this file.

The format is based on [Keep a Changelog](https://keepachangelog.com/en/1.0.0/),
and this project adheres to [Semantic Versioning](https://semver.org/spec/v2.0.0.html).


## Changelog

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