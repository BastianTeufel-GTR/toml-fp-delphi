# Changelog

All notable changes to this project will be documented in this file.

The format is based on [Keep a Changelog](https://keepachangelog.com/en/1.0.0/),
and this project adheres to [Semantic Versioning](https://semver.org/spec/v2.0.0.html).


## Changelog

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