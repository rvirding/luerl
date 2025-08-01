# Changelog

All notable changes to this project will be documented in this file.

The format is based on [Keep a Changelog](https://keepachangelog.com/en/1.1.0/),
and this project adheres to [Semantic Versioning](https://semver.org/spec/v2.0.0.html).

## Unreleased

## [1.5.0]

### Changed

- luerl_io with some basic io functions added
- main Luerl discussions moved from slack to discord
- tostring can now handle __tostring and __name metakeys
- add ex_doc and move current docs to doc_legacy to avoid collisions

### Fixed

- luerl_scan is now Lua compliant and doesn't do utf-8 encoding
- cleanup parser and fix handling of local functions
- scanner now does Lua compliant numbers and strings
- improve handling of string.format
- add attributes for local variables
- add typespecs to make dialyzer happy
- improve documentation info in files
- README now links to discord instead
- fix pcall error messages

## [1.4.1]

### Fixed

- Move `doc/` folder to `doc_legacy/` so it doesn't collide with `ex_doc` artificats


## [1.4.0]

This version was not released to Hex.pm due to a build issue

### Changed

- (breaking) `luerl:get_private` returns `{ok, Val} | error` tuple

### Fixed

- files with only comments can now be loaded
- atoms are now decoded as strings
- Erlang functions that return errors are now properly propagated upward and state is updated
- binary error messages captured in pcall are not formatted


[unreleased]: https://github.com/olivierlacan/keep-a-changelog/compare/v1.4.1...HEAD
[1.3.0]: https://github.com/rvirding/luerl/compare/v1.4.0...v1.4.1
[1.3.0]: https://github.com/rvirding/luerl/compare/v1.3.0...v1.4.0
[1.3.0]: https://github.com/rvirding/luerl/compare/v1.2.3...v1.3.0
[1.2.3]: https://github.com/rvirding/luerl/compare/v1.2.2...v1.2.3
