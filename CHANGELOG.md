# Changelog

All notable changes to this project will be documented in this file.

The format is based on [Keep a Changelog](https://keepachangelog.com/en/1.1.0/),
and this project adheres to [Semantic Versioning](https://semver.org/spec/v2.0.0.html).

## Unreleased

### Fixed

- `string.format("%.0f", ...)` (and `%.0e`, `%.0g`) raised `badarg` instead
  of formatting, because Erlang's own `io_lib:format("~.*f"/"~.*e"/"~.*g", ...)`
  cannot represent every precision C's `printf` (and Lua's `%f`/`%e`/`%g`,
  defined the same way) allows: `~f`/`~g` both refuse precision 0, and `~e`
  refuses anything below 2 (its own "precision" counts the leading digit
  too). `%.0f` now rounds to the nearest integer and omits the decimal
  point directly, matching C; `%.0g` clamps to precision 1, matching C's
  own stated rule ("if the precision is zero, it is taken as 1"); `%.0e`
  formats at Erlang's floor (1 fractional digit, already correctly rounded
  and renormalised by Erlang itself) and rounds that last digit away by
  hand, including the renormalisation that step can itself still trigger
  (9.5 and 9.96 both correctly become `1e+1`, not `10e+0`).

## [1.5.1]

### Fixed

- badges and logo in documentation
- illegal_token displaying integer value instead of characters

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
