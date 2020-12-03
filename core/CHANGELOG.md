# Lyo Core changelog

## [Unreleased/4.0.0]

### Added

- Support for abstract types by [@neormx](https://github.com/eclipse/lyo.core/pull/29).

### Changed

- **Switch to JAX-RS 2.**
- Only arrays returned from JAX-RS methods annotated with
`@OslcQueryCapability` will be automatically treated as OSLC Query results
unless the returned array type is annotated with the
`@OslcNotQueryResult` ([#88](https://github.com/eclipse/lyo.core/pull/88)).
- A checked `LyoJenaModelException` was replaced with an unchecked `LyoModelException`.

### Deprecated

- **JSON4J** has been deprecated in 2.x but is kept to maintain backwards compatibility. Jena provider is recommended together with the JSON-LD to serve JSON to OSLC Clients.

### Removed

- Extra MIME type restrictions on top of the ones enforced by the JAX-RS framework ([#22](https://github.com/eclipse/lyo.core/pull/22))
- Retired Apache Wink components.

### Fixed

- regression when `pathParameterValues` are not used ([#66](https://github.com/eclipse/lyo.core/pull/66))

---

See https://keepachangelog.com/en/1.0.0/ for more info.