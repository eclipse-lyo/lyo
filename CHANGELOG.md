# Eclipse Lyo changelog

## [Unreleased]

### Added

### Changed

- Update Apache Jena dependency to v3.17

### Deprecated

### Removed

### Fixed


## [4.0.0] - 2020-12-16

See changelog summary under https://github.com/eclipse/lyo/releases/tag/v4.0.0 

### Added

- [Core] Support for abstract types by [@neormx](https://github.com/eclipse/lyo.core/pull/29).
- [Client] Constants for OSLC version (`OSLCConstants#OSLC{2_0,2_1,3_0}`) and a ctor argument for the new `OslcClient`.
- [TRS/Server] Unit tests are added based on the Jersey test library and Grizzly embedded server.

### Changed

- [Core] 🧨 **Switch to JAX-RS 2.**
- [Core] Only arrays returned from JAX-RS methods annotated with
`@OslcQueryCapability` will be automatically treated as OSLC Query results
unless the returned array type is annotated with the
`@OslcNotQueryResult` ([#88](https://github.com/eclipse/lyo.core/pull/88)).
- [Core] A checked `LyoJenaModelException` was replaced with an unchecked `LyoModelException`.
- [Server] `HTTPConstants` is no longer a public class
- [Server] `ServletListener` was renamed to `OAuthServletListener`
- [TRS/Server] `ChangeHistories` has been extracted to the `PagedTrs` interface and the implementing class has been renamed to `InmemPagedTrs`.
- [TRS/Server]`TrackedResourceSetService` is no longer abstract (still extendable).
  - `TrackedResourceSetService` supports constructor dependency injection of the `IChangeHistories` instance.
  - You can still extend it and override `getChangeHistories()` returning an instance of `SimpleChangeHistories` to get the same behaviour as in the earlier release.
- [TRS/Server] `TrackedResourceSetService` always has a `@Path("/trs")` (because it's hardcoded in other places).


### Deprecated

- [Core] **JSON4J** has been deprecated in 2.x but is kept to maintain backwards compatibility. Jena provider is recommended together with the JSON-LD to serve JSON to OSLC Clients.
- [Store] `JenaTdbStoreImpl` has been lagging behind in functionality and will be deprecated in this release. `StoreFactory#inMemory` and `StoreFactory#onDisk` are also deprecated as they rely on this implementation. *Use `SparqlStoreImpl#SparqlStoreImpl(JenaQueryExecutor)` to pass a `DatasetQueryExecutorImpl` instance instead.*

### Removed

- [Core] Extra MIME type restrictions on top of the ones enforced by the JAX-RS framework ([#22](https://github.com/eclipse/lyo.core/pull/22))
- [Core] Retired Apache Wink components.

### Fixed

- [Core] regression when `pathParameterValues` are not used ([#66](https://github.com/eclipse/lyo.core/pull/66))
- [Client] JEEFormAuthenticator catches ProcessingException when closing the connection from last redirect

## [3.0.0] - SKIPPED

> Where is Lyo 3.0? There was a Lyo 3.0.0-SNAPSHOT development effort long time ago (after 2.1.0 but before 2.2.0), which has been since abandoned and merged into Lyo 2.2. Furthermore, next version of OSLC will be OSLC 3 and Lyo 3 might mislead users that it adds OSLC 3 support.

## [2.4.0]

See [release notes](https://open-services.net/news/2018-eclipse-lyo-2.4.0/) for further details.

### Changed

- Update Apache Jena dependency to v3.6.0

## [2.3.0]

### Changed

- 🧨 Update Apache Jena dependency to v3.0.1. Breaking change in Jena package naming `com.hp.hpl.jena => org.apache.jena` [[JENA-192](https://issues.apache.org/jira/browse/JENA-192)]

## [2.2.0]

### Changed

- Update Apache Jena dependency to v2.13.0

## [2.1.2]

### Added

- Java 8 support

## [2.1.1] - YANKED

*Not all components were released on Maven properly.*

## [2.1.0]

## [2.0.0]

## [1.1.0]

## [1.0.1]

## [1.0.0]

Initial release. Used Apache Jena 2.7.1


---

**Template**

## [Unreleased]

### Added

### Changed

### Deprecated

### Removed

### Fixed


> See https://keepachangelog.com/en/1.0.0/ for more info.
