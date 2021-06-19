# Eclipse Lyo changelog

## [Unreleased]

### Added

- [Domains] Add `org.eclipse.lyo.domains:ui-classes` module with POJOs needed by [lyo.oslc-ui](https://github.com/eclipse/lyo.oslc-ui).
- Store adds interface support for closing the Store cleanly and releasing underlying connections. 
- SparqlStoreImpl can now be set up with a custom query executor

### Changed

- Update SHaclEX from 0.0.87 to 0.1.102 (mainly due to Bintray shutdown; breaking change but should not affect the consumers of Lyo Validation).
  - SHaclEX 0.1.102 depends on Jena 3.16, which requires us to upgrade.
  - Lyo Validation returns more messages in the reports. _Make sure your code logic scans all messages the report if you are looking for a specific error._
- Update Kotlin from 1.4.20 to 1.5.10  
- Update Eclipse Paho from 1.2.1 to 1.2.5 due to a potential security vulnerability.
- Pin libthrift version to 0.14.1 due to a [vulnerability](https://snyk.io/vuln/SNYK-JAVA-ORGAPACHETHRIFT-1074898).
- Pin httpclient version to 4.5.13 due to a [vulnerability](https://github.com/eclipse/lyo/pull/103).
- Pin embedded Tomcat version (pulled in by Jena) to 8.5.66 due to [CVE-2021-25329](https://app.snyk.io/vuln/SNYK-JAVA-ORGAPACHETOMCATEMBED-1080637)

### Deprecated

### Removed

- All references to HTTP-only repos due to [a change](https://maven.apache.org/docs/3.8.1/release-notes.html#how-to-fix-when-i-get-a-http-repository-blocked) in Maven 3.8.1
- Lyo Validation removed from the release due to the shutdown of Bintray and subsequent redeploy to Github Packages (not accessible without Github credentials).

### Fixed

- Make sure every Lyo dependency uses the same version of Guava (30.0)
- Fix a few issues around rdf:nil handling in the TRS Client

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
