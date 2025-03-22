# Eclipse Lyo changelog

## [7.0.0-SNAPSHOT]

### Security

- 🔒️ Apache Jena dependency was updated from v4.8 to v4.10. Some changes have a breaking nature. **Addresses [CVE CVE-2023-32200](https://cve.mitre.org/cgi-bin/cvename.cgi?name=CVE-2023-32200)** 

### Added

This release does not contain new features.

### Changed

- TRS Client uses Lyo Store instead of using Eclipse RDF4J directly.

### Deprecated

This release does not introduce deprecations.

### Removed

- Dependency to deprecated oslc4j-json4j-provider

### Fixed

- Client now picks the correct ResponseInfo object when an OSLC Query response contains multiple ResponseInfo objects.
- Lyo object-graph mapping (OGM) framework no longer registers duplicate classes when doing recursive scans. 

## [6.0.0]

### Security

This release does not contain security updates.

### Added

- Introducing capability to set the servletUri to be used by the OAuthConfiguration
- `Store.rawUpdateQuery(String)` allows making raw SPARQL UPDATE queries.

### Changed

- Kotlin 1.9.0 is used; `kotlin-stdlib-jdk8` dependency was replaced with `kotlin-stdlib` due to [Kotlin updates](https://kotlinlang.org/docs/whatsnew18.html#updated-jvm-compilation-target).
- Allow application to reset the oauth token cached within the server, when it deems that it is no longer valid
- 🧨Corrected cardinality and range of the oslc_config:acceptedBy property (from String[0..1] to Resource[0..*])

### Deprecated

This release does not introduce deprecations.

### Removed

- 🧨 Support for JDK 11 (and all versions below 17) is removed. **JDK 17 is the new baseline for Eclipse Lyo.** The SDK and sample code has been tested using JDK 17, 20, and 21-ea.
- TRS Client no longer depends on Eclipse RDF4J. Helper methods for RDF4J were also removed.

### Fixed

This release does not contain bug fixes.

## [5.1.1]

### Added

- The OSLC Config domain model was expanded.
- An `OSLC4JUtils::isWellFormed` method was added to help check the validity of strings as valid XML literals when the inputs are supposed to be used on properties of the `rdf:XMLLiteral` type. Warning: this method is quite slow, especially if a resource contains 10s or 100s of such values. We recommend to use this method only in tests.

### Changed

- Consistently save Credentials, Connector & admin_session values under session attribute, in the oauth-core AbstractAdapterCredentialsFilter class.
- Apache Jena depency was updated from 4.5.0 to 4.8.0 due to [CVE-2023-22665](https://lists.apache.org/thread/m7lg8m88cqpjjx8g75d8kbqcrjysdhb9).
- OSLC Domains are based on latest LyoDesigner, and for `lyoVersion="5.0.1-SNAPSHOT"`.
- The `oslc4j-core` artifact (group `org.eclipse.lyo.oslc4j.core`) was refactored, extracting some essential model classes into `lyo-core-model` and legacy Wink-dependent code into `oslc4j-core-wink`. _This allows to eliminate all dependencies on Wink. Also, applications that don't need `oslc4j-core`, can eliminate the dependency on JAX-RS by replacing your dependency on `oslc4j-core` with `lyo-core-model`._ **No breaking changes were made.**
- `JenaModelHelper` was moved to `oslc4j-core`, while `JsonHelper` was moved to `oslc4j-core-wink`. **No breaking changes were made, old package names were kept.** See #292
- `OSLC4JConstants` and `OSLC4JUtils` were extracted from `oslc4j-core` to `org.eclipse.lyo.oslc4j.server:lyo-core-settings` as they are only useful in the JAX-RS server-side scenarios. **No breaking changes were made**, because `oslc4j-core` still depends on the newly introduced artifact.

### Deprecated

- The newly refactored artifact `oslc4j-core-wink` is deprecated.

### Removed

### Fixed

- A regression is fixed where `org.eclipse.lyo.oslc4j.provider.jena.OslcRdfXmlProvider` could try to unmarshal an Array or a Collection, which would interfere with the application of the suitable providers when the RDF input is correct.
- [PR 260](https://github.com/eclipse/lyo/pull/260) fixed the [bug](https://github.com/eclipse/lyo/pull/259) in `ResourcePackages.getMostConcreteClassOf` that could trigger the error "Multiple classes, not in the same inheritance tree, are annotated to map the same RDF:type".

## [5.1.0] - YANKED

The release was yanked due to a problem with the publication of release artifacts to Maven Central. Please use `5.1.1.Final` instead.

## [5.0.0]

### Added

- LyoStore: Providing a system property `OSLC4JUtils.hasLyoStorePagingPreciseLimit()` to allow the application to check whether query paging should return an exact number of elements in a paged query, or not (`OSLC4JConstants.LYO_STORE_PAGING_PRECISE_LIMIT`).
- SRI hashes for JS libraries.

### Changed
- **Addresses a security vulnerability ([CVE-2021-41042](https://nvd.nist.gov/vuln/detail/CVE-2021-41042)), which could lead to external resource loading using a maliciously crafted RDF/XML input.**
- 🧨 Lyo is now built using JDK 11
- 🧨 Jena is upgraded to 4.5.0 **(addresses [CVE-2022-28890](https://cve.mitre.org/cgi-bin/cvename.cgi?name=CVE-2022-28890))**
  - Jena renamed `RDFReader/RDFWriter` to `RDFReaderI/RDFWriterI`
- 🧨 TRS now uses BigInteger instead of 32-bit ints for `trs:order` properties, in line with the spec. 
- LyoStore: Ordering resources by their subject IDs when doing a query to store. This ordering can be disabled with a call to `OSLC4JUtils.setLyoStorePagingUnsafe(true)`
- LyoStore: `OSLC4JUtils.hasLyoStorePagingPreciseLimit()` will return true by default. Call `OSLC4JUtils.setLyoStorePagingPreciseLimit(false)` to restore the old behavior.
- `oslc4j-json4j-provider` uses `wink-json4j` version 1.4 instead of 1.2.1-incubating.
-oauth - allow for the possibility to set the official servlet URL for all oauth requests being made. This is important to set correctly to compute the digital signature.
- `OslcQueryResult` will now rethrow any encountered exceptions wrapped in a `LyoModelException` instead of throwing a blanked `IllegalStateException`.

### Deprecated

- `IQueryGenerator` is deprecated and will be removed in Lyo 6

### Removed
- 🧨 **Support for JDK 8 was removed**
- 🧨 `oslc-java-client` was removed
- **`lyo-validation` temporarily removed from the build**
- `oslc4j-wink` was removed
- `oslc4j-registry` was removed
- Store support for direct TDB1 backend was removed. You can still create a SPARQL query executor over an in-mem TDB1 dataset: `new DatasetQueryExecutorImpl(TDBFactory.createDataset())`.
- `TrsUtil.historyDataToChangeEvent` (deprecated since 4.0) was removed and replaced with `changeEventToHistoryData`. _Despite the name, the `historyDataToChangeEvent` was doing round-tripping between `HistoryData` and `ChangeEvent` in both directions._

### Fixed

- Stack traces are no longer printed from OAuth services.

## [4.1.0]

### Added

- [Core] Add a oslc:name and dcterms:description attributes to the ResourceShape class so that such information can appear in the shape documentation.
- [Core] Allow a resource shape to have a property to be represented as "inlined".
- [Domains] Add `org.eclipse.lyo.domains:ui-classes` module with POJOs needed by [lyo.oslc-ui](https://github.com/eclipse/lyo.oslc-ui).
- [UI] Support for displaying inlined properties as links.
- [Store] Add interface support for closing the Store cleanly and releasing underlying connections. 
- [Store] SparqlStoreImpl can now be set up with a custom query executor.

### Changed

- [Store] For update requests, avoid creating a String out of the SPARQL query. This is very inefficient for large queries.
- [Validation] Update SHaclEX from 0.0.87 to 0.1.102 (mainly due to Bintray shutdown; breaking change but should not affect the consumers of Lyo Validation).
  - SHaclEX 0.1.102 depends on Jena 3.16, which requires us to upgrade.
  - Lyo Validation returns more messages in the reports. _Make sure your code logic scans all messages the report if you are looking for a specific error._
- Update Kotlin from 1.4.20 to 1.5.10  
- Update Eclipse Paho from 1.2.1 to 1.2.5 due to a potential security vulnerability.
- Pin libthrift version to 0.14.1 due to a [vulnerability](https://snyk.io/vuln/SNYK-JAVA-ORGAPACHETHRIFT-1074898).
- Pin httpclient version to 4.5.13 due to a [vulnerability](https://github.com/eclipse/lyo/pull/103).
- Pin embedded Tomcat version (pulled in by Jena) to 8.5.66 due to [CVE-2021-25329](https://app.snyk.io/vuln/SNYK-JAVA-ORGAPACHETOMCATEMBED-1080637).

### Deprecated

N/A

### Removed

- All references to HTTP-only repos due to [a change](https://maven.apache.org/docs/3.8.1/release-notes.html#how-to-fix-when-i-get-a-http-repository-blocked) in Maven 3.8.1

### Fixed

- [Client] Support for Configuration Management was first added in 2.4 and was accidentally removed during the monorepo migration and the 4.0 release. [PR #205](https://github.com/eclipse/lyo/pull/206) adds this support back. Thanks to @isccarrasco!
- [TRS] Fix a few issues around rdf:nil handling in the TRS Client.
- Make sure every Lyo dependency uses the same version of Guava (30.0).

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

### Security

This release does not contain security updates.

### Added

This release does not contain new features.

### Changed

This release does not contain other significant changes.

### Deprecated

This release does not introduce deprecations.

### Removed

This release does not remove any features.

### Fixed

This release does not contain bug fixes.



> See https://keepachangelog.com/en/1.0.0/ for more info.
