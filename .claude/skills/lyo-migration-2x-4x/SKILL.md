---
name: lyo-migration-2x-4x
description: >
  Migrate Eclipse Lyo projects from version 2.x to 4.x. The largest Lyo migration involving
  JAX-RS 2.0 upgrade (Wink removal), new OSLC Client, Maven Central migration, and EPL 2.0 license.
  Also covers migration from 1.x, 2.0.0, or 3.0.0-SNAPSHOT to 2.x as a prerequisite.
  Use when upgrading a Lyo 2.x project to 4.x, or from 1.x/3.0.0-SNAPSHOT.
  Triggers: "migrate lyo 2", "upgrade to lyo 4", "lyo 2 to 4", "wink to jersey",
  "jax-rs 2.0 migration", "lyo 3.0.0-SNAPSHOT", "lyo 1.x upgrade".
  Note: Lyo 3.x does not exist (skipped to avoid confusion with OSLC 3.0).
  3.0.0-SNAPSHOT was an abandoned dev effort merged into 2.2.
---

# Lyo 2.x to 4.x Migration

The most complex Lyo migration. Use pre-releases for gradual migration.

## Pre-release Stepping Stones

| Version    | Repository       | Date         | Key Change |
|------------|------------------|--------------|------------|
| 4.0.0.M1   | Eclipse Releases | Oct 2019     | JAX-RS 2.0, old client still works |
| 4.0.0.M2   | Eclipse Releases | Nov 2020     | More complete, old client still works |
| 4.0.0.RC   | Maven Central    | Dec 2020     | `oslc4j-client` replaced with empty JAR |
| 4.0.0      | Maven Central    | Dec 2020     | Final release |

## Breaking Changes

### JAX-RS: Apache Wink -> Jersey 2.x

This is the main breaking change. Wink was retired by Apache 5+ years prior.

- Custom JAX-RS providers need rewriting for JAX-RS 2.0
- web.xml configuration changes required
- `HTTPConstants` is no longer public

### Client Library

- `oslc4j-client` replaced with `oslc-client` (groupId: `org.eclipse.lyo.clients`)
- Old client deprecated but still shipped for standalone Java SE apps
- Using old client inside new JAX-RS 2.0 server is strongly discouraged (JAX-RS 1.1/2.0 conflict)
- Old client domain classes extracted to separate package and deprecated
- In 4.0.0.RC the old `oslc4j-client` JAR is intentionally empty to force migration

### Jena Package Rename

If coming from Lyo < 2.3.0: `com.hp.hpl.jena.*` -> `org.apache.jena.*`
(This actually happened in Lyo 2.3.0 with Jena 3.0.1 upgrade)

### TRS Changes

- `ChangeHistories` extracted to `PagedTrs` interface
- Implementing class renamed to `InmemPagedTrs`
- `TrackedResourceSetService` no longer abstract, supports constructor DI
- `TrackedResourceSetService` always has `@Path("/trs")`

### Other

- JDK: 7 -> 8+
- Repository: Eclipse Maven -> Maven Central (from RC onward)
- License: EPL 1.0 -> EPL 2.0
- `LyoJenaModelException` (checked) -> `LyoModelException` (unchecked)
- `ServletListener` renamed to `OAuthServletListener`
- Core: Only arrays from `@OslcQueryCapability` methods auto-treated as query results
- Domain classes regenerated via Lyo Designer, up to date with OSLC specs

## Gradual Migration Path

1. **Pin to 4.0.0.M1** (Eclipse repo required). Fix JAX-RS 2.0 compilation errors.
2. **Move to 4.0.0.M2** (Eclipse repo). Fix remaining issues.
3. **Move to 4.0.0.RC** (Maven Central). Switch `oslc4j-client` to `oslc-client`. Remove Eclipse repo config.
4. **Move to 4.0.0** (Maven Central). Final validation.

## Repository Configuration

M1 and M2 require the Eclipse repository:

```xml
<repository>
    <id>lyo-releases</id>
    <url>https://repo.eclipse.org/content/repositories/lyo-releases/</url>
</repository>
```

RC and Final are on Maven Central (no config needed). Remove the Eclipse repo when moving to RC.

## Deprecated in 4.0.0

These still work in 4.x but are removed in 5.0.0:
- `oslc4j-json4j-provider` (use Jena provider + JSON-LD)
- `JenaTdbStoreImpl`, `StoreFactory#inMemory`, `StoreFactory#onDisk` (use `SparqlStoreImpl`)

## After 4.0.0: Consider 4.1.0

Non-breaking upgrade from 4.0.0. Adds:
- Store improvements (custom query executor, clean close)
- Security pins (Guava 30.0, httpclient 4.5.13, Tomcat 8.5.66)
- Restores Configuration Management support accidentally removed in 4.0.0
- Validation update (SHaclEX 0.1.102)

## Migrating from 1.x, 2.0.0, or 3.0.0-SNAPSHOT

These versions need to reach 2.x first before the 2.x-to-4.x migration.

**3.0.0-SNAPSHOT** was an abandoned development effort (after 2.1.0 but before 2.2.0) that was merged into Lyo 2.2.

### Steps

1. Upgrade from JDK 7 to JDK 8
2. Switch Lyo version to **2.1.2** (except `lyo.server` group dependencies like `oauth-webapp` -- keep those at 2.1.0, as not all artifacts were present in 2.1.2)
3. Then follow the 2.x to 4.x migration above

### From 2.1.2

Ensure JDK 8 migration is complete, then proceed to the 2.x-to-4.x migration.

### From 2.2.0

Two major upgrades needed simultaneously:
- From Jena 2.x (HP Labs Jena) to Jena 4.x
- From JAX-RS 1.1 to JAX-RS 2.0 (Wink to Jersey; Wink was EOLed ca. 2015)

### From 2.4.0

Use **2.4.0.M1** (2.4.0 is broken -- `store-parent` not published). Migrate to the new Lyo Client, then follow the main migration path above.
