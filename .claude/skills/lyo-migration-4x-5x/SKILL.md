---
name: lyo-migration-4x-5x
description: >
  Migrate Eclipse Lyo projects from version 4.x to 5.x. Medium complexity migration involving
  JDK 11 baseline, Jena 4.5.0 upgrade (CVE fix), removal of legacy components (Wink, old client),
  and TRS BigInteger changes. Includes step-by-step alpha-by-alpha migration guide.
  Use when upgrading a Lyo 4.x project to 5.x. Triggers: "migrate lyo 4 to 5", "upgrade to lyo 5",
  "lyo 5 migration", "jdk 11 lyo".
---

# Lyo 4.x to 5.x Migration

Medium complexity. Primarily a JDK upgrade and removal of deprecated components.

## Step-by-Step Migration (from 4.0.0 or 4.1.0)

Follow these steps in order. Each alpha isolates a specific set of changes so you can identify what breaks.

### Step 1: Try 5.0.0.beta1

If everything works, done. Otherwise continue below.

### Step 2: Switch to 5.0.0.alpha1

This release drops JDK 8, removes `server/wink` and `server/registry`.

- **Build fails due to missing Lyo artifacts?** You were using the old Wink-based client. Migrate to the new Lyo client introduced in 4.0.0 (`oslc-client` from `org.eclipse.lyo.clients`).
- **Build fails for other reasons, or runs incorrectly?** JDK upgrade issue.

### Step 3: Switch to 5.0.0.alpha2

- **Build fails due to missing artifacts?** Still using old Wink-based client. Migrate to new client.
- **Build fails otherwise, or runs incorrectly?** Jena 4.0 upgrade. Check Lyo project git history to see what code changes were needed for Jena 4.0.

### Step 4: Switch to 5.0.0.alpha3

- **Build fails?** Most likely the Jersey upgrade from 2.25 to 2.35.
- Jena upgraded from 4.0.0 to 4.2.0 but no breakage expected.

### Step 5: Switch to 5.0.0.alpha4

- **Build fails?** Most likely due to [SPARQL-related changes in Jena 4.3](https://jena.apache.org/documentation/sparql-apis/#changes).

### Step 6: Switch to 5.0.0.CR, then 5.0.0.Final

OSGi and Maven IDs are fully unified at this point.

### Step 7: Consider upgrading to 5.1.1.Final

Recommended final target:
- Fixes Jena CVE-2023-22665 (Jena 4.5.0 -> 4.8.0)
- Refactored `oslc4j-core` to eliminate Wink dependencies
- OSLC Config domain model expanded
- Bug fixes

**Note**: 5.1.0 was YANKED. Use 5.1.1.Final.

## Migration from 2.4.0

Migrate to the new Lyo Client first, then follow the 4.x -> 5.0 guide above.

Also delete Eclipse Maven repositories from POM files (Lyo is on Maven Central since 4.0).

## Migration from 2.2.0

Two major upgrades needed:
- From Jena 2.x (HP Labs Jena) to Jena 4.x
- From JAX-RS 1.1 to JAX-RS 2.0 (Wink to another JAX-RS framework; Wink was EOLed ca. 2015)

## Migration from 2.1.2

Ensure JDK 8 migration is complete, then continue with the 2.2.0 steps above.

## Migration from 1.x, 2.0.0, or 3.0.0-SNAPSHOT

1. Upgrade from JDK 7 to JDK 8
2. Switch Lyo version to 2.1.2 (except for `lyo.server` group dependencies like `oauth-webapp` -- keep those at 2.1.0)
3. Continue with the 2.2.0 upgrade steps

## Breaking Changes Summary

### JDK 11 Baseline

JDK 8 support removed.

### Jena 3.x -> 4.5.0

- `RDFReader` renamed to `RDFReaderI`
- `RDFWriter` renamed to `RDFWriterI`

### TRS BigInteger

`trs:order` properties now use `BigInteger` instead of 32-bit ints.

### OslcQueryResult

Throws `LyoModelException` instead of `IllegalStateException`.

## Removed Components

| Removed | Replacement |
|---------|-------------|
| `oslc-java-client` | `oslc-client` (from 4.0.0+) |
| `oslc4j-wink` | n/a (fully removed) |
| `oslc4j-registry` | n/a |
| `lyo-validation` | Temporarily removed |
| Direct TDB1 Store backend | `SparqlStoreImpl` with `DatasetQueryExecutorImpl` |
| `TrsUtil.historyDataToChangeEvent` | `changeEventToHistoryData` |

## Store Behavior Changes

- Resources ordered by subject IDs. Disable: `OSLC4JUtils.setLyoStorePagingUnsafe(true)`
- Precise paging limit on by default. Disable: `OSLC4JUtils.setLyoStorePagingPreciseLimit(false)`
