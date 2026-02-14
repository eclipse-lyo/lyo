---
name: lyo-snapshot-to-stable
description: >
  Migrate Eclipse Lyo Maven projects from SNAPSHOT versions to stable releases.
  Use when: (1) switching a project from a Lyo SNAPSHOT dependency to a stable release,
  (2) fixing Lyo Maven repository configuration for stable versions,
  (3) checking which stable Lyo version to target,
  (4) troubleshooting Lyo dependency resolution after leaving SNAPSHOT.
  Triggers: "upgrade lyo", "switch to stable", "lyo version", "lyo snapshot", "lyo release",
  "maven lyo", "version.lyo", "lyo milestone", "lyo alpha", "lyo RC", "stop using snapshot".
  NOT for cross-version migration (e.g. 4.x to 5.x) -- use the dedicated migration skills instead.
---

# Lyo Snapshot-to-Stable Migration

## Current Versions

| Major | Latest Stable     | Min JDK | Jena |
|-------|-------------------|---------|------|
| 7.x   | 7.0.0.Alpha9      | 21      | 6.x  |
| 6.x   | **6.0.0.Final**   | 17      | 4.8  |
| 5.x   | 5.1.1.Final       | 11      | 4.5  |
| 4.x   | 4.1.0             | 8       | 3.x  |

For the complete version list, see [references/versions.md](references/versions.md).

## How to Choose the Right Stable Version

The goal: find the latest non-SNAPSHOT version that works with the current code, without changing API usage.

### Step 1: Check if the Final release compiles

Set `version.lyo` to the Final release for your SNAPSHOT line (e.g. `5.0.0-SNAPSHOT` -> `5.0.0.Final`). If it compiles and tests pass, use it.

### Step 2: If Final fails, binary search down through pre-releases

If the code uses APIs or artifacts that were removed/changed in the Final release, the code was written against an earlier state of the SNAPSHOT. Binary search through the available pre-releases to find the latest one that works:

```
Alpha1 -> Alpha2 -> ... -> Beta -> ... -> M1 -> M2 -> ... -> CR -> Final
```

Alphas come first, then Betas, then Milestones (M), then CR, then Final. Not every release line has all of these stages.

**Example**: `4.0.0-SNAPSHOT` code fails with `4.0.0` Final because it uses `oslc4j-client` (removed in RC). Try `4.0.0.M2` -- if it works, try `4.0.0.RC`. If RC fails, `4.0.0.M2` is your target.

### Special cases

**3.0.0-SNAPSHOT**: There is no Lyo 3.x release. The 3.0.0-SNAPSHOT was an abandoned development effort (after 2.1.0 but before 2.2.0) that was merged into Lyo 2.2. Switch to **2.1.2** (except `lyo.server` group dependencies like `oauth-webapp` -- keep at 2.1.0). Then upgrade through the 2.x-to-4.x migration skill.

**5.0.0-SNAPSHOT**: Has a detailed alpha-by-alpha stepping guide. Each alpha isolates specific changes:
1. Try **5.0.0.Final** first. If it works, done.
2. If not, try **5.0.0.alpha1** -- drops JDK 8, removes Wink/registry. Failure here = old Wink client usage or JDK issue.
3. **5.0.0.alpha2** -- adds Jena 4.0 upgrade. Failure here = Jena 4.0 incompatibility.
4. **5.0.0.alpha3** -- Jersey 2.25 to 2.35 upgrade. Also Jena 4.0 to 4.2.
5. **5.0.0.alpha4** -- Jena 4.3 SPARQL API changes.
6. **5.0.0.CR** then **5.0.0.Final** -- OSGi/Maven IDs unified.
7. Consider **5.1.1.Final** as final target (Jena CVE fix, 5.1.0 was YANKED).

### Key indicators for binary search

| If code uses...                    | Present in         | Removed/changed in    |
|------------------------------------|--------------------|-----------------------|
| `oslc4j-client` artifact           | 4.0.0.M1, M2      | 4.0.0.RC (empty JAR)  |
| `oslc-java-client`                 | 4.x                | 5.0.0                 |
| `oslc4j-wink`                      | 4.x                | 5.0.0                 |
| `oslc4j-registry`                  | 4.x                | 5.0.0                 |
| `oslc4j-json4j-provider`           | up to 6.x          | 7.0.0.Alpha3          |
| `javax.*` imports                  | up to 5.x          | 6.0.0                 |

## Migration Steps

### 1. Update `version.lyo` property

```xml
<properties>
  <version.lyo>6.0.0.Final</version.lyo>
</properties>
```

### 2. Remove snapshot repository

```xml
<!-- REMOVE if no longer using any SNAPSHOT dependencies -->
<repository>
    <id>central-portal-snapshots</id>
    <url>https://central.sonatype.com/repository/maven-snapshots/</url>
    ...
</repository>
```

### 3. Handle repository configuration

**Maven Central** (no config needed): All releases from 4.0.0.RC onward.

**Eclipse lyo-releases** (config required): 4.0.0.M1, 4.0.0.M2, and all 2.x releases.
```xml
<repository>
    <id>lyo-releases</id>
    <url>https://repo.eclipse.org/content/repositories/lyo-releases/</url>
</repository>
```

### 4. Verify and rebuild

```bash
mvn clean verify -U
```

## Lyo Designer Version Detection

If the project uses Lyo Designer, match the Designer version to the SDK version using these release dates:

| Lyo Designer Version | Release Date    | SDK Version |
|----------------------|-----------------|-------------|
| 6.0.0.Final          | Sep 29, 2024    | 6.x         |
| 6.0.0.Alpha2         | Mar 27, 2024    | 6.x         |
| 5.1.1.Final          | Jun 29, 2023    | 5.x         |
| 5.0.0.Final          | Nov 09, 2022    | 5.x         |
| 4.1.0.RELEASE        | Dec 06, 2021    | 4.x         |
| 4.0.0.RELEASE        | Dec 17, 2020    | 4.x         |

The `lyoVersion` in the toolchain `.xml` model file indicates which SDK version the generated code targets.

## Known Issues

- **2.4.0 is broken**: `store-parent` not published. Use `2.4.0.M1` or jump to `4.0.0`.
- **5.1.0 was YANKED**: artifact publication failure. Use `5.1.1.Final`.
- **2.1.1 was YANKED**: incomplete Maven publication. Use `2.1.2`.
- **Lyo Store** debuted in **2.2.0**.
- **Lyo 3.x does not exist**: skipped to avoid confusion with OSLC 3.0 spec.
- **Lyo <= 4.1.0**: vulnerable to CVE-2021-41042 (malicious RDF/XML).
- **Early 2.1/2.2 releases**: not all artifacts were present in every release.

## Cross-Version Migration

This skill only covers switching from SNAPSHOT to a stable release of the **same major version**. For upgrading across major versions (e.g. 4.x to 5.x), use the dedicated per-version migration skills.
