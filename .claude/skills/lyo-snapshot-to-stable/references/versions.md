# Eclipse Lyo Version History

All published releases available on Maven Central or Eclipse lyo-releases.

## Release Progression

```
Alpha -> Beta -> M1 (Milestone) -> M2 -> ... -> CR -> Final
```

Not every release line has all stages. Alphas come first, milestones after.

## 7.0.x (in development)

JDK 21 baseline. Jena 6. Jakarta EE. No Final yet.

| Version          | Repository     | Date         | Notes |
|------------------|----------------|--------------|-------|
| 7.0.0.Alpha9     | Maven Central  | Feb 06, 2026 | Jena 6, InMemPagedTRS concurrency |
| 7.0.0.Alpha8     | Maven Central  | Dec 01, 2025 | |
| 7.0.0.Alpha7     | Maven Central  | Nov 22, 2025 | |
| 7.0.0.Alpha6     | Maven Central  | Nov 22, 2025 | |
| 7.0.0.Alpha5     | Maven Central  | Nov 22, 2025 | |
| 7.0.0.Alpha4     | Maven Central  | Nov 22, 2025 | |
| 7.0.0.Alpha3     | Maven Central  | May 17, 2025 | Jena 4.10, json4j-provider removed |
| 7.0.0.Alpha2     | Maven Central  | Nov 15, 2024 | |
| 7.0.0.Alpha1     | Maven Central  | Nov 15, 2024 | |

## 6.0.x

JDK 17 baseline. Jakarta EE (javax -> jakarta). Jersey 3.1.5.

| Version          | Repository       | Date         | Notes |
|------------------|------------------|--------------|-------|
| 6.0.0.Final      | Maven Central    | Sep 29, 2024 | Production-ready |
| 6.0.0.CR         | Maven Central    | Sep 17, 2024 | |
| 6.0.0.Beta       | Eclipse Releases | Sep 02, 2024 | Eclipse repo only |
| 6.0.0.Alpha1     | Maven Central    | Mar 27, 2024 | |

## 5.1.x

| Version          | Repository     | Date         | Notes |
|------------------|----------------|--------------|-------|
| 5.1.1.Final      | Maven Central  | Jun 29, 2023 | Fixes 5.1.0 publication issue |
| 5.1.0.Final      | Maven Central  | Jun 29, 2023 | **YANKED** - use 5.1.1.Final |
| 5.1.0-alpha      | Maven Central  | Apr 28, 2023 | |

## 5.0.x

JDK 11 baseline. Jena 4.5.0. Addresses CVE-2021-41042.

| Version          | Repository     | Date         | Notes |
|------------------|----------------|--------------|-------|
| 5.0.1.CR         | Maven Central  | Apr 28, 2023 | |
| 5.0.0.Final      | Maven Central  | May 11, 2022 | Removes JDK 8, oslc-java-client, oslc4j-wink |
| 5.0.0.CR         | Maven Central  | May 06, 2022 | |

## 4.1.x

| Version          | Repository     | Date         | Notes |
|------------------|----------------|--------------|-------|
| 4.1.0            | Maven Central  | Dec 04, 2021 | Store improvements, CM support restored |
| 4.1.0.RC         | Maven Central  | Nov 30, 2021 | |
| 4.1.0.M1         | Maven Central  | Oct 13, 2021 | |

## 4.0.x

JDK 8 baseline. JAX-RS 2.0 (Wink removed). Maven Central. EPL 2.0. New OSLC Client.

| Version          | Repository       | Date         | Notes |
|------------------|------------------|--------------|-------|
| 4.0.0            | Maven Central    | Dec 17, 2020 | Monorepo, new client |
| 4.0.0.RC         | Maven Central    | Dec 08, 2020 | oslc4j-client is empty JAR |
| 4.0.0.M2         | Eclipse Releases | Nov 07, 2020 | **Eclipse repo only** |
| 4.0.0.M1         | Eclipse Releases | Oct 12, 2019 | **Eclipse repo only**, 6 artifacts |

## 2.x (Legacy)

All on Eclipse lyo-releases repository. Not all artifacts present in every early release.

| Version          | Artifacts | Date         | Notes |
|------------------|-----------|--------------|-------|
| 2.4.0            | 11        | Nov 25, 2018 | **BROKEN** - store-parent not published |
| 2.4.0.M1         | 10        | Nov 13, 2018 | Use this instead of 2.4.0 |
| 2.3.0            | 9         | Feb 23, 2018 | Jena 3.0.1 (com.hp.hpl.jena -> org.apache.jena) |
| 2.3.0.M3         | 9         | Feb 16, 2018 | |
| 2.2.0            | 8         | Aug 03, 2017 | Lyo Store introduced |
| 2.1.2            | 6         | Mar 17, 2017 | |
| 2.1.1            | -         | -            | **YANKED** - incomplete publication |
| 2.1.0            | 4         | Mar 10, 2014 | |
| 2.0.0            | 4         | Oct 02, 2013 | |

## Version Selection Guide

- **New projects**: `6.0.0.Final` (stable) or `7.0.0.Alpha9` (cutting edge)
- **JDK 21**: `7.0.0.Alpha9`
- **JDK 17**: `6.0.0.Final`
- **JDK 11 only**: `5.1.1.Final`
- **JDK 8 only**: `4.1.0`
- **Legacy**: `2.4.0.M1` (NOT 2.4.0)

## Broken/Yanked Versions

| Version | Issue | Use Instead |
|---------|-------|-------------|
| 2.4.0   | store-parent not published | 2.4.0.M1 or 4.0.0 |
| 2.1.1   | Incomplete Maven publication | 2.1.2 |
| 5.1.0   | Artifact publication failure | 5.1.1.Final |
