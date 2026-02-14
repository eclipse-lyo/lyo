---
name: lyo-migration-6x-7x
description: 
  Migrate Eclipse Lyo projects from version 6.x to 7.x. Medium-high complexity migration involving
  Jena 6 upgrade (major), JDK 21 baseline, removal of json4j-provider, ReifiedStatement removal,
  and security fixes. 7.x has no Final release yet.
  Use when upgrading a Lyo 6.x project to 7.x. Triggers: "migrate lyo 6 to 7", "upgrade to lyo 7",
  "lyo 7 migration", "jena 6 lyo", "jena 5 lyo", "jdk 21 lyo".
---

# Lyo 6.x to 7.x Migration

Medium-high complexity. Major Jena upgrade (4.8 -> 6). No Final release yet.

## Pre-release Stepping Stones

Two major breakpoints in the alpha series:

| Version        | Date         | Key Change |
|----------------|--------------|------------|
| 7.0.0.Alpha1-2 | Nov 2024     | Initial 7.x |
| **7.0.0.Alpha3** | May 2025   | **Jena 4.10**, json4j-provider removed |
| 7.0.0.Alpha4-7 | Nov 2025     | Incremental |
| 7.0.0.Alpha8   | Dec 2025     | |
| **7.0.0.Alpha9** | Feb 2026   | **Jena 6**, JDK 21 baseline |

**Alpha3** is a good intermediate target (Jena 4.10, still JDK 17). **Alpha9** is the latest (Jena 6, JDK 21 required).

## Breaking Changes at Alpha3

### Jena 4.8 -> 4.10

Addresses CVE-2023-32200.

### json4j-provider Removed

`oslc4j-json4j-provider` dependency removed. Use `oslc4j-jena-provider` with JSON-LD instead.

## Breaking Changes at Alpha9

### Jena 4.10 -> Jena 6 (Major)

This is a significant upgrade spanning Jena 5 and 6. See `C:\src\oslc\lyo\docs\Jena5_Migration_Guide.md`.

Key changes:
- `ReifiedStatement` interface removed. Use `ReifierStd` from `org.apache.jena.rdf.model.impl`
- RDF/XML parser changed. For legacy data, register ARP1 parser explicitly:
  ```java
  RDFParserRegistry.registerLangTriples(Lang.RDFXML, ReaderRDFXML_ARP1.factory);
  ```
- Stricter IRI validation (whitespace in URIs rejected)
- `QueryExecutionFactory` simplified
- TDB1 package: `org.apache.jena.tdb` -> `org.apache.jena.tdb1`
- JSON-LD 1.1 used instead of 1.0
- Deprecated functions removed in Jena 5 (deprecated since 4.10)

### JDK 21 Baseline

JDK 17 support removed.

### lyo-validation Removed

Was using SHACLex which couldn't be migrated to Jena 5. Lyo Validation now uses Jena's native SHACL support.

### Commons Lang

Commons Lang 1 replaced with Commons Lang 3. Addresses CVE-2025-48924.

## Other Changes (Non-breaking)

- Extended properties support arrays and iterables
- Reified statements deserialized into structured data
- OSLC RDF graphs can be deserialized into raw Jena `Model.class`
- `Error`/`ExtendedError` extend `AbstractResource` (supports extended properties)
- InMemPagedTRS handles concurrency
- OslcQueryResult: InputStream preferred over Response for initialization
- RootServicesHelper can be initialized with InputStream
- Stack overflow fix in JenaModelHelper for resource graph loops
- Client picks correct ResponseInfo for multi-ResponseInfo queries

## Jena 5/6 Migration Code Examples

### Writing Reified Statements

Before (Jena 4):
```java
ReifiedStatement rs = statement.createReifiedStatement();
rs.addProperty(DCTerms.title, "Title");
```

After (Jena 6):
```java
Node reifiedNode = ReifierStd.reifyAs(model.getGraph(), null, statement.asTriple());
Resource reifiedResource = model.createResource(AnonId.create(reifiedNode.getBlankNodeLabel()));
reifiedResource.addProperty(DCTerms.title, "Title");
```

### Reading Reified Statements

Before (Jena 4):
```java
RSIterator rsIter = statement.listReifiedStatements();
```

After (Jena 6):
```java
ExtendedIterator<Node> iter = ReifierStd.allNodes(model.getGraph(), statement.asTriple());
```

### Troubleshooting: ExceptionInInitializerError

If `RDF.Alt` is null or similar initialization errors occur:
```java
static {
    JenaSystem.init();
}
```

## Resources

- Jena 5/6 migration guide: `/docs/Jena5_Migration_Guide.md`
