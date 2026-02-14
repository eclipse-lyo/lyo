---
name: lyo-migration-5x-6x
description: >
  Migrate Eclipse Lyo projects from version 5.x to 6.x. High complexity migration involving
  Jakarta EE namespace change (javax to jakarta), JDK 17 baseline, Jersey 3.1.5,
  and servlet container upgrade.
  Use when upgrading a Lyo 5.x project to 6.x. Triggers: "migrate lyo 5 to 6", "upgrade to lyo 6",
  "lyo 6 migration", "javax to jakarta lyo", "jakarta migration lyo", "jdk 17 lyo".
  Note: Lyo 3.x does not exist (skipped to avoid confusion with OSLC 3.0).
---

# Lyo 5.x to 6.x Migration

High complexity. Every `javax.*` import must change to `jakarta.*`.

## Pre-release Stepping Stones

| Version      | Repository       | Date         | Notes |
|--------------|------------------|--------------|-------|
| 6.0.0.Alpha1 | Maven Central    | Mar 2024     | |
| 6.0.0.Beta   | Eclipse Releases | Sep 2024     | Eclipse repo only |
| 6.0.0.CR     | Maven Central    | Sep 2024     | |
| 6.0.0.Final  | Maven Central    | Sep 2024     | Recommended target |

## Breaking Changes

### Jakarta EE Namespace

ALL `javax.*` -> `jakarta.*`. Every Java EE import in every file.

Examples:
- `javax.servlet.*` -> `jakarta.servlet.*`
- `javax.ws.rs.*` -> `jakarta.ws.rs.*`
- `javax.inject.*` -> `jakarta.inject.*`

**Automation**: Eclipse Transformer can automate this. See https://projects.eclipse.org/projects/technology.transformer

### JDK 17 Baseline

JDK 11 and below removed. Tested with JDK 17, 21, 23, 24-ea.

### Jersey 2.35 -> 3.1.5

Jersey 3.x uses Jakarta namespace. Configuration may differ.

### Servlet Container

Must support Jakarta EE 9+. Tomcat 10+, Jetty 11+, etc.

### Servlet Dependencies

Before (5.x):
```xml
<dependency>
  <groupId>javax.servlet</groupId>
  <artifactId>javax.servlet-api</artifactId>
  <version>3.1.0</version>
  <scope>provided</scope>
</dependency>
<dependency>
  <groupId>javax.servlet</groupId>
  <artifactId>jstl</artifactId>
  <version>1.2</version>
</dependency>
```

After (6.x):
```xml
<dependency>
  <groupId>jakarta.servlet</groupId>
  <artifactId>jakarta.servlet-api</artifactId>
  <version>6.1.0</version>
  <scope>provided</scope>
</dependency>
<dependency>
  <groupId>jakarta.servlet.jsp.jstl</groupId>
  <artifactId>jakarta.servlet.jsp.jstl-api</artifactId>
  <version>3.0.0</version>
</dependency>
```

### Jersey Dependencies

Before (5.x):
```xml
<dependency>
  <groupId>org.glassfish.jersey.core</groupId>
  <artifactId>jersey-server</artifactId>
  <version>2.35</version>
</dependency>
```

After (6.x):
```xml
<dependency>
  <groupId>org.glassfish.jersey.core</groupId>
  <artifactId>jersey-server</artifactId>
  <version>3.1.5</version>
</dependency>
```

(Same for jersey-container-servlet, jersey-hk2)

### Other Changes

- `oslc_config:acceptedBy` cardinality corrected (String[0..1] -> Resource[0..*])
- Kotlin: `kotlin-stdlib-jdk8` -> `kotlin-stdlib`
- JSP taglib URIs need updating for Jakarta
- OSLC PROMCODE domain model added
- OAuth: capability to set `servletUri` for `OAuthConfiguration`

### web.xml Changes

Update namespace and init-param:
```xml
<!-- Before -->
<param-name>javax.ws.rs.Application</param-name>
<!-- After -->
<param-name>jakarta.ws.rs.Application</param-name>
```

## Lyo Designer

Lyo Designer 6.0.0.Final generates code for JDK 17+ with Jakarta packages. If using Designer, update it first.

| Designer Version | Date | Notes |
|------------------|------|-------|
| 6.0.0.Final | Sep 29, 2024 | Jakarta, JDK 17 |
| 6.0.0.Alpha2 | Mar 27, 2024 | Early Jakarta support |
