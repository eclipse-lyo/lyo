# Migrating Lyo from Jena 4 to Jena 5

This document outlines the necessary steps to migrate Lyo-based projects from Apache Jena 4 to Apache Jena 5. The most significant change in Jena 5 relevant to Lyo is the removal of the `ReifiedStatement` interface and the standard reification mechanism changes.

## 1. Dependency Updates

Update your `pom.xml` to use the latest Jena 5 version.

```xml
<dependency>
    <groupId>org.apache.jena</groupId>
    <artifactId>jena-core</artifactId>
    <version>5.x.x</version>
</dependency>
<dependency>
    <groupId>org.apache.jena</groupId>
    <artifactId>jena-arq</artifactId>
    <version>5.x.x</version>
</dependency>
```

## 2. Reification Changes

Jena 5 removes the `ReifiedStatement` interface and the `Model.createReifiedStatement` methods. Reification is now handled via the `org.apache.jena.rdf.model.impl.ReifierStd` class (or `Reifier` in some contexts, but `ReifierStd` provides the static helpers we need).

### Writing Reified Statements

**Jena 4:**
```java
Statement statement = model.createStatement(subject, predicate, object);
ReifiedStatement rs = statement.createReifiedStatement();
rs.addProperty(DCTerms.title, "Reified Title");
```

**Jena 5:**
```java
import org.apache.jena.rdf.model.impl.ReifierStd;
import org.apache.jena.graph.Node;
import org.apache.jena.rdf.model.AnonId;

Statement statement = model.createStatement(subject, predicate, object);

// Reify the statement and get the blank node representing it
Node reifiedNode = ReifierStd.reifyAs(model.getGraph(), null, statement.asTriple());

// Create a Resource wrapper for the node to add properties
Resource reifiedResource = model.createResource(AnonId.create(reifiedNode.getBlankNodeLabel()));
reifiedResource.addProperty(DCTerms.title, "Reified Title");
```

### Reading Reified Statements

**Jena 4:**
```java
if (statement.isReified()) {
    RSIterator rsIter = statement.listReifiedStatements();
    while (rsIter.hasNext()) {
        ReifiedStatement rs = rsIter.next();
        // process rs
    }
}
```

**Jena 5:**
```java
import org.apache.jena.rdf.model.impl.ReifierStd;
import org.apache.jena.util.iterator.ExtendedIterator;

// Get all nodes that reify this statement
ExtendedIterator<Node> reifiedTriplesIter = ReifierStd.allNodes(model.getGraph(), statement.asTriple());

while (reifiedTriplesIter.hasNext()) {
    Node reifiedNode = reifiedTriplesIter.next();
    
    // Convert Node to Resource to access properties
    Resource reifiedResource;
    if (reifiedNode.isURI()) {
        reifiedResource = model.createResource(reifiedNode.getURI());
    } else if (reifiedNode.isBlank()) {
        reifiedResource = model.createResource(new AnonId(reifiedNode.getBlankNodeLabel()));
    } else {
        continue; // Should not happen for reified statements
    }
    
    // process reifiedResource
}
```

## 3. RDF/XML Parser Configuration

Jena 5 changes the default RDF/XML parser. For compatibility with legacy RDF/XML (e.g., `rdf:about` usage), you may need to explicitly register the `ARP1` parser.

```java
import org.apache.jena.riot.RDFParserRegistry;
import org.apache.jena.riot.Lang;
import org.apache.jena.riot.lang.rdfxml.ReaderRDFXML_ARP1;

// Register ARP1 parser for RDF/XML
RDFParserRegistry.registerLangTriples(Lang.RDFXML, ReaderRDFXML_ARP1.factory);
```

## 4. IRI Handling (Whitespace)

Jena 5 has stricter IRI validation. If you deal with legacy data containing spaces in URIs (which is technically invalid but sometimes encountered), you may need a custom `IRIProvider`.

```java
import org.apache.jena.irix.SystemIRIx;
import org.apache.jena.irix.IRIProvider;
// ... custom implementation of IRIProvider that sanitizes URIs ...

SystemIRIx.setProvider(new WhitespaceLaxIRIProvider(SystemIRIx.getProvider()));
```

## 5. Migration Strategies

There are two main strategies for migrating code that relies on `ReifiedStatement`:

### Strategy A: Direct Adaptation (Recommended)
Update the code to use `ReifierStd` directly as shown above. This is the approach taken in the Lyo Core (`JenaModelHelper`). It removes the dependency on the deprecated/removed `ReifiedStatement` interface.

### Strategy B: Wrapper/Adapter (Legacy Compatibility)
If you have a large codebase heavily reliant on the `ReifiedStatement` interface, you can create a wrapper class that implements your own `ReifiedStatement` interface (since the Jena one is gone) and delegates to `ReifierStd`.

Example Wrapper (inspired by `public.oslc.lyo` fork):
```java
public class ReifiedStatementImpl implements ReifiedStatement { // Your own interface
    private final Statement statement;
    private final Resource resource;

    public static ReifiedStatementImpl create(Statement statement) {
        Graph graph = statement.getModel().getGraph();
        Node reifiedNode = ReifierStd.reifyAs(graph, null, statement.asTriple());
        Resource resource = statement.getModel().createResource(AnonId.create(reifiedNode.getBlankNodeLabel()));
        return new ReifiedStatementImpl(resource, statement);
    }

    // ... implementation ...
}
```

## 6. Example: Lyo `JenaModelHelper` Adaptation

The `JenaModelHelper` class in Lyo has been updated to handle these changes.

**Serialization (Writing):**
```java
private static void addReifiedStatements(final Model model, final Statement statement, final IReifiedResource<?> reifiedResource, ...) {
    Graph graph = model.getGraph();
    Node reifiedNode = ReifierStd.reifyAs(graph, null, statement.asTriple());
    var reifiedJenaResource = model.createResource(AnonId.create(reifiedNode.getBlankNodeLabel()));
    
    // ... populate reifiedJenaResource ...
}
```

**Deserialization (Reading):**
```java
// Fill in any reified statements.
Graph stmtGraph = statement.getModel().getGraph();
ExtendedIterator<Node> reifiedTriplesIter = ReifierStd.allNodes(stmtGraph, statement.asTriple());
while (reifiedTriplesIter.hasNext()) {
    Node reifiedNode = reifiedTriplesIter.next();
    Resource reifiedStatement = getResource(statement.getModel(), reifiedNode);
    // ... populate object from reifiedStatement ...
}
```

## 7. Troubleshooting

### ExceptionInInitializerError / NullPointerException in RDF class
If you encounter `ExceptionInInitializerError` or `NullPointerException` related to `RDF` class initialization (e.g., `RDF.Alt` is null), it might be due to circular initialization dependencies when using TDB1 or other Jena components.
**Fix:** Explicitly initialize Jena system before accessing `RDF` constants in your test classes or static blocks.

```java
static {
    JenaSystem.init();
}
```

## 8. Additional Jena 5 Changes

### Deprecation Removal
Jena 5 removed many deprecated functions, methods, and classes, including those deprecated in Jena 4.10.0.

### QueryExecutionFactory
`QueryExecutionFactory` is simplified.
- Local execution: `QueryExecution.create()...`
- Remote execution: `QueryExecution.service(URL)...`

### QueryExecution Variable Substitution
"Substitution" is now preferred over "initial bindings".

### TDB1 Package Rename
`org.apache.jena.tdb` -> `org.apache.jena.tdb1`

### Fuseki
Fuseki now uses Jakarta namespace for servlets and Eclipse Jetty 12. Requires Tomcat 10+.
