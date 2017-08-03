# Lyo Store

[![Codacy Badge](https://api.codacy.com/project/badge/Grade/5f9560aee08b4c28a094b9fc2e3d43f2)](https://www.codacy.com/app/berezovskyi/lyo-store?utm_source=github.com&amp;utm_medium=referral&amp;utm_content=eclipse/lyo-store&amp;utm_campaign=Badge_Grade)

*Lyo Store* is a library that provides a simple interface for working with a triplestore via Java objects representing OSLC Resources. Lyo Store and Jena Models in the triple stores.

**[Javadoc](http://download.eclipse.org/lyo/docs/store/latest/)**

## Prerequisites

* For an in-memory option: JVM heap should be set to 1G or above (see [Jena documentation][1] for more information on this).
* For an on-disk option: an empty writable folder on disk, around 200MB of space is pre-allocated.
* For a SPARQL option: a pair of SPARQL Query and SPARQL Update URLs, optionally with basic authentication.

## Maven dependency

    <dependency>
      <groupId>org.eclipse.lyo.store</groupId>
      <artifactId>store-core</artifactId>
      <version>2.2.0</version>
    </dependency>

## Initialisation

### SPARQL

    String sparqlQueryEndpoint = properties.getProperty("sparqlQueryEndpoint");
    String sparqlUpdateEndpoint = properties.getProperty("sparqlUpdateEndpoint");
    Store store = StoreFactory.sparql(sparqlQueryEndpoint, sparqlUpdateEndpoint);

### On-disk

    String storeDirProp = properties.getProperty("storeDir");
    Path storeDir = Paths.get(storeDirProp);
    Store store = StoreFactory.onDisk(storeDir);

### In-memory

    Store store = StoreFactory.inMemory();

## Basic usage

### Add the new resources and overwrite the existing ones

```java
try {
    store.updateResources(GRAPH_NAME, // URI of the named graph
            resourceArray);           // an array of OSLC Resources
} catch (StoreAccessException e) {
    logger.error("Error executing a query on a triplestore");
}
```

### Retrieving the resources from the triplestore

```java
if (store.namedGraphExists(GRAPH_NAME)) {
    try {
        final int limit = 10; // fetch 10 resources
        final int offset = 0; // start with the first page
        // 'limit+1' is a technique that allows you to determine if there are
        // more results on the next page
        List<Requirement> requirements = store.getResources(GRAPH_NAME,
                Requirement.class, // resources of this class will be fetched and unmarshalled
                limit + 1,         // resource limit
                offset));          // how many resources to skip, use for paging
    } catch (StoreAccessException e) {
        logger.error("Error executing a query on a triplestore");
    } catch ( ModelUnmarshallingException e) {
        logger.error("Error unmarshalling the RDF from triplestore into Requirement class instances");
    }
}
```

[1]: https://jena.apache.org/documentation/tdb/architecture.html#caching-on-32-and-64-bit-java-systems
