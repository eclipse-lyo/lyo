# Lyo Store

[![Codacy Badge](https://api.codacy.com/project/badge/Grade/5f9560aee08b4c28a094b9fc2e3d43f2)](https://www.codacy.com/app/berezovskyi/lyo-store?utm_source=github.com&amp;utm_medium=referral&amp;utm_content=eclipse/lyo-store&amp;utm_campaign=Badge_Grade)

*Lyo Store* is a library that can be used to persistently store OSLC resources in a triplestore. This can be useful to cache the results of REST resource requests for a faster retrieval of resources upon subsequent requests. Alternatively, a lifecycle tool may choose to preload its OSLC resources in the triplestore, allowing its OSLC server to provide OSLC services that interact directly with the artefacts consistently and conveniently managed using the expected RDF technologies. This, for example, makes it relatively easier for an OSLC server to provide clients with a TRS provider, or a SPARQL-endpoint for more advanced query capabilities.

The Store expects the OSLC resources to be instances of IResource subclasses, with appropriate OSLC annotations. Such subclasses can be defined manually. Better still, you can use [Lyo Designer](https://wiki.eclipse.org/Lyo/Lyo_Designer) to graphically model the domain and automatically generate OSLC4J-compliant Java code.

## Links
If you encounter any API questions along the way, consult the [Javadoc][javadoc] or ask a question on the [Lyo forum][forum].

* **[Lyo Store Javadoc][javadoc]**
* [File a new Lyo Store bug](https://github.com/eclipse/lyo-store/issues)
* [Ask a question on Eclipse Lyo forum][forum]
* **[Contribute to Eclipse Lyo!](https://wiki.eclipse.org/Lyo#Contributing_to_Lyo)**

## Installation

Lyo uses Maven as a primary choice for the build system. 

First, make sure your POM file includes the Eclipse Maven repositories ```<repositories>...</repositories>``` as indicated in the general [Creating OSLC4J Project](https://wiki.eclipse.org/Lyo/creating_OSLC4J_project#Lyo_repositories) instructions.

Now, add the following dependency:
```xml
<dependency>
    <groupId>org.eclipse.lyo.store</groupId>
    <artifactId>store-core</artifactId>
    <version>2.2.0</version>
</dependency>
```
Now you are all set to start using the library. 

**NOTE!** If you are using the older versions of Lyo (2.1.2 and lower), Maven might decide to use an incompatible version of Jena. In order to prevent this, enforce the Jena version used by `lyo-store` by adding the following tag to the `pom.xml`:

```xml
<dependencyManagement>
    <dependencies>
        <dependency>
            <groupId>org.apache.jena</groupId>
            <artifactId>jena-core</artifactId>
            <version>2.13.0</version>
        </dependency>
    </dependencies>
</dependencyManagement>
```

## Getting Started

Three concrete store implementations are provided allowing for in-memory, on-disk and a SPARQL-compatible Store implementation. You can instantiate any of these concrete implementations using the [StoreFactory class](http://download.eclipse.org/lyo/docs/store/latest/org/eclipse/lyo/store/StoreFactory.html).

### Initialisation

**SPARQL**

    String sparqlQueryEndpoint = properties.getProperty("sparqlQueryEndpoint");
    String sparqlUpdateEndpoint = properties.getProperty("sparqlUpdateEndpoint");
    Store store = StoreFactory.sparql(sparqlQueryEndpoint, sparqlUpdateEndpoint);

**On-disk**

    String storeDirProp = properties.getProperty("storeDir");
    Path storeDir = Paths.get(storeDirProp);
    Store store = StoreFactory.onDisk(storeDir);

**In-memory**

    Store store = StoreFactory.inMemory();

### Basic Usage and Examples

Once initialized, the main interface for operations on OSLC Resources with the backing triplestore is the [Store interface](http://download.eclipse.org/lyo/docs/store/latest/org/eclipse/lyo/store/Store.html).

The interface includes basic Create/Read/Update/Delete methods to manage the persistance of one or more resources under a given namedGraph.

**Add the new resources and overwrite the existing ones**

Assuming you have initialised an array of resource class instances in variable
`resourceArray`, in order to add the new resources and overwrite the existing
ones you can use the following flow:

```java
try {
    store.updateResources(GRAPH_NAME, // URI of the named graph
            resourceArray);           // an array of OSLC Resources
} catch (StoreAccessException e) {
    logger.error("Error executing a query on a triplestore");
}
```

**Retrieving resources from the triplestore**

In order to retrieve resources of type `Requirement` from the triplestore in batches of 200:

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

[javadoc]: http://download.eclipse.org/lyo/docs/store/latest/overview-summary.html
[forum]: https://www.eclipse.org/forums/index.php/f/228/