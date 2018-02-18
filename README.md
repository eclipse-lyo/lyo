# Lyo Validation library

The library is intended to check whether resource instances conform to SHACL shapes.

## Getting started

Add the following repository to your POM file:

```xml
<repositories>
    <repository>
        <id>lyo-releases</id>
        <name>Eclipse Lyo Releases</name>
        <url>https://repo.eclipse.org/content/repositories/lyo-releases/</url>
        <snapshots>
            <enabled>false</enabled>
        </snapshots>
    </repository>
</repositories>
```

Now add a dependency for the library:

```xml
<dependency>
    <groupId>org.eclipse.lyo</groupId>
    <artifactId>lyo-validation</artifactId>
    <version>2.3.0.M3</version>
</dependency>
```

Start by reading up on [`Validator`](http://download.eclipse.org/lyo/docs/validation/2.3.0.M3/org/eclipse/lyo/validation/Validator.html) Javadoc.
