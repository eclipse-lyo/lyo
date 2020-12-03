# Lyo Domains

[![CI](https://github.com/eclipse/lyo.domains/workflows/CI/badge.svg?branch=master)](https://github.com/eclipse/lyo.domains/actions?query=workflow%3ACI)
[![](https://img.shields.io/badge/javadoc-latest-blue.svg)](https://download.eclipse.org/lyo/docs/domains/latest/apidocs/)
[![Discourse users](https://img.shields.io/discourse/users?color=28bd84&server=https%3A%2F%2Fforum.open-services.net%2F)](https://forum.open-services.net/)


This repository contains Java implemenations of the OSLC Domain Specifications as defined by the [OASIS OSLC Lifecycle Integration for Domains (OSLC Domains) Technical Committee](https://www.oasis-open.org/committees/oslc-domains/charter.php).

There are 2 projects:

* *oslc-domains* is a maven project that contains a set of Java classes that implement a set of OSLC Domain Specifications. The classes are annotated using OSLC4J, and are hence ready to use using the [Lyo SDK](https://wiki.eclipse.org/Lyo).

* *org.eclipse.lyo.tools.domainmodels* contains a graphical model of the OSLC Domains. The model is defined using [Lyo Designer](https://wiki.eclipse.org/Lyo/Lyo_Designer), and from which the above Java classes are generated.

## Usage

Depending on your needs, there are 3 different ways to use these domain implementations in your project:

### Use the Java class implementations 

Simply add the following dependency to your project, and you are ready to create instances of the Java classes:
```xml
<dependency>
	<groupId>oslc-domains</groupId>
	<artifactId>oslc-domains</artifactId>
	<version>0.0.1-SNAPSHOT</version>
</dependency>
```

**NOTE!** Make sure you configure the pom.xml as for any OSLC project, as described [here](https://oslc.github.io/developing-oslc-applications/eclipse_lyo/setup-an-oslc-provider-consumer-application.html#customize-project-pom-file)

**NOTE!** Make sure this library uses the same version of Lyo Core as your project, by updating the ```<version.lyo.core>``` property in the pom.xml file.

### Use the Java classes with some modifications

If you need to extend or modify these reference implementations.

1. Clone this repository
1. import the *oslc-domains* project to Eclipse (or any other IDE) and modify its content as desired.
1. run ```mvn install``` on the *oslc-domains* project.
1. Add the maven dependency to your project as instructed above.

### Graphically modify and extend the OSLC domains

You can also augument and modify the classes graphically using [Lyo Designer](https://wiki.eclipse.org/Lyo/Lyo_Designer).

1. Clone this repository
1. import both the *oslc-domains* and *org.eclipse.lyo.tools.domainmodels* projects to Eclipse (Lyo Designer is an Eclipse plugin) 
1. Modify the model as instructed under [use Lyo Designer to model domain specifications](https://wiki.eclipse.org/Lyo/DomainSpecificationModelling).
1. generate the java classes, and make sure the generated classes reside under the *oslc-domains* project.
1. Install the *oslc-domains* maven project and include it in your project as instructed above.

