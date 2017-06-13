# Eclipse Lyo core repository

This repository contains core library OSLC4J and its accompanying tests.

## Introduction

The Eclipse Lyo project is focused on providing an SDK to enable adoption of OSLC specifications. OSLC (Open Services for Lifecycle Collaboration) is an open community dedicated to reducing barriers for lifecycle tool integration. The community authors specifications for exposing lifecycle artifacts through uniform (REST) interfaces and relying on Internet and Linked Data standards.

OSLC's scope started with Application Lifecycle Management (ALM) and is expanding to include integrations across Product Lifecycle Management (PLM) and IT Service Management (ISM/DevOps), Lyo is designed to be a companion to the continuing specification efforts of the OSLC community. Its main purpose is to expand adoption of OSLC specifications and to enable the Eclipse community to easily build OSLC compliant tools.

## Getting started

If you are interested in using the library, please use the [wiki](https://wiki.eclipse.org/Lyo) and the [javadocs](http://download.eclipse.org/lyo/docs/core/2.1.2/overview-summary.html)

## CONTRIBUTING

### General information

Thanks for your interest in this project. The Eclipse project page is located at https://projects.eclipse.org/projects/technology.lyo. It points to the information regarding source code management, builds, coding standards, and more.

**Before your contribution can be accepted by the project, you need to create and electronically sign the [Eclipse Contributor Agreement (ECA)](https://eclipse.org/legal/ECA.php).**

### Useful links

* [lyo-dev mailing list](https://dev.eclipse.org/mailman/listinfo/lyo-dev)
* [open bugs](https://bugs.eclipse.org/bugs/buglist.cgi?product=Lyo)
* [report a new bug](https://bugs.eclipse.org/bugs/enter_bug.cgi?product=Lyo)

**Be sure to search for existing bugs before you create another one.** Remember that contributions are always welcome!

### Building the project

This project uses Maven as the build system for all Java projects except those which are Eclipse Plugin project. The latter projects contain all the Eclipse project files under Git for import and building using the *Import > Existing Projects into Workspace*. All other projects should be imported using the *Import > Existing Maven Projects* menu.

`core.query` project uses ANTLR for generating parser code. In order to configure Eclipse to use it, the `pom.xml` file contains some m2e-specific configuration. **After importing the projects, make sure to run *Maven > Update Project*.** If that does not work, you must add the directory `target/generated-sources/antlr3` under the *Java Project Build > Source*.
