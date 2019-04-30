# Eclipse Lyo core repository

[![](https://img.shields.io/jenkins/s/https/ci.eclipse.org/lyo/job/lyo-core-master.svg)](https://ci.eclipse.org/lyo/job/lyo-core-master/)
[![Build Status](https://travis-ci.org/eclipse/lyo.core.svg?branch=master)](https://travis-ci.org/eclipse/lyo.core)
[![](https://img.shields.io/badge/javadoc-2.4.0-blue.svg)](https://download.eclipse.org/lyo/docs/core/2.4.0/)
[![](https://img.shields.io/badge/misc-discourse-lightgrey.svg)](https://forum.open-services.net/)
[![](https://img.shields.io/badge/misc-gitter-lightgrey.svg)](https://gitter.im/eclipse/lyo)


This repository contains core library OSLC4J and its accompanying tests.

## Introduction

The Eclipse Lyo project is focused on providing an SDK to enable adoption of OSLC specifications. OSLC (Open Services for Lifecycle Collaboration) is an open community dedicated to reducing barriers for lifecycle tool integration. The community authors specifications for exposing lifecycle artifacts through uniform (REST) interfaces and relying on Internet and Linked Data standards.

OSLC's scope started with Application Lifecycle Management (ALM) and is expanding to include integrations across Product Lifecycle Management (PLM) and IT Service Management (ISM/DevOps), Lyo is designed to be a companion to the continuing specification efforts of the OSLC community. Its main purpose is to expand adoption of OSLC specifications and to enable the Eclipse community to easily build OSLC compliant tools.

## Getting started

If you are interested in using the library, please use the [wiki](https://wiki.eclipse.org/Lyo) and the [javadocs](http://download.eclipse.org/lyo/docs/core/2.4.0/overview-summary.html)

## CONTRIBUTING

We adopt the Eclipse guidlines for [contributing via Git](https://wiki.eclipse.org/Development_Resources/Contributing_via_Git), to accept contributions in this project.

Please follow these guidelines to submit your contributions. **Before your contribution can be accepted to an Eclipse Foundation project, you need to electronically sign the [Eclipse Contributor Agreement (ECA)](https://eclipse.org/legal/ECA.php).**
The preferred approach is to contribute a patch via GitHub using the standard GitHub pull request (remember to sign off on each commit and configure Git to use the same email addressed used to sign an ECA). 
Alternatively, you can submit your contribution as a patch attachment on the corresponding Bugzilla or Github issue. 
(This project no longer support Gerrit.)

The Eclipse Lyo project page is located at https://projects.eclipse.org/projects/technology.lyo. It points to the information regarding source code management, builds, coding standards, and more.

### Useful links

* [lyo-dev mailing list](https://dev.eclipse.org/mailman/listinfo/lyo-dev)
* [open bugs](https://bugs.eclipse.org/bugs/buglist.cgi?product=Lyo)
* [report a new bug](https://bugs.eclipse.org/bugs/enter_bug.cgi?product=Lyo)

**Be sure to search for existing bugs before you create another one.** Remember that contributions are always welcome!

### Building the project

This project uses Maven as the build system for all Java projects except those which are Eclipse Plugin project. The latter projects contain all the Eclipse project files under Git for import and building using the *Import > Existing Projects into Workspace*. All other projects should be imported using the *Import > Existing Maven Projects* menu.

`core.query` project uses ANTLR for generating parser code. In order to configure Eclipse to use it, the `pom.xml` file contains some m2e-specific configuration. **After importing the projects, make sure to run *Maven > Update Project*.** If that does not work, you must add the directory `target/generated-sources/antlr3` under the *Java Project Build > Source*.
