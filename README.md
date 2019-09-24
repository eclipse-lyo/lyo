# Eclipse Lyo Core

[![](https://img.shields.io/jenkins/s/https/ci.eclipse.org/lyo/job/lyo-core-master.svg)](https://ci.eclipse.org/lyo/job/lyo-core-master/)
[![Build Status](https://travis-ci.org/eclipse/lyo.core.svg?branch=master)](https://travis-ci.org/eclipse/lyo.core)
[![](https://img.shields.io/badge/javadoc-2.4.0-blue.svg)](https://download.eclipse.org/lyo/docs/core/2.4.0/)
[![](https://img.shields.io/badge/javadoc-latest-blue.svg)](https://download.eclipse.org/lyo/docs/core/latest/)
[![](https://img.shields.io/badge/misc-discourse-lightgrey.svg)](https://forum.open-services.net/)
[![](https://img.shields.io/badge/misc-gitter-lightgrey.svg)](https://gitter.im/eclipse/lyo)

This repository contains the [Eclipse Lyo](https://projects.eclipse.org/projects/technology.lyo) Core library.

## Introduction

The [Eclipse Lyo](https://projects.eclipse.org/projects/technology.lyo) project is focused on providing an SDK to enable adoption of [OSLC specifications](https://open-services.net/). OSLC (Open Services for Lifecycle Collaboration) is an open community dedicated to reducing barriers for lifecycle tool integration. The community authors specifications for exposing lifecycle artifacts through uniform (REST) interfaces and relying on Internet and Linked Data standards.

OSLC's scope started with Application Lifecycle Management (ALM) and is expanding to include integrations across Product Lifecycle Management (PLM) and IT Service Management (ISM/DevOps), Lyo is designed to be a companion to the continuing specification efforts of the OSLC community. Its main purpose is to expand adoption of OSLC specifications and to enable the Eclipse community to easily build OSLC compliant tools.

## Getting started

You can find more resources for developing OSLC applications with Lyo, under the [OSLC Developer Guide](http://oslc.github.io/developing-oslc-applications/eclipse_lyo/eclipse-lyo.html).

You are also welcome to contact the development team via [lyo-dev mailing list](https://dev.eclipse.org/mailman/listinfo/lyo-dev)

## Contributing

See [contributing](https://github.com/eclipse/lyo#contributing) under the main [Eclipse Lyo](https://github.com/eclipse/lyo) repository.

## Building the project
(Unless you need to work from source code, you need not build this project. You are instead adviced to add the necessary Lyo dependecies as adviced under the [OSLC Developer Guide](https://oslc.github.io/developing-oslc-applications/eclipse_lyo/setup-an-oslc-provider-consumer-application.html).)

This project uses Maven as the build system for all Java projects except those which are Eclipse Plugin project. The latter projects contain all the Eclipse project files under Git for import and building using the *Import > Existing Projects into Workspace*. All other projects should be imported using the *Import > Existing Maven Projects* menu.

`core.query` project uses ANTLR for generating parser code. In order to configure Eclipse to use it, the `pom.xml` file contains some m2e-specific configuration. **After importing the projects, make sure to run *Maven > Update Project*.** If that does not work, you must add the directory `target/generated-sources/antlr3` under the *Java Project Build > Source*.
