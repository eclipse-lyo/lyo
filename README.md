# Eclipse Lyo

![CI Parent](https://github.com/eclipse/lyo/workflows/CI%20Parent/badge.svg)
[![](https://img.shields.io/jenkins/s/https/ci.eclipse.org/lyo/job/lyo-monorepo.svg?label=Eclipse%20Jenkins)](https://ci.eclipse.org/lyo/job/lyo-monorepo/)
[![](https://img.shields.io/badge/javadoc-4.0.0-blue.svg)](https://download.eclipse.org/lyo/docs/all/4.0.0/apidocs/)
[![](https://img.shields.io/badge/javadoc-latest-blue.svg)](https://download.eclipse.org/lyo/docs/all/latest/apidocs/)
[![Discourse users](https://img.shields.io/discourse/users?color=28bd84&server=https%3A%2F%2Fforum.open-services.net%2F)](https://forum.open-services.net/)


## Introduction

The [Eclipse Lyo](https://projects.eclipse.org/projects/technology.lyo) project is focused on providing an SDK to enable adoption of [OSLC specifications](https://open-services.net/). OSLC (Open Services for Lifecycle Collaboration) is an open community dedicated to reducing barriers for lifecycle tool integration. The community authors specifications for exposing lifecycle artifacts through uniform (REST) interfaces and relying on Internet and Linked Data standards.

OSLC's scope started with Application Lifecycle Management (ALM) and is expanding to include integrations across Product Lifecycle Management (PLM) and IT Service Management (ISM/DevOps), Lyo is designed to be a companion to the continuing specification efforts of the OSLC community. Its main purpose is to expand adoption of OSLC specifications and to enable the Eclipse community to easily build OSLC compliant tools.

## Getting started

You can find more resources for developing OSLC applications with Lyo, under the [OSLC Developer Guide](https://oslc.github.io/developing-oslc-applications/eclipse_lyo/eclipse-lyo.html#oslc4j-sdk).

The [Lyo Samples](https://github.com/OSLC/lyo-samples) repository contains sample code that demonstrates how to use the Lyo client to interact with OSLC Service Providers in various ways.

You are welcome to post questions on the [OSLC forum](https://forum.open-services.net/c/sdks/lyo/9).

### Core

See [OSLC Developer Guide](https://oslc.github.io/developing-oslc-applications/eclipse_lyo/eclipse-lyo.html#oslc4j-sdk).

### Client

To use OSLC Client, follow the setup and development instructions under the [OSLC Developer Guide for Client setup](https://oslc.github.io/developing-oslc-applications/eclipse_lyo/setup-an-oslc-provider-consumer-application.html).

### Domains

See under [domains/README](domains/README.md).

### Store

See under [store/README](store/README.md).

### TRS Server

To use this library, follow the setup and development instructions under the [OSLC Developer Guide for TRS server setup](https://oslc.github.io/developing-oslc-applications/eclipse_lyo/setup-an-oslc-provider-consumer-application.html#provide-trs-support). The instructions assume you have followed the overall instructions to setup an OSLC4J server/client, as defined on that page. 

You can find more resources for developing OSLC applications with Lyo in general under the [OSLC Developer Guide](http://oslc.github.io/developing-oslc-applications/eclipse_lyo/eclipse-lyo.html), and in particular [for TRS development](https://oslc.github.io/developing-oslc-applications/eclipse_lyo/eclipse-lyo#trs-sdk).

See [trs/server/README](trs/server/README.md) for dev notes.

### TRS Client

See under [trs/client/README](trs/client/README.md).

### Validation

See under [validation/README](validation/README.md).


## Repositories

### Main project repositories

| Repo       | Status | PRs | Milestone | Bugs |
|------------|----|-----|-----------|------|
| [lyo.designer](https://github.com/eclipse/lyo.designer)   |  [![CI](https://github.com/eclipse/lyo.designer/workflows/CI/badge.svg)](https://github.com/eclipse/lyo.designer/actions?query=workflow%3ACI)  |  ![GitHub pull requests](https://img.shields.io/github/issues-pr/eclipse/lyo.designer?color=blue)   |   ![GitHub milestone](https://img.shields.io/github/milestones/progress/eclipse/lyo.designer/2)     | ![GitHub issues by-label](https://img.shields.io/github/issues/eclipse/lyo.designer/Type:%20Bug?color=red&label=bugs) |
| [lyo.oslc-ui](https://github.com/eclipse/lyo.oslc-ui)    |  N/A  |  ![GitHub pull requests](https://img.shields.io/github/issues-pr/eclipse/lyo.oslc-ui?color=blue)   |   ![GitHub milestone](https://img.shields.io/github/milestones/progress/eclipse/lyo.oslc-ui/1)     | ![GitHub issues by-label](https://img.shields.io/github/issues/eclipse/lyo.oslc-ui/Type:%20Bug?color=red&label=bugs) |
| [lyo.ldp](https://github.com/eclipse/lyo.ldp)     |  [![CI](https://github.com/eclipse/lyo.ldp/workflows/CI/badge.svg)](https://github.com/eclipse/lyo.ldp/actions?query=workflow%3ACI)  |  ![GitHub pull requests](https://img.shields.io/github/issues-pr/eclipse/lyo.ldp?color=blue)   |   ![GitHub milestone](https://img.shields.io/github/milestones/progress/eclipse/lyo.ldp/1)     | ![GitHub issues by-label](https://img.shields.io/github/issues/eclipse/lyo.ldp/Type:%20Bug?color=red&label=bugs) |


### Test and sample repositories

| Repo       | Status | Lyo version |  PRs | Bugs |
|------------|----|-----|-----------|------|
| [oslc-op/refimpl](https://github.com/oslc-op/refimpl)     |  [![CI](https://github.com/oslc-op/refimpl/workflows/CI/badge.svg)](https://github.com/oslc-op/refimpl/actions?query=workflow%3ACI) | ![](https://img.shields.io/badge/Lyo-4.0.0-brightgreen) |  ![GitHub pull requests](https://img.shields.io/github/issues-pr/oslc-op/refimpl?color=blue)   |  ![GitHub issues by-label](https://img.shields.io/github/issues/oslc-op/refimpl/Type:%20Bug?color=red&label=bugs) |
| [OSLC/lyo-samples/client-oauth-discovery-dui](https://github.com/OSLC/lyo-samples)     |  [![CI](https://github.com/OSLC/lyo-samples/workflows/CI/badge.svg)](https://github.com/OSLC/lyo-samples/actions?query=workflow%3ACI) | ![](https://img.shields.io/badge/Lyo-4.0.0-brightgreen) |  ![GitHub pull requests](https://img.shields.io/github/issues-pr/OSLC/lyo-samples?color=blue)   |  ![GitHub issues by-label](https://img.shields.io/github/issues/OSLC/lyo-samples/bug?color=red&label=bugs) |
| [OSLC/lyo-samples/oslc4j-client-samples](https://github.com/OSLC/lyo-samples)     |  [![CI](https://github.com/OSLC/lyo-samples/workflows/CI/badge.svg)](https://github.com/OSLC/lyo-samples/actions?query=workflow%3ACI) | ![](https://img.shields.io/badge/Lyo-4.0.0-brightgreen) |  ![GitHub pull requests](https://img.shields.io/github/issues-pr/OSLC/lyo-samples?color=blue)   |  ![GitHub issues by-label](https://img.shields.io/github/issues/OSLC/lyo-samples/bug?color=red&label=bugs) |
| [oslc-op/sysml-oslc-server](https://github.com/oslc-op/sysml-oslc-server)     |  [![CI](https://github.com/oslc-op/sysml-oslc-server/workflows/CI/badge.svg)](https://github.com/oslc-op/sysml-oslc-server/actions?query=workflow%3ACI) | ![](https://img.shields.io/badge/Lyo-4.0.0--SNAPSHOT-red) |  ![GitHub pull requests](https://img.shields.io/github/issues-pr/oslc-op/sysml-oslc-server?color=blue)   |  ![GitHub issues by-label](https://img.shields.io/github/issues/oslc-op/sysml-oslc-server/bug?color=red&label=bugs) |
| [OSLC/lyo-adaptor-sample-modelling](https://github.com/OSLC/lyo-adaptor-sample-modelling)     |  ![CI](https://circleci.com/gh/OSLC/lyo-adaptor-sample-modelling/tree/main-4.x.svg?style=svg) | ![](https://img.shields.io/badge/Lyo-4.0.0--SNAPSHOT-red) |  ![GitHub pull requests](https://img.shields.io/github/issues-pr/OSLC/lyo-adaptor-sample-modelling?color=blue)   |  ![GitHub issues by-label](https://img.shields.io/github/issues/OSLC/lyo-adaptor-sample-modelling/bug?color=red&label=bugs) |
| [OSLC/iotp-adaptor](https://github.com/OSLC/iotp-adaptor)     |  [![CI](https://github.com/OSLC/iotp-adaptor/workflows/CI/badge.svg)](https://github.com/OSLC/iotp-adaptor/actions?query=workflow%3ACI) | ![](https://img.shields.io/badge/Lyo-2.4.0-yellowgreen) |  ![GitHub pull requests](https://img.shields.io/github/issues-pr/OSLC/iotp-adaptor?color=blue)   |  ![GitHub issues by-label](https://img.shields.io/github/issues/OSLC/iotp-adaptor/bug?color=red&label=bugs) |
| [OSLC/oslc-adapter-jama](https://github.com/OSLC/oslc-adapter-jama)     |  [![CI](https://github.com/OSLC/oslc-adapter-jama/workflows/CI/badge.svg)](https://github.com/OSLC/oslc-adapter-jama/actions?query=workflow%3ACI) | ![](https://img.shields.io/badge/Lyo-2.4.0-yellowgreen) |  ![GitHub pull requests](https://img.shields.io/github/issues-pr/OSLC/oslc-adapter-jama?color=blue)   |  ![GitHub issues by-label](https://img.shields.io/github/issues/OSLC/oslc-adapter-jama/bug?color=red&label=bugs) |
| [OSLC/lyo-adaptor-bugzilla](https://github.com/OSLC/lyo-adaptor-bugzilla)     |  ![CI](https://semaphoreci.com/api/v1/berezovskyi/lyo-adaptor-bugzilla/branches/master/badge.svg) | ![](https://img.shields.io/badge/Lyo-2.3.0-red) |  ![GitHub pull requests](https://img.shields.io/github/issues-pr/OSLC/lyo-adaptor-bugzilla?color=blue)   |  ![GitHub issues by-label](https://img.shields.io/github/issues/OSLC/lyo-adaptor-bugzilla/Type:%20Bug?color=red&label=bugs) |
| [ld4mbse/oslc4tdb](https://github.com/ld4mbse/oslc4tdb)     |  N/A | ![](https://img.shields.io/badge/Lyo-2.3.0-red) | N/A |  N/A |
| [lyo.testsuite](https://github.com/eclipse/lyo.testsuite)     |  [![CI](https://github.com/eclipse/lyo.testsuite/workflows/CI/badge.svg)](https://github.com/eclipse/lyo.testsuite/actions?query=workflow%3ACI) | ![](https://img.shields.io/badge/Lyo-2.2.0-yellowgreen) |  ![GitHub pull requests](https://img.shields.io/github/issues-pr/eclipse/lyo.testsuite?color=blue)   |  ![GitHub issues by-label](https://img.shields.io/github/issues/eclipse/lyo.testsuite/Type:%20Bug?color=red&label=bugs) |
| [OSLC/lyo-samples](https://github.com/OSLC/lyo-samples)     |  [![CI](https://github.com/OSLC/lyo-samples/workflows/CI/badge.svg)](https://github.com/OSLC/lyo-samples/actions?query=workflow%3ACI) | ![](https://img.shields.io/badge/Lyo-2.2.0-yellowgreen) |  ![GitHub pull requests](https://img.shields.io/github/issues-pr/OSLC/lyo-samples?color=blue)   |  ![GitHub issues by-label](https://img.shields.io/github/issues/OSLC/lyo-samples/bug?color=red&label=bugs) |
| [lyo.rio](https://github.com/eclipse/lyo.rio)     |  [![CI](https://github.com/eclipse/lyo.rio/workflows/CI/badge.svg)](https://github.com/eclipse/lyo.rio/actions?query=workflow%3ACI) | ![](https://img.shields.io/badge/Lyo-3.0.0--SNAPSHOT-f42020) |  ![GitHub pull requests](https://img.shields.io/github/issues-pr/eclipse/lyo.rio?color=blue)   |    ![GitHub issues by-label](https://img.shields.io/github/issues/eclipse/lyo.rio/Type:%20Bug?color=red&label=bugs) |
| [ld4mbse/oslc-adapter-simulink](https://github.com/ld4mbse/oslc-adapter-simulink)     |  N/A | ![](https://img.shields.io/badge/Lyo-3.0.0--SNAPSHOT-f42020) | ![GitHub pull requests](https://img.shields.io/github/issues-pr/ld4mbse/oslc-adapter-simulink?color=blue)   |  ![GitHub issues by-label](https://img.shields.io/github/issues/ld4mbse/oslc-adapter-simulink/bug?color=red&label=bugs) |
| [ld4mbse/oslc-adapter-magicdraw-sysml](https://github.com/ld4mbse/oslc-adapter-magicdraw-sysml)     |  N/A | ![](https://img.shields.io/badge/Lyo-3.0.0--SNAPSHOT-f42020) | ![GitHub pull requests](https://img.shields.io/github/issues-pr/ld4mbse/oslc-adapter-magicdraw-sysml?color=blue)   |  ![GitHub issues by-label](https://img.shields.io/github/issues/ld4mbse/oslc-adapter-magicdraw-sysml/bug?color=red&label=bugs) |
| [ld4mbse/oslc-adapter-integrity](https://github.com/ld4mbse/oslc-adapter-integrity)     |  N/A | ![](https://img.shields.io/badge/Lyo-3.0.0--SNAPSHOT-f42020) | N/A |  N/A |
| [ld4mbse/oslc-adapter-fmi](https://github.com/ld4mbse/oslc-adapter-fmi)     |  N/A | ![](https://img.shields.io/badge/Lyo-3.0.0--SNAPSHOT-f42020) | N/A |  N/A |
| [ld4mbse/oslc-modeltransformation-simulink-magicdraw](https://github.com/ld4mbse/oslc-modeltransformation-simulink-magicdraw)     |  N/A | ![](https://img.shields.io/badge/Lyo-3.0.0--SNAPSHOT-f42020) | N/A |  N/A |
| [ld4mbse/oslc-adapter-subversion](https://github.com/ld4mbse/oslc-adapter-subversion)     |  N/A | ![](https://img.shields.io/badge/Lyo-3.0.0--SNAPSHOT-f42020) | N/A |  N/A |
| [ld4mbse/oslc-adapter-jena-tdb](https://github.com/ld4mbse/oslc-adapter-jena-tdb)     |  N/A | ![](https://img.shields.io/badge/Lyo-2.0.0-f42020) | N/A |  N/A |
| [ld4mbse/oslc-adapter-amesim](https://github.com/ld4mbse/oslc-adapter-amesim)     |  N/A | ![](https://img.shields.io/badge/Lyo-2.0.0-f42020) | N/A |  N/A |


<!-- Legend:
![](https://img.shields.io/badge/Lyo-4.1.0--SNAPSHOT-lightgreen)
![](https://img.shields.io/badge/Lyo-4.0.0--SNAPSHOT-red)
![](https://img.shields.io/badge/Lyo-4.0.0-brightgreen)
![](https://img.shields.io/badge/Lyo-4.0.0.RC-orange)
![](https://img.shields.io/badge/Lyo-4.0.0.M2-red)
![](https://img.shields.io/badge/Lyo-2.4.0-yellowgreen)
![](https://img.shields.io/badge/Lyo-2.3.0-red)
![](https://img.shields.io/badge/Lyo-2.2.0-yellowgreen)
![](https://img.shields.io/badge/Lyo-2.1.2-orange)
![](https://img.shields.io/badge/Lyo-2.1.1-red)
![](https://img.shields.io/badge/Lyo-2.1.0-red)
![](https://img.shields.io/badge/Lyo-3.0.0--SNAPSHOT-f42020)
![](https://img.shields.io/badge/Lyo-2.0.0-f42020)
![](https://img.shields.io/badge/Lyo-unknown-lightgrey) -->

### Retired repositories


| Repo       | Status | Lyo version |  PRs | Bugs |
|------------|----|-----|-----------|------|
| [lyo.core](https://github.com/eclipse/lyo.core)       |  [![CI](https://github.com/eclipse/lyo.core/workflows/CI/badge.svg)](https://github.com/eclipse/lyo.core/actions?query=workflow%3ACI)  |  ![GitHub pull requests](https://img.shields.io/github/issues-pr/eclipse/lyo.core?color=blue)   |   ![GitHub milestone](https://img.shields.io/github/milestones/progress/eclipse/lyo.core/2)     | ![GitHub issues by-label](https://img.shields.io/github/issues/eclipse/lyo.core/Type:%20Bug?color=red&label=bugs) |
| [lyo.client](https://github.com/eclipse/lyo.client)     |  [![CI](https://github.com/eclipse/lyo.client/workflows/CI/badge.svg)](https://github.com/eclipse/lyo.client/actions?query=workflow%3ACI)  |  ![GitHub pull requests](https://img.shields.io/github/issues-pr/eclipse/lyo.client?color=blue)   |   ![GitHub milestone](https://img.shields.io/github/milestones/progress/eclipse/lyo.client/2)     | ![GitHub issues by-label](https://img.shields.io/github/issues/eclipse/lyo.client/Type:%20Bug?color=red&label=bugs) |
| [lyo.server](https://github.com/eclipse/lyo.server)      |  [![CI](https://github.com/eclipse/lyo.server/workflows/CI/badge.svg)](https://github.com/eclipse/lyo.server/actions?query=workflow%3ACI)  |  ![GitHub pull requests](https://img.shields.io/github/issues-pr/eclipse/lyo.server?color=blue)   |   ![GitHub milestone](https://img.shields.io/github/milestones/progress/eclipse/lyo.server/2)     | ![GitHub issues by-label](https://img.shields.io/github/issues/eclipse/lyo.server/Type:%20Bug?color=red&label=bugs) |
| [lyo.domains](https://github.com/eclipse/lyo.domains)    |  [![CI](https://github.com/eclipse/lyo.domains/workflows/CI/badge.svg)](https://github.com/eclipse/lyo.domains/actions?query=workflow%3ACI)  |  ![GitHub pull requests](https://img.shields.io/github/issues-pr/eclipse/lyo.domains?color=blue)   |   ![GitHub milestone](https://img.shields.io/github/milestones/progress/eclipse/lyo.domains/2)     | ![GitHub issues by-label](https://img.shields.io/github/issues/eclipse/lyo.domains/Type:%20Bug?color=red&label=bugs) |
| [lyo.store](https://github.com/eclipse/lyo.store)      |  [![CI](https://github.com/eclipse/lyo.store/workflows/CI/badge.svg)](https://github.com/eclipse/lyo.store/actions?query=workflow%3ACI)  |  ![GitHub pull requests](https://img.shields.io/github/issues-pr/eclipse/lyo.store?color=blue)   |   ![GitHub milestone](https://img.shields.io/github/milestones/progress/eclipse/lyo.store/4)     | ![GitHub issues by-label](https://img.shields.io/github/issues/eclipse/lyo.store/Type:%20Bug?color=red&label=bugs) |
| [lyo.trs-server](https://github.com/eclipse/lyo.trs-server) |  [![CI](https://github.com/eclipse/lyo.trs-server/workflows/CI/badge.svg)](https://github.com/eclipse/lyo.trs-server/actions?query=workflow%3ACI)  |  ![GitHub pull requests](https://img.shields.io/github/issues-pr/eclipse/lyo.trs-server?color=blue)   |   ![GitHub milestone](https://img.shields.io/github/milestones/progress/eclipse/lyo.trs-server/1)     | ![GitHub issues by-label](https://img.shields.io/github/issues/eclipse/lyo.trs-server/Type:%20Bug?color=red&label=bugs) |
| [lyo.trs-client](https://github.com/eclipse/lyo.trs-client) |  [![CI](https://github.com/eclipse/lyo.trs-client/workflows/CI/badge.svg)](https://github.com/eclipse/lyo.trs-client/actions?query=workflow%3ACI)  |  ![GitHub pull requests](https://img.shields.io/github/issues-pr/eclipse/lyo.trs-client?color=blue)   |   ![GitHub milestone](https://img.shields.io/github/milestones/progress/eclipse/lyo.trs-client/2)     | ![GitHub issues by-label](https://img.shields.io/github/issues/eclipse/lyo.trs-client/Type:%20Bug?color=red&label=bugs) |
| [lyo.validation](https://github.com/eclipse/lyo.validation) |  [![CI](https://github.com/eclipse/lyo.validation/workflows/CI/badge.svg)](https://github.com/eclipse/lyo.validation/actions?query=workflow%3ACI)  |  ![GitHub pull requests](https://img.shields.io/github/issues-pr/eclipse/lyo.validation?color=blue)   |   ![GitHub milestone](https://img.shields.io/github/milestones/progress/eclipse/lyo.validation/2)     | ![GitHub issues by-label](https://img.shields.io/github/issues/eclipse/lyo.validation/Type:%20Bug?color=red&label=bugs) |
| [lyo.docs ](https://github.com/eclipse/lyo.docs)| N/A | ![GitHub pull requests](https://img.shields.io/github/issues-pr/eclipse/lyo.docs?color=blue)   |   N/A   | ![GitHub issues by-label](https://img.shields.io/github/issues/eclipse/lyo.docs/Type:%20Bug?color=red&label=bugs) |

***

Repositories that are maintained under OSLC organisation include:

* [OSLC4JS SDK](http://oslc.github.io/developing-oslc-applications/oslc-open-source-node-projects.html)
* [OSLC4NET SDK](https://github.com/OSLC/oslc4net)
* [Lyo sample projects](https://github.com/OSLC/lyo-samples)
* [lyo-adaptor-sample-modelling](https://github.com/OSLC/lyo-adaptor-sample-modelling)
* [lyo-adaptor-bugzilla](https://github.com/OSLC/lyo-adaptor-bugzilla)
* [iotp-adaptor](https://github.com/OSLC/iotp-adaptor)

Other repositories that are no longer actively maintained but are kept to preserve the history:

* [lyo.adapter-magicdraw](https://github.com/eclipse/lyo.adapter-magicdraw)
* [lyo.adapter-simulink](https://github.com/eclipse/lyo.adapter-simulink)
* [Tutorial: Integrating Products with OSLC](https://github.com/OSLC/integrating-products-with-oslc-tutorial)


## Contributing

We adopt the Eclipse guidlines for [contributing via Git](https://wiki.eclipse.org/Development_Resources/Contributing_via_Git), to accept contributions in this project. **Make sure you have installed https://marketplace.eclipse.org/content/editorconfig-eclipse plugin for Eclipse to make formatting minimally consistent.**

Please follow these guidelines to submit your contributions. **Before your contribution can be accepted to an Eclipse Foundation project, you need to electronically sign the [Eclipse Contributor Agreement (ECA)](https://eclipse.org/legal/ECA.php).**
The preferred approach is to contribute a patch via GitHub using the standard GitHub pull request (remember to sign off on each commit and configure Git to use the same email addressed used to sign an ECA).
Alternatively, you can submit your contribution as a patch attachment on the corresponding Bugzilla or Github issue.
(This project no longer supports Gerrit.)


This project uses Maven as the build system for all Java projects except those which are Eclipse Plugin project. The latter projects contain all the Eclipse project files under Git for import and building using the *Import > Existing Projects into Workspace*. All other projects should be imported using the *Import > Existing Maven Projects* menu.

`core.query` project uses ANTLR for generating parser code. In order to configure Eclipse to use it, the `pom.xml` file contains some m2e-specific configuration. **After importing the projects, make sure to run *Maven > Update Project*.** If that does not work, you must add the directory `target/generated-sources/antlr3` under the *Java Project Build > Source*.


The Eclipse Lyo project page is located at https://projects.eclipse.org/projects/technology.lyo. It points to the information regarding source code management, builds, coding standards, and more.

You are also welcome to contact the development team via [lyo-dev mailing list](https://dev.eclipse.org/mailman/listinfo/lyo-dev) or on Slack (ask for an invite on the mailing list or by a PM to [@andrew](https://forum.open-services.net/u/andrew)).


## Useful information for Lyo committers  

- [New Lyo wiki](https://github.com/eclipse/lyo/wiki)
- [Release Engineering Handbook](https://docs.google.com/document/d/1lnTf2NX4_1lL6Uu-4VYNs8FNgHZ1Z64xF4hQZGCKymE/edit?usp=sharing)
- [Old Lyo wiki](https://wiki.eclipse.org/Lyo)
- [Official homepage](https://www.eclipse.org/lyo/)
