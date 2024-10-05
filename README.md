# Eclipse Lyo

![GitHub Actions Status (master)](https://img.shields.io/github/actions/workflow/status/eclipse/lyo/maven.yml?branch=master&label=GH%20Actions)
[![](https://img.shields.io/jenkins/build?jobUrl=https%3A%2F%2Fci.eclipse.org%2Flyo%2Fjob%2FLyo%2520multibranch%2Fjob%2Fmaster%2F&label=Eclipse%20Jenkins)](https://ci.eclipse.org/lyo/job/Lyo%20multibranch/job/master/)
[![](https://img.shields.io/badge/javadoc-6.0.0.Final-blue.svg)](https://download.eclipse.org/lyo/docs/all/6.0.0.Final/apidocs/)
[![](https://img.shields.io/badge/javadoc-latest-blue.svg)](https://download.eclipse.org/lyo/docs/all/latest/apidocs/)
[![Discourse users](https://img.shields.io/discourse/users?color=28bd84&server=https%3A%2F%2Fforum.open-services.net%2F)](https://forum.open-services.net/)

## Introduction

The [Eclipse Lyo](https://projects.eclipse.org/projects/technology.lyo) project is focused on providing an SDK to enable adoption of [OSLC specifications](https://open-services.net/). OSLC (Open Services for Lifecycle Collaboration) is an open community dedicated to reducing barriers for lifecycle tool integration. The community authors specifications for exposing lifecycle artifacts through uniform (REST) interfaces and relying on Internet and Linked Data standards.

OSLC's scope started with Application Lifecycle Management (ALM) and is expanding to include integrations across Product Lifecycle Management (PLM) and IT Service Management (ISM/DevOps), Lyo is designed to be a companion to the continuing specification efforts of the OSLC community. Its main purpose is to expand adoption of OSLC specifications and to enable the Eclipse community to easily build OSLC compliant tools.

## Getting started

You can find more resources for developing OSLC applications with Lyo, under the [OSLC Developer Guide](https://oslc.github.io/developing-oslc-applications/eclipse_lyo/eclipse-lyo.html).You are welcome to post questions on the [OSLC forum](https://forum.open-services.net/c/sdks/lyo/9).


The [OLSLC OP Reference implementation](https://github.com/OSLC/refimpl) repository contains sample code that demonstrates how to build OSLC servers for various domains.


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

| Repo                                                    | Status                                                                                                                                       | PRs                                                                                              | Milestone                                                                                     | Bugs                                                                                                                  |
| ------------------------------------------------------- | -------------------------------------------------------------------------------------------------------------------------------------------- | ------------------------------------------------------------------------------------------------ | --------------------------------------------------------------------------------------------- | --------------------------------------------------------------------------------------------------------------------- |
| [lyo.designer](https://github.com/eclipse/lyo.designer) | [![CI](https://github.com/eclipse/lyo.designer/workflows/CI/badge.svg)](https://github.com/eclipse/lyo.designer/actions?query=workflow%3ACI) | ![GitHub pull requests](https://img.shields.io/github/issues-pr/eclipse/lyo.designer?color=blue) | ![GitHub milestone](https://img.shields.io/github/milestones/progress/eclipse/lyo.designer/2) | ![GitHub issues by-label](https://img.shields.io/github/issues/eclipse/lyo.designer/Type:%20Bug?label=bugs) |
| [lyo.oslc-ui](https://github.com/eclipse/lyo.oslc-ui)   | N/A                                                                                                                                          | ![GitHub pull requests](https://img.shields.io/github/issues-pr/eclipse/lyo.oslc-ui?color=blue)  | ![GitHub milestone](https://img.shields.io/github/milestones/progress/eclipse/lyo.oslc-ui/1)  | ![GitHub issues by-label](https://img.shields.io/github/issues/eclipse/lyo.oslc-ui/Type:%20Bug?label=bugs)  |

### Test and sample repositories

<!-- BEGIN YAML TABLE -->
| Repo | Version | Status | PRs | Activity |
| ---- | ------- | ------ | --- | -------- |
| [oslc-op/refimpl](https://github.com/oslc-op/refimpl) | ![](https://img.shields.io/badge/Lyo-6.0.0-brightgreen) | ![](https://github.com/oslc-op/refimpl/actions/workflows/maven.yml/badge.svg) | [![](https://img.shields.io/github/issues-pr/oslc-op/refimpl?label=PR)](https://github.com/oslc-op/refimpl/pulls?q=is%3Apr+is%3Aopen+sort%3Aupdated-desc) | ![](https://img.shields.io/github/last-commit/oslc-op/refimpl) |
| [lyo-samples/lyo-client-samples](https://github.com/OSLC/lyo-samples) | ![](https://img.shields.io/badge/Lyo-6.0.0-brightgreen) | ![](https://github.com/OSLC/lyo-samples/actions/workflows/maven.yml/badge.svg) | [![](https://img.shields.io/github/issues-pr/OSLC/lyo-samples?label=PR)](https://github.com/OSLC/lyo-samples/pulls?q=is%3Apr+is%3Aopen+sort%3Aupdated-desc) | ![](https://img.shields.io/github/last-commit/OSLC/lyo-samples) |
| [danlz/lyo-in-spring-boot](https://github.com/danlz/lyo-in-spring-boot) | ![](https://img.shields.io/badge/Lyo-6.0.0--S-E77728) | N/A | [![](https://img.shields.io/github/issues-pr/danlz/lyo-in-spring-boot?label=PR)](https://github.com/danlz/lyo-in-spring-boot/pulls?q=is%3Apr+is%3Aopen+sort%3Aupdated-desc) | ![](https://img.shields.io/github/last-commit/danlz/lyo-in-spring-boot) |
| [oslc-op/sysml-oslc-server](https://github.com/oslc-op/sysml-oslc-server) | ![](https://img.shields.io/badge/Lyo-5.1.1-17C3B2) | ![](https://github.com/oslc-op/sysml-oslc-server/actions/workflows/maven.yml/badge.svg) | [![](https://img.shields.io/github/issues-pr/oslc-op/sysml-oslc-server?label=PR)](https://github.com/oslc-op/sysml-oslc-server/pulls?q=is%3Apr+is%3Aopen+sort%3Aupdated-desc) | ![](https://img.shields.io/github/last-commit/oslc-op/sysml-oslc-server) |
| [OSLC/lyo-adaptor-sample-modelling](https://github.com/OSLC/lyo-adaptor-sample-modelling) | ![](https://img.shields.io/badge/Lyo-5.0.1.CR-7B1E7A) | ![](https://github.com/OSLC/lyo-adaptor-sample-modelling/actions/workflows/maven.yml/badge.svg?branch=main-5.x) | [![](https://img.shields.io/github/issues-pr/OSLC/lyo-adaptor-sample-modelling?label=PR)](https://github.com/OSLC/lyo-adaptor-sample-modelling/pulls?q=is%3Apr+is%3Aopen+sort%3Aupdated-desc) | ![](https://img.shields.io/github/last-commit/OSLC/lyo-adaptor-sample-modelling) |
| [lyo-samples/client-oauth-discovery-dui](https://github.com/OSLC/lyo-samples) | ![](https://img.shields.io/badge/Lyo-4.0.0-f42020) | ![](https://github.com/OSLC/lyo-samples/actions/workflows/maven.yml/badge.svg) | [![](https://img.shields.io/github/issues-pr/OSLC/lyo-samples?label=PR)](https://github.com/OSLC/lyo-samples/pulls?q=is%3Apr+is%3Aopen+sort%3Aupdated-desc) | ![](https://img.shields.io/github/last-commit/OSLC/lyo-samples) |
| [OSLC/lyo-adaptor-bugzilla](https://github.com/OSLC/lyo-adaptor-bugzilla) | ![](https://img.shields.io/badge/Lyo-2.4.0-f42020) | ![](https://github.com/OSLC/lyo-adaptor-bugzilla/actions/workflows/maven.yml/badge.svg) | [![](https://img.shields.io/github/issues-pr/OSLC/lyo-adaptor-bugzilla?label=PR)](https://github.com/OSLC/lyo-adaptor-bugzilla/pulls?q=is%3Apr+is%3Aopen+sort%3Aupdated-desc) | ![](https://img.shields.io/github/last-commit/OSLC/lyo-adaptor-bugzilla) |
| [OSLC/iotp-adaptor](https://github.com/OSLC/iotp-adaptor) | ![](https://img.shields.io/badge/Lyo-2.4.0-f42020) | ![](https://github.com/OSLC/iotp-adaptor/actions/workflows/maven.yml/badge.svg) | [![](https://img.shields.io/github/issues-pr/OSLC/iotp-adaptor?label=PR)](https://github.com/OSLC/iotp-adaptor/pulls?q=is%3Apr+is%3Aopen+sort%3Aupdated-desc) | ![](https://img.shields.io/github/last-commit/OSLC/iotp-adaptor) |
| [OSLC/oslc-adapter-jama](https://github.com/OSLC/oslc-adapter-jama) | ![](https://img.shields.io/badge/Lyo-2.4.0-f42020) | ![](https://github.com/OSLC/oslc-adapter-jama/actions/workflows/maven.yml/badge.svg) | [![](https://img.shields.io/github/issues-pr/OSLC/oslc-adapter-jama?label=PR)](https://github.com/OSLC/oslc-adapter-jama/pulls?q=is%3Apr+is%3Aopen+sort%3Aupdated-desc) | ![](https://img.shields.io/github/last-commit/OSLC/oslc-adapter-jama) |
| [eclipse/lyo.testsuite](https://github.com/eclipse/lyo.testsuite) | ![](https://img.shields.io/badge/Lyo-2.4.0-f42020) | ![](https://github.com/eclipse/lyo.testsuite/actions/workflows/maven.yml/badge.svg) | [![](https://img.shields.io/github/issues-pr/eclipse/lyo.testsuite?label=PR)](https://github.com/eclipse/lyo.testsuite/pulls?q=is%3Apr+is%3Aopen+sort%3Aupdated-desc) | ![](https://img.shields.io/github/last-commit/eclipse/lyo.testsuite) |
| [ld4mbse/oslc4tdb](https://github.com/ld4mbse/oslc4tdb) | ![](https://img.shields.io/badge/Lyo-2.3.0-f42020) | N/A | [![](https://img.shields.io/github/issues-pr/ld4mbse/oslc4tdb?label=PR)](https://github.com/ld4mbse/oslc4tdb/pulls?q=is%3Apr+is%3Aopen+sort%3Aupdated-desc) | ![](https://img.shields.io/github/last-commit/ld4mbse/oslc4tdb) |
| [kth-mda/adaptor-bugzilla-trs](https://gitlab.com/kth-mda/lyo/adaptor-bugzilla-trs) | ![](https://img.shields.io/badge/Lyo-2.2.0-f42020) | ![](https://gitlab.com/kth-mda/lyo/adaptor-bugzilla-trs/badges/master/pipeline.svg?ignore_skipped=true) | [![](https://img.shields.io/gitlab/merge-requests/open/kth-mda%2Flyo%2Fadaptor-bugzilla-trs?label=MR)](https://gitlab.com/kth-mda/lyo/adaptor-bugzilla-trs/-/merge_requests) | ![](https://img.shields.io/gitlab/last-commit/kth-mda%2Flyo%2Fadaptor-bugzilla-trs) |
| [berezovskyi/oslc-adapter-subversion](https://github.com/berezovskyi/oslc-adapter-subversion) | ![](https://img.shields.io/badge/Lyo-2.1.2-f42020) | ![](https://github.com/berezovskyi/oslc-adapter-subversion/actions/workflows/maven.yml/badge.svg) | [![](https://img.shields.io/github/issues-pr/berezovskyi/oslc-adapter-subversion?label=PR)](https://github.com/berezovskyi/oslc-adapter-subversion/pulls?q=is%3Apr+is%3Aopen+sort%3Aupdated-desc) | ![](https://img.shields.io/github/last-commit/berezovskyi/oslc-adapter-subversion) |
| [lyo-samples-attic/oslc-java-samples](https://github.com/OSLC/lyo-samples-attic/tree/main/lyo-4.1/oslc-java-samples) | ![](https://img.shields.io/badge/Lyo-4.1.0-f42020) | N/A | N/A | N/A |
| [lyo-samples-attic/oslc4j-stockquote](https://github.com/OSLC/lyo-samples-attic/tree/main/lyo-2.4/oslc4j-stockquote) | ![](https://img.shields.io/badge/Lyo-2.3.0-f42020) | N/A | N/A | N/A |
| [lyo-samples-attic/oslc4j-test](https://github.com/OSLC/lyo-samples-attic/tree/main/lyo-2.3/oslc4j-test) | ![](https://img.shields.io/badge/Lyo-2.3.0-f42020) | N/A | N/A | N/A |
| [lyo-samples-attic/oslc4j-bugzilla-sample](https://github.com/OSLC/lyo-samples-attic/tree/main/lyo-2.2/oslc4j-bugzilla-sample) | ![](https://img.shields.io/badge/Lyo-2.2.0-f42020) | N/A | N/A | N/A |
| [lyo-samples-attic/trs4j-bugzilla-sample](https://github.com/OSLC/lyo-samples-attic/tree/main/lyo-2.1/trs4j-bugzilla-sample) | ![](https://img.shields.io/badge/Lyo-2.1.2-f42020) | N/A | N/A | N/A |
| [ld4mbse/oslc-adapter-simulink](https://github.com/ld4mbse/oslc-adapter-simulink) | ![](https://img.shields.io/badge/Lyo-3.0.0--S-f42020) | N/A | [![](https://img.shields.io/github/issues-pr/ld4mbse/oslc-adapter-simulink?label=PR)](https://github.com/ld4mbse/oslc-adapter-simulink/pulls?q=is%3Apr+is%3Aopen+sort%3Aupdated-desc) | ![](https://img.shields.io/github/last-commit/ld4mbse/oslc-adapter-simulink) |
| [ld4mbse/oslc-adapter-magicdraw-sysml](https://github.com/ld4mbse/oslc-adapter-magicdraw-sysml) | ![](https://img.shields.io/badge/Lyo-3.0.0--S-f42020) | N/A | [![](https://img.shields.io/github/issues-pr/ld4mbse/oslc-adapter-magicdraw-sysml?label=PR)](https://github.com/ld4mbse/oslc-adapter-magicdraw-sysml/pulls?q=is%3Apr+is%3Aopen+sort%3Aupdated-desc) | ![](https://img.shields.io/github/last-commit/ld4mbse/oslc-adapter-magicdraw-sysml) |
| [ld4mbse/oslc-adapter-integrity](https://github.com/ld4mbse/oslc-adapter-integrity) | ![](https://img.shields.io/badge/Lyo-3.0.0--S-f42020) | N/A | [![](https://img.shields.io/github/issues-pr/ld4mbse/oslc-adapter-integrity?label=PR)](https://github.com/ld4mbse/oslc-adapter-integrity/pulls?q=is%3Apr+is%3Aopen+sort%3Aupdated-desc) | ![](https://img.shields.io/github/last-commit/ld4mbse/oslc-adapter-integrity) |
| [ld4mbse/oslc-adapter-fmi](https://github.com/ld4mbse/oslc-adapter-fmi) | ![](https://img.shields.io/badge/Lyo-3.0.0--S-f42020) | N/A | [![](https://img.shields.io/github/issues-pr/ld4mbse/oslc-adapter-fmi?label=PR)](https://github.com/ld4mbse/oslc-adapter-fmi/pulls?q=is%3Apr+is%3Aopen+sort%3Aupdated-desc) | ![](https://img.shields.io/github/last-commit/ld4mbse/oslc-adapter-fmi) |
| [ld4mbse/oslc-modeltransformation-simulink-magicdraw](https://github.com/ld4mbse/oslc-modeltransformation-simulink-magicdraw) | ![](https://img.shields.io/badge/Lyo-3.0.0--S-f42020) | N/A | [![](https://img.shields.io/github/issues-pr/ld4mbse/oslc-modeltransformation-simulink-magicdraw?label=PR)](https://github.com/ld4mbse/oslc-modeltransformation-simulink-magicdraw/pulls?q=is%3Apr+is%3Aopen+sort%3Aupdated-desc) | ![](https://img.shields.io/github/last-commit/ld4mbse/oslc-modeltransformation-simulink-magicdraw) |
| [ld4mbse/oslc-adapter-jena-tdb](https://github.com/ld4mbse/oslc-adapter-jena-tdb) | ![](https://img.shields.io/badge/Lyo-2.0.0-f42020) | N/A | [![](https://img.shields.io/github/issues-pr/ld4mbse/oslc-adapter-jena-tdb?label=PR)](https://github.com/ld4mbse/oslc-adapter-jena-tdb/pulls?q=is%3Apr+is%3Aopen+sort%3Aupdated-desc) | ![](https://img.shields.io/github/last-commit/ld4mbse/oslc-adapter-jena-tdb) |
| [ld4mbse/oslc-adapter-amesim](https://github.com/ld4mbse/oslc-adapter-amesim) | ![](https://img.shields.io/badge/Lyo-2.0.0-f42020) | N/A | [![](https://img.shields.io/github/issues-pr/ld4mbse/oslc-adapter-amesim?label=PR)](https://github.com/ld4mbse/oslc-adapter-amesim/pulls?q=is%3Apr+is%3Aopen+sort%3Aupdated-desc) | ![](https://img.shields.io/github/last-commit/ld4mbse/oslc-adapter-amesim) |
| [kth-mda/se.kth.md.cpse.adaptor.bugzilla](https://gitlab.com/kth-mda/lyo/se.kth.md.cpse.adaptor.bugzilla) | ![](https://img.shields.io/badge/Lyo-2.0.0-f42020) | ![](https://gitlab.com/kth-mda/lyo/se.kth.md.cpse.adaptor.bugzilla/badges/master/pipeline.svg?ignore_skipped=true) | [![](https://img.shields.io/gitlab/merge-requests/open/kth-mda%2Flyo%2Fse.kth.md.cpse.adaptor.bugzilla?label=MR)](https://gitlab.com/kth-mda/lyo/se.kth.md.cpse.adaptor.bugzilla/-/merge_requests) | ![](https://img.shields.io/gitlab/last-commit/kth-mda%2Flyo%2Fse.kth.md.cpse.adaptor.bugzilla) |
| [kth-mda/adaptor-bugzilla-store](https://gitlab.com/kth-mda/lyo/adaptor-store-support) | ![](https://img.shields.io/badge/Lyo-2.1.0-f42020) | N/A | [![](https://img.shields.io/gitlab/merge-requests/open/kth-mda%2Flyo%2Fadaptor-store-support?label=MR)](https://gitlab.com/kth-mda/lyo/adaptor-store-support/-/merge_requests) | ![](https://img.shields.io/gitlab/last-commit/kth-mda%2Flyo%2Fadaptor-store-support) |
<!-- END YAML TABLE -->

> [!CAUTION]
> 
> Lyo versions 4.1.0 and below are potentially vulnerable to [CVE-2021-41042](https://nvd.nist.gov/vuln/detail/CVE-2021-41042) when processing untrusted RDF/XML inputs.


### Retired repositories

| Repo                                                        | Status                                                                                                                                           | Lyo version                                                                                        | PRs                                                                                             | Bugs                                                                                                                    |
| ----------------------------------------------------------- | ------------------------------------------------------------------------------------------------------------------------------------------------ | -------------------------------------------------------------------------------------------------- | ----------------------------------------------------------------------------------------------- | ----------------------------------------------------------------------------------------------------------------------- |
| [lyo.core](https://github.com/eclipse/lyo.core)             | N/A             | ![GitHub pull requests](https://img.shields.io/github/issues-pr/eclipse/lyo.core?color=blue)       | ![GitHub milestone](https://img.shields.io/github/milestones/progress/eclipse/lyo.core/2)       | ![GitHub issues by-label](https://img.shields.io/github/issues/eclipse/lyo.core/Type:%20Bug?label=bugs)       |
| [lyo.client](https://github.com/eclipse/lyo.client)         | N/A         | ![GitHub pull requests](https://img.shields.io/github/issues-pr/eclipse/lyo.client?color=blue)     | ![GitHub milestone](https://img.shields.io/github/milestones/progress/eclipse/lyo.client/2)     | ![GitHub issues by-label](https://img.shields.io/github/issues/eclipse/lyo.client/Type:%20Bug?label=bugs)     |
| [lyo.server](https://github.com/eclipse/lyo.server)         | N/A       | ![GitHub pull requests](https://img.shields.io/github/issues-pr/eclipse/lyo.server?color=blue)     | ![GitHub milestone](https://img.shields.io/github/milestones/progress/eclipse/lyo.server/2)     | ![GitHub issues by-label](https://img.shields.io/github/issues/eclipse/lyo.server/Type:%20Bug?label=bugs)     |
| [lyo.domains](https://github.com/eclipse/lyo.domains)       | N/A      | ![GitHub pull requests](https://img.shields.io/github/issues-pr/eclipse/lyo.domains?color=blue)    | ![GitHub milestone](https://img.shields.io/github/milestones/progress/eclipse/lyo.domains/2)    | ![GitHub issues by-label](https://img.shields.io/github/issues/eclipse/lyo.domains/Type:%20Bug?label=bugs)    |
| [lyo.store](https://github.com/eclipse/lyo.store)           | N/A           | ![GitHub pull requests](https://img.shields.io/github/issues-pr/eclipse/lyo.store?color=blue)      | ![GitHub milestone](https://img.shields.io/github/milestones/progress/eclipse/lyo.store/4)      | ![GitHub issues by-label](https://img.shields.io/github/issues/eclipse/lyo.store/Type:%20Bug?label=bugs)      |
| [lyo.trs-server](https://github.com/eclipse/lyo.trs-server) | N/A | ![GitHub pull requests](https://img.shields.io/github/issues-pr/eclipse/lyo.trs-server?color=blue) | ![GitHub milestone](https://img.shields.io/github/milestones/progress/eclipse/lyo.trs-server/1) | ![GitHub issues by-label](https://img.shields.io/github/issues/eclipse/lyo.trs-server/Type:%20Bug?label=bugs) |
| [lyo.trs-client](https://github.com/eclipse/lyo.trs-client) | N/A | ![GitHub pull requests](https://img.shields.io/github/issues-pr/eclipse/lyo.trs-client?color=blue) | ![GitHub milestone](https://img.shields.io/github/milestones/progress/eclipse/lyo.trs-client/2) | ![GitHub issues by-label](https://img.shields.io/github/issues/eclipse/lyo.trs-client/Type:%20Bug?label=bugs) |
| [lyo.validation](https://github.com/eclipse/lyo.validation) | N/A | ![GitHub pull requests](https://img.shields.io/github/issues-pr/eclipse/lyo.validation?color=blue) | ![GitHub milestone](https://img.shields.io/github/milestones/progress/eclipse/lyo.validation/2) | ![GitHub issues by-label](https://img.shields.io/github/issues/eclipse/lyo.validation/Type:%20Bug?label=bugs) |
| [lyo.ldp](https://github.com/eclipse/lyo.ldp)           | N/A           | ![GitHub pull requests](https://img.shields.io/github/issues-pr/eclipse/lyo.ldp?color=blue)      | ![GitHub milestone](https://img.shields.io/github/milestones/progress/eclipse/lyo.ldp/1)      | ![GitHub issues by-label](https://img.shields.io/github/issues/eclipse/lyo.ldp/Type:%20Bug?label=bugs)      |
| [lyo.rio](https://github.com/eclipse/lyo.rio)                                                                                 | [![CI](https://github.com/eclipse/lyo.rio/workflows/CI/badge.svg)](https://github.com/eclipse/lyo.rio/actions?query=workflow%3ACI)                     | ![](https://img.shields.io/badge/Lyo-3.0.0--S-f42020)      | ![GitHub pull requests](https://img.shields.io/github/issues-pr/eclipse/lyo.rio?color=blue)                      | ![GitHub issues by-label](https://img.shields.io/github/issues/eclipse/lyo.rio/Type:%20Bug?label=bugs)              |
| [lyo.docs ](https://github.com/eclipse/lyo.docs)            | N/A                                                                                                                                              | ![GitHub pull requests](https://img.shields.io/github/issues-pr/eclipse/lyo.docs?color=blue)       | N/A                                                                                             | ![GitHub issues by-label](https://img.shields.io/github/issues/eclipse/lyo.docs/Type:%20Bug?label=bugs)       |

---

Repositories that are maintained under OSLC organisation include:

-   [OSLC4JS SDK](http://oslc.github.io/developing-oslc-applications/oslc-open-source-node-projects.html)
-   [OSLC4NET SDK](https://github.com/OSLC/oslc4net)
-   [Lyo sample projects](https://github.com/OSLC/lyo-samples)
-   [lyo-adaptor-sample-modelling](https://github.com/OSLC/lyo-adaptor-sample-modelling)
-   [lyo-adaptor-bugzilla](https://github.com/OSLC/lyo-adaptor-bugzilla)
-   [iotp-adaptor](https://github.com/OSLC/iotp-adaptor)

Other repositories that are no longer actively maintained but are kept to preserve the history:

-   [lyo.adapter-magicdraw](https://github.com/eclipse/lyo.adapter-magicdraw)
-   [lyo.adapter-simulink](https://github.com/eclipse/lyo.adapter-simulink)
-   [Tutorial: Integrating Products with OSLC](https://github.com/OSLC/integrating-products-with-oslc-tutorial)

## Contributing

See [CONTRIBUTING.MD](CONTRIBUTING.MD)

## Useful information for Lyo committers

-   [New Lyo wiki](https://github.com/eclipse/lyo/wiki)
-   [Release Engineering Handbook](https://docs.google.com/document/d/1lnTf2NX4_1lL6Uu-4VYNs8FNgHZ1Z64xF4hQZGCKymE/edit?usp=sharing)
-   [Old Lyo wiki](https://wiki.eclipse.org/Lyo)
-   [Official homepage](https://www.eclipse.org/lyo/)

### Acknowledgments

![](https://www.yourkit.com/images/yklogo.png)

Thanks to YourKit for providing us an open source license of YourKit Java Profiler!

YourKit supports open source projects with innovative and intelligent tools
for monitoring and profiling Java and .NET applications.
YourKit is the creator of [YourKit Java Profiler](https://www.yourkit.com/java/profiler/),
[YourKit .NET Profiler](https://www.yourkit.com/.net/profiler/),
and [YourKit YouMonitor](https://www.yourkit.com/youmonitor/).
