# TRS Server library

![CI](https://github.com/eclipse/lyo.trs-server/workflows/CI/badge.svg)
[![](https://img.shields.io/badge/javadoc-2.4.0-blue.svg)](https://download.eclipse.org/lyo/docs/trs-server/2.4.0/overview-summary.html)
[![](https://img.shields.io/badge/javadoc-SNAPSHOT-blue.svg)](https://download.eclipse.org/lyo/docs/trs-server/latest/overview-summary.html)
[![](https://img.shields.io/badge/misc-discourse-lightgrey.svg)](https://forum.open-services.net/)
[![](https://img.shields.io/badge/misc-gitter-lightgrey.svg)](https://gitter.im/eclipse/lyo)

This repository contains the [Eclipse Lyo](https://projects.eclipse.org/projects/technology.lyo) *TRS Server* library.

The *TRS Server* library is a set of ready-to-use classes that provide the required REST services for TRS, with minimal effort. 

**Note:** The current implementation supports an In-memory TRS Server that does not persist its TRS resources.
These classes are however designed to be extended for a persistent solution. 

For a thourough walkthough of TRS solutions, which among other things ensures persisting the TRS Logs, visit the [additional information on TRS](https://oslc.github.io/developing-oslc-applications/eclipse_lyo/eclipse-lyo#trs-sdk).

## Introduction

The [Eclipse Lyo](https://projects.eclipse.org/projects/technology.lyo) project is focused on providing an SDK to enable adoption of [OSLC specifications](https://open-services.net/). OSLC (Open Services for Lifecycle Collaboration) is an open community dedicated to reducing barriers for lifecycle tool integration. The community authors specifications for exposing lifecycle artifacts through uniform (REST) interfaces and relying on Internet and Linked Data standards.

OSLC's scope started with Application Lifecycle Management (ALM) and is expanding to include integrations across Product Lifecycle Management (PLM) and IT Service Management (ISM/DevOps), Lyo is designed to be a companion to the continuing specification efforts of the OSLC community. Its main purpose is to expand adoption of OSLC specifications and to enable the Eclipse community to easily build OSLC compliant tools.

## Getting started

To use this library, follow the setup and development instructions under the [OSLC Developer Guide for TRS server setup](https://oslc.github.io/developing-oslc-applications/eclipse_lyo/setup-an-oslc-provider-consumer-application.html#provide-trs-support). The instructions assume you have followed the overall instructions to setup an OSLC4J server/client, as defined on that page. 

You can find more resources for developing OSLC applications with Lyo in general under the [OSLC Developer Guide](http://oslc.github.io/developing-oslc-applications/eclipse_lyo/eclipse-lyo.html), and in particular [for TRS development](https://oslc.github.io/developing-oslc-applications/eclipse_lyo/eclipse-lyo#trs-sdk).

You are also welcome to contact the development team via [lyo-dev mailing list](https://dev.eclipse.org/mailman/listinfo/lyo-dev)

## Contributing

See [contributing](https://github.com/eclipse/lyo#contributing) under the main [Eclipse Lyo](https://github.com/eclipse/lyo) repository.

## Building the project
(Unless you need to work from source code, you need not build this project. You are instead adviced to add the necessary Lyo dependecies as described under the [OSLC Developer Guide](https://oslc.github.io/developing-oslc-applications/eclipse_lyo/setup-an-oslc-provider-consumer-application.html).)

This project uses Maven as the build system for all Java projects except those which are Eclipse Plugin project. The latter projects contain all the Eclipse project files under Git for import and building using the *Import > Existing Projects into Workspace*. All other projects should be imported using the *Import > Existing Maven Projects* menu.

## Internal implementation

The TRS interface consists mainly of two classes:

- `ChangeHistories`
- `TrackedResourceSetService`

`ChangeHistories` class represents the backbone of the TRS interface and
manages the TRS artifact and the objects representing the history information
served through TRS.

`TrackedResourceSetService` class manages the rest calls to the TRS service
e.g. a REST HTTP GET request for the TRS of the adapter, or an HTTP GET request
for some page of the change log or the base.

In order to implement a TRS interface two classes need to be implemented, each
one of these classes should respectively implement one of the classes above.

## Implementation note

Change events inside the Change Log can lose ordering in the response that is
served to the clients. This will not be fixed due to the following:

- there are no requirements on how the ordering of change events is reflected on the rdf model
- the change events inside a change log are not elements of any rdf list concept so there is no way of reflecting in the rdf model of the change Log the order of the change events.
- The only requirements in TRS is that the change log segmentation respects the ordering which is the case here.
