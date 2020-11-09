# Eclipse Lyo Client

[![](https://img.shields.io/jenkins/s/https/ci.eclipse.org/lyo/job/lyo-client-master.svg)](https://ci.eclipse.org/lyo/job/lyo-client-master/)
![CI](https://github.com/eclipse/lyo.client/workflows/CI/badge.svg)
[![Discourse status](https://img.shields.io/discourse/https/meta.discourse.org/status.svg)](https://forum.open-services.net/)
[![Gitter](https://img.shields.io/gitter/room/nwjs/nw.js.svg)](https://gitter.im/eclipse/lyo)

This repository contains the [Eclipse Lyo](https://projects.eclipse.org/projects/technology.lyo) Client library.

## Introduction

The [Eclipse Lyo](https://projects.eclipse.org/projects/technology.lyo) project is focused on providing an SDK to enable adoption of [OSLC specifications](https://open-services.net/). OSLC (Open Services for Lifecycle Collaboration) is an open community dedicated to reducing barriers for lifecycle tool integration. The community authors specifications for exposing lifecycle artifacts through uniform (REST) interfaces and relying on Internet and Linked Data standards.

OSLC's scope started with Application Lifecycle Management (ALM) and is expanding to include integrations across Product Lifecycle Management (PLM) and IT Service Management (ISM/DevOps), Lyo is designed to be a companion to the continuing specification efforts of the OSLC community. Its main purpose is to expand adoption of OSLC specifications and to enable the Eclipse community to easily build OSLC compliant tools.

## Getting started

To use this library, follow the setup and development instructions under the [OSLC Developer Guide for Client setup](https://oslc.github.io/developing-oslc-applications/eclipse_lyo/setup-an-oslc-provider-consumer-application.html)

You can find more resources for developing OSLC applications with Lyo, under the [OSLC Developer Guide](https://oslc.github.io/developing-oslc-applications/eclipse_lyo/eclipse-lyo.html#oslc4j-sdk).

The [Lyo Samples](https://github.com/OSLC/lyo-samples) repository contains sample code that demonstrates how to use the Lyo client to interact with OSLC Service Providers in various ways.

You are also welcome to contact the development team via [lyo-dev mailing list](https://dev.eclipse.org/mailman/listinfo/lyo-dev)

## Contributing

See [contributing](https://github.com/eclipse/lyo#contributing) under the main [Eclipse Lyo](https://github.com/eclipse/lyo) repository.

## Building the project
(Unless you need to work from source code, you need not build this project. You are instead adviced to add the necessary Lyo dependecies as described under the [OSLC Developer Guide](https://oslc.github.io/developing-oslc-applications/eclipse_lyo/setup-an-oslc-provider-consumer-application.html).)

This project uses Maven as the build system for all Java projects except those which are Eclipse Plugin project. The latter projects contain all the Eclipse project files under Git for import and building using the *Import > Existing Projects into Workspace*. All other projects should be imported using the *Import > Existing Maven Projects* menu.
