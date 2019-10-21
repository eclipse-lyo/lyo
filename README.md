# TRS Server library

[![Build Status](https://travis-ci.org/eclipse/lyo.trs-server.svg?branch=master)](https://travis-ci.org/eclipse/lyo.trs-server)
[![](https://img.shields.io/badge/javadoc-2.4.0-blue.svg)](https://download.eclipse.org/lyo/docs/trs-server/2.4.0/overview-summary.html)
[![](https://img.shields.io/badge/javadoc-SNAPSHOT-blue.svg)](https://download.eclipse.org/lyo/docs/trs-server/latest/overview-summary.html)
[![Discourse status](https://img.shields.io/discourse/https/meta.discourse.org/status.svg)](https://forum.open-services.net/)
[![Gitter](https://img.shields.io/gitter/room/nwjs/nw.js.svg)](https://gitter.im/eclipse/lyo)

The purpose of the *TRS Server* library is to provide a developer with a ready
to use set of classes over which he can provide a minimal implementation that
will result in a TRS interface with minimal effort.

## Getting started

Add a dependency for the TRS Server library:

    <dependency>
      <groupId>org.eclipse.lyo.trs</groupId>
      <artifactId>trs-server</artifactId>
      <version>2.4.0</version>
    </dependency>
    
Ensure your POM file points to the Eclipse Lyo repositories:

    <repositories>
      <repository>
        <id>lyo-releases</id>
        <name>lyo-releases repository</name>
        <url>https://repo.eclipse.org/content/repositories/lyo-releases/</url>
        <snapshots>
          <enabled>false</enabled>
        </snapshots>
      </repository>
      <repository>
        <id>lyo-snapshots</id>
        <name>lyo-snapshots repository</name>
        <url>https://repo.eclipse.org/content/repositories/lyo-snapshots/</url>
        <releases>
          <enabled>false</enabled>
        </releases>
      </repository>
    </repositories>

Then, create two classes:

1. `YourChangeLog extends ChangeHistories`
1. `YourTrsService extends TrackedResourceSetService`

Next, register `YourTrsService` in the `Application` class:

    // TRS
    RESOURCE_CLASSES.add(YourTrsService.class);

After that, implement `HistoryData[] getHistory(HttpServletRequest, Date)` method in your newly created `YourChangeLog` class and return  an array of `HistoryData` objects.

After that, the server will be ready to respond to the requests of the TRS Client according to the OSLC TRS 2.0 WD spec.

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
