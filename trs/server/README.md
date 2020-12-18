# TRS Server library

This repository contains the [Eclipse Lyo](https://projects.eclipse.org/projects/technology.lyo) *TRS Server* library.

The *TRS Server* library is a set of ready-to-use classes that provide the required REST services for TRS, with minimal effort. 

**Note:** The current implementation supports an In-memory TRS Server that does not persist its TRS resources.
These classes are however designed to be extended for a persistent solution. 

For a thourough walkthough of TRS solutions, which among other things ensures persisting the TRS Logs, visit the [additional information on TRS](https://oslc.github.io/developing-oslc-applications/eclipse_lyo/eclipse-lyo#trs-sdk).

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
