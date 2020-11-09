# TRS Client

![CI](https://github.com/eclipse/lyo.trs-client/workflows/CI/badge.svg)
[![](https://img.shields.io/badge/javadoc-latest-blue.svg)](https://download.eclipse.org/lyo/docs/trs-client/latest/)
[![](https://img.shields.io/badge/misc-discourse-lightgrey.svg)](https://forum.open-services.net/)
[![](https://img.shields.io/badge/misc-gitter-lightgrey.svg)](https://gitter.im/eclipse/lyo)


## Motivation

The *TRS Client* utility uses the TRS interfaces of existing OSLC adapters in order to cache the data exposed by these OSLC adadapters in a preconfigured HTTP SPARQL enabled triplestore and to keep this data cache up to date with the data coming from the adapters. for more information please check the  [OSLC TRS 2.0 draft spec][1]

## Supported features

The following features are currently supported:

* Full processing of the TRS information of a TRS provider
* Concurrent processing of Base Members of a Tracked Resource Set
* Concurrent processing of Change Events
* Concurrent processing of TRS Providers
* Basic Http Authentication support

The implementation of the following features is planned:

* Concurrent retrieval of Base and ChangeLog resources from TRS Providers
* Support of OAuth authentication
* Management of Server Rollback to an earlier state


## FAQ

**There is an error in `ModelCreationUtil.java`**

Comment `//import javax.servlet.ServletException;` and related class.

**There is an error JRE**

Make sure you are using the JDK, not JRE, eg. in Eclipse: `Windows - Preferences - Java - Installed JREs`

**TRS provider is running in Virtual Machine and TRS Client can't access it from the Local Machine**

Set Port Forwarding in virtual machine:

* Host port: 8080 - Guest port: 8080
* Host port: 80 - Guest port: 80

Set hosts in local machine

* Edit `C:\Windows\System32\drivers\etc\hosts`
* Check VM hostname: `hostname` in VM's terminal.
* Add line `127.0.0.1 %VM_hostname%` to the hosts file.
* If you cannot edit hosts file, google how to get permissions to that file

[1]: http://open-services.net/wiki/core/TrackedResourceSet-2.0/
