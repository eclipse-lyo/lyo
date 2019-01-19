# TRS Client

[![Build Status](https://travis-ci.org/eclipse/lyo.trs-client.svg?branch=master)](https://travis-ci.org/eclipse/lyo.trs-client)
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

## Getting started

Run with Maven:

	$ mvn clean package

In the `target` directory, you should see:

1. `appassembler` directory.
2. Archives ending with `-bin`.

Use the executables in the `appassembler` directory to run locally, and use the archives for server deployment.

## Starting up

1. Download Eclipse for JEE development and start it
2. Clone the Lyo Git Repo located under:
3. Import the org.eclipse.lyo.oslc4j.trs.Client project into your eclipse workspace.
3. Under Run Configurations in Eclipse select TRS Client build verify clean install and run it. This builds the TRS Client binary under the target folder in the project the binary.
4. Configure the TRS Client as mentioned in the [config](#config) chapter.
5. Run the TRS Client fro the Launch configuration of the TRS Client using the TRs Client launch Java run Configuration

## TRS Client configuration <a name="config"></a>

The Application data for the TRS Client is stored in the application data folder. The application data folder has the following form in each operating system

	%USER_HOME% is the user home dir in this case

	A. UNIX

	%USER_HOME%/.TrsClient

	B. Windows

	%APPDATA%/TrsClient

	C. Mac

	%USER_HOME%/Library/Application Support/TrsClient

For example on windows:

    C:\Users\Omar\AppData\Roaming\TrsClient

**The configuration files themselves are located in the `config` subfolder, e.g. `C:\Users\Omar\AppData\Roaming\TrsClient\config`.**

TRS Client has the following configuration:

- A TRSClient properties file (`trsClient.properties`)
- A `trsProviders` folder containing a configuration file (eg. `providerBugzilla.properties`) for each TRS Provider connected to the client

The TRS client properties file contains the urls of the sparql http endpoint
sof the triplestore used as lifecycle data backend.If the triplestore uses
basic http authentication, the basic authentication credentials are included
in this configuration file. The configuration file has the following form:

    sparqlUpdateBase=https://vservices.offis.de/rtp/fuseki/v1.0/ldr/update
    sparqlQueryBase=https://vservices.offis.de/rtp/fuseki/v1.0/ldr/query
    baseAuth_user=okacimi
    baseAuth_pwd=

The TRS provider file contains the uri of the provider, eg,

    trs_uri=http://localhost:8080/oslc4jbugzilla/services/trs/
    baseAuth_user=
    baseAuth_pwd=

## Run TRS Client

After adding the configuration, run TRS Client with command:

    cd trs-Client
    mvn clean package
    ./target/appassembler/bin/trsClient

## Tips for common issues

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
