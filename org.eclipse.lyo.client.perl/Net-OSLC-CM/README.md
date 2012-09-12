# Net-OSLC-CM version 0.01

Net-OSLC-CM is a module to help implement a [OSLC][open-services] client for Change Management,
according to [OSLC-CM specifications v2][oslcc-cm] .

Web page about the project: [http://arroway.github.com/Net-OSLC-CM][webpage].

## Installation

Dependencies:

* HTTP::MessageParser
* RDF::Trine
* RDF::Query

To install this module type the following:

~~~ sh
   perl Makefile.PL
   make
   make test
   make install
~~~ 


## How to generate documentation

To generate perl documentation type the following:

~~~ sh
   $ perldoc name_of_module
~~~


## IMPORTANT

This module was built and tested using [Simple Defects][sd] and the [OSLC-CM connector for Bugzilla][oslccm-connector]. 
I am working on a localhost database and using HTTP basic auth. 


## About this module
  
Net::OSLC::CM provides a Perl interface to help implement OSLC‐CM Consumers according to OSLC specifications described at
open−services.net. In the current state, this module implements function to retrieve changeRequests from Service Providers and Service
Providers Catalog.  
It uses HTTP basic authentication to connect to the distant changeRequest database.

Net::OSLC::CM relies on:
* Net::OSLC::CM::Connection
* Net::OSLC::CM::Parser
* Net::OSLC::CM::Catalog
* Net::OSLC::CM::ServiceProvider
* Net::OSLC::CM::ChangeRequest
* Net::OSLC::CM::Util

## Examples

    use Net::OSLC::CM;

    my $oslccm = Net::OSLC::CM−>new(
                url      => $self−>remote_url,
                username => $username,
                password => $password
    ));

    # Getting an array of changeRequests from the changeRequest database 
    # (array of Net::OSLC::CM::ChangeRequest objects)
    my @results = $oslccm−>get_oslc_resources;


get_oslc_resources is a wrapping function that calls successively the following:


    $oslccm−>url = "http://example.com";

    # RDF data parser
    $oslccm−>parser(
        Net::OSLC::CM::Parser−>new(cm => $oslccm)
    );

    # Assumes it'll get a Service Providers Catalog
    $oslccm−>create_catalog;

    # Gets the catalog (assuming it does exists to get the Service Providers information)
    $oslccm−>get_provider_catalog_resource;

    # Retrieves URLs of the Service Providers given by the Catalog
    $oslccm−>get_service_providers;

    # Gets changeRequests URLs from each Service Provider, creates a Net::OSLC::CM::ChangeRequest 
    # object and pushes it into the $oslccm−>changeRequests array
    $oslccm−>get_changeRequests($oslccm−>providers);

    # Gets data for each changeRequest
    $oslccm−>load_changeRequests();

    my @results = $oslccm−>changeRequests;


## Methods you can call from Net::OSLC::CM

`new ( $url, $username, $password )`
  Returns a new Net::OSLC::CM object to make a connection to the changeRequest database of given $url. 
  When the distant database requires HTTP basic authentication, you provide a username and a password at the creation.

`get_oslc_resources`
  Returns an array of Net::OSLC::CM::ChangeRequest objects.

`get_provider_catalog_resource`
  Gets if it exists the Service Provider Catalog as a Net::OSLC::CM::Catalog object and performs a query to get the
  referenced Service Providers .  An OSLC Service Provider Catalog Document describes a catalog whose entries describe service
  providers or out‐of‐line subcatalogs.  OSLC CM service providers must provide a Service Provider Resource and *MAY* provide a
  Service Provider Catalog Resource.

`create_catalog`
 Creates an instance of the Net::OSLC::CM:Catalog class.

`get_service_providers`
 Populates an array of Service Providers objects.

`_get_service_providers ( $provider )`
 For a given Catalog, gets the resources and properties for the provided Net::OSLC::CM::ServiceProvider object: queryCapability,
 resourceShape and creationFactory.

(the following is quoted from the specifications on open-services.net)

* queryCapability: Enables clients to query across a collection of resources via HTTP GET or POST.  To perform an HTTP GET query, 
  an OSLC client starts with the base URI as defined by the oslc:queryBase property of a Query Capability, and appends to it query 
  parameters in a syntax supported by the service.
* resourceShape: In some cases, to create resources and to query those that already exist within an OSLC Service, OSLC clients needs 
  a way to learn which properties are commonly used in or required by the service. Resources meet this need by providing a machine‐readable
  definition of an OSLC resource type.  A Resource Shape describes the properties that are allowed or required by one type of resource.
  Resource Shapes are intended to provide simple "hints" to clients at resource creation, update or query time.
* creationFactory: Enables clients to create new resources via HTTP POST.

`get_changeRequests`
 Wrapping function to get every changeRequest from every Service Provider enlisted and its attributes.

`_get_changeRequest ( $model )`
 Populates an array of Net::OSLC::CM::ChangeRequest objects. Takes in argument a RDF::Trine::Model object with the RDF model that was
 parsed from the RDF data.

`load_changeRequests`
 Loads the attributes (id, title, creator, description...) of the changeRequests by calling the `load` method of the
 Net:OSLC::CM::ChangeRequest class. See Net::OSLC::CM::ChangeRequest documentation for more information.


## Bugs

Please report any bugs or feature requests to "steph[dot]ouillon[at]gmail[dot]com"


## Copyright

Copyright (C) 2012 Stéphanie Ouillon
 
All rights reserved. This program and the accompanying materials
are made available under the terms of the Eclipse Public License v1.0
and Eclipse Distribution License v. 1.0 which accompanies this distribution. 
 
The Eclipse Public License is available at http://www.eclipse.org/legal/epl-v10.html
and the Eclipse Distribution License is available at 
http://www.eclipse.org/org/documents/edl-v10.php.
 
Contributors:
  Stéphanie Ouillon - initial API and implementation


This project has been initiated as a student project with the support of Télécom SudParis. 


[webpage]: http://arroway.github.com/Net-OSLC-CM
[sd]: http://syncwith.us
[open-services]: http://open-services.net
[oslccm-connector]: http://wiki.eclipse.org/Lyo/BuildBugzilla
[oslcc-cm]: http://open-services.net/bin/view/Main/CmHome
