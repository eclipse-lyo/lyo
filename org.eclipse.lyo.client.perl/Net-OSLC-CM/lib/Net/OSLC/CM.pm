#######################################################################################
# Copyright (c) 2012 Stéphanie Ouillon
#
# All rights reserved. This program and the accompanying materials
# are made available under the terms of the Eclipse Public License v1.0
# and Eclipse Distribution License v. 1.0 which accompanies this distribution. 
#
# The Eclipse Public License is available at http://www.eclipse.org/legal/epl-v10.html
# and the Eclipse Distribution License is available at 
# http://www.eclipse.org/org/documents/edl-v10.php.
#
# Contributors:
#
#   Stéphanie Ouillon - initial API and implementation
#######################################################################################

package Net::OSLC::CM;
use Any::Moose;

use Net::OSLC::CM::Catalog;
use Net::OSLC::CM::Connection;
use Net::OSLC::CM::Parser;
use Net::OSLC::CM::ServiceProvider;
use Net::OSLC::CM::ChangeRequest;
use RDF::Trine;
use RDF::Query;
use HTTP::MessageParser;

our $VERSION = '0.01';

=head1 NAME

Net::OSLC::CM - module to help implement a OSLC client for Change Management

=head1 VERSION

This document describes Net::OSLC::CM version 0.01

=head1 DESCRIPTION
  
Net::OSLC::CM provides a Perl interface to help implement OSLC-CM Consumers according to OSLC specifications described at open-services.net.
In the current state, this module implements function to retrieve changeRequests from Service Providers and Service Providers Catalog.
It uses HTTP basic authentication to connect to the distant changeRequest database.

An example of use:

  use Net::OSLC::CM;   

  my $oslccm = Net::OSLC::CM->new(
                  url      => $self->remote_url,
                  username => $username,
                  password => $password 
  ));

  # Getting an array of changeRequests from the changeRequest database (array of Net::OSLC::CM::ChangeRequest objects)
  my @results = $oslccm->get_oslc_resources;

get_oslc_resources is a wrap function that calls successively the following:

  $oslccm->url = "http://example.com";
  
  # RDF data parser
  $oslccm->parser( 
    Net::OSLC::CM::Parser->new(cm => $oslccm) 
  );
  
  # Assumes it'll get a Service Providers Catalog
  $oslccm->create_catalog;
             
  # Gets the catalog (assuming it does exists to get the Service Providers information)
  $oslccm->get_provider_catalog_resource;
 
  # Retrieves URLs of the Service Providers given by the Catalog
  $oslccm->get_service_providers;
             
  # Gets changeRequests URLs from each Service Provider, creates a Net::OSLC::CM::ChangeRequest object and 
  # pushes it into the $oslccm->changeRequests array
  $oslccm->get_changeRequests($oslccm->providers);

  # Gets data for each changeRequest
  $oslccm->load_changeRequests();

  my @results = $oslccm->changeRequests;


Net::OSLC::CM relies on:

=over 4                      

=item * Net::OSLC::CM::Connection

=item * Net::OSLC::CM::Parser

=item * Net::OSLC::CM::Catalog

=item * Net::OSLC::CM::ServiceProvider

=item * Net::OSLC::CM::ChangeRequest

=item * Net::OSLC::CM::Util

=back

=cut

has url => (
  isa => 'Str',
  is => 'rw',
);

has connection => (
  isa => 'Net::OSLC::CM::Connection',
  is => 'rw',
);

has catalog => (
  isa => 'Net::OSLC::CM::Catalog',
  is => 'rw'
);

has providers => (
  isa => 'ArrayRef',
  is => 'rw',
  default => sub {[]}
);

has changeRequests => (
  isa => 'ArrayRef',
  is => 'rw',
  default => sub {[]} 
);

has parser => (
  isa => 'Net::OSLC::CM::Parser',
  is => 'rw',
);

sub BUILDARGS {
  my $self = shift;
  my %args = @_;
  
  $args{connection} = Net::OSLC::CM::Connection->new(
    url      => delete $args{url},
    username => delete $args{username},
    password => delete $args{password}
  );

  return $self->SUPER::BUILDARGS(%args);
}

=head1 METHODS

=over 4

=item C<< new ( $url, $username, $password ) >>

Returns a new Net::OSLC::CM object to make a connection to the changeRequest database of given $url.
When the distant database requires HTTP basic authentication, you provide a username and a password at the creation.

=cut

=item C<< get_oslc_resources >>

Returns an array of Net::OSLC::CM::ChangeRequest objects.

=cut 

sub get_oslc_resources {
  my $self = shift;
  $self->url($self->connection->url);

  $self->create_catalog;
  $self->parser( 
    Net::OSLC::CM::Parser->new(cm => $self) 
  );
  
  $self->get_provider_catalog_resource;
  $self->get_service_providers;
  
  $self->get_changeRequests($self->providers);
  $self->load_changeRequests();
  return $self->changeRequests;
}

=item C<< get_provider_catalog_resource >>

Gets if it exists the Service Provider Catalog as a Net::OSLC::CM::Catalog object and performs a query to get 
the referenced Service Providers .
An OSLC Service Provider Catalog Document describes a catalog whose entries describe service providers or out-of-line subcatalogs.
OSLC CM service providers must provide a Service Provider Resource and *MAY* provide a Service Provider Catalog Resource.

=cut
 
sub get_provider_catalog_resource {
  my $self =shift;

  my $body_catalog = $self->catalog->get_catalog($self->connection);
  if (defined($body_catalog)){
    my $model =  $self->catalog->parse_catalog($self->parser, $body_catalog);
    $self->catalog->query_providers($self->parser, $model);
  } else {
    print "No catalog available.\n"
  }
}

=item C<< create_catalog >>

Creates an instance of the Net::OSLC::CM:Catalog class.

=cut

sub create_catalog {
  my $self = shift;
  my $catalog_url = "";

  if ($self->url =~ m/\/$/){
    $catalog_url = $self->url . "catalog";
  }
  else {
    $catalog_url =  $self->url . "/catalog";
  }
   
  $self->catalog(
    Net::OSLC::CM::Catalog->new(
      url => $catalog_url,
      cm => $self)
  );
}

=item C<< get_service_providers >>

Populates an array of Service Providers objects.

=cut

sub get_service_providers {
  my $self =shift;

  my $i = 0;
  for( $i=0; $i < @{$self->catalog->providers_url}; $i++){

    my $url = ${$self->catalog->providers_url}[$i];
    if (defined($url)){
      my $provider = Net::OSLC::CM::ServiceProvider->new(
                      cm => $self,
                      url => $url);
      
      $self->_get_service_provider($provider);
    
      push(@{$self->providers}, $provider);                         
    }
  }
}

=item C<< _get_service_providers ( $provider ) >>

For a given Catalog, gets the resources and properties for the provided Net::OSLC::CM::ServiceProvider object: 
queryCapability, resourceShape and creationFactory.

=cut 

=back

=head3 Query Capability

Enables clients to query across a collection of resources via HTTP GET or POST.
To perform an HTTP GET query, an OSLC client starts with the base URI 
as defined by the oslc:queryBase property of a Query Capability, and 
appends to it query parameters in a syntax supported by the service.

=cut

=head3 Resource Shape

In some cases, to create resources and to query those that already exist
within an OSLC Service, OSLC clients needs a way to learn which properties
are commonly used in or required by the service. Resource Shape Resources 
meet this need by providing a machine-readable definition of an OSLC resource 
type. 
A Resource Shape describes the properties that are allowed or required by 
one type of resource. Resource Shapes are intended to provide simple "hints" 
to clients at resource creation, update or query time.

=cut

=head3 Creation Factory

Enables clients to create new resources via HTTP POST.

=cut

sub _get_service_provider {
  
  my $self = shift;
  my $provider = shift;
  
  my $body_provider = $provider->get_service_provider($self->connection, $provider->url);
  if (defined($body_provider)){
      my $model =  $provider->parse_service_provider($self->parser, $body_provider);

      $provider->query_resource($self->parser, $model, 
                                  "queryCapability", 
                                  "queryBase", 
                                  $provider->queryBase);
  
     $provider->query_resource($self->parser, $model, 
                                  "queryCapability", 
                                  "resourceShape", 
                                  $provider->resourceShape);
 
     $provider->query_resource($self->parser, $model, 
                                  "creationFactory", 
                                   "resourceShape", 
                                   $provider->creationFactory);
 }
}

=over

=item C<< get_changeRequests >>
  
  Wrapping function to get every changeRequest from every Service Provider enlisted and its attributes.

=cut

sub get_changeRequests {
  my $self = shift;
  
  my $i; 
  for ( $i=1 ; $i < @{$self->providers} ; $i++) {
    my $provider = ${$self->providers}[$i];
    my $url = ${$provider->queryBase}[0];
    my $body = $provider->get_service_provider($self->connection, $url);

    if (defined($body)){
      my $model = $provider->parse_service_provider($self->parser, $body);
      $self->_get_changeRequest($model);
    }
  }
}

=item C<< _get_changeRequest ( $model ) >>

Populates an array of Net::OSLC::CM::ChangeRequest objects. Takes in argument a RDF::Trine::Model object with the RDF model
that was parsed from the RDF data.

=cut

sub _get_changeRequest {
  my $self = shift;
  my $model = shift;
  
  my $resource = "member";
  my $property = "ChangeRequest";
  #XXX: improve the query
  my $rdf_query = "SELECT DISTINCT ?url WHERE
                     {
                     ?url rdf:type ?u
                     }";
                     #?z rdfs:" . $resource . " ?x .
                     #?x oslc_cm:" . $property . " ?y .  
                     #}";
  my $result = [];
  $self->parser->query_rdf($model, $rdf_query, $result);
  
  my $i = 0;
  for ( $i=0; $i < @{$result}; $i++){
    if ( ${$result}[$i] =~ m/{ url=<(.*)> }/){
      my $res = $1;
      if ($res =~ m/http:\/\/(.*)\/changerequest\?id\=(.*)/){
        my $changeRequest = Net::OSLC::CM::ChangeRequest->new(url => $res);
        push(@{$self->changeRequests}, $changeRequest);
      }
    }
  }
}

=item C<< load_changeRequests >>

Loads the attributes (id, title, creator, description...) of the changeRequests by calling the load() method of
the Net:OSLC::CM::ChangeRequest class. See Net::OSLC::CM::ChangeRequest documentation for more information.

=cut

sub load_changeRequests {
  my $self = shift;
  my $i; 
  
  for ( $i=0 ; $i < @{$self->changeRequests} ; $i++) {
    my $changeRequest = ${$self->changeRequests}[$i];
    my $body = $changeRequest->get_changeRequest($self->connection);
    
    if (defined($body)){
      my $model = $changeRequest->parse_changeRequest($self->parser, $body);
      $changeRequest->load();
    }
  }
}

1;

__END__

=back

=head1 BUGS

Please report any bugs or feature requests to C<< steph[dot]ouillon[at]gmail[dot]com >>

=head1 AUTHOR

Stéphanie Ouillon

=head1 copyright & license

Copyright (C) 2012 by Stephanie Ouillon

All rights reserved. This program and the accompanying materials
are made available under the terms of the Eclipse Public License v1.0
and Eclipse Distribution License v. 1.0 which accompanies this distribution. 

The Eclipse Public License is available at http://www.eclipse.org/legal/epl-v10.html
and the Eclipse Distribution License is available at 
http://www.eclipse.org/org/documents/edl-v10.php.

Contributors:

    Stéphanie Ouillon - initial API and implementation

=cut
