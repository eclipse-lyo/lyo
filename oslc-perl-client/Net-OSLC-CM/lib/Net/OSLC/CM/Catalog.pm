#######################################################################################
## Copyright (c) 2012 Stéphanie Ouillon
##
## All rights reserved. This program and the accompanying materials
## are made available under the terms of the Eclipse Public License v1.0
## and Eclipse Distribution License v. 1.0 which accompanies this distribution. 
##
## The Eclipse Public License is available at http://www.eclipse.org/legal/epl-v10.html
## and the Eclipse Distribution License is available at 
## http://www.eclipse.org/org/documents/edl-v10.php.
##
## Contributors:
##
##   Stéphanie Ouillon - initial API and implementation
########################################################################################

package Net::OSLC::CM::Catalog;

use Any::Moose;

=head1 NAME

Net::OSLC::CM::Catalog - OSLC-CM Service Provider Catalog Ressource class

=head1 VERSION

This document describes Net::OSLC::CM::Catalog version 0.01

=head1 DESCRIPTION

The Catalog enables OSLC-CM clients to find Service Providers offered. 
These catalogs may contain other nested catalogs as well as 
service providers.

=cut

has cm => (
  isa => 'Net::OSLC::CM',
  is => 'rw',
);

has url => (
  isa => 'Str',
  is => 'rw',
);

has providers_url => (
  isa => 'ArrayRef',
  is => 'rw',
  default => sub { [] },
);

=head1 METHODS

=over

=item C<< get_catalog connection ( $connection ) >>

Returns HTTP body as a string.
It perfoms a GET HTTP request to get XML data from the Service Provider Catalog
of an OSLC service. It takes in argument a Net::OSLC::CM::Connection object.

=cut

sub get_catalog {
  my $self = shift;
  my $connection = shift;

  # The service provider should provide a catalog in RDF or HTML.
  # We ask for the XML version. 

  my $request = HTTP::Request->new(GET => $self->url);

  $request->header('Accept' => 'application/rdf+xml');
  $request->authorization_basic($connection->username, $connection->password);

  my $http_response = $connection->connection->request($request);
  
  if ($http_response->is_success) {
    my $body = $connection->get_http_body($http_response);
    return $body;
  }
  else {
    print $http_response->status_line . "\n";
    return;
  }
}

=item C<< parse_catalog ( $parser, $xml_data ) >>

Returns the RDF model as a RDF::Trine::Model object corresponding with the provided RDF/XML data.
It takes in arguments a Net::OSLC::CM::Parser (built on a RDF::Trine::Parser class) and a stringof RDF/XML data.

=cut

sub parse_catalog {
  my $self = shift;
  my $parser = shift;
  my $body = shift;

  my $model = $parser->parse_xml_ressources($self->url, $body);
  return $model;
}

=item C<< query_providers ( $parser, $rdf_model) >>

Populates an array of Service Providers URLs.
It performs a SPARQL query to get the URIs of every Service Provider that 
is referenced in the Service Providers Catalog.
It takes in argument a Net::OSLC::CM::Parser and a the RDF::Trine::Model corresponding to the RDF/XML data
we want to explore.

=cut

sub query_providers {
  my $self = shift;
  my $parser = shift;
  my $model = shift;
  my $arrayref = [];

  my $rdf_query = "SELECT DISTINCT ?url WHERE  { ?url dcterms:title ?u }";
  $parser->query_rdf($model, $rdf_query, $arrayref);

  my $i = 0;
  for ( $i=0; $i < @{$arrayref}; $i++){
    if ( ${$arrayref}[$i] =~ m/{ url=<(.*)> }/){
      my $provider = $1;
      if ($provider =~ m/http:\/\/(.*)/ and $provider !~ m/$self->url/){
        push($self->providers_url,$provider);
      }
    }


  }

}

1;

=back
