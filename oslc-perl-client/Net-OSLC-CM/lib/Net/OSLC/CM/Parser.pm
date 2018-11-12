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

package Net::OSLC::CM::Parser;

use Any::Moose;
use RDF::Trine;
use RDF::Query;

=head1 NAME

Net::OSLC::CM::Parser - RDF Parser

=head1 VERSION

This document describes Net::OSLC::CM::Parser version 0.01

=head1 DESCRIPTION

Utility for parsing RDF/XML ressources into a RDF model and 
performing SPARQL queries in the given model.

=cut

has cm => (
  isa => 'Net::OSLC::CM',
  is => 'rw'
);

=head1 METHODS

=over

=item C<< parse_xml_ressources ( $base_uri, $rdfxml_data ) >>

Returns a RDF::Trine::Model object.
The argument $rdfxml_data is a string (RDF/XML data) we got as the body of the HTTP GET request we performed.
Through the regex, we retrive only the XML data we're interested in, 
and we parse it into a RDF model located in memory.

=cut

sub parse_xml_ressources {
  my $self = shift;
  my ($base_uri, $rdf_data) = @_;

  # we only want rdf data from the body of the HTTP response
  $rdf_data =~ m/(<rdf.*RDF>)/;

  my $store = RDF::Trine::Store::Memory->new();
  my $parser = RDF::Trine::Parser->new('rdfxml');
  my $model = RDF::Trine::Model->new($store);

  $parser->parse_into_model( $base_uri, $rdf_data, $model );
  return $model;
} 

=item C<< query_rdf model ( $rdf_query, $result_storage ) >>

Performs the given SPARQL $rdf_query and stores the result in $result_storage which is an array reference.

=cut

sub query_rdf {
  my $self = shift;
  my ($model, $rdf_query, $result_storage) = @_;

  my $string_query = "
    PREFIX dcterms: <http://purl.org/dc/terms/>
    PREFIX rdf:     <http://www.w3.org/1999/02/22-rdf-syntax-ns#>
    PREFIX rdfs:    <http://www.w3.org/2000/01/rdf-schema#>
    PREFIX oslc:    <http://open-services.net/ns/core#>
    PREFIX oslc_cm: <http://open-services.net/ns/cm#> "
    . $rdf_query;
 
    
  my $query = RDF::Query->new($string_query);
  my $iterator = $query->execute( $model );
  while (my $row = $iterator->next) {
    push($result_storage, $row);
  }
}

1;

=back
