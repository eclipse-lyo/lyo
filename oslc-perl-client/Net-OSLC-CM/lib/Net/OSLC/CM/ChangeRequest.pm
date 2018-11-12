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

package Net::OSLC::CM::ChangeRequest;
use Any::Moose;

use Net::OSLC::CM::Connection;
use Net::OSLC::CM::Parser;
use Net::OSLC::CM::ServiceProvider;
use RDF::Helper;
use Net::OSLC::CM::Util;

=head1 NAME

Net::OSLC::CM::ChangeRequest - a class for a OSLC-CM Change Request

=head1 VERSION

This document describes Net::OSLC::CM::Request version 0.01

=head1 DESCRIPTION

A class for OSLC-CM Change Request that provides common OSLC attributes, and some additional attributes specific to Bugzilla.

=cut


has model => (isa => 'RDF::Trine::Model', is => 'rw');

has url =>(isa => 'Str', is => 'rw');
has contributor => (isa => 'Str', is => 'rw');
has creator => (isa => 'Str', is => 'rw');
has created => (isa => 'DateTime', is => 'rw');
has description => (isa => 'Str', is => 'rw');
has identifier => (isa => 'Str', is => 'rw');
has modified => (isa => 'DateTime', is => 'rw');
has status => (isa => 'Str', is => 'rw');
has subject => (isa => 'Str', is => 'rw');
has title => (isa => 'Str', is => 'rw');

#Specific to Bugzilla database
has bugz_component => (isa => 'Str', is => 'rw');
has bugz_opsys => (isa => 'Str', is => 'rw');
has bugz_product => (isa => 'Str', is => 'rw');
has bugz_version => (isa => 'Str', is => 'rw');

=head1 METHODS

=over

=item C<< load >>

Loads every know attributes of the Change Request.

=cut

sub load {
  my $self = shift;

  my $rdf = RDF::Helper->new(
    BaseInterface => 'RDF:Trine',
    Namespaces => { 
      dcterms => 'http://purl.org/dc/terms/',
      rdf => 'http://www.w3.org/1999/02/22-rdf-syntax-ns#',
      foaf => 'http://xmlns.com/foaf/0.1/',
      oslc => 'http://open-services.net/ns/core#',
      oslccm => 'http://open-services.net/ns/cm#',
      bugz => 'http://www.bugzilla.org/rdf#',
      '#default' => "http://xmlns.com/foaf/0.1/"
    },
  );
 
  $rdf->include_model($self->model);
  
  # XXX
  #my $url = $self->url;
  #$url =~ s/192.168.56.101/localhost/;
  #$self->url($url);
  
  my $obj = $rdf->get_object($self->url);

  if (defined($obj->dcterms_contributor)){
      my $person = $obj->dcterms_contributor;
      #if (defined($person->foaf_mail)){
        $self->contributor($person->foaf_email);
        print "contributor: " . $self->contributor . "\n";
      #}
  }
    
  if (defined($obj->dcterms_creator)){
     $self->creator($obj->dcterms_creator);
     print "creator: " . $self->creator . "\n";
  }

  if (defined($obj->dcterms_created)){
     my $xsd = $obj->dcterms_created;
     my $datetime = Net::OSLC::CM::Util->XSDToDateTime($xsd);
     $self->created($datetime);
     print "created: " . $self->created . "\n";
  }
  
  if (defined($obj->dcterms_description)){
    $self->description($obj->dcterms_description);
    print "description: " . $self->description . "\n";
  }

  if (defined($obj->dcterms_identifier)){
    $self->identifier($obj->dcterms_identifier);
    print "identifier: " . $self->identifier . "\n";
  }
  
  if (defined($obj->dcterms_modified)){
    my $xsd = $obj->dcterms_modified;
    my $datetime = Net::OSLC::CM::Util->XSDToDateTime($xsd);
    $self->modified($datetime);
    print "modified: " . $self->modified . "\n";
  }
  
  if (defined($obj->oslccm_status)){
    $self->status($obj->oslccm_status);
    print "status: " . $self->status . "\n";
  }
  
  if (defined($obj->dcterms_subject)){
    $self->subject($obj->dcterms_subject);
    print "subject: " . $self->subject . "\n";
  }
  
  if (defined($obj->dcterms_title)){
    $self->title($obj->dcterms_title);
    print "title: " . $self->title . "\n";
  }
  
  #Specific to Bugzilla
  if (defined($obj->bugz_component)){
    $self->bugz_component($obj->bugz_component);
    print "component: " . $self->bugz_component . "\n";
  }

  if (defined($obj->bugz_opsys)){
    $self->bugz_opsys($obj->bugz_opsys);
    print "opsys: " . $self->bugz_opsys . "\n";
  }
  
  if (defined($obj->bugz_product)){
    $self->bugz_product($obj->bugz_product);
    print "product: " . $self->bugz_product . "\n";
  }
  
  if (defined($obj->bugz_version)){
    $self->bugz_version($obj->bugz_version);
    print "version: " . $self->bugz_version . "\n";
  }

}

=item C<< get_changeRequest ( $connection ) >>

Gets the Change request RDF/XML data of the object URL attribute and returns it as a string.
$connection is a Net::OSLC::CM::Connection object.

=cut

sub get_changeRequest {
  my $self = shift;
  my $connection = shift;

  print $self->url . "\n";

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

=item C<< parse_changeRequest ($parser, $xml_data) >>

Parses the XML data and returns the associated RDF model.
$parser is a Net::OSLC::CM::Parser object and $xml_data is a string.

=cut

sub parse_changeRequest {
  my $self = shift;
  my ($parser, $body) = @_ ;

  my $model = $parser->parse_xml_ressources($self->url, $body);
  $self->model($model);
  return $self->model;
}

1;

=back
