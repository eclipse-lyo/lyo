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

package Net::OSLC::CM::Connection;
use Any::Moose;

use LWP::UserAgent;
use HTTP::MessageParser;

=head1 NAME

Net::OSLC::CM::Connection - Class for the connection to a distant ticket database (Service Provider)

=head1 VERSION 

This document describes Net::OSLC::CM::Connection version 0.01

=head1 DESCRIPTION

Connects to a server with the given URL using HTTP basic authentication.

  use Net::OSLC::CM::Connection;

  #URL of the distant tickets database (OSLC-CM Service Provider)
  my $url = "http://example.com"
  
  # HTTP basic auth credentials
  my $username = "toto";
  my $password = "pwd";

  my $connection = Net::OSLC::CM::Connection->new(
       url      => $url,
       username => $username,
       password => $password
 );

=cut

has url => (
  isa => 'Str',
  is  => 'ro',
  required => 1
);

has username => (
  isa => 'Str',
  is => 'ro',
#  required => 1
);

has password => (
  isa => 'Str',
  is => 'ro',
#  required => 1
);

has connection => (
  isa => 'LWP::UserAgent', 
  is => 'rw',
  lazy =>1,
  default => sub {
    my $self = shift;
    my $connection = LWP::UserAgent->new(keep_alive => 1);
    return $connection;
  }
);

=head1 METHODS

=over 

=item C<< get_http_body ( $http_response ) >>

Returns the body part of a HTTP GET response.
$http_response is a HTTP::Response object.

=cut

sub get_http_body {
  my $self = shift;
  my $http_response = shift;

  # parse_response() returns body as a string reference
  my ( $HTTP_version, $status_Code, $reason_phrase, $headers, $body )
          = HTTP::MessageParser->parse_response($http_response->as_string());

  return $$body;
 
}

1;

=back
