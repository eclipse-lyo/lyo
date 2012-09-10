#! /usr/bin/env/ perl -Tw

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


use strict;
use warnings;

use Test::More tests => 14;
use Net::OSLC::CM::Connection;
use File::Temp qw/tempdir/;

BEGIN { use_ok('Net::OSLC::CM::Connection') };

require_ok('LWP::UserAgent');
require_ok('HTTP::MessageParser');
require_ok('HTTP::Response');

my $test_url = "http://192.168.56.101:8282/bugz";
my $username = 'stephanie@minet.net';
my $password = "password";

my $connection = Net::OSLC::CM::Connection->new( 
  url => $test_url,
  username => $username,
  password => $password 
);

#Checks the new created CM object is correct
ok(defined $connection,                   'Net::OSLC::CM::Connection cm attribute is defined');
ok($connection->isa('Net::OSLC::CM::Connection'),'connection is the right class');

#Checks the Connection attribute
ok(defined $connection->url,                  'connection url $connection->url is defined');
is($connection->url, $test_url,               'registered url \'' . $connection->url . '\' is correct');
ok(defined $connection->username,             'username $connection->username is defined');
is($connection->username, $username,          'registered username \'' . $connection->username . '\' is correct');
ok(defined $connection->password,             'password $connection->password is defined');
is($connection->password, $password,          'registered password \'' . $connection->password . '\' is correct');

my $cm = Net::OSLC::CM->new( 
  url => $test_url,
  username => $username,
  password => $password 
);

#Checks the new created CM object is correct
ok(defined $cm,                                   'Net::OSLC::CM new object is defined');
ok($cm->isa('Net::OSLC::CM'),                     'and is the right class');

#Checks the Connection attributes are correct
ok(defined $cm->connection,                       'Net::OSLC::CM::Connection cm attribute is defined');
ok($cm->connection->isa('Net::OSLC::CM::Connection'),'connection attribute is the right class');
ok(defined $cm->connection->url,                  'connection url $cm->connection->url is defined');
is($cm->connection->url, $test_url,               'registered url \'' . $cm->connection->url . '\' is correct');
ok(defined $cm->connection->username,             'username $cm->connection->username is defined');
is($cm->connection->username, $username,          'registered username \'' . $cm->connection->username . '\' is correct');
ok(defined $cm->connection->password,             'password $cm->connection->password is defined');
is($cm->connection->password, $password,          'registered password \'' . $cm->connection->password . '\' is correct');


#Gets local data to test functions
local $/=undef;
open CATALOG, "data/catalog.html" or die "Couldn't open file: $!";
my $catalog = <CATALOG>;
close CATALOG;
my $http_response = HTTP::Response->parse($catalog);

#Parsing the HTTP response to get the body (that should be XML/RDF data)
my $body = $connection->get_http_body($http_response);
ok(defined $body,                               'HTTP body of the request is defined');
my $is_xml = 0;
if ($body =~ m/^<\?xml version="1\.0"(.*)/){
  $is_xml = 1;
}
is($is_xml, 1,                                  'we get XML data version 1.0');

