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
use Net::OSLC::CM;
use Net::OSLC::CM::Connection;

BEGIN { use_ok('Net::OSLC::CM') };
BEGIN { use_ok('Net::OSLC::CM::Catalog') };
BEGIN { use_ok('Net::OSLC::CM::Parser') };

require_ok('RDF::Trine::Model');

my $test_url = "http://192.168.56.101:8282/bugz";
my $username = 'stephanie@minet.net';
my $password = "password";

my $cm = Net::OSLC::CM->new( 
  url => $test_url,
  username => $username,
  password => $password 
);
$cm->url($test_url);

#Checks the new created CM object is correct
ok(defined $cm,                                   'Net::OSLC::CM new object is defined');
ok($cm->isa('Net::OSLC::CM'),                     'and is the right class');


my $test_url_catalog = $test_url . "/catalog";

$cm->create_catalog();
ok(defined $cm->catalog,                          'new Catalog object is defined');
ok($cm->catalog->isa('Net::OSLC::CM::Catalog'),   'and is the right class');
is($cm->catalog->url, $test_url_catalog,          'registered catalog url \'' . $cm->catalog->url . ' \'is correct');

#Tests how it handles different URL forms
$test_url = "http://192.168.56.101:8282/bugz/";
$cm = Net::OSLC::CM->new( 
  url => $test_url,
  username => $username,
  password => $password 
);
$cm->url($test_url);
$cm->create_catalog();
is($cm->catalog->url, $test_url_catalog,          'registered catalog url \'' . $cm->catalog->url . ' \'is correct');


#Tests parse_catalog
$cm->parser(
  Net::OSLC::CM::Parser->new(cm => $cm)
);
ok(defined $cm->parser,                            'new Parser object is defined');
ok($cm->parser->isa('Net::OSLC::CM::Parser'),      'and is the right class');

#Gets local data to test functions
local $/=undef;
open CATALOG, "data/catalog.xml" or die "Couldn't open file: $!";
my $body_catalog = <CATALOG>;
close CATALOG;

my $model = $cm->catalog->parse_catalog($cm->parser, $body_catalog);
ok($model->isa('RDF::Trine::Model'),                  'parse_catalog returns a RDF::Trine::Model object');

$cm->catalog->query_providers($cm->parser, $model);

my @test_data_catalog = ("http://localhost:8282/bugz/catalog",
  "http://localhost:8282/bugz/provider?productId=1",
  "http://localhost:8282/bugz/provider?productId=2",
  "http://localhost:8282/bugz/provider?productId=3");

is(@{$cm->catalog->providers_url}, @test_data_catalog,  'test data in the catalog is correct');
