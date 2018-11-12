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

use Test::More tests => 26;
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

$cm->parser(
  Net::OSLC::CM::Parser->new(cm => $cm)
);

my $url_test = "http://192.168.56.101:8282/bugz/changerequest?id=20";
my $changeRequest = Net::OSLC::CM::ChangeRequest->new(
  url => $url_test
);

ok(defined $changeRequest,                              'Net::OSLC::CM::ChangeRequest new object is defined');
ok($changeRequest->isa('Net::OSLC::CM::ChangeRequest'), 'and is the right class');
is($changeRequest->url, $url_test,                      'registered change request url \'' . $changeRequest->url . '\' is correct.' );

#Gets local data to test functions
local $/=undef;
open CHANGEREQUEST, "data/changerequest.xml" or die "Couldn't open file: $!";
my $body = <CHANGEREQUEST>;
close CHANGEREQUEST;

my $model = $changeRequest->parse_changeRequest($cm->parser, $body);
ok($model->isa('RDF::Trine::Model'),                  'parse_changeRequest returns a RDF::Trine::Model object');

$changeRequest->model($model);
ok($changeRequest->model->isa('RDF::Trine::Model'),    '$changeRequest is a RDF::Trine::Model object');

$changeRequest->load();
is($changeRequest->contributor, 'stephanie@minet.net',  'found correct contributor');
is($changeRequest->creator, undef,                      'found correct creator');
ok($changeRequest->created->isa('DateTime'),            'transforms created date from XSD format to DateTime format');
is($changeRequest->created, '2012-04-22T16:16:00',      'found correct date of creation');
is($changeRequest->description, undef,                  'found correct description');
is($changeRequest->identifier, "20",                    'found correct identifier');
ok($changeRequest->modified->isa('DateTime'),           'transforms modified date from XSD format to DateTime format');
is($changeRequest->modified, '2012-04-30T20:48:36',     'found correct date of modification');
is($changeRequest->status, "RESOLVED",                  'found correct status');
is($changeRequest->subject, undef,                      'found correct subject');
is($changeRequest->title, "bug 1",                      'found correct title');
is($changeRequest->bugz_component, "Datastore",         'found correct bugz component');
is($changeRequest->bugz_opsys, "Linux",                 'found correct bugz opsys');
is($changeRequest->bugz_product, "FakePayroll",         'found correct bugz product');
is($changeRequest->bugz_version, "1.0",                 'found correct bugz version');


