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

$cm->parser(
  Net::OSLC::CM::Parser->new(cm => $cm)
);

my $provider_url = "http://localhost:8282/bugz/provider?productId=1";

my $provider = Net::OSLC::CM::ServiceProvider->new(
  cm => $cm,
  url => $provider_url,
);

ok(defined $provider,                               'Net::OSLC::CM::ServiceProvider is defined');
ok($provider->isa('Net::OSLC::CM::ServiceProvider'),'and is the right class');
is($provider->url, $provider_url,                   'registered service provider url \'' . $provider->url . '\' is correct.' );
is($provider->cm, $cm,                              'registered cm \'' . $cm . '\' is correct');

#Gets local data to test functions
local $/=undef;
open PROVIDER, "data/serviceprovider.xml" or die "Couldn't open file: $!";
my $body_provider = <PROVIDER>;
close PROVIDER;

my $model = $provider->parse_service_provider($cm->parser, $body_provider);
ok($model->isa('RDF::Trine::Model'),                  'parse_servce_provider returns a RDF::Trine::Model object');

$provider->query_resource($cm->parser, $model,
                            "queryCapability",
                            "queryBase",
                            $provider->queryBase);

my $test_queryBase = "http://192.168.56.101:8282/bugz/changerequests?productId=1";

is(${$provider->queryBase}[0], $test_queryBase,        'found queryBase \'' . ${$provider->queryBase}[0] . '\' is correct.');
ok(!defined ${$provider->queryBase}[1],                'found only one queryBase url.');

$provider->queryBase([]);

$provider->query_resource($cm->parser, $model,
                            "queryCapability",
                            "queryBaseError",
                            $provider->queryBase);

ok(!defined ${$provider->queryBase}[0],                 'query is incorrect.');

