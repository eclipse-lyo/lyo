@rem = '--*-Perl-*--
@echo off
if "%OS%" == "Windows_NT" goto WinNT
if not exist %0.bat goto w98_next1
perl "%0.bat" %1 %2 %3 %4 %5 %6 %7 %8 %9
goto endofperl
:w98_next1
if not exist %0 goto w98_next2
perl "%0" %1 %2 %3 %4 %5 %6 %7 %8 %9
goto endofperl
:w98_next2
perl -S "%0" %1 %2 %3 %4 %5 %6 %7 %8 %9
goto endofperl
:WinNT
if not exist %0.bat goto next1
perl %0.bat %*
goto propagate_exit_status
:next1
if not exist %0 goto next2
perl %0 %*
goto propagate_exit_status
:next2
for %%p in ( %0 ) do perl -S %%~np.bat %*
:propagate_exit_status
if NOT "%COMSPEC%" == "%SystemRoot%\system32\cmd.exe" goto endofperl
if %errorlevel% == 9009 echo You do not have perl in your PATH.
if errorlevel 1 goto script_failed_so_exit_with_non_zero_val 2>nul
goto endofperl
@rem ';
#!perl

# Your perl script starts here.

###############################################################################
# Copyright (c) 2012 IBM Corporation.
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
#  Max Vohlken - initial API and implementation
###############################################################################

use strict;
use FindBin;
use lib "$FindBin::Bin/../lib/perl";
use File::Basename;
use Getopt::Long;
use Data::Dumper;
use XML::Simple;
#use LWP::Debug qw(+);
use URI;
use Lyo::OSLC::RTC::CM;
use constant MSWIN => $^O =~ /MSWin32|Windows_NT/i ? 1 : 0;

sub Usage {
	my(@msg) = @_;
	my $cmd = basename($0);

	my $usage = <<EOF;
Usage: $cmd [-h] 
       $cmd -baseuri <RTC jazz url> -user <user> -password <password> <url>
       $cmd -baseuri <RTC jazz url> -user <user> -password <password> \\
         -project <project area> \\
         -oslcwhere <simple query> \\
         [-property <property> ...]
       $cmd -baseuri <RTC jazz url> -user <user> -password <password> \\
         -workitem <work item #> \\
         [-property <property> ...] \\
         [-update <field>=<value> ...]
       $cmd -baseuri <RTC jazz url> -user <user> -password <password> \\
         -project <project area> \\
         -enum <enum>
       $cmd -baseuri <RTC jazz url> -user <user> -password <password> \\
         -project <project area> \\
         -state <state>
       $cmd -baseuri <RTC jazz url> -user <user> -password <password> \\
         -project <project area> \\
         -resolution <resolution>
EOF

	if(@msg) {
		print "\n$cmd: Error: ", @msg, "\n";
		print <<EOF;

$usage
EOF
	} else {
		print <<EOF;
		
This script can be used to interact with Rational Team Concert(RTC)
using its OSLC REST APIs. It implements behaviors documented at:

https://jazz.net/wiki/bin/view/Main/ResourceOrientedWorkItemAPIv2

$usage
Options:
  -baseuri <RTC jazz url>  - The base URI of the RTC server
  -user <user>             - RTC user id to authenicate with
  -password <password>     - RTC password
  -format <xml|perl|json>  - Specify the format of the result.
  -project <project area>  - The project area to run queries against
  -oslcwhere <simple query> - Simple query style query string. Reference:
                  http://open-services.net/bin/view/Main/CmQuerySyntaxV1
  -property <property>     - The property to retrieve using -oslcwhere
                             or -workitem.  More than one property can be
                             retrieved by supplying more than one -property
                             option. Reference:
    http://open-services.net/bin/view/Main/CmSpecificationV2#Resource_ChangeRequest
  -workitem <work item #>  - Retrieve a specific work item
  -update <field>=<value>  - Update field <field> with the new value 
                             of <value> in the work item specified with
                             -workitem. More than one field can be update
                             by supplying more than one -update option.
                             <field>=<value> has to be contained in one
                             command line argument so remember to quote
                             appropriately.
  -rootservices            - Retrieve the root services document.
  -services <project area> - Retrieve the services document for <project area>.
  -enum <enum type>        - Retrieve all of the enumeration records with
                             the internal name <enum type>.
  -enumItem <name>         - Retrieve the enumeration that belongs to the -enum enumeration 
                             type with the name <name>.
  -states <state type>     - Retrieve all of the state records with
                             the internal name <state type>.
  -state <name>            - Retrieve the state that belongs to the -states state type
                             with the name <name>.
  -resolutions <resolution type> - Retrieve all of the resolution records with
                             the internal name <resolution type>.
  -resolution <name>       - Retrieve the resolution that belongs to the -resolutions 
                             resolution type with the name <name>.
  -iterations              - Retrieve all of the iteration records.
  -iteration <name>        - Retrieves the iteration with the name <name>.

Examples:

  rtcclient -user me -password pw -baseuri https://rtc.mydomain.com/rtc \\
            -project CM -oslcwhere 'dcterms:identifier="99"' -prop 'dcterms:creator{*}'
  rtcclient -user me -password pw -baseuri https://rtc.mydomain.com/rtc \\
            -workitem 99 -update 'dcterms:description->{content}=99 beers on the wall'
  
EOF
	}
	exit 1;
}

our %opt;

sub process_args {
	my(@args) = @_;
	local(@ARGV);
	@ARGV = @args;
	$opt{'format'} = 'perl';
	GetOptions(\%opt,
		'help|?', 
		'debugging+',
		'user=s',
		'password=s',
		'format|fmt=s',
		'baseuri=s',
		'oslcwhere=s',
		'oslcsearchTerms=s',
		'oslcorderby=s',
		'update=s@',
		'workitem=s',
		'project=s',
		'property=s@',
		'rootservices',
		'services=s',
		'enum=s',
		'enumitem=s',
		'states=s',
		'state=s',
		'resolutions=s',
		'resolution=s',
		'iterations',
		'iteration=s@',
		'iteration2=s@',
	) or Usage();
    Usage() if($opt{'help'});
	@ARGV;
}

# Turn off buffering of STDOUT and STDERR.
select((select(STDOUT), $| = 1)[0]);
select((select(STDERR), $| = 1)[0]);

my $ScriptName = basename($0);

@ARGV = process_args(@ARGV);

Usage("Missing baseuri option.") if($opt{'baseuri'} eq '');
Usage("Missing user option.") if($opt{'user'} eq '');
Usage("Missing password option.") if($opt{'password'} eq '');

$Data::Dumper::Indent = 1;

my $client = Lyo::OSLC::RTC::CM->new;
$client->credentials($opt{'user'}, $opt{'password'});
$client->setBaseURI($opt{'baseuri'});
$client->debugging($opt{'debugging'}-1) if($opt{'debugging'} > 1);

if($opt{'format'} eq 'json') {
	$client->getUseragent()->default_header(
		#'Accept' => 'application/rdf+json',
		'Accept' => 'application/json',
	);
} else {
	$client->getUseragent()->default_header(
		'Accept' => 'application/rdf+xml',
	);
}

sub formatResponse {
	print("response:\n", $client->responseContent(), "\n") if($opt{'debugging'});
	if($opt{'format'} eq 'perl') {;
		my $xml = XMLin($client->responseContent(), 
			'ForceArray' => ['rdf:Description', 'rdfs:member'],
			'KeyAttr' => {'rdf:Description' => '+rdf:about'},
			#'KeyAttr' => {'rdf:Description' => 'rdf:type'},
			#'ValueAttr' => {'rdf:type' => 'rdf:resource'},
		);
		print Dumper($xml), "\n";
		#Lyo::OSLC::InlineRDFReferences($xml);
		#print Dumper($xml), "\n";
		
	} else {
		print $client->responseContent(), "\n";
	}
}

if($opt{'oslcwhere'}) {
	Usage("Missing project area argument.") if($opt{'project'} eq '');
	my $data = $client->oslcWhere(
		$opt{'project'},
		'Change request queries', # Hard code the queryCapability name. 
		$opt{'oslcwhere'}, 
		defined($opt{'property'}) ? $opt{'property'} : ['*{*}']
	);
	print Dumper($data->QueryResultsByIdentifier()), "\n";
	exit 0;
}

if($opt{'rootservices'}) {
	my $data = $client->rootservices();
	print Dumper($data), "\n";
	exit 0;
}

if($opt{'services'}) {
	my $data = $client->services($opt{'services'});
	print Dumper($data), "\n";
	exit 0;
}

if($opt{'enumitem'}) {
	Usage("Missing project area argument.") if($opt{'project'} eq '');
	Usage("Missing enum argument.") if($opt{'enum'} eq '');
	my $data = $client->enumerationItem($opt{'project'}, $opt{'enum'}, $opt{'enumitem'});
	print Dumper($data), "\n";
	exit 0;
}

if($opt{'enum'}) {
	Usage("Missing project area argument.") if($opt{'project'} eq '');
	my $data = $client->enumeration($opt{'project'}, $opt{'enum'});
	print Dumper($data->QueryResultsByTitle()), "\n";
	exit 0;
}

if($opt{'state'}) {
	Usage("Missing project area argument.") if($opt{'project'} eq '');
	Usage("Missing states argument.") if($opt{'states'} eq '');
	my $data = $client->state($opt{'project'}, $opt{'states'}, $opt{'state'});
	print Dumper($data), "\n";
	exit 0;
}

if($opt{'states'}) {
	Usage("Missing project area argument.") if($opt{'project'} eq '');
	my $data = $client->states($opt{'project'}, $opt{'states'});
	print Dumper($data->QueryResultsByTitle()), "\n";
	exit 0;
}

if($opt{'resolution'}) {
	Usage("Missing project area argument.") if($opt{'project'} eq '');
	Usage("Missing resolutions argument.") if($opt{'resolutions'} eq '');
	my $data = $client->resolution($opt{'project'}, $opt{'resolutions'}, $opt{'resolution'});
	print Dumper($data), "\n";
	exit 0;
}

if($opt{'resolutions'}) {
	Usage("Missing project area argument.") if($opt{'project'} eq '');
	my $data = $client->resolutions($opt{'project'}, $opt{'resolutions'});
	print Dumper($data->QueryResultsByTitle()), "\n";
	exit 0;
}

if($opt{'iterations'}) {
	my $data = $client->iterations($opt{'property'});
	print Dumper($data->QueryResults()), "\n";
	exit 0;
}

if($opt{'iteration'}) {
	for(@{$opt{'iteration'}}) {
		my $data = $client->iteration($_);
		print Dumper($data), "\n";
	}
	exit 0;
}

if($opt{'iteration2'}) {
	for(@{$opt{'iteration2'}}) {
		my $data = $client->iteration2($_);
		print Dumper($data), "\n";
	}
	exit 0;
}

if(defined($opt{'update'}) and @{$opt{'update'}}) {
	Usage("Missing work item argument.") if($opt{'workitem'} eq '');
	
	my @updates;
	foreach my $new_item (@{$opt{'update'}}) {
		my($var, $val) = split(/=/, $new_item, 2);
		push(@updates, [$var, $val]);
	}
	
	$client->workitemUpdate($opt{'workitem'}, \@updates, 1);
	exit 0;
}

if($opt{'workitem'}) {
	my $data = $client->workitem($opt{'workitem'}, $opt{'property'});
	if($opt{'format'} eq 'perl') {;
		my $results = $data->QueryResults();
		if(defined($results) and @$results) {
			print Dumper($results->[0]), "\n";
		}
		#print Dumper($data), "\n";
	} else {
		print $client->responseContent(), "\n";
	}

	exit 0;
}

my($url) = @ARGV;

Usage("Missing arguments.") unless(@ARGV);

# Example:
# rtcclient oslc/workitems/32271.xml
my %headers = (
);
# Default the format of the response to xml unless .json is in the url.
if($url =~ s/\.json$//) {
	$headers{'Accept'} = 'application/rdf+json';
}
$client->GET($url, \%headers);
if($client->responseCode() != 200) {
	print "responseCode:", $client->responseCode(), "\n";
	print $client->responseContent(), "\n";
	exit 1;
} else {
	formatResponse();
}

exit 0;

__END__
:endofperl
