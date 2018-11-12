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

use strict;
use FindBin;
use lib "$FindBin::Bin/../lib/perl";
use File::Basename;
use Getopt::Long;
use Data::Dumper;
use XML::Simple;
#use LWP::Debug qw(+);
use URI::Escape;
use Lyo::OSLC::CQ::CM;
use constant MSWIN => $^O =~ /MSWin32|Windows_NT/i ? 1 : 0;

sub Usage {
	my(@msg) = @_;
	my $cmd = basename($0);

	my $usage = <<EOF;
Usage: $cmd [-h] 
       $cmd -baseuri <CQ oslc url> -user <user> -password <password> <url>
       $cmd -baseuri <CQ oslc url> -user <user> -password <password> \\
         -db <CQ user db> \\
         -oslcwhere <simple query> \\
         [-property <property> ...]
       $cmd -baseuri <CQ oslc url> -user <user> -password <password> \\
         -cr <change request #> \\
         [-property <property> ...] \\
         [-update <field>=<value> ...]
EOF

	if(@msg) {
		print "\n$cmd: Error: ", @msg, "\n";
		print <<EOF;

$usage
EOF
	} else {
		print <<EOF;
		
This script can be used to interact with ClearQuest(CQ)
using its OSLC REST APIs. It implements behaviors documented at:

https://jazz.net/wiki/bin/view/Main/CqOslcV2

$usage
Options:
  -baseuri <CQ oslc url>   - The base URI of the CQ server
  -user <user>             - CQ user id to authenicate with
  -password <password>     - CQ password
  -db <CQ user db>         - The CQ user database to run queries against
  -recordtype <record type> - The CQ record type to query against.
  -oslcwhere <simple query> - oslc.where query string. Reference:
    https://jazz.net/wiki/bin/view/Main/CqOslcV2#Query_Capabilities
  -property <property>     - The property to retrieve using -oslcwhere
                             or -cr.  More than one property can be
                             retrieved by supplying more than one -property
                             option. Reference:
    https://jazz.net/wiki/bin/view/Main/CqOslcV2#Requesting_Specific_Properties
    http://open-services.net/bin/view/Main/CmSpecificationV2#Resource_ChangeRequest
  -cr <change request #>   - Retrieve a specific change request
  -update <field>=<value>  - Update field <field> with the new value 
                             of <value> in the change request specified with
                             -cr. More than one field can be update
                             by supplying more than one -update option.
                             <field>=<value> has to be contained in one
                             command line argument so remember to quote
                             appropriately.
  -rootservices            - Retrieve the root services document.
  -services <CQ user db>   - Retrieve the services document for <CQ user db>.

Examples:

  cqclient -user me -password pw -baseuri https://cq.mydomain.com/cqweb/oslc -db SAMPL \\
           -recordtype ChangeRequest -oslcwhere 'dc:identifier="SAMPL00000001"' \\
           -prop 'dcterms:creator{*}'
  cqclient -user me -password pw -baseuri https://cq.mydomain.com/cqweb/oslc -db SAMPL \\
           -cr SAMPL00000001 -update 'dcterms:description=99 beers on the wall'
  
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
		'baseuri=s',
		'recordtype=s',
		'oslcwhere=s',
		'oslcsearchTerms=s',
		'oslcorderby=s',
		'update=s@',
		'cr=s',
		'db=s',
		'property=s@',
		'rootservices',
		'services=s',
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

my $client = Lyo::OSLC::CQ::CM->new;
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
	Usage("Missing db argument.") if($opt{'db'} eq '');
	Usage("Missing recordtype argument.") if($opt{'recordtype'} eq '');
	my $data = $client->oslcWhere(
		$opt{'db'}, 
		$opt{'recordtype'}, 
		$opt{'oslcwhere'}, 
		#defined($opt{'property'}) ? $opt{'property'} : undef
		defined($opt{'property'}) ? $opt{'property'} : ['*']
	);
	#print Dumper($data->QueryResultsByIdentifier()), "\n";
	print Dumper($data->QueryResults()), "\n";
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

if(defined($opt{'update'}) and @{$opt{'update'}}) {
	Usage("Missing change request(cr) argument.") if($opt{'cr'} eq '');
	Usage("Missing db argument.") if($opt{'db'} eq '');
	
	my @updates;
	foreach my $new_item (@{$opt{'update'}}) {
		my($var, $val) = split(/=/, $new_item, 2);
		push(@updates, [$var, $val]);
	}
	
	$client->changeRequestUpdate($opt{'db'}, $opt{'cr'}, \@updates, 1);
	exit 0;
}

if($opt{'cr'}) {
	Usage("Missing db argument.") if($opt{'db'} eq '');
	my $data = $client->changeRequest($opt{'db'}, $opt{'cr'}, $opt{'property'});
	if($opt{'format'} eq 'perl') {;
		print Dumper($data->QueryResults()->[0]), "\n";
		#print Dumper($data), "\n";
	} else {
		print $client->responseContent(), "\n";
	}

	exit 0;
}

my($url) = @ARGV;

Usage("Missing arguments.") unless(@ARGV);

# Example:
# cqclient repo/MREPO/db/SAMPL/record/16777237-33554486
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
