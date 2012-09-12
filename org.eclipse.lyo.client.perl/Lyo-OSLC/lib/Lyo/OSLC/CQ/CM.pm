###############################################################################
# Copyright (c) 2012 IBM Corporation.
#
# All rights reserved. This program and the accompanying materials
# are made available under the terms of the Eclipse Public License v1.0
# and Eclipse Distribution License v. 1.0 which accompanies this distribution. 
#
# The Eclipse Public License is available at 
# http://www.eclipse.org/legal/epl-v10.html
# and the Eclipse Distribution License is available at 
# http://www.eclipse.org/org/documents/edl-v10.php.
#
# Contributors:
#
#  Max Vohlken - initial API and implementation
###############################################################################

package Lyo::OSLC::CQ::CM;

=head1 NAME

Lyo::OSLC::CQ::CM - A simple client for interacting with ClearQuest(CQ) CM OSLC resources

=head1 SYNOPSIS

 use Lyo::OSLC::CQ::CM;
 use Lyo::OSLC::RDF;
 
 my $client = Lyo::OSLC::CQ::CM->new(
   'user' => 'myuser', 
   'password' => 'mypassword', 
   'baseuri' => 'https://cq.mydomain.com/cqweb/oslc',
 );
 
 my $client = Lyo::OSLC::CQ::CM->new;
 $client->credentials('myuser', 'mypassword);
 $client->setBaseURI('https://cq.mydomain.com/cqweb/oslc');
 
 $perl = serviceProviders ();

 Lyo::OSLC::CQ::CM extends Lyo::OSLC::CQ and REST::Client so please refer to their documentation for
 more details about how to use this module.
 
=head1 DESCRIPTION

Lyo::OSLC::CQ::CM provides methods for interacting with CQ CM OSLC resources. 

=cut

=head1 METHODS

=cut

use strict;
use warnings;
use 5.008_000;

our ($VERSION) = 1.0100;

use File::Basename;
use URI;
#use LWP::Debug qw(+);
use XML::Simple;
use Data::Dumper;
use Lyo::OSLC::CQ;

use vars qw(@ISA @EXPORT @EXPORT_OK $ScriptName $DontFollowMembers);
@ISA        = qw(Lyo::OSLC::CQ Exporter);
@EXPORT     = qw();
@EXPORT_OK  = qw();
$ScriptName = basename($0);

# Member to disable reference resolution for because they cause
# a recursion loop.
$DontFollowMembers = [
	# Don't follow this related link resource since it will cause 
	# an infinite loop following the references between the two
	# related work items.
#TODO	'rtc_cm:com.ibm.team.workitem.linktype.relatedworkitem.related',
	
	# oslc:discussionAbout contains a reference back to the work item which
	# will trigger an infinite loop.
	'oslc:discussionAbout',
];

=head3 new ( [%$config] )

Construct a new Lyo::OSLC::CQ::CM. This class extends the REST::Client class
with the following additional flags. It takes an optional hash or hash 
reference.

=over 4

=item baseuri

The base URI of the CQ server.

=item user

The user to log onto CQ with.

=item password

The password to log onto CQ with.

=back

=cut

sub new {
    my $class = shift;
	
	my $self = $class->SUPER::new(@_);
	
	$self->{'serviceProviders'} = undef;
	$self->{'services'} = {};
	
	$self;
}

=head3 $hashRef = serviceProviders ( )

Retrieve the serviceProviders XML document from the CQ server. 
This assumes you have already specified the base URI using
the setBaseURI() method or the baseuri property when calling new().
Returns a hash reference created by XML::Simple from the XML returned. 
The hash is cached to avoid querying the server next time.

=cut

sub serviceProviders {
	my $self = shift;
	
	if(defined($self->{'serviceProviders'})) {
		return $self->{'serviceProviders'};
	}
	
	my $rootservices = $self->rootservices();

	# This is the CQ Repository info
	$self->{'serviceProviders'}->{'Repository'} = $rootservices->{'ServiceProviderCatalog'}->[0];
	
	# Now get the User database records from the repository URI.
	my $userDBs = $self->{'serviceProviders'}->{'Repository'}->{'serviceProviderCatalog'}->{'ServiceProviderCatalog'}->[0]->{'rdf:about'};
	if(!defined($userDBs) or $userDBs eq '') {
		die("$ScriptName: serviceProviders: ERROR: failure querying for the CQ user database service provider records\n");
	}
	#print "serviceProviders: $userDBs\n";

	$self->GET($userDBs, {'Accept' => 'application/xml'});
	if($self->responseCode() != 200) {
		die("$ScriptName: serviceProviders: ERROR: failure querying $userDBs: ", $self->responseMessage, "\n");
	}
	
	my $xml = XMLin($self->responseContent(), 'ForceArray' => ['ServiceProviderCatalog', 'ServiceProvider']);
	#print Dumper($xml), "\n";
	
	$self->{'serviceProviders'}->{'Databases'} = $xml->{'ServiceProviderCatalog'};
	
	$self->{'serviceProviders'};
}

=head3 $url = servicesURL ( $userDB )

Returns the services URL from the serviceProviders xml document
for a specific CQ user database. $userDB is the name of
the CQ user database. 
This assumes you have already specified the base URI using
the setBaseURI() method or the baseuri property when calling new().

=cut

sub servicesURL {
	my($self, $userDB) = @_;

	my $userDBs = $self->serviceProviders();

	# Find the URL for the user database record matching $userDB.
	my $servicesURL;
	foreach my $item (@{$userDBs->{'Databases'}}) {
		foreach my $item2 (@{$item->{'serviceProvider'}->{'ServiceProvider'}}) {
			if(basename($item2->{'dc:title'}) eq $userDB) {
				$servicesURL = $item2->{'rdf:about'};
				last;
			}
		}
	}
	if(!defined($servicesURL) or $servicesURL eq '') {
		die("$ScriptName: servicesURL: ERROR: unable to find project area named $userDB\n");
	}
	print("servicesURL: $servicesURL\n") if($self->debugging());
	
	$servicesURL;
}

=head3 $hashRef = services ( $userDB )

Retrieve the services xml document from the CQ server
for a specific CQ user database. $userDB is the title of
the CQ user database. 
This assumes you have already specified the base URI using
the setBaseURI() method or the baseuri property when calling new().
Returns a hash reference created by XML::Simple from the XML returned. 
The hash is cached to avoid querying the server next time.

=cut

sub services {
	my($self, $userDB) = @_;

	if(exists($self->{'services'}->{$userDB})
	&& defined($self->{'services'}->{$userDB})
	) {
		return $self->{'services'}->{$userDB};
	}
	
	my $servicesURL = $self->servicesURL($userDB);

	# Are these headers correct ???
	$self->GET($servicesURL, {'Accept' => 'application/rdf+xml'});
	if($self->responseCode() != 200) {
		die("$ScriptName: services: ERROR: failure querying $servicesURL: ", $self->responseMessage, "\n");
	}
	
	$self->{'services'}->{$userDB} = XMLin($self->responseContent());
}

=head3 $hashRef = queryCapability ( $userDB, $queryCapability )

Retrieve the queryCapability record in $userDB with label
$queryCapability.
$userDB is the title of the CQ user database. 
This assumes you have already specified the base URI using
the setBaseURI() method or the baseuri property when calling new().
Returns a hash reference created by XML::Simple from the XML returned. 
The hash is cached to avoid querying the server next time.

=cut

sub queryCapability {
	my($self, $userDB, $queryCapability) = @_;
	
	my $service = $self->services($userDB);
	
	my $record;
	foreach my $item (@{$service->{'ServiceProvider'}->{'service'}->{'Service'}->{'queryCapability'}}) {
		if($item->{'QueryCapability'}->{'label'} eq $queryCapability) {
			$record = $item->{'QueryCapability'};
			last;
		}
	}
	
	$record;
}

=head3 $url = queryCapabilityURL ( $userDB, $queryCapability )

Returns the queryCapability URL from the services xml document
for a specific CQ user database. $userDB is the name of
the CQ user database. 
This assumes you have already specified the base URI using
the setBaseURI() method or the baseuri property when calling new().

=cut

sub queryCapabilityURL {
	my($self, $userDB, $queryCapability) = @_;
		
	my $queryCapabilityHR = $self->queryCapability($userDB, $queryCapability);
	
	$queryCapabilityHR->{'queryBase'}->{'rdf:resource'};
}


# Custom Lyo::OSLC::RDF object to deal with CQ specific 
{
	package Lyo::OSLC::RDF::CQ;
	our @ISA = qw(Lyo::OSLC::RDF);
	use Lyo::OSLC::RDF;
	
	sub new {
		my $class = shift;
		$class->SUPER::new(@_);
	}
}

=head3 $oslcRdf = oslcWhere ( $userDB, $queryCapability, $query, $propertiesAR )

Executes a query in the CQ user database with the name 
$userDB using the queryCapability service $queryCapability.
This assumes you have already specified the base URI using
the setBaseURI() method or the baseuri property when calling new().
See ???
for the syntax of the $where and $properties arguments. The
response from the server is returned in XML format. If the request
is not successful die() is called with a message about what happened.

=cut

sub oslcWhere {
	my($self, $userDB, $queryCapability, $where, $propertiesAR) = @_;

	my $queryCapabilityURL = $self->queryCapabilityURL($userDB, $queryCapability);
	
	my $uri = URI->new($queryCapabilityURL);

	my %form;
	$form{'oslc.where'} = $where;
	
	my @props = ();
	if(defined($propertiesAR) && @{$propertiesAR}) {
		push(@props, @{$propertiesAR});
	}
	$form{'oslc.select'} = join(',', @props) if(@props);
	
	$uri->query_form(%form);
	print("oslcWhere URL: $uri\n") if($self->debugging());

	my $perl = $self->getAllPages($uri, undef, {'DontFollowMembers' => $DontFollowMembers});
	$self->errorHandler("oslcWhere: ERROR: query failed");

	$perl;
}

=head3 $oslcRdf = changeRequest ( $changeRequestNumber, $propertiesAR )

Retrieve the details about work item $changeRequestNumber.
This assumes you have already specified the base URI using
the setBaseURI() method or the baseuri property when calling new().
See
  http://open-services.net/bin/view/Main/CmSpecificationV2
  http://open-services.net/bin/view/Main/OSLCCoreSpecQuery
for the syntax of the $propertiesAR arguments.
#??TODO
The response from the server is returned in XML format.
If the request is not successful die() is called with a 
message about what happened.

=cut

sub changeRequest {
	my($self, $userDB, $changeRequestNumber, $propertiesAR) = @_;
	
	$self->oslcWhere($userDB, 'ChangeRequest', qq(oslc:shortTitle="$changeRequestNumber"), $propertiesAR);
}

=head3 changeRequestUpdate ( $changeRequestNumber, $newValuesAR, $verbose )

Update work item $changeRequestNumber. The fields to update
are defined in $newValuesAR. 

  $newValuesAR = [
    ['field' => 'new value'],
    ['field,append' => 'append this'],
    ['field,append_if_not_empty' => 'append this only if not empty'],
    ['field->{member}' => 'new value'],
  ];

  # For example:
  $self->changeRequestUpdate('ABCDE0000001234', [
		['dcterms:title' => 'new description'],
		['dcterms:title,append' => ' more details'],
		['dcterms:title,append_if_not_empty' => ' more details'],
		['cq:Project->{rdf:resource}' => 'https://cq.mydomain.com/cqweb/oslc/repo/MASTR/db/ABCDE/record/12345678-12345678'],
	]
  );

die() is called if an error is encountered.

=cut

sub changeRequestUpdate {
	my($self, $userDB, $changeRequestNumber, $newValuesAR, $verbose) = @_;

	# We first have to run a query to get the resource URL of the change request.
	my $data = $self->changeRequest($userDB, $changeRequestNumber);
	
	# If the change request doesn't exist then just return.
	# Should we die here instead?
	my $records = $data->QueryResults();
	if(!@$records) {
		return $self;
	}
	
	my $url = $records->[0]->{'rdf:about'};
	
	my $uri = URI->new($url);
	
	my @updated_fields;
	foreach my $new_valueAR (@$newValuesAR) {
		my($field, $val) = @$new_valueAR;
		my $append = '';
		if($field =~ s/,(append(_if_not_empty)?)$//) {
			$append = $1;
		}
		my $sub;
		($field, $sub) = split(/->/, $field, 2);
		push(@updated_fields, $field);
	}
	
	if(@updated_fields) {
		$uri->query_form('oslc.properties' => join(',', @updated_fields));
	}
	
	print("changeRequestUpdate: $uri\n") if($self->debugging());
	
	$self->GET($uri);
	$self->errorHandler("changeRequestUpdate: ERROR: work item query failed for $changeRequestNumber");
	
	my $response = $self->responseContent();
	print("$response\n") if($self->debugging());
	my $ETag = $self->responseHeader('Etag');
	print("changeRequestUpdate: ETag: $ETag\n") if($self->debugging());

	my $xml = OLSC::RDF::XML::Simple->new();
	my $perl = $xml->XMLin($response, 
		'KeyAttr' => [],
		'ForceArray' => ['rdf:Description', 'rdfs:member'],
	);
	
	my $cr = $perl->{'oslc_cm:ChangeRequest'};
	
	if($verbose) {
		print("\nData structure before updates.\n");
		print("Use this to determine the syntax of the -update argument.\n");
		print("\n", Dumper($cr), "\n");
	}
	
	print("\nchangeRequestUpdate:", Dumper($perl), "\n") if($self->debugging());
	
	foreach my $new_valueAR (@$newValuesAR) {
		my($field, $val) = @$new_valueAR;
		my $append = '';
		if($field =~ s/,(append(_if_not_empty)?)$//) {
			$append = $1;
		}
		my $sub;
		($field, $sub) = split(/->/, $field, 2);
		print("changeRequestUpdate: field: $field, sub: $sub, append: $append, val: $val\n") if($self->debugging());
		Lyo::OSLC::RDF::updateUtil($cr->{$field}, $field, $sub, $val, $append);
	}

	if($verbose) {
		print("\nData structure after updates.\n");
		print("\n", Dumper($cr), "\n");
	}

	my $new = $xml->XMLout($perl, 'RootName' => 'rdf:RDF', 'AttrIndent' => 1 );
	print("changeRequestUpdate: new XML:\n$new\n") if($self->debugging());
	
	$uri->query_form('oslc.properties' => join(',', @updated_fields));
	print("changeRequestUpdate: PUT url: $uri\n") if($self->debugging());
	$self->PUT($uri, $new, 
		{
			'If-Match' => $ETag,
			'Content-Type' => 'application/rdf+xml',
		}
	);
	$self->errorHandler("changeRequestUpdate: ERROR: work item update failed for $changeRequestNumber");
	
	$self;
}

1;

__END__

=head1 AUTHOR

Max Vohlken <maxvohlken@cpan.org>

=head1 COPYRIGHT AND LICENSE

Copyright (c) 2012 IBM Corporation.

All rights reserved. This program and the accompanying materials
are made available under the terms of the Eclipse Public License v1.0
and Eclipse Distribution License v. 1.0 which accompanies this distribution. 

The Eclipse Public License is available at 
http://www.eclipse.org/legal/epl-v10.html
and the Eclipse Distribution License is available at 
http://www.eclipse.org/org/documents/edl-v10.php.

=head1 SEE ALSO

L<REST::Client>, L<Lyo::OSLC>, L<Lyo::OSLC::RDF>, L<Lyo::OSLC::CQ>

=cut
