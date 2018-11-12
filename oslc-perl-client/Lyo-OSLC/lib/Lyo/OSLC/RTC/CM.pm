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

package Lyo::OSLC::RTC::CM;

=head1 NAME

Lyo::OSLC::RTC::CM - A simple client for interacting with Rational 
Team Concert(RTC) CM OSLC resources

=head1 SYNOPSIS

 use Lyo::OSLC::RTC::CM;
 
 my $client = Lyo::OSLC::RTC::CM->new(
   'user' => 'myuser', 
   'password' => 'mypassword', 
   'baseuri' => 'https://rtc.mydomain.com/jazz',
 );
  
 my $client = Lyo::OSLC::RTC::CM->new;
 $client->credentials('myuser', 'mypassword);
 $client->setBaseURI('https://rtc.mydomain.com/jazz');
  
 Lyo::OSLC::RTC::CM extends Lyo::OSLC::RTC and REST::Client so please refer to their documentation for
 more details about how to use this module.
 
=head1 DESCRIPTION

Lyo::OSLC::RTC::CM provides methods for interacting with RTC CM OSLC resources. 

=cut

=head1 METHODS

=cut

use strict;
use warnings;
use 5.008_000;

our ($VERSION) = 1.0102;

use File::Basename;
use URI;
#use LWP::Debug qw(+);
use XML::Simple;
use Data::Dumper;
use Lyo::OSLC::RTC;

use vars qw(@ISA @EXPORT @EXPORT_OK $ScriptName $WorkItemURI $DontFollowMembers);
@ISA        = qw(Lyo::OSLC::RTC Exporter);
@EXPORT     = qw();
@EXPORT_OK  = qw();
$ScriptName = basename($0);

$WorkItemURI = 'resource/itemName/com.ibm.team.workitem.WorkItem';

# Member to disable reference resolution for because they cause
# a recursion loop.
$DontFollowMembers = [
	# Don't follow this related link resource since it will cause 
	# an infinite loop following the references between the two
	# related work items.
	'rtc_cm:com.ibm.team.workitem.linktype.relatedworkitem.related',
	
	# oslc:discussionAbout contains a reference back to the work item which
	# will trigger an infinite loop.
	'oslc:discussionAbout',
];

=head3 new ( [%$config] )

Construct a new Lyo::OSLC::RTC::CM. This class extends the REST::Client class
with the following additional flags. It takes an optional hash or hash 
reference.

=over 4

=item baseuri

The base URI of the RTC server.

=item user

The user to log onto RTC with.

=item password

The password to log onto RTC with.

=back

=cut

sub new {
    my $class = shift;
	
	my $self = $class->SUPER::new(@_);
	
	$self->{'serviceProviders'} = undef;
	$self->{'services'} = {};
	$self->{'enumerations'} = {};
	$self->{'states'} = {};
	$self->{'workflows'} = {};
	$self->{'iterations'} = undef;
	
	$self;
}

# RTC isn't escaping <br>'s in the multiline text
# fields in the response.
# So we need a custom GET method to fix them.
sub GET {
	my $self = shift;
	my $rc = $self->SUPER::GET(@_);
	my $response = $self->response()->content();
	$response =~ s!<br>!\&lt;br\&gt;!g;
	$response =~ s!</br>!\&lt;/br\&gt;!g;
	$response =~ s!<br/>!\&lt;br/\&gt;!g;
	$self->response()->content($response);
	$rc;
}

=head3 $hashRef = serviceProviders ( )

Retrieve the serviceProviders XML document from the RTC server. 
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

	my $projectAreas = $rootservices->{'oslc_cm:cmServiceProviders'}->{'rdf:resource'};
	if(!defined($projectAreas) or $projectAreas eq '') {
		die("$ScriptName: serviceProviders: ERROR: failure querying for oslc_cm:cmServiceProviders\n");
	}
	#print "serviceProviders: $projectAreas\n";

	$self->GET($projectAreas, {'Accept' => 'application/xml'});
	if($self->responseCode() != 200) {
		die("$ScriptName: serviceProviders: ERROR: failure querying $projectAreas: ", $self->responseMessage, "\n");
	}
	print("\nserviceProviders response:\n", $self->responseContent(), "\n") if($self->debugging() > 1);
	
	$self->{'serviceProviders'} = XMLin($self->responseContent(),
		'ForceArray' => ['oslc:serviceProvider'],
	);
	print("\nserviceProviders XML object: ", Dumper($self->{'serviceProviders'})) if($self->debugging() > 1);
	
	$self->{'serviceProviders'};
}

=head3 $url = servicesURL ( $projectArea )

Returns the services URL from the serviceProviders xml document
for a specific project area. $projectArea is the title of
the project area. 
This assumes you have already specified the base URI using
the setBaseURI() method or the baseuri property when calling new().

=cut

sub servicesURL {
	my($self, $projectArea) = @_;

	my $projectAreas = $self->serviceProviders();

	# Find the serviceProvider record matching the $projectArea in the serviceProviders document.
	my $servicesURL;
	foreach my $item (@{$projectAreas->{'oslc:ServiceProviderCatalog'}->{'oslc:serviceProvider'}}) {
		if($item->{'oslc:ServiceProvider'}->{'dcterms:title'}->{'content'} eq $projectArea) {
			$servicesURL = $item->{'oslc:ServiceProvider'}->{'rdf:about'};
			last;
		}
	}
	if(!defined($servicesURL) or $servicesURL eq '') {
		die("$ScriptName: servicesURL: ERROR: unable to find project area named $projectArea\n");
	}
	print("\nservicesURL: $servicesURL\n") if($self->debugging());
	
	$servicesURL;
}

=head3 $hashRef = services ( $projectArea )

Retrieve the services xml document from the RTC server
for a specific project area. $projectArea is the title of
the project area. 
This assumes you have already specified the base URI using
the setBaseURI() method or the baseuri property when calling new().
Returns a hash reference created by XML::Simple from the XML returned. 
The hash is cached to avoid querying the server next time.

=cut

sub services {
	my($self, $projectArea) = @_;

	if(exists($self->{'services'}->{$projectArea})
	&& defined($self->{'services'}->{$projectArea})
	) {
		return $self->{'services'}->{$projectArea};
	}
	
	my $servicesURL = $self->servicesURL($projectArea);

	$self->GET($servicesURL, {'Accept' => 'application/rdf+xml'});
	if($self->responseCode() != 200) {
		die("$ScriptName: services: ERROR: failure querying $servicesURL: ", $self->responseMessage, "\n");
	}
	print("\nservices response:\n", $self->responseContent(), "\n") if($self->debugging() > 1);
	
	$self->{'services'}->{$projectArea} = XMLin($self->responseContent(),
		'ForceArray' => ['oslc:queryCapability'],
	);

	print("\nservices XML object: ", Dumper($self->{'services'}->{$projectArea})) if($self->debugging() > 1);
	
	$self->{'services'}->{$projectArea};
}

sub projectAreaContextID {
	my($self, $projectArea) = @_;
	
	my $service = $self->services($projectArea);
	
	basename($service->{'oslc:ServiceProvider'}->{'oslc:details'}->{'rdf:resource'});
}

=head3 $hashRef = queryCapability ( $projectArea, $queryCapability )

Retrieve the queryCapability record in $projectArea with label
$queryCapability.
$projectArea is the title of the project area. 
This assumes you have already specified the base URI using
the setBaseURI() method or the baseuri property when calling new().
Returns a hash reference created by XML::Simple from the XML returned. 
The hash is cached to avoid querying the server next time.

=cut

sub queryCapability {
	my($self, $projectArea, $queryCapability) = @_;
	
	my $service = $self->services($projectArea);
	
	my $record;
	foreach my $item (@{$service->{'oslc:ServiceProvider'}->{'oslc:service'}}) {
		foreach my $item2 (@{$item->{'oslc:Service'}->{'oslc:queryCapability'}}) {
			if($item2->{'oslc:QueryCapability'}->{'dcterms:title'}->{'content'} eq $queryCapability) {
				$record = $item2->{'oslc:QueryCapability'};
				last;
			}
		}
	}
	
	$record;
}

=head3 $url = queryCapabilityURL ( $projectArea, $queryCapability )

Returns the queryCapability URL from the services xml document
for a specific project area. $projectArea is the title of
the project area. 
This assumes you have already specified the base URI using
the setBaseURI() method or the baseuri property when calling new().

=cut

sub queryCapabilityURL {
	my($self, $projectArea, $queryCapability) = @_;
		
	# Since RTC only has one queryCapability at the moment
	# lets default an empty queryCapability to 'Change request queries'.
	$queryCapability = 'Change request queries' if(!defined($queryCapability) or $queryCapability eq '');
	
	my $queryCapabilityHR = $self->queryCapability($projectArea, $queryCapability);
	
	my $url = $queryCapabilityHR->{'oslc:queryBase'}->{'rdf:resource'};
	print("\nqueryCapabilityURL: $url\n") if($self->debugging());
	$url;
}

=head3 $oslcRdf = oslcWhere ( $projectArea, $queryCapability, $query, $propertiesAR )

Executes a query in the RTC project area with the name 
$projectArea using the queryCapability service $queryCapability.
This assumes you have already specified the base URI using
the setBaseURI() method or the baseuri property when calling new().
See
  http://open-services.net/bin/view/Main/CmSpecificationV2
  http://open-services.net/bin/view/Main/OSLCCoreSpecQuery
for the syntax of the $where and $propertiesAR arguments.
Returns an Lyo::OSLC::RDF object.
If the request is not successful die() is called with a 
message about what happened.

=cut

sub oslcWhere {
	my($self, $projectArea, $queryCapability, $where, $propertiesAR) = @_;

	my $queryCapabilityURL = $self->queryCapabilityURL($projectArea, $queryCapability);
	
	my $uri = URI->new($queryCapabilityURL);
	
	my %form;
	$form{'oslc.where'} = $where;
	
	# Always select the identifier.
	my @props = ('dcterms:identifier');
	if(defined($propertiesAR) && @{$propertiesAR}) {
		push(@props, @{$propertiesAR});
	}
	$form{'oslc.select'} = join(',', @props);
	
	$uri->query_form(%form);
	print("\noslcWhere URL: $uri\n") if($self->debugging());

	my $data = $self->getAllPages($uri, undef, {'DontFollowMembers' => $DontFollowMembers});
	$self->errorHandler("oslcWhere: ERROR: query failed");

	$data;
}

=head3 $oslcRdf = workitem ( $workItemNumber, $propertiesAR )

Retrieve the details about work item $workItemNumber.
This assumes you have already specified the base URI using
the setBaseURI() method or the baseuri property when calling new().
See
  http://open-services.net/bin/view/Main/CmSpecificationV2
  http://open-services.net/bin/view/Main/OSLCCoreSpecQuery
for the syntax of the $propertiesAR arguments.
Returns an Lyo::OSLC::RDF object.
If the request is not successful die() is called with a 
message about what happened.

=cut

sub workitem {
	my($self, $workItemNumber, $propertiesAR) = @_;
	
	# Using a hardcoded URI here because this URL doesn't exist in the
	# discovery mechanism.
	my $uri = URI->new("$WorkItemURI/$workItemNumber");
	
	if(defined($propertiesAR) and @{$propertiesAR}) {
		$uri->query_form('oslc.properties' => join(',', @{$propertiesAR}));
	}
	
	print("\nworkitem: $uri\n") if($self->debugging());
	$self->GET($uri);
	$self->errorHandler("workitem: ERROR: work item query failed for $workItemNumber");
	
	my $data = Lyo::OSLC::RDF->new($self->responseContent(), 'URL' => $uri, 'DontFollowMembers' => $DontFollowMembers);
	
	# Move the description with type http://open-services.net/ns/cm#ChangeRequest to the results record.
	my $desc = $data->Descriptions();
	foreach my $key (keys(%{$desc})) {
		if(Lyo::OSLC::RDF::IsA($desc->{$key}, 'http://open-services.net/ns/cm#ChangeRequest')) {
			$data->QueryResults([$desc->{$key}]);
			delete($desc->{$key});
			last;
		}
	}
	
	print("\nworkitem: ", Dumper($data)) if($self->debugging());

	$data;
}

=head3 workitemUpdate ( $workItemNumber, $newValuesAR, $verbose )

Update work item $workItemNumber. The fields to update
are defined in $newValuesAR. 
  
  $newValuesAR = [
    ['field' => 'new value'],
    ['field,append' => 'append this'],
    ['field,append_if_not_empty' => 'append this only if not empty'],
    ['field->{member}' => 'new value'],
  ];

  # For example:
  $self->workitemUpdate('1234', [
		['dcterms:title' => 'new description'],
		['dcterms:title,append' => ' more details'],
		['dcterms:title,append_if_not_empty' => ' more details'],
		['rtc_cm:resolution->{rdf:resource}' => 'https://myrtc.com/resolutions/fixed'],
		['rtc_cm:resolution->{rdf:resource}' => $self->resolution('Fixed')],
	]
  );

die() is called if an error is encountered.

=cut

sub workitemUpdate {
	my($self, $workItemNumber, $newValuesAR, $verbose) = @_;

	# Using a hardcoded URI here because this URL doesn't exist in the
	# discovery mechanism.
	my $uri = URI->new("$WorkItemURI/$workItemNumber");
	
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
	
	print("\nworkitemUpdate: $uri\n") if($self->debugging());
	
	$self->GET($uri);
	$self->errorHandler("workitemUpdate: ERROR: work item query failed for $workItemNumber");
	
	my $response = $self->responseContent();
	print("\n$response\n") if($self->debugging());
	my $ETag = $self->responseHeader('Etag');
	print("\nworkitemUpdate: ETag: $ETag\n") if($self->debugging());

	my $xml = OLSC::RDF::XML::Simple->new();
	my $perl = $xml->XMLin($response, 
		'KeyAttr' => [],
		'ForceArray' => ['rdf:Description', 'rdfs:member'],
	);
	
	# Find the change request record and make sure the content key exists
	# for string values.
	my $cr;
	foreach my $record (@{$perl->{'rdf:Description'}}) {
		Lyo::OSLC::RDF::_makeSureContentKeyExists($record);
		
		if( exists($record->{'rdf:type'})
		and exists($record->{'rdf:type'}->{'rdf:resource'})
		and $record->{'rdf:type'}->{'rdf:resource'} eq 'http://open-services.net/ns/cm#ChangeRequest'
		) {
			$cr = $record;
		}
	}
	
	if($verbose) {
		print("\nData structure before updates.\n");
		print("Use this to determine the syntax of the -update argument.\n");
		print("\n", Dumper($perl));
	}
	
	print("\nworkitemUpdate:", Dumper($perl)) if($self->debugging());
	
	foreach my $new_valueAR (@$newValuesAR) {
		my($field, $val) = @$new_valueAR;
		my $append = '';
		if($field =~ s/,(append(_if_not_empty)?)$//) {
			$append = $1;
		}
		my $sub;
		($field, $sub) = split(/->/, $field, 2);
		print("\nworkitemUpdate: field: $field, sub: $sub, append: $append, val: $val\n") if($self->debugging());
		Lyo::OSLC::RDF::updateUtil($cr->{$field}, $field, $sub, $val, $append);
	}

	if($verbose) {
		print("\nData structure after updates.\n");
		print("\n", Dumper($perl));
	}

	my $new = $xml->XMLout($perl, 'RootName' => 'rdf:RDF', 'AttrIndent' => 1 );
	print("\nworkitemUpdate: new XML:\n$new\n") if($self->debugging());
	
	$uri->query_form('oslc.properties' => join(',', @updated_fields));
	print("\nworkitemUpdate: PUT url: $uri\n") if($self->debugging());
	$self->PUT($uri, $new, 
		{
			'If-Match' => $ETag,
			'Content-Type' => 'application/rdf+xml',
		}
	);
	$self->errorHandler("workitemUpdate: ERROR: work item update failed for $workItemNumber");
	
	$self;
}

=head3 $oslcRdf = enumeration ( $projectArea, $enum )

Retrieves the RDF document for the enumeration specified in $enum in the
project area with the title $projectArea.
Returns an Lyo::OSLC::RDF object. 
The Lyo::OSLC::RDF object is cached to avoid querying the server next time.

=cut

sub enumeration {
	my($self, $projectArea, $enum) = @_;
	
	if(exists($self->{'enumerations'}->{$projectArea})
	&& defined($self->{'enumerations'}->{$projectArea}->{$enum})
	) {
		return $self->{'enumerations'}->{$projectArea}->{$enum};
	}

	# Calculate the enumerations URL from the queryCapability URL.
	
	my $url = $self->queryCapabilityURL($projectArea, 'Change request queries');
	
	# queryCapabilityURL: https://rtc.com:9443/jazz/oslc/contexts/<ID>/workitems
	# Enumeration URL we want: https://rtc.com:9443/jazz/oslc/enumerations/<ID>/$enum
	
	$url =~ s,/contexts/,/enumerations/,;
	$url =~ s,/workitems$,/$enum,;
	print("\nenumerationURL: $url\n") if($self->debugging());
	
	my $data = $self->getAllPages($url, undef, {'DontFollowMembers' => $DontFollowMembers});
	$self->errorHandler("enumeration: ERROR: Lookup of enum $enum failed");

	$self->{'enumerations'}->{$projectArea}->{$enum} = $data;
}

=head3 $hashRef = enumerationItem ( $projectArea, $enum, $name )

Retrieve the RDF data for the enumeration item with the name
$name in the enumeration specified in $enum in the
project area with the title $projectArea.
Returns a hash reference.

=cut

sub enumerationItem {
	my($self, $projectArea, $enum, $name) = @_;
	
	my $data = $self->enumeration($projectArea, $enum);
	
	$data->QueryResultsByTitle()->{$name};
}

=head3 $oslcRdf = workflow ( $projectArea, $type, $name )

Retrieves the XML document for the workflow type $type with name $name in the
project area with the title $projectArea.
Returns an Lyo::OSLC::RDF object. 
The Lyo::OSLC::RDF object is cached to avoid querying the server next time.

=cut

sub workflow {
	my($self, $projectArea, $type, $name) = @_;
	
	if(exists($self->{'workflows'}->{$projectArea})
	&& exists($self->{'workflows'}->{$projectArea}->{$type})
	&& defined($self->{'workflows'}->{$projectArea}->{$type}->{$name})
	) {
		return $self->{'workflows'}->{$projectArea}->{$type}->{$name};
	}

	# Calculate the workflow URL from the queryCapability URL.
	
	my $url = $self->queryCapabilityURL($projectArea, 'Change request queries');
	
	# queryCapabilityURL: https://rtc.com:9443/jazz/oslc/contexts/<ID>/workitems
	# workflow URL we want: https://rtc.com:9443/jazz/oslc/workflows/<ID>/$type/$name
	
	$url =~ s,/contexts/,/workflows/,;
	$url =~ s,/workitems$,/$type/$name,;
	
	my $data = $self->getAllPages($url, undef, {'DontFollowMembers' => $DontFollowMembers});
	$self->errorHandler("workflow: ERROR: Lookup of workflow type $type with name $name failed");
	 
	$self->{'workflows'}->{$projectArea}->{$type}->{$name} = $data;
}

=head3 $oslcRdf = states ( $projectArea, $stateType )

Retrieves the RDF information for the state type specified in $stateType in the
project area with the title $projectArea.
Returns an Lyo::OSLC::RDF object. 
The Lyo::OSLC::RDF object is cached to avoid querying the server next time.

=cut

sub states {
	my($self, $projectArea, $stateType) = @_;
	$self->workflow($projectArea, 'states', $stateType);
}

=head3 $hashRef = state ( $projectArea, $enum, $name )

Retrieve the RDF data for the state with the name
$name in the state type specified in $stateType in the
project area with the title $projectArea.
Returns a hash reference.

=cut

sub state {
	my($self, $projectArea, $stateType, $name) = @_;
	
	my $data = $self->states($projectArea, $stateType);
	
	$data->QueryResultsByTitle()->{$name};
}

=head3 $oslcRdf = resolutions ( $projectArea, $resolutionType )

Retrieves the XML document for the resolution type specified in $resolutionType in the
project area with the title $resolutionType.
Returns an Lyo::OSLC::RDF object. 
The Lyo::OSLC::RDF object is cached to avoid querying the server next time.

=cut

sub resolutions {
	my($self, $projectArea, $resolutionType) = @_;
	$self->workflow($projectArea, 'resolutions', $resolutionType);
}

=head3 $hashRef = resolution ( $projectArea, $resolutionType, $name )

Retrieve the RDF data for the resolution with the name
$name in the resolution type specified in $resolutionType in the
project area with the title $projectArea.
Returns a hash reference.

=cut

sub resolution {
	my($self, $projectArea, $resolutionType, $name) = @_;
	
	my $data = $self->resolutions($projectArea, $resolutionType);
	
	$data->QueryResultsByTitle()->{$name};
}

=head3 $oslcRdf = iterations ( )

Retrieves all of the iterations defined on the server.
Returns an Lyo::OSLC::RDF object. 
The Lyo::OSLC::RDF object is cached to avoid querying the server next time.

=cut

sub iterations {
	my($self) = @_;
	
	if(exists($self->{'iterations'})
	&& defined($self->{'iterations'})
	) {
		return $self->{'iterations'};
	}

	my $uri = URI->new('oslc/iterations');
	
	# Didn't return all of the fields for each iteration as expected.
	#$uri->query_form('oslc.select' => '*');
	
	# So lets get all of the fields explicitly. This list is as of 5/2012.
	# I found the query took longer as you requested more properties.
	# dcterms:identifier: 10s
	# first 7: 17s
	# all fields: 22s
	# So a useful feature might be to add the ability to specify what
	# properties you want to query. But if we do this then we should
	# probably not cache the result. The caching is only interesting if
	# we cache all of the info.
	$uri->query_form('oslc.select' => join(',',
			'dcterms:identifier',
 			'dcterms:description',
 			'dcterms:title',
			'rtc_cm:startDate',
			'rtc_cm:endDate',
			'rtc_cm:hasDeliverable',
			'rtc_cm:archived',
			'rtc_cm:parent',
			'rtc_cm:timeline',
			'rtc_cm:projectArea',
		),
		# default seems to be 50.
		# 100 worked
		# 120 didn't
		# 150 didn't
		'oslc.pageSize' => 100,
	);
	
	my $data = $self->getAllPages($uri, undef, {'DontFollowMembers' => $DontFollowMembers});
	$self->errorHandler('iterations: ERROR: Lookup of iterations failed');
	
	print("\niterations:\n", Dumper($data)) if($self->debugging());
	
	$self->{'iterations'} = $data;
}

=head3 $hashRef = iteration ( $title )

Retrieves the definition of the iteration with dcterms:title of $title.
Returns a hash reference.
The definition is cached to avoid querying the server next time.

=cut

sub iteration {
	my($self, $title) = @_;
	
	if(exists($self->{'iteration'}->{$title})
	&& defined($self->{'iteration'}->{$title})
	) {
		return $self->{'iteration'}->{$title};
	}

	my $uri = URI->new('oslc/iterations');
	
	# Didn't return all of the fields for each iteration as expected.
	#$uri->query_form('oslc.properties' => '*');
	
	# So lets get all of the fields explicitly. This list is as of 5/2012.
	$uri->query_form(
		'oslc.select' => join(',',
			'rtc_cm:timeline',
			'rtc_cm:projectArea',
			'rtc_cm:archived',
			'rtc_cm:startDate',
			'rtc_cm:endDate',
			'rtc_cm:hasDeliverable',
			'dcterms:title',
			'dcterms:identifier',
			'rtc_cm:parent{*}',
			'dcterms:description',
		),
		'oslc.where' => qq(dcterms:title="$title"),
	);
	
	my $data = $self->getAllPages($uri, undef, {'DontFollowMembers' => $DontFollowMembers});
	$self->errorHandler("iterations: ERROR: Lookup of iterations failed");
	
	$self->{'iteration'}->{$title} = $data->QueryResults()->[0];
}

# Above is better if you just need to query one iteration.
# If you are going to lookup multiple then the caching
# of all of the iterations in the above routine might
# be faster for many lookups.
# But I tested 5 lookups with 598 iterations:
#   iteration  took  6.32s
#   iteration2 took 11.70s
# iteration was faster. 

sub iteration2 {
	my($self, $title) = @_;
	
	if(exists($self->{'iteration'}->{$title})) {
		return $self->{'iteration'}->{$title};
	}
	
	$self->{'iteration'}->{$title} = undef;
	
	my $iterations = $self->iterations();
	
	foreach my $itemHR (@{$iterations->QueryResults()}) {
		if(exists($itemHR->{'dcterms:title'})
		and $itemHR->{'dcterms:title'}->{'content'} eq $title
		) {
			$self->{'iteration'}->{$title} = $itemHR;
			last;
		}
	}
	
	$self->{'iteration'}->{$title};
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

L<REST::Client>, L<Lyo::OSLC>, L<Lyo::OSLC::RDF>, L<Lyo::OSLC::RTC>

=cut
