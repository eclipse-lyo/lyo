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

package Lyo::OSLC::RDF;

=head1 NAME

Lyo::OSLC::RDF - Class for converting RDF XML responses into a Perl object.

=head1 SYNOPSIS

 use Lyo::OSLC::RDF;
 
 my $result = Lyo::OSLC::RDF->new($oslc->responseContent());
 my $result = Lyo::OSLC::RDF->new($oslc->responseContent(), 
  'DontFollowMembers' => 'oslc:discussionAbout'
 );
 
 # The OSLC methods that query the service provided return an object of this type.
 my $oslcRdf = $oslc->getAllPages($url);
 
=head1 DESCRIPTION

Lyo::OSLC::RDF is a class for converting an OSLC response formatted
in MIME type application/rdf+xml to a Perl object.

=cut

=head1 METHODS

=cut

use strict;
use warnings;
use 5.008_000;

our ($VERSION) = 1.0100;

use File::Basename;
use XML::Simple;
use Data::Dumper;

use vars qw(@ISA @EXPORT @EXPORT_OK $ScriptName $Debugging);
@ISA        = qw(Exporter);
@EXPORT     = qw();
@EXPORT_OK  = qw(ISA);
$ScriptName = basename($0);
$Debugging  = 0;

=head3 new ( $rdfxml, [%config] )

Construct a new Lyo::OSLC::RDF instance.

=over 4

=item $rdfxml

Optional RDF XML document to convert.

=item ResolveReferences => 0|1

Turn off|on the mechanism that resolves references in the result
records based on the RDF descriptions in the XML. On by default.

=item DontFollowMembers => []

When resolving references in the results it is possible to enter
a recusive loop when certain fields in the record are followed
back onto themselves. This setting is a list of fields that are
known to cause an infinite loop and should not be followed after
being resolved.

=item MaxResolveReferencesDepth => 6

When recursively resolving references in the results this is the maximum
depth to which references will be resolved. This is used to
avoid an infinite loop during reference resolution.

=back

=cut

sub new {
    my $class = shift;
	my $rdfxml = shift;
	
	# See Init for the other members.
	my $self = {
		'Config' => {
			'Debugging' => $Debugging,
			'ResolveReferences' => 1,
			'MaxResolveReferencesDepth' => 6, # Guard against infinite inline loops
			@_,
		},
	};
	
	# Convert the DontFollowMembers array into a hash for quick lookups.
	$self->{'DontFollowMembers'} = {};
	if(exists($self->{'Config'}->{'DontFollowMembers'})) {
		map {$self->{'DontFollowMembers'}->{$_}++;} @{$self->{'Config'}->{'DontFollowMembers'}};
	}
	
	bless($self, $class);
	
	$self->Init($rdfxml);
	
	$self;
}

=head3 Debugging ( $level )

Method to set or get the debugging level.

=cut

sub Debugging {
	my $self = shift;
	$self->{'Config'}->{'Debugging'} = $_[0] if(@_);
	$self->{'Config'}->{'Debugging'};
}

=head3 Init ( $rdfxml )

Method to initialize the object from the contents
of an RDF XML document stored in $rdfxml.
The RDF XML is separated into a hash of the rdf:Description
records keyed on the rdf:about value. This hash is
retrieved using the Descriptions() method.
The query result are separated into an array that
is retrieved using the QueryResults() method.
If the 'ResolveReferences' setting is true then all rdf:resource
references in the result records will be replaced
by their corresponding record in the Descriptions
hash. The ResponseInfo record in the RDF XML is separated
out and can be retrieved using the ResponseInfo() method.

=cut

sub Init {
	my($self, $rdfxml) = @_;
	
	# Initialize these to be valid references.
	$self->{'xmlns'} = {};
	$self->{'Results'} = [];
	$self->{'ResponseInfo'} = {};
	$self->{'Descriptions'} = {};
	$self->{'byTitle'} = -1;
	$self->{'byIdentifier'} = -1;
	$self->{'referenceResolutionNeeded'} = 1;
	
	if(!defined($rdfxml) or $rdfxml eq '') {
		return $self;
	}
	
	print("Init:\n", $rdfxml, "\n") if($self->Debugging());
	
	# Not all rdf:Description items contain rdf:about
	# so we can't use XMLin's KeyAttr of rdf:about to convert the
	# array of rdf:Description's into a hash keyed on rdf:about.
	my $xml = OLSC::RDF::XML::Simple->new();
	my $perl = $xml->XMLin($rdfxml, 
		'KeyAttr' => [],
		'ForceArray' => ['rdf:Description', 'rdfs:member'],
	);
	
	print("Init 1 ", Dumper($perl), "\n") if($self->Debugging());
	
	# Save the namespace definitions.
	foreach my $key (keys(%{$perl})) {
		if($key =~ /^xmlns:/) {
			$self->{'xmlns'}->{$key} = $perl->{$key};
		}
	}	
	
	my $results;
	
	# Loop thru the array of rdf:Description items and move them
	# into a hash for quick lookup later.
	foreach my $record (@{$perl->{'rdf:Description'}}) {
		my $processed = 0;
		
		# Assuming there is only one record with rdfs:member as a member.
		# In some cases the rdfs:member item are a member of the ResponseInfo
		# record. So in this case rdfs:member gets assigned to Results and
		# the ResponseInfo record that contained rdfs:member get assigned to
		# ResponseInfo.
		if(exists($record->{'rdfs:member'})) {
			$results = $record->{'rdfs:member'};
			$processed = 1;
		}
		
		# Find the ResponseInfo record.
		# In RTC the ResponseInfo record is one of the rdf:Description's.
		if(IsA($record, 'http://open-services.net/ns/core#ResponseInfo')) {
			$self->{'ResponseInfo'} = $record;
			$processed = 1;
		}
		
		if(exists($record->{'rdf:nodeID'})) {
			push(@{$self->{'Links'}}, $record);
			$processed = 1;
		}
		
		# Only add a description to the Descriptions hash if it wasn't
		# already assigned to one of the other lists above.
		next if($processed);
		
		if(exists($record->{'rdf:about'})) {
			_makeSureContentKeyExists($record);
			$self->{'Descriptions'}->{$record->{'rdf:about'}} = $record;
		}		
	}

	# In CQ the ResponseInfo record is a top level record
	if(exists($perl->{'oslc:ResponseInfo'})) {
		$self->{'ResponseInfo'} = $perl->{'oslc:ResponseInfo'};
	}
		
	print("Init 2 ", Dumper($self), "\n") if($self->Debugging());
	
	$self->QueryResults($results);

	print("Init 3 ", Dumper($self), "\n") if($self->Debugging());
	
	$self;
}

# Utility function to deal with the issue 
# that XML::Simple doesn't create the
# content key if the value of the field is an empty value.
# For example when 'rtc_ext:PatchBundle' is empty then the content key won't exist.
# <rdf:Description rdf:about="https://jazzop23.rtp.raleigh.ibm.com:9443/jazz/resource/itemName/com.ibm.team.workitem.WorkItem/35225?oslc.properties=rtc_ext%3APatchBundle">
#   <rdf:type rdf:resource="http://open-services.net/ns/cm#ChangeRequest"/>
#   <rtc_ext:PatchBundle rdf:datatype="http://www.w3.org/2001/XMLSchema#string"></rtc_ext:PatchBundle>
# </rdf:Description>
# {
#   'rtc_ext:PatchBundle' => {
#     'rdf:datatype' => 'http://www.w3.org/2001/XMLSchema#string'
#   },
#   'rdf:about' => 'https://jazzop23.rtp.raleigh.ibm.com:9443/jazz/resource/itemName/com.ibm.team.workitem.WorkItem/35225?oslc.properties=rtc_ext%3APatchBundle',
#   'rdf:type' => {
#     'rdf:resource' => 'http://open-services.net/ns/cm#ChangeRequest'
#   }
# }
# which causes an update to fail with Lyo::OSLC::RDF::updateUtil reporting,
# "Missing subscript for hash item, rtc_ext:PatchBundle"
# because the 'content' key is missing.
# So we need to inject the content key into the hash if it doesn't exist.
# So at the moment this seems to make sense to do for items with 
# 'rdf:datatype' == 'http://www.w3.org/2001/XMLSchema#string'.
# 'rdf:parseType' == 'Literal'
sub _makeSureContentKeyExists {
	my($record) = @_;
	
	foreach my $key (keys(%{$record})) {
		if( 
		    ref($record->{$key}) eq 'HASH'
		  and
		    (
		      (
		        exists($record->{$key}->{'rdf:datatype'})
		      and 
				$record->{$key}->{'rdf:datatype'} eq 'http://www.w3.org/2001/XMLSchema#string'
		      )
		    or
		      (
		        exists($record->{$key}->{'rdf:parseType'})
		      and 
				$record->{$key}->{'rdf:parseType'} eq 'Literal'
		      )
		    )
		  and 
		    !exists($record->{$key}->{'content'})
		) {
			$record->{$key}->{'content'} = '';
		}
	}
}

BEGIN {
	package OLSC::RDF::XML::Simple;

	use XML::Simple;
	our @ISA = qw(XML::Simple);

	##############################################################################
	# Method: escape_value()
	#
	# Helper routine for automatically escaping values for XMLout().
	# Expects a scalar data value.  Returns escaped version.
	#
	# Extends the original to escape newlines and carriage returns in the
	# Perl values.
	#

	sub escape_value {
		my($self, $data) = @_;

		return '' unless(defined($data));

		$data = $self->SUPER::escape_value($data);

		$data =~ s/\r/&#x0D;/sg;
		$data =~ s/\n/&#x0A;\n/sg;

		$data;
	}
}

=head3 QueryResults ( [$resultAR] )

Method to get or set the query results contained in the
object. An array references is returned. The query results
can be set by passing an array ref as the first argument.

=cut

sub QueryResults {
	my $self = shift;
	if(@_ and defined($_[0])) {
		
		$self->{'Results'} = $_[0];
		
		# Since the results have changed we must
		# invalidate the byTitle and IndexByIdentifier hashes
		# so that they will get recalculated when they are
		# accessed again.
		$self->{'byTitle'} = -1;
		$self->{'byIdentifier'} = -1;
		$self->{'referenceResolutionNeeded'} = 1;	

		# The results for CQ are a list of hashes with one oslc_cm:ChangeRequest member
		# whose value is a hash reference to the actual record contents.
		# This code removes oslc_cm:ChangeRequest hash.
		# 'Results' => [
		#   {
		#     'oslc_cm:ChangeRequest' => {
		#       'dcterms:identifier' => '33554486',
		#       'oslc_cm:status' => 'Closed',
		#     },
		#   },
		#   {
		#     'oslc_cm:ChangeRequest' => {
		#       'dcterms:identifier' => '33554487',
		#       'oslc_cm:status' => 'Closed',
		#     },
		#   }
		# ];
		foreach my $record (@{$self->{'Results'}}) {
			if( keys(%$record) == 1
			and exists($record->{'oslc_cm:ChangeRequest'})
			) {
				$record = $record->{'oslc_cm:ChangeRequest'};
			}
		}
	
		# Inline the references into the query result records.
		if($self->{'Config'}->{'ResolveReferences'}) {
			$self->ResolveReferences();
		}
		
	}
	$self->{'Results'};
}

=head3 QueryResultsByTitle ( )

Method to get the query results as a hash reference keyed on the
dcterms:title field of each record.  If any of the records in the result
is missing the dcterms:title field then undef is returned instead of a
reference to a hash.

=cut

sub QueryResultsByTitle {
	my $self = shift;
	if($self->{'byTitle'} == -1) {
		$self->UpdateByTitle();
	}
	$self->{'byTitle'};
}

=head3 QueryResultsByIdentifier ( )

Method to get the query results as a hash reference keyed on the
dcterms:identifier field of each record.  If any of the records in the
result is missing the dcterms:identifier field then undef is returned
instead of a reference to a hash.

=cut

sub QueryResultsByIdentifier {
	my $self = shift;
	if($self->{'byIdentifier'} == -1) {
		$self->UpdateByIdentifier();
	}
	$self->{'byIdentifier'};
}

=head3 Descriptions ( )

Method to get the hash of descriptions stored in the object.
The descriptions correspond to the rdf:Description items in the
RDF keyed on the rdf:about value.

=cut

sub Descriptions {
	my $self = shift;
	$self->{'Descriptions'};
}

=head3 QueryResponse ( )

Method to get the ResponseInfo record from the last RDF XML
that was added to the object.

=cut

sub QueryResponse {
	my $self = shift;
	$self->{'ResponseInfo'};
}

=head3 Add ( $rdfxml)

Method to Add the contents of an RDF XML document to the object.
This is useful for page based queries where the full result of the
query is spread across multiple requests to the server.

=cut

sub Add {
	my $self = shift;
	my $rdfxml = shift;
	
	# Disabling reference resolution. We'll do that later.
	my $add = ref($self)->new($rdfxml, %{$self->{'Config'}}, 'ResolveReferences' => 0);
	
	print("Add: ", Dumper($add), "\n") if($self->Debugging());

	# Add the result to our results.
	push(@{$self->{'Results'}}, @{$add->QueryResults()});
	
	# Add the xmlns items
	while(my($key, $val) = each(%{$add->{'xmlns'}})) {
		$self->{'xmlns'}->{$key} = $val;
	}
	
	# Since the results have changed we must
	# invalidate the byTitle and byIdentifier hashes
	# so that they will get recalculated when they are
	# accessed again.
	$self->{'byTitle'} = -1;
	$self->{'byIdentifier'} = -1;
	$self->{'referenceResolutionNeeded'} = 1;
	
	# Replace the response with the new response.
	$self->{'ResponseInfo'} = $add->QueryResponse();

	# Add the new descriptions.
	while(my($key, $val) = each(%{$add->Descriptions()})) {
		$self->{'Descriptions'}->{$key} = $val;
	}
	
	# Inline the references into the query result records
	# using the new list of descriptions.
	if($self->{'Config'}->{'ResolveReferences'}) {
		$self->ResolveReferences();
	}
	
	$self;
}

=head3 UpdateByTitle ( )

Utility method to make sure the query result by title hash is
in sync with the query result records. The title hash is only
updated if every record in the result contain a dcterms:title
value.

=cut

sub UpdateByTitle {
	my $self = shift;

	$self->ResolveReferences();
	
	$self->{'byTitle'} = {};
	
	# Create a hash keyed on {dcterms:title} or {dcterms:title}->{content}.
	my $ok = 1;
	foreach my $record (@{$self->{'Results'}}) {
		# dcterms:title hash element must exist
		# If dcterms:title is a hash ref then it must contain a content element
		# else we assume since dcterms:title exists it must have a valid value.
		if(
		    !exists($record->{'dcterms:title'})
		or  (ref($record->{'dcterms:title'}) eq 'HASH' and !exists($record->{'dcterms:title'}->{'content'}))
		) {
			$ok = 0;
			$self->{'byTitle'} = undef;
			last;
		}
	}
	if($ok) {
		foreach my $record (@{$self->{'Results'}}) {
			# The title can be stored in dcterms:title or dcterms:title->content.
			my $title = $record->{'dcterms:title'};
			$title = $record->{'dcterms:title'}->{'content'} if(ref($record->{'dcterms:title'}) eq 'HASH');
			$self->{'byTitle'}->{$title} = $record;
		}
	}
	
	$self;
}

=head3 UpdateByIdentifier ( )

Utility method to make sure the query result by identifier hash is
in sync with the query result records.  The identifier hash is only
updated if every record in the result contain a dcterms:identifier
value.

=cut

sub UpdateByIdentifier {
	my $self = shift;
	
	$self->ResolveReferences();

	$self->{'byIdentifier'} = {};
	
	# Create a hash keyed on {dcterms:identifier} or {dcterms:identifier}->{content}.
	my $ok = 1;
	foreach my $record (@{$self->{'Results'}}) {
		# dcterms:identifier hash element must exist
		# If dcterms:identifier is a hash ref then it must contain a content element
		# else we assume since dcterms:identifier exists it must have a valid value.
		if(
		    !exists($record->{'dcterms:identifier'})
		or  (ref($record->{'dcterms:identifier'}) eq 'HASH' and !exists($record->{'dcterms:identifier'}->{'content'}))
		) {
			$ok = 0;
			$self->{'byIdentifier'} = undef;
			last;
		}
	}
	if($ok) {
		foreach my $record (@{$self->{'Results'}}) {
			# The identifier can be stored in {dcterms:identifier} or {dcterms:identifier}->{content}.
			my $id = $record->{'dcterms:identifier'};
			$id = $record->{'dcterms:identifier'}->{'content'} if(ref($record->{'dcterms:identifier'}) eq 'HASH');
			$self->{'byIdentifier'}->{$id} = $record;
		}
	}
	
	$self;
}

=head3 ResolveReferences ( $force )

Utility method to resolve the references in the query results based on the
other description records in the object. The rdf:resource member is removed and
replaced with the members from the referenced record. Reference
resolution is controlled by the 'ResolveReferences' configuration setting.
If 'ResolveReferences' is false then this method does nothing.
$force if true can be used to force the references to be resolved even if the 
'ResolveReferences' configuration setting is false.

=cut

sub ResolveReferences {
	my $self = shift;
	my $force = shift;
	
	if($self->{'referenceResolutionNeeded'} or $force) {
		$self->InlineRDFReferences();
		$self->{'referenceResolutionNeeded'} = 0;
	}
	
	$self;
}

=head3 InlineRDFReferences ( )

Utility method to resolve the references in the query results based on the
other description records in the object. The rdf:resource member is removed and
replaced with the members from the referenced record.

=cut

# To debug infinite loop recursion issues enable debugging and trace
# back the "InlineRDFReferences: depth" value to find when the
# reference lookup starts to repeat.
sub InlineRDFReferences {
	my $self = shift;
	
	# These args are normally used for the recursive call to ourself.
	my $node = shift;
	my $lookupHR = shift;
	my $inlineDepth = shift;
	
	$inlineDepth = 1 if(!defined($inlineDepth));

	# We've hit our max inline depth.
	if($inlineDepth > $self->{'Config'}->{'MaxResolveReferencesDepth'}) {
		warn("$ScriptName: InlineRDFReferences: exceeded max inline depth of $self->{'Config'}->{'MaxResolveReferencesDepth'} processing:\n".Dumper($node)) if($self->Debugging());
		return;
	}
	
	# Default $node to the results;
	$node = $self->QueryResults() if(!defined($node));
	
	# Default lookupHR to the descriptions;
	$lookupHR = $self->Descriptions() if(!defined($lookupHR));
	
	if(ref($node) eq 'HASH') {
		if( exists($node->{'rdf:resource'})
		and exists($lookupHR->{$node->{'rdf:resource'}})
		) {
			my $lookup = $lookupHR->{$node->{'rdf:resource'}};
			if(ref($lookup) eq 'HASH') {
				# Add all of the values associated with the resource
				# to current hash.
				foreach my $key (keys(%$lookup)) {
					$node->{$key} = $lookup->{$key};
				}
			}
			# Remove rdf:resource it is replaced with rdf:about from
			# the referenced record. This avoids an infinite loop.
			delete($node->{'rdf:resource'});
		}
	
		# Recursively process the keys.
		foreach my $key (keys %$node) {
			# Skip scalar values;
			next if(ref($node->{$key}) eq '');
			
			# I used this to debug "Deep recursion on subroutine" errors.
			print("InlineRDFReferences: depth $inlineDepth: following $key => $node->{$key}\n") if($self->Debugging() > 1);
			
			# The DontFollowMembers hash is used to avoid resolving references
			# for members that trigger a recursive loop.
			# In RTC rtc_cm:com.ibm.team.workitem.linktype.relatedworkitem.related work item
			# member triggered a loop.
			if(!$self->{'DontFollowMembers'}->{$key}) {
				$self->InlineRDFReferences($node->{$key}, $lookupHR, $inlineDepth+1);
			}
		}
	}
	
	if(ref($node) eq 'ARRAY') {
		foreach my $item (@$node) {
			$self->InlineRDFReferences($item, $lookupHR, $inlineDepth);
		}
	}
}

=head3 IsA ( $hashRef, $type )

Utility method to test if record pointed to by $hashRef
is of type $type. The rdf:type values are queried to see
if any of them match $type. A record can have multiple 
rdf:type values so this method deals with that.

=cut

sub IsA {
	my $hashRef = shift;
	my $type = shift;
	
	if( exists($hashRef->{'rdf:type'})
	and exists($hashRef->{'rdf:type'}->{'rdf:resource'})
	) {
		if(ref($hashRef->{'rdf:type'}->{'rdf:resource'}) eq 'ARRAY') {
			foreach my $item (@{$hashRef->{'rdf:type'}->{'rdf:resource'}}) {
				if($item eq $type) {
					return 1;
				}
			}
		} elsif($hashRef->{'rdf:type'}->{'rdf:resource'} eq $type) {
			return 1;
		}
	}
		
	0;
}

#
# Subroutine to recursively call to deal with
# assignment to reference types in the XML data.
# For each reference a subcript needs to be supplied
# to specify how to dereference the reference.
# 
# $record = 'abcd';
# $record = {'abcd' => 'val', 'efgh'=>'val'}};
# $record = ['abcd', 'efgh'];
# $record = [{'abcd'=>'val', 'efgh'=>'val'}];
# $record = [{}];
# $subscript = {abcd};
# $subscript = [0];
# $subscript = [1];
# $subscript = [0]->{abcd};
# $subscript = {abcd}->[0]->{efgh};
#
sub updateUtil
{
    my($record, $parent, $subscript, $val, $append) = @_;
	$subscript = '' if(!defined($subscript));
    my($sub, $next_sub) = split(/->/, $subscript, 2);
    if(ref($record) eq 'ARRAY') {
    	my $index;
		# convenience so that an empty subscript will update the first item in
		# a single element array.
		if(!defined($sub) or $sub eq '') {
			if(@{$record} == 1) {
	    		$index = 0;
			} else {
    			die("Missing subscript for array item, $parent\n");
			}
		} else {
			if($sub =~ /^\[(\d+)\]$/) {
    			$index = $1;
				if($index > $#{$record}) {
    				die("Invalid subscript, $sub. It is larger than the size of the array\n");
				}
			} else {
    			die("Invalid subscript, $sub, for array item, $parent. Expecting field\->[N]=val\n");
			}
		}
    	updateUtil($record->[$index], "$parent\->$sub", $next_sub, $val, $append);
    } elsif(ref($record) eq 'HASH') {
		if(defined($sub) and $sub ne '') {
	    	if($sub =~ /^\{([^}]+)\}$/) {
    	    	updateUtil($record->{$1}, "$parent\->$sub", $next_sub, $val, $append);
	    	} else {
    	    	die("Invalid subscript, $sub, for hash item, $parent. Expecting field\->{key}=val\n");
	    	}
		} else {
	    	# Deal with the case where XMLin converts an empty element into an empty hash.
	    	# Provide a convenience so that an empty subscript will replace the empty hash with a
	    	# a simple scalar. 
	    	if(keys(%$record) == 0) {
	    		$_[0] = '';
				updateUtil($_[0], $parent, undef, $val, $append);
	    	} elsif(exists($record->{'content'})) {
			# Provide a convenience so that an empty subscript will replace the content field
				$record->{'content'} = $val;
			} else {
				die("Missing subscript for hash item, $parent\n");
	    	}
		}
    } else {
		if(defined($sub) and $sub ne '') {
    	    	die("Unexpected subscript, $sub, supplied for scalar item, $parent\n");
		} else {
	    	if($append eq 'append_if_not_empty') {
	    		# Append new content only if the existing value
				# is not empty.
				if($_[0] ne '') {
		    		$_[0] .= $val;
				}
	    	} elsif($append eq 'append') {
				$_[0] .= $val;
	    	} else {
				$_[0] = $val;
	    	}
		}
    }
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

L<XML::Simple>

=cut
