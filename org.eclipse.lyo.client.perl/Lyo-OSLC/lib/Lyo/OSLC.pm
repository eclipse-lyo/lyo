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

package Lyo::OSLC;

=head1 NAME

Lyo::OSLC - Perl modules for interacting with OSLC resources

=head1 SYNOPSIS

 use Lyo::OSLC;
 
 my $client = Lyo::OSLC->new(
   'user' => 'myuser', 
   'password' => 'mypassword', 
   'baseuri' => 'http://example.com/oslc',
 );
 
 my $client = Lyo::OSLC->new;
 $client->credentials('myuser', 'mypassword);
 $client->setBaseURI('http://example.com/olsc');
 
=head1 DESCRIPTION

Lyo::OSLC is the base class use to implement the other modules for interacting with OSLC resources.

=cut

=head1 METHODS

=cut

use strict;
use warnings;
use 5.008_000;

our ($VERSION) = 1.0102;

use File::Basename;
#use LWP::Debug qw(+);
use REST::Client 243;
use XML::Simple;
use Data::Dumper;
use Lyo::OSLC::RDF;

use vars qw(@ISA @EXPORT @EXPORT_OK $ScriptName $Debugging);
@ISA        = qw(REST::Client Exporter);
@EXPORT     = qw();
@EXPORT_OK  = qw();
$ScriptName = basename($0);
$Debugging  = 0;

=head3 new ( [%$config] )

Construct a new Lyo::OSLC instance. This class extends the REST::Client class
with the following additional flags. It takes an optional hash or hash 
reference.

=over

=item baseuri

The base URI of the OLSC provider.

=item user

The user to log onto the OLSC provider with.

=item password

The password to log onto the OLSC provider with.

=back

=cut

sub new {
    my $class = shift;
	
	my $self = $class->SUPER::new(@_);
	
	$self->{'rootservices'} = undef;
	
	# Set the initial debugging level from the class variable.
	$self->{'Debugging'} = $Debugging;
	
	$self;
}

# Extend the _buildAccessors method of REST::Client to add 
# support for the attributes User, Password, and BaseURI.
sub _buildAccessors {
	my $self = shift;
	
	$self->SUPER::_buildAccessors(@_);
	
	# copied from the _buildAccessors method in REST::Client.
	
    return if $self->can('setUser');

    my @attributes = qw(User Password BaseURI);

    for my $attribute (@attributes){
        my $set_method = "
        sub {
        my \$self = shift;
        \$self->{'_config'}{lc('$attribute')} = shift;
        return \$self->{'_config'}{lc('$attribute')};
        }";

        my $get_method = "
        sub {
        my \$self = shift;
        return \$self->{'_config'}{lc('$attribute')};
        }";


        {
            no strict 'refs';
            *{'REST::Client::set'.$attribute} = eval $set_method ;
            *{'REST::Client::get'.$attribute} = eval $get_method ;
        }

    }

	1;
}

# Replace the _prepareURL method of REST::Client to use 
# the BaseURI attribute instead of the Host attribute 
# when converting a relative URL to a absolute one.
# Urls that begin with https?::// are assumed to be fully qualified.
sub _prepareURL {
    my $self = shift;
    my $url = shift;

    unless($url =~ m,^https?://,){
    	my $host = $self->getBaseURI;
    	if($host){
        	$url = '/'.$url unless($url eq '' or $url =~ m,^/,);
        	$url = $host . $url;
    	}
	}
    unless($url =~ m,^https?://,){
        $url = ($self->getCert ? 'https://' : 'http://') . $url;
    }
	
	$self->lastURL($url);
	
    return $url;
}

=head3 $respose = response ()

Extends the REST::Client APIs to returns the HTTP::Response 
object of the last request.

=cut

sub response {
    my $self = shift;
    return $self->{_res};
}

=head3 $message = responseMessage ()

Extends the REST::Client APIs to return the HTTP response
message of the last request.

=cut

sub responseMessage {
    my $self = shift;
    return $self->{_res}->message;
}

=head3 ( $user, $password ) = credentials ( [$user, $password] )

Method to set or get the credentials used to access the OSLC service provider.

=cut

sub credentials {
	my $self = shift;
	if(@_) {
		my($user, $password) = @_;
		$self->setUser($user);
		$self->setPassword($password);
	}
	($self->getUser(), $self->getPassword());
}

=head3 my $url = lastURL ()

Return the URL of the last request.

=cut

sub lastURL {
    my $self = shift;
	if(@_) {
		$self->{'lastURL'} = $_[0];
	}
	$self->{'lastURL'};
}

=head3 $level = debugging ( $level )

Method to set or get the debugging level.

=cut

sub debugging {
	my $self = shift;
	if(@_) {
		$self->{'Debugging'} = $_[0];
		$Lyo::OSLC::RDF::Debugging = $self->{'Debugging'};
	}
	$self->{'Debugging'};
}

=head3 $oslcRDF = getAllPages ( $url, $headerHR, $oslcRDFConfigHR )

Method to use instead of GET for a query that returns
its results over multiple pages. This method will fetch the
full result by making multiple GET requests to the
server until all of the pages have been retrived.
Returns an Lyo::OSLC::RDF object. $headerHR are additional
headers to add to the GET request. $oslcRDFConfigHR is used
to pass configuration settings to the new method of the 
Lyo::OSLC::RDF object.

=cut

sub getAllPages {
	my($self, $url, $headerHR, $oslcRDFConfigHR) = @_;
	
	my $nextPage = $url;

	$headerHR = {} unless(defined($headerHR));
	my %headers = (
		'Accept' => 'application/rdf+xml',
		%$headerHR,
	);
	
	$oslcRDFConfigHR = {} unless(defined($oslcRDFConfigHR));
	my %dataConfig = (
		%$oslcRDFConfigHR,
		'ResolveReferences' => 0,
	);
		
	my $perl = Lyo::OSLC::RDF->new(undef, %dataConfig);
	
	while(1) {
		$self->GET($nextPage, \%headers);
		if($self->responseCode() != 200) {
			last;
		}
		
		$perl->Add($self->responseContent());
		
		print("getAllPages: ", Dumper($perl), "\n") if($self->debugging());

		my $response = $perl->QueryResponse();
		
		last if(!exists($response->{'oslc:nextPage'}));
		
		$nextPage = $response->{'oslc:nextPage'}->{'rdf:resource'};
	}
	
	$perl->ResolveReferences(1);
	
	print("getAllPages: Done: ", Dumper($perl), "\n") if($self->debugging());

	$perl;
}

=head3 errorHandler ( $errorMessage )

Generic error handler to deal with testing the response
code from the last web request. die() is called if an
error is encountered. The format of the error message is
<ScriptName>: $errorMessage: <error messages from server>.

=cut

sub errorHandler {
	my($self, $errorMessage) = @_;
	
	if($self->responseCode() != 200) {
		my $reason = $self->responseMessage();
		my $response = $self->responseContent();
		print("ErrorHandler: responseCode: ", $self->responseCode(), " reason: $reason response: $response\n") if($self->debugging());
		if(defined($response) and $response ne '') {
			my $xml = XMLin($response,
				'ForceArray' => ['cq:error'],
			);
			print("ErrorHandler: parsed xml:\n", Dumper($xml), "\n") if($self->debugging());
			
			# RTC
			if( exists($xml->{'rdf:Description'})
			and exists($xml->{'rdf:Description'}->{'oslc:message'})
			) {
				$reason = $xml->{'rdf:Description'}->{'oslc:message'};
			}
			
			# CQ
			if( exists($xml->{'Error'})
			and exists($xml->{'Error'}->{'message'})
			) {
				my @reason;
				push(@reason, $xml->{'Error'}->{'message'});
				
				if(exists($xml->{'Error'}->{'cq:error'})) {
					foreach my $record (@{$xml->{'Error'}->{'cq:error'}}) {
						push(@reason, $record->{'Error'}->{'message'});
					}
				}
				$reason = join("\n", @reason);
			}
		}
		die("$ScriptName: $errorMessage: $reason\n");
	}
}

1;

__END__

=head1 ERROR HANDLING

Error conditions are handled by calling die() with an appropriate error message.
The caller can use the eval() function to catch the call to die() and take
appropriate action.

  my $o;
  eval {
    $o = OLSC->new();
	$o->GET('some/url');
	$o->errorHandler("GET of some/url failed");
  }
  if($@) {
    # Deal with error.
	warn $@;
  }
  
See <Try::Tiny> if you are interested in using try/catch syntaxes instead of eval.

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

L<REST::Client>, L<Lyo::OSLC::RDF>

=cut
