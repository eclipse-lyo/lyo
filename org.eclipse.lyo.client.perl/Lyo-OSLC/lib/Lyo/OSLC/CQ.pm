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

package Lyo::OSLC::CQ;

=head1 NAME

Lyo::OSLC::CQ - Base class for interacting with ClearQuest(CQ) 
OSLC resources

=head1 SYNOPSIS

 use Lyo::OSLC::CQ;
 
 my $client = Lyo::OSLC::CQ->new(
   'user' => 'myuser', 
   'password' => 'mypassword', 
   'baseuri' => 'https://cq.mydomain.com/cqweb/oslc',
 );
 
 my $client = Lyo::OSLC::CQ->new;
 $client->credentials('myuser', 'mypassword);
 $client->setBaseURI('https://cq.mydomain.com/cqweb/oslc');

 $rootservices = $client->rootservices();
   
 Lyo::OSLC::CQ extends Lyo::OSLC and REST::Client so please refer to their documentation for
 more details about how to use this module.
 
=head1 DESCRIPTION

Lyo::OSLC::CQ provides a simple way to interact with CQ OSLC resources. 
Lyo::OSLC::CQ extends Lyo::OSLC and REST::Client.

=cut

=head1 METHODS

=cut

use strict;
use warnings;
use 5.008_000;

our ($VERSION) = 1.0102;

use File::Basename;
use LWP::UserAgent;
#use LWP::Debug qw(+);
use HTTP::Cookies;
use Lyo::OSLC;
use XML::Simple;

use vars qw(@ISA @EXPORT @EXPORT_OK $ScriptName $Debugging);
@ISA        = qw(Lyo::OSLC Exporter);
@EXPORT     = qw();
@EXPORT_OK  = qw();
$ScriptName = basename($0);

=head3 new ( [%$config] )

Construct a new Lyo::OSLC::CQ object.

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
	
	my $self = $class->SUPER::new(
		# For CQ authenication we need to follow redirects.
		'follow' => 1,
		# We need a custom user agent to handle
		# the basic authentication.
		'useragent' => 
			Lyo::OSLC::CQ::LWP::UserAgent->new(
			'agent' => "Lyo::OSLC::CQ/$VERSION ",
			# CQ requires cookies.
			'cookie_jar' => HTTP::Cookies->new,
		),
		@_,
	);
	
	$self->getUseragent()->client_data($self);

	# By default always request xml.
	$self->getUseragent()->default_header(
		'OSLC-Core-Version' => '2.0',
		'Accept' => 'application/rdf+xml',
	);
	
	$self;
}

# A new class that overrides the get_basic_credentials method
# to return the user and password for basic authentication.
{
	package Lyo::OSLC::CQ::LWP::UserAgent;
	our @ISA = qw(LWP::UserAgent);
	use HTTP::Request::Common;
	use Data::Dumper;
	
	sub new {
		my $class = shift;
		$class->SUPER::new(@_);
	}
	
	sub client_data {
		my $self = shift;
		if(@_) {
			$self->{'client_data'} = $_[0];
		}
		$self->{'client_data'};
	}
	
	sub get_basic_credentials {
		my($self, $realm, $uri, $isproxy) = @_;
		my $user = $self->{'client_data'}->getUser();
		my $password = $self->{'client_data'}->getPassword();
		($user, $password);
	}	
}

=head3 $hashRef = rootservices ( )

Retrieve the rootservices XML document from the CQ server. 
This assumes you have already specified the jazz URL using
the setHost() method or the Host property when calling new().
Returns a hash reference created by XML::Simple from the XML returned. 
The hash is cached to avoid querying the server next time.

=cut

sub rootservices {
	my $self = shift;
	
	if(defined($self->{'rootservices'})) {
		return $self->{'rootservices'};
	}
	
	# Are these headers correct ???
	$self->GET('/', {'Accept' => 'application/xml'});
	if($self->responseCode() != 200) {
		die("$ScriptName: rootservices: ERROR: failure querying ", $self->response->base->as_string, ": ", $self->responseMessage, "\n");
	}
	
	$self->{'rootservices'} = XMLin($self->responseContent(), 'ForceArray' => ['ServiceProviderCatalog']);
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

L<REST::Client>, L<Lyo::OSLC>

=cut
