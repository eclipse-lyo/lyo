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

package Lyo::OSLC::RTC;

=head1 NAME

Lyo::OSLC::RTC - Base class for interacting with Rational Team Concert(RTC) 
OSLC resources

=head1 SYNOPSIS

 use Lyo::OSLC::RTC;
 
 my $client = Lyo::OSLC::RTC->new(
   'user' => 'myuser', 
   'password' => 'mypassword', 
   'baseuri' => 'https://rtc.mydomain.com/jazz',
 );
 
 my $client = Lyo::OSLC::RTC->new;
 $client->credentials('myuser', 'mypassword);
 $client->setBaseURI('https://rtc.mydomain.com/jazz');

 $rootservices = $client->rootservices();
   
 Lyo::OSLC::RTC extends Lyo::OSLC and REST::Client so please refer to their documentation for
 more details about how to use this module.
 
=head1 DESCRIPTION

Lyo::OSLC::RTC provides a simple way to interact with RTC OSLC resources. 
Lyo::OSLC::RTC extends Lyo::OSLC and REST::Client. This class implements the RTC
form based authentication scheme.

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

use vars qw(@ISA @EXPORT @EXPORT_OK $ScriptName);
@ISA        = qw(Lyo::OSLC Exporter);
@EXPORT     = qw();
@EXPORT_OK  = qw();
$ScriptName = basename($0);

=head3 new ( [%$config] )

Construct a new Lyo::OSLC::RTC object. This class extends the REST::Client class
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
	
	my $self = $class->SUPER::new(
		# For RTC authenication we need to follow redirects.
		'follow' => 1,
		# We need a custom user agent to handle
		# the RTC form based authentication.
		'useragent' => 
			Lyo::OSLC::RTC::LWP::UserAgent->new(
				'agent' => "Lyo::OSLC::RTC/$VERSION ",
				# RTC requires cookies.
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


# A new class that overrides the redirect_ok method
# to provide RTC form based authenication.
{
	package Lyo::OSLC::RTC::LWP::UserAgent;
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
	
	sub redirect_ok {
		my($self, $prospective_request, $response) = @_;
		
		if($self->{'client_data'}->debugging() >= 3) {
			print "\n";
			print "response:\n", $response->as_string, "\n";
			print "prospective_request:\n", $prospective_request->as_string, "\n";
		}

		# Intercept the redirect to the log on form and POST the user credentials.
		# The RTC form based login mechanism and the use of x-com-ibm-team-repository-web-auth-msg 
		# is documented at https://jazz.net/wiki/bin/view/Main/JazzFormBasedAuth
		if(defined($response->header('x-com-ibm-team-repository-web-auth-msg')) 
		       and $response->header('x-com-ibm-team-repository-web-auth-msg') eq 'authrequired'
		) {
			my($user, $password) = $self->get_basic_credentials(undef, $prospective_request->uri, undef);
			
			# Apache Tomcat requires that we first request the contents of the 
			# login page before we POST the user's credentials to the j_security_check URI.
			my $dummy = $self->simple_request($prospective_request);
			#print "response from prospective_request:\n", $dummy->as_string, "\n";
			
			# Calculate the j_security_check URI.
			my $j_security_check = $prospective_request->uri()->clone();
			my $p = $j_security_check->path();
			$p =~ s,/[^/]+$,/j_security_check,;
			$j_security_check->path($p);
			#print("j_security_check2:", Dumper($j_security_check), "\n") if($self->{'client_data'}->debugging() >=3);
			
			# POST the user's credentials to the j_security_check URI.
			my $req = HTTP::Request::Common::POST($j_security_check, [
			   'j_username' => $user,
			   'j_password' => $password,
			]);
			if($self->{'client_data'}->debugging() >=3) {
				my $r = $req->as_string;
				$r =~ s/j_password=[^&]*$/j_password=********/m;
				print "request to j_security_check:\n", $r, "\n";
				print "cookies before j_security_check:\n", $self->cookie_jar->as_string, "\n";
			}
			my $res = $self->simple_request($req);
			if($self->{'client_data'}->debugging() >=3) {
				print "response from j_security_check:\n", $res->as_string, "\n";
				print "cookies after j_security_check:\n", $self->cookie_jar->as_string, "\n";
			}
			if($res->code == 302) {
				if($res->header('location') =~ m,/authfailed$,) {
					# Change the response to unauthorized.
					$response->code(401);
					$response->message("Log in failed.");
					print Dumper($response);
					return 0;
				} else {
					return 1;
				}
			} else {
				print("j_security_check failure response:", $res->as_string, "\n") if($self->{'client_data'}->debugging() >=3);
				# Replace the response from the original redirect with the response from j_security_check.
				$_[2] = $res;
				return 0;
			}
		}

		# Normal redirects are OK.
		return 1;
	}
}

# Custom request method that uses the existing credentials method
# in the LWP::UserAgent module to communicate the user and password 
# to the custom class used to authenicate with the OSLC server.
sub request {
    my $self = shift;
    my $method  = shift;
    my $url     = shift;
    my $content = shift;
    my $headers = shift;
	
	if($self->getUser() ne '') {
		$self->getUseragent()->credentials(URI->new($self->_prepareURL($url))->host_port, undef, $self->getUser(), $self->getPassword());
	}
	
	$self->SUPER::request($method, $url, $content, $headers);
}

=head3 $hashRef = rootservices ( )

Retrieve the rootservices XML document from the RTC server. 
This assumes you have already specified the jazz URL using
the setBaseURI() method or the baseuri property when calling new().
Returns a hash reference created by XML::Simple from the XML returned. 
The hash is cached to avoid querying the server next time.

=cut

sub rootservices {
	my $self = shift;
	
	if(defined($self->{'rootservices'})) {
		return $self->{'rootservices'};
	}
	
	$self->GET('rootservices', {'Accept' => 'application/xml'});
	if($self->responseCode() != 200) {
		die("$ScriptName: rootservices: ERROR: failure querying rootservices: ", $self->responseMessage, "\n");
	}
	
	$self->{'rootservices'} = XMLin($self->responseContent());
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
