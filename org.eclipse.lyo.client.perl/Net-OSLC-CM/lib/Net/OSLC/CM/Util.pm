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

package Net::OSLC::CM::Util;

use DateTime;

=head1 NAME

Net::OSLC::CM::Util - Utility module

=head1 VERSION
This document describes Net::OSLC::CM::Util version 0.01

=head1 DESCRIPTION

Provides utility functions when necessary

=head1 FUNCTIONS

=over

=item C<< XSDToDateTime ( $datetime ) >>

Returns a DateTime object.
Converts a date/time at the XSD format (XML date format) into a DateTime format.

=cut

sub XSDToDateTime {
  my $self = shift;
  my $XSDTime = shift;
  my $dt = undef;

  my ($y, $m, $d, $h, $mi, $s) = ($XSDTime =~ 
    m/^([0-9]{4})-([0-9]{2})-([0-9]{2})T([0-9]{2}):([0-9]{2}):([0-9]{2})Z/) ;

  my ($other, $z, $zh, $zm) = ($XSDTime =~ m/(.*)Z([+|-]{1})([0-9]{2}):([0-9]{2})/);

  if(!defined($z) and !defined($zh) and !defined($zm)){
    $z = "+";
    $zh = "00";
    $zm = "00";
  }
  
  #print "test: " . $y . " " . $m  . " " . $d . " " . $h . " " . $mi . " " . $s . " " . $z . " " . $zh . " " . $zm . "\n";  

  if (defined($y) and defined($m) and defined($d) and 
      defined($h) and defined($mi) and defined($s) and 
      defined($z) and defined($zh) and defined($zm)){

     $dt = DateTime->new(
       year => $y,
       month => $m, 
       day => $d, 
       hour => $h, 
       minute => $mi,
       second => $s,
       time_zone => $z . $zh . $zm
     );

  } else {
    print "Error at parsing XSD Time data\n";
  }
  return $dt;
}

1;

=back
