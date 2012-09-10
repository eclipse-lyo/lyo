use inc::Module::Install;
name('Net-OSLC-CM');
author('Stephanie Ouillon');
license('Eclipse Public License v1.0 and Eclipse Distribution License v. 1.0');
version_from('lib/Net/OSLC/CM.pm');

requires('Any::Moose');
requires('LWP::UserAgent');
requires('HTTP::MessageParser' => 0.3);
requires('RDF::Trine' => 0.138);
requires('RDF::Query' => 2.908);

tests("t/*.t");
#extra_tests;
auto_install;
&WriteAll;


