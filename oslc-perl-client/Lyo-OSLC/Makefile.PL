use ExtUtils::MakeMaker;

WriteMakefile(
    NAME            => 'Lyo::OSLC',
    VERSION_FROM    => 'lib/Lyo/OSLC.pm',
    ABSTRACT_FROM   => 'lib/Lyo/OSLC.pm',
	AUTHOR			=> 'Max Vohlken',
	PREREQ_PM => {
		"REST::Client" => 0,
		"LWP::UserAgent" => 0,
		"XML::Simple" => 0,
		"URI" => 0,
		"URI::Escape" => 0,
		"HTTP::Cookies" => 0,
	},
);
