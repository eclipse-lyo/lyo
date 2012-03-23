You must have connect to an existing Bugzilla server to run this sample.
Simplest is to connect to the Bugzilla Landfull <http://landfill.bugzilla.org/>.

To install the j2bugzilla API (a 3rd party dependency), download the
2.0-SNAPSHOT version from
			
    http://code.google.com/p/j2bugzilla/downloads/list
			
then run this command.
			
mvn install:install-file -Dfile=<path-to-file> -DgroupId=com.j2bugzilla -DartifactId=j2bugzilla -Dversion=2.0-SNAPSHOT -Dpackaging=jar 

To run the Bugzilla adapter, edit src/main/resources/bugz.properties to
point to your Bugzilla server (or use the default, Bugzilla Landfill).
Then from the project root directory,

mvn install
mvn jetty:run

The adapter will be running at

http://localhost:8282/bugz/
