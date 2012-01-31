You must have Bugzilla 4.0 to run this sample. Simplest is to
connect to the Bugzilla Landfull <http://landfill.bugzilla.org/>.

This project also relies on j2bugzilla, a Java API for Bugzilla.
Download j2bugzilla 1.0 from Google Code,

  http://j2bugzilla.googlecode.com/files/j2bugzilla-1.0.jar

Then install j2bugzilla into your local repository with the command,

  mvn install:install-file -Dfile=j2bugzilla-1.0.jar -DgroupId=j2bugzilla -DartifactId=j2bugzilla -Dversion=1.0 -Dpackaging=jar

To run the Bugzilla adapter, edit src/main/resources/bugz.properties to
point to your Bugzilla server (or use the default, Bugzilla Landfill).
Then from the project root directory,

mvn install
mvn jetty:run

The adapter will be running at

http://localhost:8282/bugz/
