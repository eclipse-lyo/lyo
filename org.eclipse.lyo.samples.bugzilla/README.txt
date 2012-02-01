You must have Bugzilla 4.0 to run this sample. Simplest is to
connect to the Bugzilla Landfull <http://landfill.bugzilla.org/>.

To run the Bugzilla adapter, edit src/main/resources/bugz.properties to
point to your Bugzilla server (or use the default, Bugzilla Landfill).
Then from the project root directory,

mvn install
mvn jetty:run

The adapter will be running at

http://localhost:8282/bugz/
