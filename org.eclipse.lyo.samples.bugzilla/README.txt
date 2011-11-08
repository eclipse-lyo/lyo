You must have Subversion and Maven command line tools installed to run this sample. You
must also have Bugzilla 3.4 (or 3.6). Consider using the ALMWorks Bugzilla 3.4 VMWare
image.

  http://almworks.com/vbs/download.html

To install j2bugzilla, a Bugzilla Java API, into your local maven repository,

svn checkout http://j2bugzilla.googlecode.com/svn/trunk/ j2bugzilla-read-only
cd j2bugzilla-read-only/JBugz
mvn install

To run the Bugzilla adapter, edit src/main/resources/bugz.properties to point to your
Bugzilla server. Then from the project root directory,

mvn install
mvn jetty:run

The adapter will be running at

http://localhost:8282/bugz/
