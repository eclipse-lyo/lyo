You must have Bugzilla 3.4 to run this sample. You can connect to the
Bugzilla Landfull <http://landfill.bugzilla.org/> or use a VM image
such as the ALMWorks Bugzilla 3.4 VMWare image.

  http://almworks.com/vbs/download.html

This project also relies on j2bugzilla, a Java API for Bugzilla.

Download j2bugzilla 1.0 from Google Code.

  http://j2bugzilla.googlecode.com/files/j2bugzilla-1.0.jar

Install j2bugzilla into your local repository with the command,

  mvn install:install-file -Dfile=j2bugzilla-1.0.jar -DgroupId=j2bugzilla -DartifactId=j2bugzilla -Dversion=1.0 -Dpackaging=jar

To run the Bugzilla adapter, edit src/main/resources/bugz.properties to
point to your Bugzilla server. Then from the project root directory,

mvn install
mvn jetty:run

The adapter will be running at

http://localhost:8282/bugz/
