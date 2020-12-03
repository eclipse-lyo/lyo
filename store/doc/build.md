# Build instructions

## Useful maven commands:

To add missing copyright headers:

    mvn license:update-file-header

To scan for CVEs on any dependency:

    mvn org.owasp:dependency-check-maven:check

To replace `$version-stub$` with the correct version in the Javadoc:

    mvn git-version-insert:insert-version

To set the new artifact version (run in on parent):

    mvn versions:set -DnewVersion=2.3.0

Show the transitive dependency tree (in offline mode):

    mvn -DskipTests dependency:tree -Dverbose -o

Aggregate the Javadoc from all modules:

    mvn clean package javadoc:aggregate

Sign the artifacts:

    mvn package gpg:sign

Analyse unused and/or undeclared dependencies:

    mvn dependency:analyze -Xlint:unchecked

Generate binary shims for running a standalone application:

    mvn appassembler:assemble

Pass profile ID to the execution:

    mvn install -P platform-mars