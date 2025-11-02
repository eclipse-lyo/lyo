_Describe what does this PR do..._

Closes #0; Closes #00 _(use exactly this syntax to link to issues, one issue per statement only!)_

## Checklist

- [ ] This PR adds an entry to the CHANGELOG. _See https://keepachangelog.com/en/1.0.0/ for instructions. Minor edits are exempt._
- [ ] This PR was tested on at least one Lyo OSLC server (comment `@oslc-bot /test-all` if not sure) or adds unit/integration tests.
- [ ] This PR does NOT break the API
- [ ] Lint checks pass (run `mvn package org.openrewrite.maven:rewrite-maven-plugin:run spotless:apply -DskipTests -P'!enforcer'` if not, commit & push)

