# Copilot agent instructions for eclipse-lyo/lyo

## Safety and approval gates
- Never run `git add`, `git commit`, or `git push` without explicit human approval in interactive sessions.
- Never eliminate/adjust unit test assertions or test data files as a means of making tests pass.
- Always run tests without asking for permission when code changes are made.

## Build and test workflow constraints
- Do not run single-module builds or tests that bypass reactor dependencies for `net.oauth`.
- The `net.oauth` artifacts are produced via `transformer-maven-plugin` and require a full reactor build so the transformed jars are created before dependent modules compile.
- Prefer running full reactor build/test (`mvn test` or `mvn install`) rather than per-module commands when `net.oauth`-dependent modules are involved.
- When running a single test in a reactor build on Windows, quote `-D` system properties and set `-Dsurefire.failIfNoSpecifiedTests=false` to avoid failures in upstream modules without that test (e.g., `mvn "-Dtest=org.eclipse.lyo.store.SparqlStoreImplTest" "-Dsurefire.failIfNoSpecifiedTests=false" -pl store/store-core -am test`).
- Before any commit, run `org.openrewrite.maven:rewrite-maven-plugin:run` and `spotless:apply` as in the CI workflow.

## Merge conflict handling
- Resolve conflicts in working tree only; ask for approval before staging or committing.

## Communication
- Summarize findings and next steps concisely.
- Explicitly call out any approvals needed before git or test/data changes.