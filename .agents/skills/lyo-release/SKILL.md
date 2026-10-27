---
name: lyo-release
description: Prepare and execute Eclipse Lyo Java SDK releases, from the version and changelog PR through Jenkins publication, GitHub tagging, and return to SNAPSHOT development. Use for SDK releases, not Lyo Designer releases.
---

# Eclipse Lyo SDK release

Use the repository checkout containing this skill; resolve its root with `git rev-parse --show-toplevel`. Work in the task’s attached worktree when present. The repository is `eclipse-lyo/lyo`, with development on `main`. Update this skill when an actual release reveals a procedural gap, and submit skill changes separately from release PRs.

## Decide upfront

Read current GitHub releases, tags, root POM, `CHANGELOG.md`, and `Jenkinsfile`. Distinguish the newest preview in the release line from the latest stable release. Propose logical next versions, but obtain the developer’s choice. After `7.0.0.Beta3`, examples are `7.0.0.Beta4`, `7.0.0.CR1`, and `7.0.0.Final`.

Also obtain these choices before merging or publishing:

- **Autopilot:** after required CI and reviews pass, merge the release PR, verify Jenkins publication, publish the GitHub release, and restore development without another approval at each stage.
- **Semi-auto:** prepare each major stage completely, then obtain approval for the release PR merge, GitHub publication, and return to development.
- **Next development version:** previews normally restore the original same-line SNAPSHOT, such as `7.0.0-SNAPSHOT`. Finals normally advance to the next patch, such as `7.0.1-SNAPSHOT` after `7.0.0.Final`. Confirm any alternative upfront.

Record the selected mode, release and development versions, original development SHA, PR URLs and SHAs, Jenkins build URL and revision, tag SHA, and completed stages in task notes. Resume from verified remote state after interruption; never repeat artifact publication blindly.

## Prepare the release PR

Start a `codex/` branch from current remote `main`, preserving unrelated work. Use the JDK required by the POM and Jenkinsfile (currently JDK 21), and a verified Maven installation home.

Use Maven Versions from the reactor root:

```sh
mvn -B versions:set -DnewVersion="$release_version" -DprocessAllModules=true -DgenerateBackupPoms=false
```

Inspect all POM changes. Update internal Lyo version properties missed by the plugin, including `version.lyo` in `domains/oslc-domains/pom.xml` if still set to the old SNAPSHOT. Check reactor modules, BOM, parents and explicit internal dependencies for consistent release versions. Examine remaining SNAPSHOT references individually; model generation inputs and documentation may intentionally target development. Do not run the old multi-repository Ruby release helper.

The changelog convention is:

```markdown
## [UNRELEASED] - YYYY-MM-DD
```

Replace only the active heading with `## [<exact-version>] - <today>` using the developer’s local date. Preserve the full accumulated release-line changelog, including for previews. Fix legacy changelog formatting once in its own commit before the release commit, so the preview revert will preserve that correction. Do not invent dates for historical releases or conflate GitHub publication dates with original release dates.

Maintain named links below the template:

```markdown
[UNRELEASED]: https://github.com/eclipse-lyo/lyo/compare/v<previous-version>...HEAD
[<version>]: https://github.com/eclipse-lyo/lyo/compare/v<previous-version>...v<version>
```

Use actual existing tags, including historical tags whose spelling differs. A first release without a meaningful predecessor may link to its release page. Avoid duplicate named-link definitions. The bottom template’s heading remains `## [UNRELEASED] - YYYY-MM-DD`.

Run the repository’s required checks and reactor tests appropriate to a version change (currently `mvn -B install -P'!spotbugs'`, then `mvn -B verify -Drevision="$release_version" --file bom/lyo-bom-consumer-test/pom.xml`, as used by CI; the separate SpotBugs job runs `mvn -B verify -DskipTests -P'!enforcer'`). Review the diff, commit the release version and changelog changes separately from formatting repairs, push normally, and open a PR against `main`. Attach the PR to the chat. Never force-push or squash without explicit authorization.

## Merge and verify publication

Wait for required checks on the PR’s current head and required reviews. Pending or missing checks are not success. In semi-auto mode present the ready PR and validation evidence for merge approval. Inspect the repository’s permitted merge methods. Preserve the separate commits: use a merge commit when allowed, or rebase merge when merge commits are disabled (as in `eclipse-lyo/lyo`). Do not squash without explicit authorization. After rebase merge, fetch `main` and record the rewritten release-change SHA as well as the resulting main SHA; use those rewritten SHAs for publication and restoration.

Wait for the Jenkins run at:

https://ci.eclipse.org/lyo/job/Lyo%20multibranch/job/main/

Identify the run that built the merged release revision. Verify its revision, successful completion, and successful **Publish (OSSRH)** stage; an older successful run is insufficient.

When Jenkins cannot be read directly, inspect GitHub commit statuses for the merged release SHA (`gh api repos/eclipse-lyo/lyo/commits/<sha>/status`). Use the `continuous-integration/jenkins/branch` context and retain its exact `target_url`. Require `success` on that release SHA and a URL identifying its `main` build. A successful whole-pipeline status is sufficient evidence when the Jenkinsfile at that SHA necessarily executes **Publish (OSSRH)** on `main` and propagates publication failures. Check that the stage was not made optional, skipped, or wrapped in failure suppression. A PR build status does not establish publication. If the association or semantics cannot be verified, obtain build evidence before proceeding. The current pipeline signs and deploys Maven artifacts using `dev,gpg-sign,central-deploy`, then publishes Javadocs. The `central-deploy` profile currently sets `autoPublish=true` and `waitUntil=published`, so successful completion waits for Central publication rather than only uploading a staging bundle. Verify these settings at the release SHA when interpreting the result. Do not add legacy manual staging, Sonatype promotion, Designer, or downstream release steps to this SDK procedure.

If Jenkins fails, is inaccessible, or publication is uncertain, stop dependent release actions and report the exact missing evidence. Do not rerun a non-SNAPSHOT deployment automatically: immutable artifacts might already exist. Do not leave `main` at a release version longer than necessary, because subsequent builds can attempt another immutable deployment.

## Publish the GitHub release and tag

Prepare notes before publication:

- **`.Final`:** use the full body of the corresponding `## [<version>]` changelog section, stopping before the next version heading or bottom template.
- **Preview (alpha/beta/CR):** include only changes since the previous preview in the same release line. Compare its actual tag with the verified release SHA, review the intervening PRs and changelog additions, and exclude already released entries. If there is no previous preview, use the release-line changes since the preceding final release. Include a comparison link for the selected interval.

Resolve named links used by extracted notes so the GitHub release body renders independently. Keep the cumulative changelog intact. Prepare notes in a UTF-8 file for CLI `--notes-file` or a structured tool argument.

In semi-auto mode show the exact notes, target SHA, tag and prerelease setting, then obtain publication approval. Create the release with tag `v<exact-version>` at the verified release SHA on `main`, not the moving branch tip. GitHub release creation can create the tag; alternatively push only that exact tag. Mark every version not ending in `.Final` as a prerelease. Before creating, check whether the tag or release already exists; verify and resume instead of overwriting it. Verify the published release, prerelease flag, notes and remote tag target before restoring development.

## Restore SNAPSHOT development

Prepare the restoration on fresh remote `main`, preserving intervening changes. In semi-auto mode present the concrete restoration diff for approval before merging or pushing it to `main`. Use a PR when branch protection requires it. Restore promptly after publication is verified.

- **Preview:** revert only the release PR’s version and release-heading/link changes to restore the original SNAPSHOT and cumulative UNRELEASED section. Use the exact release-change commit, never blindly `git revert HEAD`. If the merge included the formatting repair, preserve that repair. Do not rewrite history or undo unrelated later changes.
- **Final:** set the agreed next SNAPSHOT version through Maven, check internal version properties, and copy the bottom changelog template above the released entries. Keep the just-released section intact. Set `[UNRELEASED]` to `compare/v<just-released-version>...HEAD` and keep the release’s named link.

Verify the development POM versions and changelog links, commit, push normally, and verify the resulting SNAPSHOT CI. Record final release and restoration URLs and update this skill for any demonstrated gaps.
