# Contributing to Eclipse Lyo

We adopt the Eclipse guidlines for [contributing via Git](https://wiki.eclipse.org/Development_Resources/Contributing_via_Git), to accept contributions in this project. **Make sure you have installed https://marketplace.eclipse.org/content/editorconfig-eclipse plugin for Eclipse to make formatting minimally consistent.**

Please follow these guidelines to submit your contributions. **Before your contribution can be accepted to an Eclipse Foundation project, you need to electronically sign the [Eclipse Contributor Agreement (ECA)](https://eclipse.org/legal/ECA.php).**
The preferred approach is to contribute a patch via GitHub using the standard GitHub pull request (remember to sign off on each commit and configure Git to use the same email addressed used to sign an ECA).
Alternatively, you can submit your contribution as a patch attachment on the corresponding Bugzilla or Github issue.
(This project no longer supports Gerrit.)


This project uses Maven as the build system for all Java projects except those which are Eclipse Plugin project. The latter projects contain all the Eclipse project files under Git for import and building using the *Import > Existing Projects into Workspace*. All other projects should be imported using the *Import > Existing Maven Projects* menu.

`core.query` project uses ANTLR for generating parser code. In order to configure Eclipse to use it, the `pom.xml` file contains some m2e-specific configuration. **After importing the projects, make sure to run *Maven > Update Project*.** If that does not work, you must add the directory `target/generated-sources/antlr3` under the *Java Project Build > Source*.


The Eclipse Lyo project page is located at https://projects.eclipse.org/projects/technology.lyo. It points to the information regarding source code management, builds, coding standards, and more.

You are also welcome to contact the development team via [lyo-dev mailing list](https://dev.eclipse.org/mailman/listinfo/lyo-dev) or on Slack (ask for an invite on the mailing list or by a PM to [@andrew](https://forum.open-services.net/u/andrew)).

## PRs

- Use `nolint` label to skip CodeQL checks
