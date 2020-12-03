# Eclipse Lyo issue tracker label sync

## Rationale

Update all label strings/colours across all Eclipse Lyo repos. Helps when you look at them on "my issues page" (e.g. https://github.com/issues?q=is%3Aissue+is%3Aopen+archived%3Afalse+sort%3Aupdated-desc+assignee%3Aberezovskyi) or the multi-project board (e.g. https://github.com/orgs/eclipse/projects/1).

## Getting started

Install dotenv gem:

    gem install dotenv

Add a `.env` file with the Github access token with the "private repos" scope:

    GITHUB_ACCESS_TOKEN=your111key

Run the script:

    ruby sync-labels.rb

If you like the result, remove `-d` flag in the code, rerun but do not commit the change.