## Getting started

Install dotenv gem:

    gem install dotenv

Add a `.env` file with the Github access token with the "private repos" scope:

    GITHUB_ACCESS_TOKEN=your111key

Run the script:

    ruby sync-labels.rb

If you like the result, remove `-d` flag in the code, rerun but do not commit the change.