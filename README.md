# matrufsc-frontend

Caravela's MatrUFSC built using Elm.

# Setup
Install elm by using the [official guide](https://guide.elm-lang.org/install.html).

Compile Elm using

`elm-make Main.elm --output index.html`

or use `elm-reactor` and visit `http://localhost:8000`

# Why Elm?

- https://elmprogramming.com/why-elm.html
- https://hackernoon.com/should-i-learn-elm-if-i-am-a-javascript-developer-fba282cef6a0

# GitLab Deploy
This project uses GitLab CI to deploy to GitHub Pages, the following variables need to be set in the [CI configuration panel](https://gitlab.com/caravelahc/matrufsc/matrufsc-frontend/-/settings/ci_cd).

Variable|Description
|-|-|
REMOTE_URL|Used to set the remote to be pushed inside the CI, must use this format: `https://<deploy-bot-name>:<deploy-token>@github.com/caravelahc/matrufsc-frontend.git`
DEPLOY_USERNAME|GitHub username for the deploy commit.
DEPLOY_EMAIL|GitHub account email for the deploy commit
