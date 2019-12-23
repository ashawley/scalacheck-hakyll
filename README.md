ScalaCheck.org site with Hakyll
===============================

[![Build Status](https://travis-ci.org/ashawley/scalacheck-hakyll.svg?branch=develop)](https://travis-ci.org/ashawley/scalacheck-hakyll)

Install GHC and Cabal.

- https://www.haskell.org/downloads/
- https://www.haskell.org/cabal/

Install Hakyll.

    $ cabal install hakyll

Compile the `site` executable command:

    $ cabal build

Generate the site from the sources:

    $ cabal run site build

You can preview the site from http://localhost:8000 in a browser:

    $ cabal run site watch

If you change `site.hs`, then re-compile the executable:

    $ cabal build

And then you need to rebuild the entire site:

    $ cabal run site rebuild

More information on Hakyll:

- https://jaspervdj.be/hakyll/

Create a project in Firebase.

- https://console.firebase.google.com

Install the Firebase CLI.

    $ npm install -g firebase-tools

- https://firebase.google.com/docs/cli

Authenticate with Firebase.

    $ firebase login

Initialize your project.

    $ firebase init

- Choose "hosting"
- Choose "use an existing project"
- Set `_site` as the "public directory"
- Answer "no" to "Configure as single-page app"

Preview the site at http://localhost:5000 in a browser:

    $ firebase serve

Upload the site.

    $ firebase deploy
