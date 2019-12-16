ScalaCheck.org site with Hakyll
===============================

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
