--------------------------------------------------------------------------------
{-# LANGUAGE OverloadedStrings #-}
import           Data.Monoid (mappend)
import           Data.Maybe  (fromMaybe)
import           Data.Time (formatTime)
import           Data.Time (defaultTimeLocale)
import           Data.Time (getZonedTime)
import           Hakyll


--------------------------------------------------------------------------------
main :: IO ()
main = hakyll $ do
    match "img/*" $ do
        route   idRoute
        compile copyFileCompiler

    match "fonts/*" $ do
        route   idRoute
        compile copyFileCompiler

    match "css/*" $ do
        route   idRoute
        compile compressCssCompiler

    match (fromList ["download.markdown", "documentation.markdown"]) $ do
        route   $ setExtension "html"
        compile $ pandocCompiler
            >>= loadAndApplyTemplate "templates/default.html" siteCtx
            >>= relativizeUrls

    match "download/*.markdown" $ do
        route $ setExtension "html"
        compile $ do
            let releaseCtx =
                    dateField "date" "%B %e, %Y" `mappend`
                    siteCtx
                downloadCtx =
                    -- This defines a field "scalaVersions" in the
                    -- YAML preamble that is a list and can be
                    -- iterated over in the template file.  However,
                    -- this does not support either a single value nor
                    -- a comma-separated list of values.
                    listFieldWith "scalaVersions"
                        (field "scalaver" (return . itemBody) `mappend`
                         releaseCtx)
                        (\item -> do
                          metadata <- getMetadata (itemIdentifier item)
                          let scalaVersions = fromMaybe [] $ lookupStringList "scalaVersions" metadata
                          mapM makeItem scalaVersions) `mappend`
                          releaseCtx
            pandocCompiler
                >>= loadAndApplyTemplate "templates/download.html" downloadCtx
                >>= loadAndApplyTemplate "templates/default.html"  siteCtx
                >>= relativizeUrls

    match "index.markdown" $ do
        route   $ setExtension "html"
        compile $ do
            let indexCtx =
                    constField "foo" "bar" `mappend`
                    siteCtx
            pandocCompiler
                >>= loadAndApplyTemplate "templates/default.html" indexCtx
                >>= relativizeUrls

    match "404.markdown" $ do
        route   $ setExtension "html"
        compile $ do
            let fourOhFourCtx =
                    constField "foo" "bar" `mappend`
                    siteCtx
            pandocCompiler
                >>= loadAndApplyTemplate "templates/default.html" fourOhFourCtx

    create ["releases.html"] $ do
        route idRoute
        compile $ do
            releases <- recentFirst =<< loadAll "download/*.markdown"
            let releasesCtx =
                    listField "releases" siteCtx (return releases) `mappend`
                    constField "title" "All ScalaCheck Releases"   `mappend`
                    siteCtx
            makeItem ""
                >>= loadAndApplyTemplate "templates/releases.html" releasesCtx
                >>= loadAndApplyTemplate "templates/default.html"  siteCtx
                >>= relativizeUrls

    match "templates/*" $ compile templateBodyCompiler


--------------------------------------------------------------------------------
siteCtx :: Context String
siteCtx = 
    defaultContext <> (field "year" $ \i ->
        unsafeCompiler $ formatTime defaultTimeLocale "%Y" <$> getZonedTime)
