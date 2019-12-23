--------------------------------------------------------------------------------
{-# LANGUAGE OverloadedStrings #-}
import           Data.Monoid (mappend)
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
        compile $ pandocCompiler
            >>= loadAndApplyTemplate "templates/download.html" downloadCtx
            >>= relativizeUrls

    match "index.markdown" $ do
        route   $ setExtension "html"
        compile $ do
            let indexCtx =
                    dateField "date" "%B %e, %Y" `mappend`
                    siteCtx
            pandocCompiler
                >>= loadAndApplyTemplate "templates/default.html" indexCtx
                >>= relativizeUrls

    match "404.markdown" $ do
        route   $ setExtension "html"
        compile $ do
            let indexCtx =
                    dateField "date" "%B %e, %Y" `mappend`
                    siteCtx
            pandocCompiler
                >>= loadAndApplyTemplate "templates/default.html" indexCtx

    match "templates/*" $ compile templateBodyCompiler


--------------------------------------------------------------------------------
siteCtx :: Context String
siteCtx =
    dateField "year" "%Y" `mappend`
    defaultContext
downloadCtx :: Context String
downloadCtx =
    constField "currentVer" "1.14.0" `mappend`
    dateField "date" "%B %e, %Y" `mappend`
    siteCtx
