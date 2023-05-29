--------------------------------------------------------------------------------
{-# LANGUAGE OverloadedStrings #-}

import Data.Monoid (mappend)
import Hakyll
  ( Context,
    applyAsTemplate,
    compile,
    compressCssCompiler,
    copyFileCompiler,
    dateField,
    defaultContext,
    getResourceBody,
    hakyll,
    hasVersion,
    idRoute,
    listField,
    loadAll,
    loadAllSnapshots,
    loadAndApplyTemplate,
    match,
    pandocCompiler,
    recentFirst,
    relativizeUrls,
    route,
    saveSnapshot,
    setExtension,
    teaserField,
    templateBodyCompiler,
    version,
    (.&&.),
  )

--------------------------------------------------------------------------------
main :: IO ()
main = hakyll $ do
  match "assets/*" $ do
    route idRoute
    compile copyFileCompiler

  match "assets/images/*" $ do
    route idRoute
    compile copyFileCompiler

  match "assets/stylesheets/*" $ do
    route idRoute
    compile compressCssCompiler

  match "posts/*.md" $ version "init" $ do
    route $ setExtension "html"
    compile $ do
      pandocCompiler
        >>= saveSnapshot "content"
        >>= relativizeUrls

  match "posts/*.md" $ version "final" $ do
    route $ setExtension "html"
    compile $ do
      posts <- recentFirst =<< loadAllSnapshots ("posts/*" .&&. hasVersion "init") "content"
      let otherBlogPostsCtx =
            dateField "date" "%B %e, %y"
              `mappend` listField "posts" defaultPostCtx (return posts)
              `mappend` defaultContext
              `mappend` defaultPostCtx

      pandocCompiler
        >>= saveSnapshot "content"
        >>= loadAndApplyTemplate "templates/post.html" otherBlogPostsCtx
        >>= relativizeUrls

  match "index.html" $ do
    route idRoute
    compile $ do
      posts <- recentFirst =<< loadAllSnapshots "posts/*" "content"
      let indexCtx =
            listField "posts" teaserCtx (return (take 1 posts))
              `mappend` defaultContext

      getResourceBody
        >>= applyAsTemplate indexCtx
        >>= loadAndApplyTemplate "templates/index.html" indexCtx
        >>= relativizeUrls

  match "blog.html" $ do
    route idRoute
    compile $ do
      posts <- recentFirst =<< loadAll ("posts/*" .&&. hasVersion "final")
      let blogPostsCtx =
            listField "posts" defaultPostCtx (return posts)
              `mappend` defaultContext

      getResourceBody
        >>= applyAsTemplate blogPostsCtx
        >>= loadAndApplyTemplate "templates/blog.html" blogPostsCtx
        >>= relativizeUrls

  match "templates/**" $ compile templateBodyCompiler

--------------------------------------------------------------------------------
defaultPostCtx :: Context String
defaultPostCtx =
  dateField "date" "%B %e, %Y"
    `mappend` defaultContext

teaserCtx :: Context String
teaserCtx =
  teaserField "teaser" "content" `mappend` defaultPostCtx
