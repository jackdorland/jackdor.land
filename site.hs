--------------------------------------------------------------------------------
{-# LANGUAGE OverloadedStrings #-}

import Data.Monoid (mappend)
import Hakyll
    ( getResourceBody,
      saveSnapshot,
      loadAll,
      loadAllSnapshots,
      copyFileCompiler,
      idRoute,
      setExtension,
      compile,
      match,
      route,
      hakyll,
      compressCssCompiler,
      relativizeUrls,
      pandocCompiler,
      dateField,
      defaultContext,
      listField,
      teaserField,
      applyAsTemplate,
      loadAndApplyTemplate,
      templateBodyCompiler,
      recentFirst,
      Context,
      version,
      hasVersion,
      (.&&.))

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
            listField "posts" defaultPostCtx (return posts) `mappend`
            defaultPostCtx

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

  match "templates/*" $ compile templateBodyCompiler

--------------------------------------------------------------------------------
defaultPostCtx :: Context String
defaultPostCtx =
  dateField "date" "%B %e, %Y"
    `mappend` defaultContext

teaserCtx =
  teaserField "teaser" "content" `mappend` defaultPostCtx
