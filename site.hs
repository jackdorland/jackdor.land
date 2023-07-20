--------------------------------------------------------------------------------
{-# LANGUAGE OverloadedStrings #-}

import Data.Monoid (mappend)
import Hakyll

--------------------------------------------------------------------------------
main :: IO ()
main = hakyll $ do
  -- sitemap
  create ["sitemap.xml"] $ do
    route idRoute
    compile $ do
      posts <- recentFirst =<< loadAll ("posts/*" .&&. hasVersion "production")
      singlePages <- loadAll (fromList ["blog.html", "index.html", "about.html", "robots.html"])

      let pages = posts <> singlePages
          sitemapCtx =
            constField "root" root
              <> listField "pages" defaultPostCtx (return pages)
      makeItem ""
        >>= loadAndApplyTemplate "templates/sitemap.xml" sitemapCtx

  -- css compression
  match "assets/stylesheets/*" $ do
    route idRoute
    compile compressCssCompiler

  -- post generation
  match "posts/*.md" $ version "staging" $ do
    route $ setExtension "html"
    compile $ do
      pandocCompiler
        >>= saveSnapshot "content"
        >>= relativizeUrls

  match "posts/*.md" $ version "production" $ do
    route $ setExtension "html"
    compile $ do
      posts <- recentFirst =<< loadAllSnapshots ("posts/*" .&&. hasVersion "staging") "content"
      let otherBlogPostsCtx =
            dateField "date" "%B %e, %Y"
              <> listField "posts" defaultPostCtx (return posts)
              <> teaserField "teaser" "content"
              <> defaultPostCtx

      pandocCompiler
        >>= saveSnapshot "content"
        >>= loadAndApplyTemplate "templates/post.html" otherBlogPostsCtx
        >>= relativizeUrls

  -- index.html post filling
  match "index.html" $ do
    route idRoute
    compile $ do
      posts <- recentFirst =<< loadAllSnapshots "posts/*" "content"
      let indexCtx =
            listField "posts" teaserCtx (return (take 1 posts))
              <> defaultContext

      getResourceBody
        >>= applyAsTemplate indexCtx
        >>= loadAndApplyTemplate "templates/index.html" indexCtx
        >>= relativizeUrls

  match
    ( "robots.txt"
        .||. "assets/images/**"
        .||. "assets/*"
        .||. "assets/svg/*"
    )
    $ do
      route idRoute
      compile copyFileCompiler

  -- template static files
  match (fromList ["about.html"]) $ do
    route idRoute
    compile $ do
      getResourceBody
        >>= loadAndApplyTemplate "templates/about.html" defaultContext
        >>= relativizeUrls

  -- fill blog page
  match "blog.html" $ do
    route idRoute
    compile $ do
      posts <- recentFirst =<< loadAll ("posts/*" .&&. hasVersion "production")
      let blogPostsCtx =
            listField "posts" defaultPostCtx (return posts)
              <> defaultContext

      getResourceBody
        >>= applyAsTemplate blogPostsCtx
        >>= loadAndApplyTemplate "templates/blog.html" blogPostsCtx
        >>= relativizeUrls

  -- compile template bodies
  match "templates/**" $ compile templateBodyCompiler

  -- atom/rss
  create ["atom.xml"] $ do
    route idRoute
    compile $ do
      let feedCtx = defaultPostCtx `mappend` bodyField "description"
      posts <-
        fmap (take 10) . recentFirst
          =<< loadAllSnapshots ("posts/*" .&&. hasVersion "production") "content" 
      renderAtom feedConfiguration feedCtx posts

  create ["rss.xml"] $ do
    route idRoute
    compile $ do
      let feedCtx = defaultPostCtx `mappend` bodyField "description"
      posts <-
        fmap (take 10) . recentFirst
          =<< loadAllSnapshots ("posts/*" .&&. hasVersion "production") "content" 
      renderRss feedConfiguration feedCtx posts

--------------------------------------------------------------------------------
root :: String
root = "https://jackdor.land"

defaultPostCtx :: Context String
defaultPostCtx =
  constField "root" root
    <> dateField "date" "%B %e, %Y"
    <> defaultContext

teaserCtx :: Context String
teaserCtx =
  teaserField "teaser" "content" <> defaultPostCtx

feedConfiguration :: FeedConfiguration
feedConfiguration =
  FeedConfiguration
    { feedTitle = "pens, pencils & politics",
      feedDescription = "Posts about pens, pencils, and - you guessed it - American politics!",
      feedAuthorName = "Jack Dorland",
      feedAuthorEmail = "jackdor.land",
      feedRoot = "https://jackdor.land"
    }
