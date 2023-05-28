--------------------------------------------------------------------------------
{-# LANGUAGE OverloadedStrings #-}
import           Data.Monoid (mappend)
import           Hakyll


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

    match "posts/*.md" $ do
        route $ setExtension "html"
        compile $ pandocCompiler
	    >>= saveSnapshot "content"
            >>= loadAndApplyTemplate "templates/post.html"    postCtx
            >>= relativizeUrls
    	
    match "index.html" $ do
        route idRoute
        compile $ do
            posts <- recentFirst =<< loadAllSnapshots "posts/*" "content"
            let indexCtx =
                    listField "posts" teaserCtx (return (take 1 posts)) `mappend`
                    defaultContext

            getResourceBody
                >>= applyAsTemplate indexCtx
                >>= loadAndApplyTemplate "templates/index.html" indexCtx
                >>= relativizeUrls
    
    match "blog.html" $ do
        route idRoute
        compile $ do
            posts <- recentFirst =<< loadAll "posts/*"
            let blogPostsCtx =
                    listField "posts" postCtx (return posts) `mappend`
                    defaultContext

            getResourceBody
                >>= applyAsTemplate blogPostsCtx
                >>= loadAndApplyTemplate "templates/blog.html" blogPostsCtx
                >>= relativizeUrls


    match "templates/*" $ compile templateBodyCompiler


--------------------------------------------------------------------------------
postCtx :: Context String
postCtx =
    dateField "date" "%B %e, %Y" `mappend`
    defaultContext

teaserCtx = 
    teaserField "teaser" "content" `mappend` postCtx
