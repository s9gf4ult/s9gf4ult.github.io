--------------------------------------------------------------------------------
{-# LANGUAGE OverloadedStrings #-}
import           Control.Lens             hiding (Context (..))
import           Data.Maybe
import           Data.Monoid
import           Hakyll
import           Text.Pandoc.Highlighting (pygments)
import           Text.Pandoc.Options

--------------------------------------------------------------------------------
main :: IO ()
main = hakyll $ do
    match "images/*" $ do
        route   idRoute
        compile copyFileCompiler

    match "css/*" $ do
        route   idRoute
        compile compressCssCompiler

    -- match "vendor/highlight.pack.js" $ do
    --   route $ constRoute "hightlight.js"
    --   compile copyFileCompiler

    for ["en"] \lang -> do
      match (fromList ["about.markdown", "projects.markdown"]) $ do
          route   $ setExtension "html"
          compile $ myPandocCompiler
              >>= loadAndApplyTemplate "templates/default.html" defaultContext
              >>= relativizeUrls

      match "posts/*" $ do
          route $ setExtension "html"
          compile $ myPandocCompiler
              >>= loadAndApplyTemplate "templates/post.html"    postCtx
              >>= loadAndApplyTemplate "templates/en.html" postCtx
              >>= loadAndApplyTemplate "templates/default.html" postCtx
              >>= relativizeUrls

      create ["en/index.html"] $ do
          route idRoute
          compile $ do
              posts <- recentFirst =<< loadAll "posts/*"
              let
                ctx
                  =  listField "posts" postCtx (pure posts)
                  <> constField "title" "Posts"
                  <> defaultContext
              makeItem ""
                  >>= loadAndApplyTemplate "templates/index.html" ctx
                  >>= loadAndApplyTemplate "templates/en.html" ctx
                  >>= loadAndApplyTemplate "templates/default.html" ctx
                  >>= relativizeUrls

      match "templates/*" $ compile templateBodyCompiler


--------------------------------------------------------------------------------
postCtx :: Context String
postCtx =
    dateField "date" "%Y-%m-%d" `mappend`
    defaultContext

myPandocCompiler :: Compiler (Item String)
myPandocCompiler = do
  ident <- getUnderlying
  toc   <- isJust <$> getMetadataField ident "withtoc"
  tocd <- (preview $ _Just . _Show) <$> getMetadataField ident "tocdepth"
  let
    r = defaultHakyllReaderOptions
    w = defaultHakyllWriterOptions
      { writerHighlightStyle = Just pygments
      , writerTableOfContents = toc
      , writerTOCDepth = fromMaybe 3 tocd
      , writerTemplate = Just $ if toc
        then  "<div class=\"post__toc toc\">\n$toc$\n</div>\n\
              \<div class=\"post__body\">\n$body$\n"
        else "<div class=\"post__body\">\n$body$\n"
      }
  pandocCompilerWith r w
