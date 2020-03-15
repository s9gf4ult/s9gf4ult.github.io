--------------------------------------------------------------------------------
{-# LANGUAGE OverloadedStrings #-}
import           Control.Lens               hiding (Context (..))
import           Control.Monad.IO.Class
import qualified Data.List                  as L
import           Data.Maybe
import           Data.Monoid
import           Data.Traversable
import           Hakyll
import           Hakyll.Core.Rules.Internal
import           Text.Pandoc.Highlighting   (pygments)
import           Text.Pandoc.Options

deriving instance MonadIO Rules
instance Show Tags where
  show t = show $ tagsMap t

--------------------------------------------------------------------------------
main :: IO ()
main = hakyll $ do
    match "images/*" $ do
        route   idRoute
        compile copyFileCompiler

    match "css/*" $ do
        route   idRoute
        compile compressCssCompiler

    match (fromList ["about.markdown"]) $ do
        route   $ setExtension "html"
        compile $ myPandocCompiler
            >>= loadAndApplyTemplate "templates/default.html" defaultContext
            >>= relativizeUrls

    tags <- buildTags "posts/*" (fromCapture "tags/*.html")

    tagsRules tags $ \tag pt -> do
      route idRoute
      compile $ do
        let
          title = "Tag: " <> tag
          ctx = constField "title" title
            <> listField "posts" (postCtx tags) (recentFirst =<< loadAll pt)
            <> defaultContext
        makeItem ""
          >>= loadAndApplyTemplate "templates/index.html" ctx
          >>= loadAndApplyTemplate "templates/default.html" ctx
          >>= relativizeUrls

    create ["index.html"] $ do
      route idRoute
      compile $ do
        let
          ctx
            =  listField "posts" (postCtx tags) (recentFirst =<< loadAll "posts/*")
            <> constField "title" "Posts"
            <> defaultContext
        makeItem ""
          >>= loadAndApplyTemplate "templates/index.html" ctx
          >>= loadAndApplyTemplate "templates/default.html" ctx
          >>= relativizeUrls

    create [ "tags.html" ] $ do
      route idRoute
      compile $ do
        let
          tagItem tag url count _ _ =
            let co = "(" <> show count <> ")"
            in "<li><a href=\"" <> url <> "\">" <> tag <> co <> "</a></li>"
          joinItems titems =
            let inner = mconcat titems
            in "<ul>" <> inner <> "</ul>"
          ctx = constField "title" "Tags page"
            <> defaultContext
          byCount (n, a) (m, b) = case compare (length a) (length b) of
            GT -> LT
            LT -> GT
            EQ -> compare n m
        tagsHtml <- renderTags tagItem joinItems $ sortTagsBy byCount tags
        makeItem tagsHtml
          >>= loadAndApplyTemplate "templates/default.html" ctx
          >>= relativizeUrls

    match "posts/*" $ do
        route $ setExtension "html"
        compile $ myPandocCompiler
            >>= loadAndApplyTemplate "templates/post.html"    (postCtx tags)
            >>= loadAndApplyTemplate "templates/default.html" (postCtx tags)
            >>= relativizeUrls


    match "templates/*" $ compile templateBodyCompiler


--------------------------------------------------------------------------------
postCtx :: Tags -> Context String
postCtx tags
  =  tagsField "tags" tags
  <> dateField "date" "%Y-%m-%d"
  <> defaultContext

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
