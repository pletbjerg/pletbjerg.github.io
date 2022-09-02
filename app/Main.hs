{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE LambdaCase #-}
{-# LANGUAGE DerivingStrategies #-}
{-# LANGUAGE GeneralizedNewtypeDeriving #-}
{-# LANGUAGE TupleSections #-}
{-# LANGUAGE UndecidableInstances #-}
module Main where

import Hakyll 
    ( Configuration (deployCommand, destinationDirectory)
    , Compiler
    , Context
    , Writable
    , Item (itemIdentifier, itemBody)
    , Identifier
    , Snapshot
    )
import Control.Monad
import qualified Hakyll
import Text.Pandoc (Pandoc, ReaderOptions (..), WriterOptions (..), HTMLMathMethod (..), Template)
import qualified Text.Pandoc as Pandoc
import qualified Text.Pandoc.Readers as Readers
import qualified Text.Pandoc.Writers as Writers
import qualified Text.Pandoc.UTF8 as UTF8
import qualified Text.Pandoc.Options as Options
import Data.ByteString (ByteString)
import Data.Text (Text)
import qualified Data.Text as Text
import qualified Text.Pandoc.Highlighting as Highlighting
import qualified Text.Pandoc.Templates as Templates
import qualified System.IO.Unsafe as Unsafe
import Text.DocTemplates (ToContext)
import Text.DocTemplates.Internal (Variable, Pipe, Alignment, Border, TemplateTarget, TemplateMonad (getPartial))
import Data.Binary (Binary)
import Text.DocLayout (Doc)
import Data.Typeable (Typeable)
import Text.Pandoc.Definition (Meta,Block,MetaValue,Inline,QuoteType, Format, Citation, ListNumberStyle, CitationMode, MathType, ListNumberDelim,Caption,Alignment, ColWidth,TableHead, Row, TableBody, RowHeadColumns, TableFoot, Cell, RowSpan, ColSpan)

import Debug.Trace 


-- * Constants 

-- | Configuration for the main hakyll call.
conf :: Configuration 
conf = Hakyll.defaultConfiguration
    { deployCommand = "git add docs && git commit -m 'Deployment' && git push"
    , destinationDirectory = "docs/"
    }


-- * Post compilation
-- | 'getResourceBodyText' gets the resource body as 'Text'.
getResourceBodyText :: Compiler (Item Text)
getResourceBodyText = fmap (fmap Text.pack) Hakyll.getResourceBody

-- | 'pandocTemplateBodyCompilerTextSnapshot' is the 'Snapshot' used internally
-- for partial templates in pandoc to work in the 'Compiler' monad.
pandocTemplateBodyCompilerTextSnapshot :: Snapshot
pandocTemplateBodyCompilerTextSnapshot = "pandocTemplateBodyCompilerTextSnapshot"

-- | 'pandocTemplateBodyCompiler' reads a template (without the metadata header
-- from Hakyll)
pandocTemplateBodyCompiler :: Compiler (Item (Template Text))
pandocTemplateBodyCompiler = do
    item <-  getResourceBodyText >>= Hakyll.saveSnapshot pandocTemplateBodyCompilerTextSnapshot
    file <- Hakyll.getResourceFilePath
    Hakyll.cached "pandocTemplateBodyCompiler-template" $ Hakyll.withItemBody (go file) item 
  where
    go :: FilePath -> Text -> Compiler (Template Text)
    go fp item = Templates.compileTemplate fp item >>= \case
        Left err -> fail $ "error 'pandocTemplateBodyCompiler' could not compile template: " ++ err
        Right tp -> return tp

-- | @'postCompilerWithPandocTemplate' template@ compiles a post with the provided
-- template. The template must be previously loaded via 'pandocTemplateBodyCompiler'
postCompilerWithPandocTemplate :: 
    -- | 'Template' to use
    Template Text -> 
    -- | resulting post
    Compiler (Item String)
postCompilerWithPandocTemplate template = Hakyll.getResourceBody >>= \item -> 
    let mPandoc = 
            let readerOpts :: ReaderOptions
                readerOpts = Options.def
                    { readerStandalone = True
                    }

                writerOpts :: WriterOptions
                writerOpts = Options.def
                    { writerHTMLMathMethod = MathJax Options.defaultMathJaxURL
                    , writerHighlightStyle = Just Highlighting.haddock
                    , writerTemplate = Just template
                    , writerReferenceLinks = True
                    }
            in Readers.readLaTeX readerOpts (Text.pack $ itemBody item) >>= 
                \pandoc -> (pandoc,) <$> Writers.writeHtml5String writerOpts pandoc 
    in case Pandoc.runPure mPandoc of
        Left err -> fail $ "'postCompilerWithPandocTemplate' failed with: " ++ show err
        Right (pandoc, res) -> do
            traceShowM pandoc
            Hakyll.saveSnapshot pandocSnapshot (Hakyll.itemSetBody pandoc item)
            Hakyll.relativizeUrls $ Hakyll.itemSetBody (Text.unpack res) item 

pandocSnapshot :: Snapshot
pandocSnapshot = "pandoc"

-- * Main function
-- | Entry point of program
main :: IO ()
main = Hakyll.hakyllWith conf $ do
    -- templates.
    Hakyll.match "templates/*" $ do
        Hakyll.compile pandocTemplateBodyCompiler
 
    -- posts.
    Hakyll.match "posts/**/*.tex" $ do
        Hakyll.route $ Hakyll.setExtension "html"
        Hakyll.compile $ Hakyll.loadBody "templates/post.html" 
            >>= \template -> postCompilerWithPandocTemplate template

    -- index.html
    Hakyll.create ["index.html"] $ do
        Hakyll.route $ Hakyll.constRoute "index.html"
        Hakyll.compile $ Hakyll.makeItem ("wahoo"  :: String)
    {-
    Hakyll.create ["archive.html"] $ do
        Hakyll.route Hakyll.idRoute
    -}

-- * Orphan instances
instance Binary a => Binary (Template a) where
instance Binary a => Binary (Doc a) where
instance Binary Variable where
instance Binary Pipe where
instance Binary Text.DocTemplates.Internal.Alignment where
instance Binary Border where

instance Writable (Template a) where
    -- writing a template is impossible
    write _ _ = return ()
instance Writable Text where
    write fp = UTF8.writeFile fp . itemBody
instance TemplateMonad Compiler where
    getPartial = fmap itemBody . flip Hakyll.loadSnapshot pandocTemplateBodyCompilerTextSnapshot . Hakyll.fromFilePath 

instance Binary Pandoc where
instance Binary Meta where
instance Binary Block where
instance Binary MetaValue where
instance Binary Inline where
instance Binary QuoteType where
instance Binary Format where
instance Binary Citation where
instance Binary ListNumberStyle where
instance Binary CitationMode where
instance Binary MathType where
instance Binary ListNumberDelim where
instance Binary Caption where
instance Binary Text.Pandoc.Definition.Alignment where
instance Binary ColWidth where
instance Binary TableHead where
instance Binary Row where
instance Binary TableBody where
instance Binary RowHeadColumns where 
instance Binary TableFoot where 
instance Binary Cell where
instance Binary RowSpan where
instance Binary ColSpan where
