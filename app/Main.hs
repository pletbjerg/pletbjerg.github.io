{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE LambdaCase #-}
{-# LANGUAGE DerivingStrategies #-}
{-# LANGUAGE GeneralizedNewtypeDeriving #-}
module Main where

import Development.Shake
import Development.Shake.FilePath

import Control.Arrow
import Control.Monad
import Control.Monad.Except
import Data.Functor
import Data.Maybe 

import Data.Text (Text)

import Text.Pandoc (Pandoc (..), MetaValue (..), ReaderOptions (..), WriterOptions (..), Template)
import Text.Pandoc.Options ( HTMLMathMethod (MathJax))
import qualified Text.Pandoc.Templates as Pandoc
import qualified Text.Pandoc.Definition as Pandoc
import qualified Text.Pandoc.Options as Pandoc
import qualified Text.Pandoc.Class as Pandoc
import qualified Text.Pandoc.UTF8 as Pandoc
import qualified Text.Pandoc.Highlighting as Pandoc
import qualified Text.Pandoc.Readers.LaTeX as Pandoc
import qualified Text.Pandoc.Writers as Pandoc
import qualified Text.Pandoc.Shared as Pandoc
import qualified Text.Pandoc.Writers.Shared as Pandoc

import qualified Text.DocTemplates as DocTemplates
import qualified Text.DocLayout as DocLayout

import qualified Data.Aeson as Aeson

import Debug.Trace

-- | 'defaultReaderOpts' are the default reader options for pandocs.
defaultReaderOpts :: ReaderOptions
defaultReaderOpts = Pandoc.def
    { readerStandalone = True
    }

-- | 'defaultWriterOpts' are the default writer options for pandocs
defaultWriterOpts :: WriterOptions
defaultWriterOpts = Pandoc.def
    { writerHTMLMathMethod = MathJax Pandoc.defaultMathJaxURL
    , writerHighlightStyle = Just Pandoc.pygments
    , writerReferenceLinks = True
    , writerNumberSections = True 
    -- Eh, we don't really need this I suppose.
    -- , writerTableOfContents = True 
    }

-- | 'compileTemplate' compiles the given template. N.B. you do not need to
-- 'need' the 'FilePath' given, and you do not need to 'need' any required
-- partial templates.
compileTemplate :: 
    -- | File path of the 'Template' to compile.
    FilePath -> 
    -- | Compiled 'Template'
    Action (Template Text)
compileTemplate templatePath = runTemplateAction $ do
    liftAction $ need [templatePath]
    templateContents <- liftIO $ Pandoc.runIOorExplode $ Pandoc.getTemplate templatePath 
    eitherTemplate <- Pandoc.compileTemplate templatePath templateContents 
    liftIO $ liftEither $ Pandoc.mapLeft userError eitherTemplate


-- | @'readLaTeXAndWriteHtml5String' readerOpts writerOpts filePath@ reads the
-- LaTeX file @filePath@; and converts it into the corresponding HTML5 string
-- via pandoc using the given @readerOpts@ and @writerOpts@ and returns the
-- internal 'Pandoc' representation.
--
-- N.B. this automatically calls 'need' on the given 'FilePath'
readLaTeXAndWriteHtml5String :: ReaderOptions -> WriterOptions -> FilePath -> Action (Text, Pandoc)
readLaTeXAndWriteHtml5String readerOpts writerOpts filePath = do
    need [filePath]
    liftIO $ Pandoc.runIOorExplode $ do
        fileContents <- liftIO $ Pandoc.readFile filePath
        pandoc <- Pandoc.readLaTeX readerOpts fileContents
        htmlPost <- Pandoc.writeHtml5String writerOpts pandoc
        return (htmlPost, pandoc)


main :: IO ()
main = shakeArgs shakeOptions{shakeFiles = "docs"} $ do 
    phony "clean" $ removeFilesAfter "docs" ["//*"]

    -- The homepage
    want ["docs/index.html"]

    -- Ensures that '.nojekyll' exists (as needed by Github)
    let noJekyll = "docs/.nojekyll"
    action $ doesFileExist noJekyll >>= \b -> 
        unless b $ command_  [] "touch" [noJekyll]

    -- Building the @docs/index.html@
    "docs/index.html" %> \indexHtmlPath -> do
        texPosts <- getDirectoryFiles "posts" ["*//*.tex"]
        let htmlPostPaths = ["docs" </> "posts" </> texPost -<.> "html" | texPost <- texPosts]
            pandocPostPaths = ["docs" </> "posts" </> texPost -<.> "json" | texPost <- texPosts]

        -- Assert that we need to both the HTML posts and the pandoc JSON
        -- representations.
        need $ htmlPostPaths ++ pandocPostPaths 

        let templatePath = "templates/index.html"

        template <- compileTemplate templatePath

        pandocPosts <- liftIO 
            $ traverse 
                (liftEither . Pandoc.mapLeft userError <=< Aeson.eitherDecodeFileStrict) 
                pandocPostPaths 

        -- TODO: we still need to add the date...
        let indexTexPath = "posts" </> dropDirectory1 indexHtmlPath -<.> "tex"
            readerOpts = defaultReaderOpts
            writerOpts = defaultWriterOpts
                { writerTemplate = Just template
                , writerNumberSections = False
                , writerVariables = 
                    -- Add the variables for the archive. We need to include
                    --
                    --      1. the @title@
                    --
                    --      2. the @link@ i.e., the relative URL which contains
                    --      the post.
                    --
                    --      3. the @date@ TODO it doesn't do this yet.
                    flip (Pandoc.defField "archive") (writerVariables defaultWriterOpts) $ 
                    map (first dropDirectory1) (zip htmlPostPaths pandocPosts) <&> 
                        \(htmlPostPath, Pandoc meta _blocks) -> 
                            Aeson.object 
                            [ "title" Aeson..= 
                                ( Aeson.toJSON $ fromMaybe  "Untitled" $ 
                                    fmap Pandoc.stringify $ Pandoc.lookupMeta "title" meta
                                )
                            , "link" Aeson..= (Aeson.toJSON htmlPostPath :: Aeson.Value)
                            ]
                }

        (indexHtml, _) <- readLaTeXAndWriteHtml5String readerOpts writerOpts indexTexPath

        void $ liftIO $ Pandoc.writeFile indexHtmlPath indexHtml

    -- Building the LaTeX to HTML page for *posts*, and saves the 'Pandoc'
    -- intermediate representation. 
    --
    -- Note we match on both the HTML and the pandoc JSON file.
    ["docs/posts/*/*.html", "docs/posts/*/*.json"] &%> \[htmlPostPath, jsonPostPath] -> do
        let texPostPath = dropDirectory1 htmlPostPath -<.> "tex"
            templatePath = "templates/post.html"

        template <- compileTemplate templatePath

        let readerOpts = defaultReaderOpts
            writerOpts = defaultWriterOpts
                { writerTemplate = Just template
                }

        (htmlPost, htmlPandoc) <- readLaTeXAndWriteHtml5String readerOpts writerOpts texPostPath

        void $ liftIO $ Pandoc.writeFile htmlPostPath htmlPost
        void $ liftIO $ Aeson.encodeFile jsonPostPath $ Aeson.toJSON htmlPandoc

-- * Instances

-- | 'TemplateAction' is a newtype wrapper around 'Action' to provide and
-- instance for 'DocTemplates.TemplateMonad' which automatically will 'need' partial
-- templates from pandocs.
newtype TemplateAction a = TemplateAction { runTemplateAction :: Action a }
  deriving (Functor, Applicative, Monad, MonadIO)

-- | 'liftAction' lifts an 'Action' into a 'TemplateAction'
liftAction :: Action a -> TemplateAction a
liftAction = TemplateAction

instance DocTemplates.TemplateMonad TemplateAction where
    getPartial filePath = liftAction $ need [filePath] >> liftIO (Pandoc.readFile filePath)
