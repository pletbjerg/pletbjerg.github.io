{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE LambdaCase #-}
{-# LANGUAGE DerivingStrategies #-}
{-# LANGUAGE GeneralizedNewtypeDeriving #-}
module Main where

-- Prelude / base
import Control.Arrow
import Control.Monad
import Control.Monad.Except
import Data.Functor
import Data.Maybe 
import Data.List 
import Data.Ord 
import Data.Foldable 
import Data.Traversable 
import System.Environment
import Control.Exception
import Debug.Trace

-- shake
import Development.Shake
import Development.Shake.FilePath

-- Text / bytestring
import Data.Text (Text)
import qualified Data.Text as Text

import Data.ByteString (ByteString)
import qualified Data.ByteString as ByteString

-- Pandoc
import Text.Pandoc 
    ( Pandoc (..)
    , MetaValue (..)
    , ReaderOptions (..)
    , WriterOptions (..)
    , Template
    , PandocError
    , PandocMonad
    )
import Text.Pandoc.Options ( HTMLMathMethod (MathJax), CiteMethod (Citeproc) )
import Text.Pandoc.Class ( PandocIO (unPandocIO), CommonState (..) )
import qualified Text.Pandoc.Templates as Pandoc
import qualified Text.Pandoc.Definition as Pandoc
import qualified Text.Pandoc.Options as Pandoc
import qualified Text.Pandoc.Class as Pandoc
import qualified Text.Pandoc.UTF8 as Pandoc
import qualified Text.Pandoc.Citeproc as Pandoc
import qualified Text.Pandoc.Highlighting as Pandoc
import qualified Text.Pandoc.Readers.LaTeX as Pandoc
import qualified Text.Pandoc.Readers.BibTeX as Pandoc
import qualified Text.Pandoc.Writers as Pandoc
import qualified Text.Pandoc.Shared as Pandoc
import qualified Text.Pandoc.Writers.Shared as Pandoc
import qualified Text.Pandoc.Error as Pandoc

-- Pandocs templates
import qualified Text.DocTemplates as DocTemplates
import qualified Text.DocLayout as DocLayout

-- Monads
import Control.Monad.Except (ExceptT, MonadError)
import qualified Control.Monad.Except as Except
import Control.Monad.State (StateT, MonadState)
import qualified Control.Monad.State as State
import Control.Monad.Reader (ReaderT, MonadReader)
import qualified Control.Monad.Reader as Reader
import qualified Control.Monad.Trans as Trans
-- TODO: perhaps file an issue on shake s.t. they include these instances in
-- the 'Action' monad
-- import Control.Monad.Catch (MonadCatch, MonadMask, MonadThrow)
-- import qualified Control.Monad.Catch as Catch

-- Json
import Data.Aeson (Value (..))
import qualified Data.Aeson as Aeson
import qualified Data.Aeson.KeyMap as Aeson

-- Time / date parsing
import Data.Time (Day)
import qualified Data.Time as Time

-- * Pandoc reader / writer wrappers

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
    liftActionToTemplateAction $ need [templatePath]
    templateContents <- liftIO $ Pandoc.runIOorExplode $ Pandoc.getTemplate templatePath 
    eitherTemplate <- Pandoc.compileTemplate templatePath templateContents 
    liftIO $ liftEither $ Pandoc.mapLeft userError eitherTemplate

-- | @'readLaTeXWithBib' readerOpts filePath bibsFilePaths@ reads the LaTeX
-- file @filePath@ alongside with all provided
-- bibliography files @bibsFilePaths@; and converts it into the corresponding
-- internal 'Pandoc' representation.
--
-- N.B. this automatically calls 'need' on all the given 'FilePath'
readLaTeXWithBib :: 
    -- | pandoc reader options
    ReaderOptions -> 
    -- | filepath to LaTeX file to compile
    FilePath -> 
    -- | list of *.bib files
    [FilePath] -> 
    Action Pandoc
readLaTeXWithBib readerOpts filePath bibsFilePaths = do
    need $ [filePath] ++ bibsFilePaths
    let texDir = dropFileName filePath
        -- reading the source code, @TEXINPUTS@ is the environment variable of
        -- colon (":") seperated direectories that pandocs goes looking for when
        -- searching for @\input{<file>}@, etc from a LaTeX file.
        -- So, we add the current directory of the thing we wish to compile
        -- there.
        magicTEXINPUTSEnvVar = "TEXINPUTS"
        newEnvironment = [(magicTEXINPUTSEnvVar, Text.pack texDir)]
    environment <- fmap (map (Text.pack *** Text.pack)) $ liftIO getEnvironment 
    -- TODO: perhaps instead of overriding the @TEXINPUTS@ environment variable
    -- (internally, this uses 'lookup' which finds the _first_ key value pair),
    -- we should really augment the current environment if it exists.
    runPandocShakeWithEnvOrExplode (newEnvironment ++ environment) $ do
        -- Include the directory of @filePath@ in the resource lookup path
        Pandoc.modifyCommonState $ \commonState -> 
            commonState { stResourcePath = stResourcePath commonState ++ [texDir]}

        -- compile the LaTeX document to Pandoc
        fileContents <- liftIO $ Pandoc.readFile filePath
        pandoc <- Pandoc.readLaTeX readerOpts fileContents

        -- compile the bib to Pandoc (note this only contains the bibilography
        -- metadata
        bib <- fmap fold $ for bibsFilePaths $ \bibFilePath -> do
            bibFileContents <- liftIO $ Pandoc.readFile bibFilePath
            Pandoc.readBibTeX readerOpts bibFileContents

        -- add the biblographies metadata to the @pandoc@, and ensure that we
        -- have the citations printed out in the body with the CiteProcfilter
        let pandoc' = pandoc <> bib :: Pandoc
        pandoc'' <- Pandoc.processCitations pandoc'

        return pandoc''

-- * Main
main :: IO ()
main = shakeArgs shakeOptions{shakeFiles = "docs"} $ do 
    -- an alias to clean up the entire website
    phony "clean" $ removeFilesAfter "docs" ["//*"]

    -- an alias to deploy (i.e,. push the docs)
    phony "deploy" $ 
        let deployCmds :: [String]
            deployCmds =
                [ "git add docs/*"
                , "git commit -m 'Deployment'"
                , "git push"
                ]
        in traverse_ cmd_ deployCmds

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

        -- Assert that we need the HTML posts
        need $ htmlPostPaths

        let templatePath = "templates/index.html"

        template <- compileTemplate templatePath

        pandocPosts <- liftIO 
            $ traverse 
                (liftEither . Pandoc.mapLeft userError <=< Aeson.eitherDecodeFileStrict) 
                pandocPostPaths 

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
                    --      3. the @date@
                    flip (Pandoc.defField "archive") (writerVariables defaultWriterOpts) 
                    $ sortOn (Down . onDate)
                    $ map (first dropDirectory1) (zip htmlPostPaths pandocPosts) <&> 
                        \(htmlPostPath, Pandoc meta _blocks) -> 
                            Aeson.object 
                            [ "title" Aeson..= 
                                ( Aeson.toJSON $ fromMaybe  "Untitled" $ 
                                    fmap Pandoc.stringify $ Pandoc.lookupMeta "title" meta
                                )
                            , "date" Aeson..= 
                                ( Aeson.toJSON $ fromMaybe  "undated" $ 
                                    fmap Pandoc.stringify $ Pandoc.lookupMeta "date" meta
                                )
                            , "link" Aeson..= (Aeson.toJSON htmlPostPath :: Aeson.Value)
                            ]
                }

            -- To sort lexicographically by @undated < year < month < day@
            onDate :: Value -> Maybe Day
            onDate = \case 
                Object keyMap -> Aeson.lookup "date" keyMap >>= \case
                    String d -> 
                        Time.parseTimeM 
                            True 
                            Time.defaultTimeLocale 
                            "%B %_d, %_Y" 
                             -- Some unfortunate type tricks to get the
                             -- @[Word8]@ type to the @String@ type
                            $ map (toEnum . fromIntegral)
                            $ ByteString.unpack (Pandoc.fromText d)
                    _ -> Nothing
                _ -> Nothing

        -- create the pandoc representation
        pandocHtml <- readLaTeXWithBib readerOpts indexTexPath []

        -- write out the tex page
        indexHtml <- liftIO $ Pandoc.runIOorExplode $ Pandoc.writeHtml5String writerOpts pandocHtml

        void $ liftIO $ Pandoc.writeFile indexHtmlPath indexHtml

    -- Building the intermediate 'Pandoc' representation from LaTeX files.
    -- Note: this also grabs all the bibliography files in the _same_
    -- directory to include as metadata in the Pandoc type.
    "docs/posts/*/*.json" %> \jsonPostPath -> do
        let texPostPath = dropDirectory1 jsonPostPath -<.> "tex"

        -- grab the bib files in the same directory as the tex file we are
        -- compiling.
        bibsFilePaths <- getDirectoryFiles ""  [dropFileName texPostPath </> "*.bib"]

        -- read the LaTeX file
        let readerOpts = defaultReaderOpts
        pandoc <- readLaTeXWithBib readerOpts texPostPath bibsFilePaths

        -- write the file out
        liftIO $ void $ Aeson.encodeFile jsonPostPath $ Aeson.toJSON pandoc

    -- Building the HTML page for *posts* from the intermediate 'Pandoc'
    -- representation. 
    "docs/posts/*/*.html" %> \htmlPostPath -> do
        let 
            jsonPostPath = htmlPostPath -<.> "json"
            templatePath = "templates/post.html"

        -- ensure we have the json intermediate representation
        need [jsonPostPath]
        pandoc <- liftIO $ Aeson.eitherDecodeFileStrict' jsonPostPath >>= \eitherJson -> case eitherJson of
            Left err -> throwIO $ userError err
            Right pandoc -> return (pandoc :: Pandoc)

        -- grab the template
        template <- compileTemplate templatePath

        -- create the HTML from the pandoc string
        let writerOpts = defaultWriterOpts
                { writerTemplate = Just template
                }
        htmlPost <- liftIO $ Pandoc.runIOorExplode $ Pandoc.writeHtml5String writerOpts pandoc

        void $ liftIO $ Pandoc.writeFile htmlPostPath htmlPost

-- * Instances

-- | 'PandocShake' is essentially 'PandocIO' with the following differences
--
--      - shake's 'Action' monad is at the bottom of the monad transformer
--      stack instead of IO
--
--      - 'lookupEnv' lookups environment variables in the provided environment
--      instead of consulting the process's environment variable. This is because 
newtype PandocShake a = PandocShake 
    { unPandocShake :: 
        ExceptT PandocError (ReaderT [(Text,Text)] (StateT CommonState Action)) a
    } deriving
        ( MonadIO
        , Functor
        , Applicative
        , Monad
        , MonadError PandocError
        )

-- | @'runPandocShakeWithEnvOrExplode' env pandocShake @ runs the 
-- @pandocShake :: 'PandocShake'@ with @env@ as the environment variables, and
-- returns the resulting shake 'Action'.
runPandocShakeWithEnvOrExplode :: [(Text, Text)] -> PandocShake a -> Action a
runPandocShakeWithEnvOrExplode env pandocShake = do
    result <- State.evalStateT (Reader.runReaderT (Except.runExceptT (unPandocShake pandocShake)) env) Pandoc.def 
    liftIO $ Pandoc.handleError result

instance PandocMonad PandocShake where
    lookupEnv envVar = PandocShake 
        $ Trans.lift 
        $ Reader.asks 
        $ lookup envVar
    getCurrentTime = liftPandocIOToPandocShake Pandoc.getCurrentTime
    getCurrentTimeZone = liftPandocIOToPandocShake Pandoc.getCurrentTimeZone
    newStdGen = liftPandocIOToPandocShake Pandoc.newStdGen
    newUniqueHash = liftPandocIOToPandocShake Pandoc.newUniqueHash
    openURL = liftPandocIOToPandocShake . Pandoc.openURL

    readFileStrict filePath = do
        file <- liftPandocIOToPandocShake $ Pandoc.readFileStrict filePath
        -- Note [Why we 'need' after reading the file]
        -- Internally, in 'Text.Pandoc.Class.PandocMonad' there are functions
        -- like 'Text.Pandoc.Class.PandocMonad.readFileFromDirs' which _rely_
        -- on the fact that 'Text.Pandoc.readFileStrict' throws exceptions when
        -- a file is not found.
        -- This is quite unfortunate as this is essentially contrary to the
        -- functionality that shake provides.
        --
        -- TODO: perhaps this is worth filing an issue in pandocs to move this
        -- functionality out?
        liftActionToPandocShake $ need [filePath]
        return file
    readFileLazy filePath = do
        file <- liftPandocIOToPandocShake $ Pandoc.readFileLazy filePath
        -- See: Note [Why we 'need' after reading the file]
        liftActionToPandocShake $ need [filePath]
        return file

    readStdinStrict = liftPandocIOToPandocShake $ Pandoc.readStdinStrict
    glob globPattern = do
        files <- liftPandocIOToPandocShake $ Pandoc.glob globPattern
        liftActionToPandocShake $ need files
        return files
    fileExists filePath = do
        -- TODO: is this right? if it doesn't exist, then shake will probably
        -- complain about it before pandoc can get mad
        liftActionToPandocShake $ need [filePath]
        liftPandocIOToPandocShake $ Pandoc.fileExists filePath
    getDataFileName filePath = do
        -- TODO: is this right? do we 'need' the path given OR the path after
        -- it is given? this needs to be investigated more..
        filePath' <- liftPandocIOToPandocShake $ Pandoc.getDataFileName filePath
        liftActionToPandocShake $ need [filePath']
        return filePath'
    getModificationTime filePath = do
        liftActionToPandocShake $ need [filePath]
        utcTime <- liftPandocIOToPandocShake $ Pandoc.getModificationTime filePath
        return utcTime
    getCommonState = liftPandocIOToPandocShake $ Pandoc.getCommonState
    putCommonState st = liftPandocIOToPandocShake $ Pandoc.putCommonState st
    logOutput = liftPandocIOToPandocShake . Pandoc.logOutput

-- | lifts a 'Action' into a 'PandocShake'
liftActionToPandocShake :: Action a -> PandocShake a
liftActionToPandocShake ma = PandocShake $ Trans.lift $ Trans.lift $ Trans.lift ma

-- | lifts a 'PandocIO' into a 'PandocShake'
liftPandocIOToPandocShake :: PandocIO a -> PandocShake a 
liftPandocIOToPandocShake ma = PandocShake $ do
    st <- Trans.lift $ Trans.lift State.get
    (a, st') <- liftIO $ State.runStateT (Except.runExceptT (unPandocIO ma)) st
    Trans.lift $ Trans.lift $ State.put st'
    Except.liftEither a

-- | 'TemplateAction' is a newtype wrapper around 'Action' to provide and
-- instance for 'DocTemplates.TemplateMonad' which automatically will 'need' partial
-- templates from pandocs.
newtype TemplateAction a = TemplateAction { runTemplateAction :: Action a }
  deriving (Functor, Applicative, Monad, MonadIO)

-- | 'liftActionToTemplateAction' lifts an 'Action' into a 'TemplateAction'
liftActionToTemplateAction :: Action a -> TemplateAction a
liftActionToTemplateAction = TemplateAction

instance DocTemplates.TemplateMonad TemplateAction where
    getPartial filePath = liftActionToTemplateAction $ need [filePath] >> liftIO (Pandoc.readFile filePath)
