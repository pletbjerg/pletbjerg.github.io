{-# LANGUAGE OverloadedStrings #-}
module Main where

import Hakyll 
    ( Configuration (deployCommand, destinationDirectory)
    )
import qualified Hakyll

-- | Configuration for the main hakyll call.
conf :: Configuration 
conf = Hakyll.defaultConfiguration
    { deployCommand = "git add docs && git commit 'Deployment' && git push"
    , destinationDirectory = "docs/"
    }

-- | main function
main :: IO ()
main = Hakyll.hakyllWith conf $ do
    -- posts.
    Hakyll.match "posts/**/*.tex" $ do
        Hakyll.route $ Hakyll.setExtension "html"
        Hakyll.compile $ Hakyll.pandocCompiler 

    -- index.html
    Hakyll.create ["index.html"] $ do
        Hakyll.route $ Hakyll.constRoute "index.html"
        Hakyll.compile $ Hakyll.makeItem ("wahoo"  :: String)


            
    {-
    Hakyll.create ["archive.html"] $ do
        Hakyll.route Hakyll.idRoute
    -}
    
