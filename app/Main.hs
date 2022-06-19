{-# LANGUAGE OverloadedStrings #-}
module Main where

import Hakyll

main :: IO ()
main = hakyll $ do
    create ["archive.html"] $ do
        route idRoute
    
