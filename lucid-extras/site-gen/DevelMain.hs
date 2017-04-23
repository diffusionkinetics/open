{-# LANGUAGE OverloadedStrings #-}

module DevelMain where

import System.Directory

import Lucid.Rdash
import Lucid

html :: Html ()
html = indexPage

update :: IO ()
update = do
  createDirectoryIfMissing True "sample-site"
  renderToFile "sample-site/index.html" html
