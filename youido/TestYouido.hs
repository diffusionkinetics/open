{-# LANGUAGE OverloadedStrings #-}

module Main (main) where

import Youido.Serve
import Youido.Types
import Lucid

main :: IO ()
main = serve () hs

hs = [H $ \()-> return ("you hit the default!"::Html ())]
