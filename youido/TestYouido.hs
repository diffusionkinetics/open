{-# LANGUAGE OverloadedStrings #-}

module Main (main) where

import Youido.Serve
import Youido.Types
import Youido.Dashdo
import Lucid

main :: IO ()
main = do
  ddH <- dashdoGlobal
  serve () (ddH:hs)

hs = [] -- [H $ \()-> return ("you hit the default!"::Html ())]
