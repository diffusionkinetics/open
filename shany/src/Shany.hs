{-# LANGUAGE OverloadedStrings, ExistentialQuantification, ExtendedDefaultRules, FlexibleContexts, Rank2Types, TemplateHaskell, ScopedTypeVariables #-}

module Shany where

import qualified Data.Text.Lazy as TL
import Shany.Types
import Data.Monoid ((<>))

pureShany :: a -> (a -> SHtml a ()) -> Shany a
pureShany ini f = Shany ini (const (return ())) (\x () -> f x)


shanyGenOut :: Shany a -> a -> IO (TL.Text, FormFields a)
shanyGenOut (Shany _ f r) x = do
  y <- f x
  let (formFs, htmlText) = runSHtml x $ r x y
  return (htmlText, formFs)

parseForm :: a -> FormFields a -> [(TL.Text, TL.Text)] -> a
parseForm x [] _ = x
parseForm x ((n,f):nfs) pars =
  let fldName = "f"<>TL.pack (show n)
      newx = case lookup fldName pars of
               Nothing -> x
               Just lt -> f x (TL.toStrict lt)
  in parseForm newx nfs pars
