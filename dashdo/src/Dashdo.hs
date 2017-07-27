{-# LANGUAGE OverloadedStrings, ExistentialQuantification, ExtendedDefaultRules, FlexibleContexts, Rank2Types, TemplateHaskell, ScopedTypeVariables #-}

module Dashdo where

import qualified Data.Text.Lazy as TL
import Dashdo.Types
import Data.Monoid ((<>))

pureDashdo :: Monad m => a -> (a -> SHtml m ()) -> Dashdo m a
pureDashdo ini f = undefined -- Dashdo ini (\x () -> f x)

dashdoGenOut :: Monad m => Dashdo m a -> a -> m (TL.Text, FormFields a)
dashdoGenOut (Dashdo _ r) val = do
  (formFs, htmlText) <- runSHtml val r
  return (htmlText, formFs)

parseForm :: a -> FormFields a -> [(TL.Text, TL.Text)] -> a
parseForm x [] _ = x
parseForm x ((n,f):nfs) pars =                  -- x=initial d (accumulator)
  let fldName = "f"<>TL.pack (show n)           -- fn
      newx = case lookup fldName pars of        -- looking for fn in params [(TL.Text, TL.Text)]
               Just lt -> f x (TL.toStrict lt)  -- apply function corresponding to `n` in list of FormFields - number-function pairs
               Nothing -> case filter ((== fldName <> "[]") . fst) pars of -- if nothing found, try looking for fn[]
                 [] -> x
                 listParams -> foldl f x (map (TL.toStrict . snd) listParams)
  in parseForm newx nfs pars
