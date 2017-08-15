{-# LANGUAGE OverloadedStrings, ExistentialQuantification, ExtendedDefaultRules, FlexibleContexts, Rank2Types, TemplateHaskell, ScopedTypeVariables #-}

module Dashdo where

import qualified Data.Text.Lazy as TL
import Dashdo.Types
import Data.Monoid ((<>))

pureDashdo :: a -> (a -> SHtml a ()) -> Dashdo a
pureDashdo ini f = Dashdo ini (const (return ())) (\x () -> f x)


dashdoGenOut :: Dashdo a -> a -> IO (TL.Text, FormFields a)
dashdoGenOut (Dashdo _ f r) x = do
  y <- f x
  let (formFs, htmlText) = runSHtml x $ r x y
  return (htmlText, formFs)

parseForm :: a -> FormFields a -> [(TL.Text, TL.Text)] -> a
parseForm x [] _ = x
parseForm x ((fnm,f):nfs) pars =                  -- x=initial d (accumulator)
  let fldName = TL.fromStrict fnm         -- fn
      newx = case lookup fldName pars of        -- looking for fn in params [(TL.Text, TL.Text)]
               Just lt -> f x (TL.toStrict lt)  -- apply function corresponding to `n` in list of FormFields - number-function pairs
               Nothing -> case filter ((== fldName <> "[]") . fst) pars of -- if nothing found, try looking for fn[]
                 [] -> x
                 listParams -> foldl f x (map (TL.toStrict . snd) listParams)
  in parseForm newx nfs pars
