{-# LANGUAGE OverloadedStrings, ExistentialQuantification, ExtendedDefaultRules, FlexibleContexts, Rank2Types, TemplateHaskell, ScopedTypeVariables #-}

module Dashdo where

import qualified Data.Text.Lazy as TL
import Data.List
import Dashdo.Types
import Data.Monoid ((<>))
import Data.Text (Text, pack)


dashdoGenOut :: Monad m
             => Dashdo m a
             -> a
             -> [(TL.Text, TL.Text)]
             -> m (TL.Text, FormFields a, [(Text, a -> m ())] )
dashdoGenOut (Dashdo _ r) x pars = runSHtml x r pars


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
{-parseForm x _ [] = x
parseForm x ffs pars@((k,v):parsTail) =
  let
    currentKeyIsList = "[]" `isSuffixOf` (TL.unpack k)

    lookupKey =
      if not currentKeyIsList
        then TL.toStrict k
        else TL.toStrict $ TL.dropEnd 2 k

    newx =
      case lookup lookupKey ffs of  -- TODO: will not find fn[]
        Just f  ->
          if not currentKeyIsList
            then
              f x $ TL.toStrict v
            else
              foldl f x (map (TL.toStrict . snd) $ filter ((==k) . fst) pars)
        Nothing -> x

    newparsTail =
      if not currentKeyIsList
        then parsTail
        else filter (not . (==k) . fst) parsTail -- we have used that key, will not use anymore

  in parseForm newx ffs newparsTail -}