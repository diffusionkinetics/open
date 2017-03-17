{-# LANGUAGE OverloadedStrings #-}
module Lucid.Bootstrap3 where

import Lucid
import Data.Char (toLower)
import Data.Text (pack)
infixr 0 $:

($:) :: (Monad m, ToHtml a) => (HtmlT m () -> HtmlT m ()) -> a -> HtmlT m ()
f $: x = f (toHtml x)



data Breakpoint = XS | SM | MD | LG deriving Show

rowEven :: Monad m => Breakpoint -> [HtmlT m ()] -> HtmlT m ()
rowEven bp cols = div_ [class_ "row"] $ do
  let ncols = length cols
      spans = 12 `div` ncols
      cls = pack $ concat ["col-", map toLower (show bp),"-",show spans]
  mapM_ (div_ [class_ cls]) cols
