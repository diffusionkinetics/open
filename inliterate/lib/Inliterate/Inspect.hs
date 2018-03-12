{-# LANGUAGE OverloadedStrings #-}

module Inliterate.Inspect where

import Cheapskate
import Cheapskate.Html
import qualified Data.Text as T
import qualified Data.Text.Lazy as TL
import qualified Data.Text.IO as T
import Data.Foldable (toList)
import Text.Blaze.Html.Renderer.Text (renderHtml)



readDoc :: FilePath -> IO Doc
readDoc fp = do
  t <- T.readFile fp
  return $ markdown def t


inspect :: FilePath -> IO (T.Text, T.Text)
inspect fp = do
  d <- readDoc fp

  return $ getTitleFirstP d

getTitleFirstP :: Doc -> (T.Text, T.Text)
getTitleFirstP (Doc _ sblocks) = go allBlocks where
  allBlocks = toList sblocks
  go (Header _ inls : rest ) = (T.concat $ map inlineToText $ toList inls, goPara rest)
  go (_ : blocks ) = go blocks
  go [] = ("No Title", goPara allBlocks)
  goPara (Para inls : _ )  = TL.toStrict $ renderHtml $ renderInlines def inls
  goPara (_:rest) = goPara rest
  goPara [] = ""

--strip formatting
inlineToText :: Inline -> T.Text
inlineToText (Str s) = s
inlineToText Space = " "
inlineToText SoftBreak = " " -- ??
inlineToText LineBreak = " "
inlineToText (Emph inls) = T.concat $ map inlineToText $ toList inls
inlineToText (Strong inls) = T.concat $ map inlineToText $ toList inls
inlineToText n = error $ "inlineToText: unsupported type "++show n
