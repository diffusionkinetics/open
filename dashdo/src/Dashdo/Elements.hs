{-# LANGUAGE OverloadedStrings, ExistentialQuantification, ExtendedDefaultRules, FlexibleContexts, Rank2Types, TemplateHaskell #-}

module Dashdo.Elements where

import Dashdo.Types

import Lucid
import Lucid.Bootstrap
import Lucid.Bootstrap3
import Graphics.Plotly (Plotly)
import Graphics.Plotly.Lucid
import Data.Text (Text, unpack, pack, breakOn, tail)
import Control.Monad.RWS.Strict
import Text.Read (readMaybe)
import Lens.Micro
import Lens.Micro.TH
import Data.Monoid ((<>))
import Prelude hiding (tail)
import Data.Aeson

data Tag a = Tag { _tagText :: Text, _tagVal :: a }

makeLenses ''Tag

instance Eq (Tag a) where
  Tag x _ == Tag y _ = x == y

tagOpt :: Text -> a -> (Text, Tag a)
tagOpt nm x = (nm, Tag nm x)

strOpt :: Text -> (Text, Text)
strOpt t = (t,t)

showOpt :: Show a => a -> (Text, a)
showOpt x = (pack $ show x, x)

wrap :: SHtml a () -> SHtml a () ->  SHtml a ()
wrap hdr h =  doctypehtml_ $ do
  head_ $ do
    meta_ [charset_ "utf-8"]
    cdnCSS
    cdnThemeCSS
    cdnJqueryJS
    hdr

  body_ $ do
    container_ $ form_ [action_ "/", method_ "post", id_ "dashdoform"] $ do
      h
    cdnBootstrapJS
    script_ [src_ "/js/dashdo.js"] ""
    script_ [src_ "/js/runners/base.js"] ""

textInput :: Lens' a Text -> SHtml a ()
textInput f = do
  n <- fresh
  (_,val,_) <- lift $ get

  putFormField (n, lensSetter f)
  input_ [type_ "text", fieldName n, value_ (val ^. f)]

select :: Eq b => [(Text, b)] -> Lens' a b -> SHtml a ()
select opts f = do
  (val,n) <- freshAndValue
  let ft s t = case lookup t opts of
                 Nothing -> s
                 Just x -> lensSetter f s x
  putFormField (n, ft)
  select_ [fieldName n] $ do
    forM_ opts $ \(optNm, optVal) ->
      if val ^. f == optVal
         then option_ [value_ optNm, selected_ ""] $ toHtml optNm
         else option_ [value_ optNm] $ toHtml optNm

numInput :: (Num b, Show b, Read b) => Maybe b -> Maybe b -> Maybe b -> Lens' a b -> SHtml a ()
numInput mmin mmax mstep f = do
  (val,n) <- freshAndValue
  let amin = maybe [] ((:[]) . min_ . pack . show) mmin
      amax = maybe [] ((:[]) . max_ . pack . show) mmax
      astep = maybe [] ((:[]) . step_ . pack . show) mstep
      ft s t = case readMaybe $ unpack t of
                 Nothing -> s
                 Just x -> lensSetter f s x
  putFormField (n, ft)
  input_ (amin  ++ amax ++ astep ++ [type_ "number",
                                     fieldName n,
                                     value_ (pack . show $ val ^. f) ])

manualSubmit :: SHtml a ()
manualSubmit = do
  input_ [type_ "submit", value_ "Submit"]
  script_ "var manual_submit = true;"

submitPeriodic :: Int -> SHtml a ()
submitPeriodic delaySecs = do
  let delayMs = pack $ show $ delaySecs*1000
  input_ [type_ "hidden", class_ "dashdo-periodic-submit", value_ delayMs]

checkbox :: Eq b => Text -> b -> b -> Lens' a b -> SHtml a ()
checkbox text vTrue vFalse f = do
  (val, n) <- freshAndValue
  let ft s t = case t of
                "true" -> lensSetter f s vTrue
                _      -> lensSetter f s vFalse
      fid = "id" <> pack (show n)
      checked = if val ^. f == vTrue then [checked_] else []
  putFormField (n, ft)
  div_ [class_ "checkbox"] $
    label_ $ do
      input_ $ [type_ "checkbox", id_ fid, fieldName n, value_ "true"] ++ checked
      toHtml text
  -- if checkbox doesn't supply a value we get this one instead
  input_ [type_ "hidden", fieldName n, value_ "false"]

plotlySelect :: Plotly -> Text -> Lens' a Text -> SHtml a ()
plotlySelect plot attr f = do
  (val, n) <- freshAndValue
  putFormField (n, lensSetter f)
  div_ [class_ "dashdo-plotly-select"] $ do
    toHtml plot
    input_ [type_ "hidden", fieldName n, value_ (val ^. f)]
    input_ [type_ "hidden", class_ "dashdo-plotly-select-attr", value_ attr]
