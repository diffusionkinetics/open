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

wrap :: Monad m => SHtml m () -> SHtml m a -> SHtml m a
wrap hdr h = undefined {- doctypehtml_ $ do
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
-}
textInput :: Monad m => Lens' a Text -> SHtml m ()
textInput f = undefined {-do
  n <- fresh
  (_,val,_) <- lift $ get

  putFormField (n, lensSetter f)
  input_ [type_ "text", fieldName n, value_ (val ^. f)]
-}
select :: (Monad m, Eq b) => [(Text, b)] -> Lens' a b -> SHtml m ()
select opts f = undefined {-do
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
-}
numInput :: (Monad m, Num b, Show b, Read b) => Maybe b -> Maybe b -> Maybe b -> Lens' a b -> SHtml m ()
numInput mmin mmax mstep f = undefined {- do
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
-}
manualSubmit :: Monad m => SHtml m ()
manualSubmit = undefined {-do
  input_ [type_ "submit", value_ "Submit"]
  script_ "var manual_submit = true;"
-}
submitPeriodic :: Monad m => Int -> SHtml m ()
submitPeriodic delaySecs = undefined {- do
  let delayMs = pack $ show $ delaySecs*1000
  input_ [type_ "hidden", class_ "dashdo-periodic-submit", value_ delayMs]
-}
checkbox :: (Monad m, Eq b) => Text -> b -> b -> Lens' a b -> SHtml m ()
checkbox text vTrue vFalse f = undefined {- do
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
-}
resetLink :: Monad m => SHtml m ()
resetLink = undefined {-do
  a_ [href_ "#", class_"dashdo-resetlink"] "reset"
-}
plotlySelect :: Monad m => Plotly -> Lens' a Text -> SHtml m ()
plotlySelect plot f = undefined {-do
  (val, n) <- freshAndValue
  putFormField (n, lensSetter f)
  div_ [class_ "dashdo-plotly-select"] $ do
    toHtml plot
    resetLink
    input_ [type_ "hidden", fieldName n, value_ (val ^. f)]
    input_ [type_ "hidden", class_ "dashdo-plotly-select-attr", value_ attr]
-}
plotlySelectMultiple :: Monad m => Plotly -> Lens' a [Text] -> SHtml m ()
plotlySelectMultiple plot f = undefined {-do
  (val, n) <- freshAndValue
  putFormField (n, lensPusher f)
  div_ [class_ "dashdo-plotly-select"] $ do
    toHtml plot
    resetLink
    input_ [type_ "hidden", class_ "dashdo-plotly-multi-select-names", value_ $ mkFieldNameMultiple n]
    forM_ (val ^. f) $ \(v) ->
      input_ [type_ "hidden", fieldNameMultiple n, value_ v]
-}

(~>) :: Monad m => SimpleGetter t b -> (b -> Html ()) -> SHtml m t
g ~> f = undefined {- do
  (_,v,_) <- lift get
  toHtml $ f $ v ^. g
-}
toHtmls :: (Monad m, ToHtml b) => SimpleGetter t b -> SHtml m t
toHtmls g = undefined -- g ~> toHtml

(#>) :: Monad m => SimpleGetter t b -> SHtml m () -> SHtml m t
g #> f = undefined