{-# LANGUAGE OverloadedStrings, ExistentialQuantification, ExtendedDefaultRules, FlexibleContexts, Rank2Types, TemplateHaskell #-}

module Dashdo.Elements where

import Dashdo.Types

import Lucid
import Lucid.Bootstrap
import Lucid.Bootstrap3
import Graphics.Plotly (Plotly)
import Graphics.Plotly.Lucid
import Data.Text (Text, unpack, pack)
import Control.Monad.RWS.Strict
import Control.Monad.State.Strict
import Text.Read (readMaybe)
import Lens.Micro
import Lens.Micro.TH
import Data.Monoid ((<>))
import Prelude hiding (tail)

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

wrap :: Monad m => SHtml m a () -> SHtml m a () ->  SHtml m a ()
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

textInput :: Monad m => Lens' a Text -> SHtml m a ()
textInput f = do
  n <- fresh
  (_,val,_) <- lift $ get

  putFormField (n, lensSetter f)
  input_ [type_ "text", name_ n, value_ (val ^. f)]

select :: (Monad m, Eq b) => [(Text, b)] -> Lens' a b -> SHtml m a ()
select opts f = do
  (val,n) <- freshAndValue
  let ft s t = case lookup t opts of
                 Nothing -> s
                 Just x -> lensSetter f s x
  putFormField (n, ft)
  select_ [name_ n] $ do
    forM_ opts $ \(optNm, optVal) ->
      if val ^. f == optVal
         then option_ [value_ optNm, selected_ ""] $ toHtml optNm
         else option_ [value_ optNm] $ toHtml optNm

numInput :: (Num b, Show b, Read b, Monad m) => Maybe b -> Maybe b -> Maybe b -> Lens' a b -> SHtml m a ()
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
                                     name_ n,
                                     value_ (pack . show $ val ^. f) ])

manualSubmit :: Monad m => SHtml m a ()
manualSubmit = do
  input_ [type_ "submit", value_ "Submit"]
  script_ "var manual_submit = true;"

submitPeriodic :: Monad m => Int -> SHtml m a ()
submitPeriodic delaySecs = do
  let delayMs = pack $ show $ delaySecs*1000
  input_ [type_ "hidden", class_ "dashdo-periodic-submit", value_ delayMs]

checkbox :: (Monad m, Eq b) => Text -> b -> b -> Lens' a b -> SHtml m a ()
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
      input_ $ [type_ "checkbox", id_ fid, name_ n, value_ "true"] ++ checked
      toHtml text
  -- if checkbox doesn't supply a value we get this one instead
  input_ [type_ "hidden", name_ n, value_ "false"]

resetLink :: Monad m => SHtml m a ()
resetLink = do
  a_ [href_ "#", class_"dashdo-resetlink"] "reset"

plotlySelect :: Monad m => Plotly -> Lens' a Text -> SHtml m a ()
plotlySelect plot f = do
  (val, n) <- freshAndValue
  putFormField (n, lensSetter f)
  div_ [class_ "dashdo-plotly-select"] $ do
    toHtml plot
    resetLink
    input_ [type_ "hidden", name_ n, value_ (val ^. f)]

plotlySelectMultiple :: Monad m => Plotly -> Lens' a [Text] -> SHtml m a ()
plotlySelectMultiple plot f = do
  (val, n) <- freshAndValue
  putFormField (n, lensPusher f)
  div_ [class_ "dashdo-plotly-select"] $ do
    toHtml plot
    resetLink
    input_ [type_ "hidden", class_ "dashdo-plotly-multi-select-names", value_ $ (n<>"[]") ]
    forM_ (val ^. f) $ \(v) ->
      input_ [type_ "hidden", name_ (n<>"[]"), value_ v]

(~>) :: Monad m => SimpleGetter t b -> (b -> Html ()) -> SHtml m t ()
g ~> f = do
  (_,v,_) <- lift get
  toHtml $ f $ v ^. g

toHtmls :: (ToHtml b, Monad m) => SimpleGetter t b -> SHtml m t ()
toHtmls g = g ~> toHtml

toParentFormField :: Lens' t b -> FormField b -> FormField t
toParentFormField g (n, f) = 
  (n, f')
    where f' t txt = t & g .~ (f (t ^. g) txt)

(#>) :: (Monad m) => Lens' t b -> SHtml m b () -> SHtml m t ()
g #> r = do
  (n, v, ffs) <- lift get

  -- we running r, but say: take n as the counter for form fields
  let stT = renderTextT r
  (txt, (_, _, subFs)) <- (lift . lift) $ runStateT stT (n, v ^. g, [])
  
  toHtmlRaw txt
  
  {- put fields to the monad based on whole state 
  counter has been corrected during runStateT, 
  so we call fresh just to increment current monad's counter -}
  forM_ subFs $ \(ff) -> fresh >> putFormField (toParentFormField g ff)
