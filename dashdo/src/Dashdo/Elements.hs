{-# LANGUAGE OverloadedStrings, ExistentialQuantification, ExtendedDefaultRules, FlexibleContexts, Rank2Types, TemplateHaskell #-}

module Dashdo.Elements where

import Dashdo.Types

import Lucid
import Lucid.Bootstrap
import Lucid.Bootstrap3
import Graphics.Plotly (Plotly)
import Graphics.Plotly.Lucid
import Data.Text (Text, unpack, pack)
import qualified Data.Text.Lazy as TL
import Data.Hashable
import Data.List
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

manualSubmit :: Monad m => SHtml m a ()
manualSubmit = do
  input_ [type_ "submit", value_ "Submit"]
  script_ "var manual_submit = true;"

submitPeriodic :: Monad m => Int -> SHtml m a ()
submitPeriodic delaySecs = do
  let delayMs = pack $ show $ delaySecs*1000
  input_ [type_ "hidden", class_ "dashdo-periodic-submit", value_ delayMs]

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
  v <- getValue
  toHtml $ f $ v ^. g

toHtmls :: (ToHtml b, Monad m) => SimpleGetter t b -> SHtml m t ()
toHtmls g = g ~> toHtml

toParentFormField :: Lens' t b -> FormField b -> FormField t
toParentFormField g (n, f) = 
  (n, f')
    where f' t txt = t & g .~ (f (t ^. g) txt)

(#>) :: (Monad m, Hashable b) => Lens' t b -> SHtml m b () -> SHtml m t ()
g #> r = do
  hashFieldName <- fresh  -- before n is taken from state
  putFormField (hashFieldName, const)

  subFieldsNumberName <- fresh
  putFormField (subFieldsNumberName, const)
  
  (n, v, pars, ffs) <- lift get
  let 
      stT          = renderTextT r

      subValue     = v ^. g
      subValueHash = (pack . show . hash) subValue
      subValueHashChanged  = 
        case find (((==) hashFieldName) . TL.toStrict . fst) pars of
          Nothing                -> True
          Just (_, oldHashValue) -> (subValueHash /= TL.toStrict oldHashValue)

      subFieldsNumber = 
        case find (((==) subFieldsNumberName) . TL.toStrict . fst) pars of
          Nothing     -> -1  -- TODO: a hack, change to MaybeT or something
          Just (_, s) -> read ((unpack . TL.toStrict) s) :: Int

  span_ [data_ "dashdo-cashed" hashFieldName] $ do
    -- we running r, but say: take n as the counter for form fields
    if not subValueHashChanged && subFieldsNumber > -1
      then do
        span_ [class_ "dashdo-cashed-not-changed"] ""
        forM_ [1..subFieldsNumber] $ \fieldN -> fresh >> putFormField ("f" <> ((pack . show) fieldN), const)
      else do
        (txt, (_, _, _, subFs)) <- (lift . lift) $ runStateT stT (n, subValue, pars, [])

        input_ [type_ "hidden", name_ hashFieldName, value_ subValueHash]
        input_ [type_ "hidden", name_ subFieldsNumberName, value_ $ (pack . show . length) subFs]
        
        toHtmlRaw txt

        {- put fields to the monad based on whole state 
        counter has been corrected during runStateT, 
        so we call fresh just to increment current monad's counter -}
        forM_ subFs $ \(ff) -> fresh >> putFormField (toParentFormField g ff)