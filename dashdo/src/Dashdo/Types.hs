{-# LANGUAGE OverloadedStrings, ExistentialQuantification, ExtendedDefaultRules, FlexibleContexts, Rank2Types, TemplateHaskell, ScopedTypeVariables #-}

module Dashdo.Types where

import Lucid
import Data.Text (Text, pack)
import qualified Data.Text.Lazy as TL
import Control.Monad.State.Strict
import Lens.Micro
import Data.Monoid ((<>))
import Lens.Micro.TH
import Lens.Micro
import Lens.Micro.Mtl

type FormField t = (Text, t -> Text -> t)
type FormFields t = [FormField t]
type FieldName = Text

data DD t = DD
  { _freshSupply :: [FieldName]
  , _value :: t
  , _rawParams :: [(TL.Text, TL.Text)]
  , _formFields :: FormFields t
  }

makeLenses ''DD

type SHtml m t a = HtmlT (StateT (DD t) m) a

data RDashdo m = forall t. RDashdo
  { rdFid    :: String
  , rdTitle  :: Text
  , rdDashdo :: Dashdo m t }

data Dashdo m t = Dashdo
  { initial :: t
  , render :: SHtml m t () }

runSHtml :: Monad m => t -> SHtml m t () -> [(TL.Text, TL.Text)] -> m (FormFields t, TL.Text)
runSHtml val shtml pars = do
  let stT = renderTextT shtml
      iniFldNams = map (("f"<>) . pack . show) [(0::Int)..]
  (t, (DD _ _ _ ffs)) <- runStateT stT (DD iniFldNams val pars [])
  return (ffs, t)

fresh :: Monad m => SHtml m a FieldName
fresh = do
  head <$> (freshSupply <<%= tail)

freshAndValue :: Monad m => SHtml m a (a, FieldName)
freshAndValue = (,) <$> getValue <*> fresh

named :: Monad m => FieldName -> SHtml m t a -> SHtml m t a
named nm mx = do
  freshSupply %= (nm:)
  mx

getValue :: Monad m =>  SHtml m a a
getValue = use value

putFormField :: Monad m => FormField t -> SHtml m t ()
putFormField ff = do
  formFields %= (ff:)

lensSetter :: ASetter' s a -> (s -> a -> s)
lensSetter l x y = x & l .~ y

lensPusher :: ASetter' s [a] -> (s -> a -> s)
lensPusher l x y = x & l %~ ((:) y)