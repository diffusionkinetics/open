{-# LANGUAGE OverloadedStrings, ExistentialQuantification, ExtendedDefaultRules, FlexibleContexts, Rank2Types, TemplateHaskell, ScopedTypeVariables #-}

module Dashdo.Types where

import Lucid
import Data.Text (Text, pack)
import qualified Data.Text.Lazy as TL
import Control.Monad.State.Strict
import Lens.Micro
import Data.Monoid ((<>))

type FormField t = (Text, t -> Text -> t)
type FormFields t = [FormField t]
type FieldName = Text

type SHtml m t a = HtmlT (StateT ([FieldName],t,[(TL.Text, TL.Text)],FormFields t) m) a

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
  (t, (_, _, _, ffs)) <- runStateT stT (iniFldNams, val,pars, [])
  return (ffs, t)

fresh :: Monad m => SHtml m a FieldName
fresh = do
  (n:ns, v, pars, ffs) <- lift $ get
  lift $ put (ns, v, pars, ffs)
  return n

freshAndValue :: Monad m => SHtml m a (a, FieldName)
freshAndValue = do
  (n:ns, v, pars, ffs) <- lift $ get
  lift $ put (ns, v, pars, ffs)
  return (v, n)

named :: Monad m => FieldName -> SHtml m t a -> SHtml m t a
named nm mx = do
  (ns, v, pars, ffs) <- lift $ get
  lift $ put (nm:ns, v, pars, ffs)
  mx

getValue :: Monad m =>  SHtml m a a
getValue = do
  (_, v, _, _) <- lift $ get
  return $ v

putFormField :: Monad m => FormField t -> SHtml m t ()
putFormField ff = do
  (n, v, pars, ffs) <- lift $ get
  lift $ put (n, v, pars, ff:ffs)
  return ()

lensSetter :: ASetter' s a -> (s -> a -> s)
lensSetter l x y = x & l .~ y

lensPusher :: ASetter' s [a] -> (s -> a -> s)
lensPusher l x y = x & l %~ ((:) y)