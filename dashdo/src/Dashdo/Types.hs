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

type SHtml t a = HtmlT (State ([FieldName],t,FormFields t)) a

data RDashdo = forall t. RDashdo
  { rdFid    :: String
  , rdTitle  :: Text
  , rdDashdo :: Dashdo t }

data Dashdo t = forall b. Dashdo
  { initial :: t
  , fetch :: t -> IO b
  , render :: t -> b -> SHtml t () }

runSHtml :: t -> SHtml t () -> (FormFields t, TL.Text)
runSHtml val shtml =
  let stT = renderTextT shtml
      iniFldNams = map (("f"<>) . pack . show) [(0::Int)..]
      (t, (_, _, ffs)) = runState stT (iniFldNams, val, [])
  in (ffs, t)

fresh :: SHtml a FieldName
fresh = do
  (n:ns, v, ffs) <- lift $ get
  lift $ put (ns, v, ffs)
  return n

freshAndValue :: SHtml a (a, FieldName)
freshAndValue = do
  (n:ns, v, ffs) <- lift $ get
  lift $ put (ns, v, ffs)
  return (v, n)

getValue :: SHtml a a
getValue = do
  (n, v, ffs) <- lift $ get
  return v

putFormField :: FormField t -> SHtml t ()
putFormField ff = do
  (n, v, ffs) <- lift $ get
  lift $ put (n, v, ff:ffs)
  return ()

lensSetter :: ASetter' s a -> (s -> a -> s)
lensSetter l x y = x & l .~ y

lensPusher :: ASetter s t [a] [a] -> s -> a -> t
lensPusher l x y = over l ((:) y) x