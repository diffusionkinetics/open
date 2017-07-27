{-# LANGUAGE OverloadedStrings, ExistentialQuantification, ExtendedDefaultRules, FlexibleContexts, Rank2Types, TemplateHaskell, ScopedTypeVariables #-}

module Dashdo.Types where

import Lucid
import Data.Text (Text, pack)
import qualified Data.Text.Lazy as TL
import Control.Monad.State.Strict
import Lens.Micro
import Data.Monoid ((<>))

type FormField t = (Int, t -> Text -> t)
type FormFields t = [FormField t]

type SHtml m t = HtmlT (StateT (Int, t, [FormField t]) m) t

data RDashdo m = forall t. RDashdo
  { rdFid    :: String
  , rdTitle  :: Text
  , rdDashdo :: Dashdo m t }

data Dashdo m t = Dashdo
  { initial :: t
  , render :: SHtml m t }

runSHtml :: Monad m => t -> SHtml m t -> m (FormFields t, TL.Text)
runSHtml val shtml = do
  let stT = renderTextT shtml
  (t, (_, _, ffs)) <- runStateT stT (0, val, [])  -- TODO: val in initial state can not match with () in stT
  return (ffs, t)

mkFieldName :: Int -> Text
mkFieldName = ((<>) "f") . pack . show

mkFieldNameMultiple :: Int -> Text
mkFieldNameMultiple n = mkFieldName n <> "[]"

fieldName :: Int -> Attribute
fieldName = name_ . mkFieldName

fieldNameMultiple :: Int -> Attribute
fieldNameMultiple = name_ . mkFieldNameMultiple

fresh :: Monad m => SHtml m Int
fresh = undefined {-- do
  (n, v, ffs) <- lift $ get
  lift $ put (n+1, v, ffs)
  return n --}

freshAndValue :: Monad m => SHtml m (a, Int)
freshAndValue = undefined {-- do
  (n, v, ffs) <- lift $ get
  lift $ put (n+1, v, ffs)
  return (v, n) --}

putFormField :: Monad m => FormField t -> SHtml m ()
putFormField ff = undefined {-- do
  (n, v, ffs) <- lift $ get
  lift $ put (n, v, ff:ffs)
  return () --}

lensSetter :: ASetter' s a -> (s -> a -> s)
lensSetter l x y = x & l .~ y

lensPusher :: ASetter s t [a] [a] -> s -> a -> t
lensPusher l x y = over l ((:) y) x