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

type SHtml t a = HtmlT (State (Int,t,FormFields t)) a

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
      (t, (_, _, ffs)) = runState stT (0, val, [])
  in (ffs, t)

fieldName :: Int -> Attribute
fieldName n = name_ $ "f"<>pack (show n)

fieldNameMultiple :: Int -> Attribute
fieldNameMultiple n = name_ $ "f" <>pack (show n) <> "[]"

fresh :: SHtml a Int
fresh = do
  (n, v, ffs) <- lift $ get
  lift $ put (n+1, v, ffs)
  return n

freshAndValue :: SHtml a (a, Int)
freshAndValue = do
  (n, v, ffs) <- lift $ get
  lift $ put (n+1, v, ffs)
  return (v, n)

putFormField :: FormField t -> SHtml t ()
putFormField ff = do
  (n, v, ffs) <- lift $ get
  lift $ put (n, v, ff:ffs)
  return ()

lensSetter :: ASetter' s a -> (s -> a -> s)
lensSetter l x y = x & l .~ y

lensPusher :: ASetter s t [a] [a] -> s -> a -> t
lensPusher l x y = over l ((:) y) x