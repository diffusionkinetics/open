{-# LANGUAGE OverloadedStrings, ExtendedDefaultRules,FlexibleInstances #-}
{-# LANGUAGE DefaultSignatures,FlexibleContexts,TypeApplications,TypeOperators #-}
{-# LANGUAGE InstanceSigs, TypeSynonymInstances,ScopedTypeVariables, DeriveGeneric #-}

module Lucid.Tables where

import Lucid
import Data.Proxy
import qualified Data.Text as T
import GHC.Generics
import Data.Monoid

class ToHtmlTable a where
  headers:: Proxy a->[T.Text]
  
  default headers:: (Generic a, GToTable (Rep a)) => Proxy a -> [T.Text]
  headers _ = gheaders (from (Proxy:: Proxy (Rep a)))
  
  toHtmlRow:: Monad m => a ->[HtmlT m ()]
  default toHtmlRow:: (Monad m, Generic a, GToTable (Rep a)) => a -> [HtmlT m ()]
  toHtmlRow a = gtoHtmlRow (from a)


class GToTable f where
  gheaders:: f (Proxy a)->[T.Text]
  gtoHtmlRow:: Monad m => f a ->[HtmlT m ()]
  
instance GToTable U1 where
  gheaders _ = ["u1"]
  gtoHtmlRow U1 = []

instance GToTable a => GToTable  (M1 C c a) where
  gheaders= gheaders. unM1
  gtoHtmlRow =gtoHtmlRow . unM1


instance GToTable a => GToTable (M1 D c a) where
  gheaders= gheaders. unM1
  gtoHtmlRow =gtoHtmlRow . unM1

instance (GToTable a, GToTable b) => GToTable (a :*: b) where
  gheaders (a :*: b) =  (gheaders a) <> (gheaders b)
  gtoHtmlRow (a :*: b) =  (gtoHtmlRow a) <> (gtoHtmlRow b)
  
instance (Selector d, ToHtml a) => GToTable (M1 S d (K1 R a)) where
  gheaders _ =
    let sel = (selName (undefined :: M1 S d (K1 R a) ()))
    in [T.pack sel]
  gtoHtmlRow (M1 (K1 x)) =[toHtml x ]
  
data Test = Test{
  foo :: String,
  bar ::String} deriving Generic
  
instance ToHtmlTable Test

h = headers $ Proxy @Test
b :: [Html ()]

b = toHtmlRow $ Test "hello" "world"
g = from $ Test "hello" "world"


