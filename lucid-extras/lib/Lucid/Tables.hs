{-# LANGUAGE OverloadedStrings, ExtendedDefaultRules,FlexibleInstances #-}
{-# LANGUAGE DefaultSignatures,FlexibleContexts,TypeApplications,TypeOperators #-}
{-# LANGUAGE InstanceSigs, TypeSynonymInstances,ScopedTypeVariables, DeriveGeneric #-}
{-# LANGUAGE TypeFamilies, GeneralizedNewtypeDeriving, DeriveGeneric, DefaultSignatures,
             PolyKinds, TypeOperators, ScopedTypeVariables, FlexibleContexts,
             FlexibleInstances, UndecidableInstances,
             OverloadedStrings, TypeApplications, StandaloneDeriving #-}

module Lucid.Tables where

import Lucid
import Data.Proxy
import qualified Data.Text as T
import GHC.Generics
import Data.Monoid

class ToHtmlTable a where
  headers:: Proxy a->[T.Text]
  
  default headers:: (Generic a,TableSelectors (Rep a)) => Proxy a -> [T.Text]
  headers  = map (T.pack) . tableSelectors 
  
  toHtmlRow:: Monad m => a ->[HtmlT m ()]
  default toHtmlRow:: (Monad m, Generic a, GToTable (Rep a)) => a -> [HtmlT m ()]
  toHtmlRow a = gtoHtmlRow (from a)


class GToTable f where
  gtoHtmlRow:: Monad m => f a ->[HtmlT m ()]
  
instance GToTable U1 where
  gtoHtmlRow U1 = []

instance GToTable a => GToTable  (M1 C c a) where

  gtoHtmlRow =gtoHtmlRow . unM1


instance GToTable a => GToTable (M1 D c a) where

  gtoHtmlRow =gtoHtmlRow . unM1

instance (GToTable a, GToTable b) => GToTable (a :*: b) where
  gtoHtmlRow (a :*: b) =  (gtoHtmlRow a) <> (gtoHtmlRow b)
  
instance (Selector d, ToHtml a) => GToTable (M1 S d (K1 R a)) where

  gtoHtmlRow (M1 (K1 x)) =[toHtml x ]
  
-- https://hackage.haskell.org/package/hpack-0.15.0/src/src/Hpack/GenericsUtil.hs
-- Copyright (c) 2014-2016 Simon Hengel <sol@typeful.net>

tableSelectors :: (TableSelectors (Rep a)) => Proxy a -> [String]
tableSelectors = f
  where
    f :: forall a. (TableSelectors (Rep a)) => Proxy a -> [String]
    f _ = selNames (Proxy :: Proxy (Rep a))
  
class TableSelectors a where
  selNames :: Proxy a -> [String]

instance TableSelectors f => TableSelectors (M1 D x f) where
  selNames _ = selNames (Proxy :: Proxy f)

instance TableSelectors f => TableSelectors (M1 C x f) where
  selNames _ = selNames (Proxy :: Proxy f)

instance Selector s => TableSelectors (M1 S s (K1 R t)) where
  selNames _ = [selName (undefined :: M1 S s (K1 R t) ())]

instance (TableSelectors a, TableSelectors b) => TableSelectors (a :*: b) where
  selNames _ = selNames (Proxy :: Proxy a) ++ selNames (Proxy :: Proxy b)

instance TableSelectors U1 where
  selNames _ = []
  
data Test = Test{
  foo :: String,
  bar ::String} deriving Generic
  
instance ToHtmlTable Test

h = headers $ Proxy @Test
b :: [Html ()]

b = toHtmlRow $ Test "hello" "world"
g = from $ Test "hello" "world"


