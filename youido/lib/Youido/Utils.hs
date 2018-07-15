{-# LANGUAGE KindSignatures, TypeApplications, TypeOperators,
  ScopedTypeVariables, FlexibleInstances, AllowAmbiguousTypes,
  DefaultSignatures, FlexibleContexts, OverloadedStrings #-}

module Youido.Utils where

import           Data.Map.Strict (Map)
import qualified Data.Map.Strict as M
import           Data.Monoid ((<>))
import           Data.Text (Text, pack)
import           GHC.Generics

--------------------------------------------------

data CtorSearchResult =
    CtorNotFound
  | CtorFoundLeft
  | CtorFoundRight
  deriving Show

class HasConName (f :: * -> *) where
  hasConName :: Text -> Bool
  hasU1ConName :: Text -> Bool

instance (HasConName f) => HasConName (D1 d f) where
  hasConName = hasConName @f
  hasU1ConName = hasU1ConName @f

instance {-# OVERLAPPABLE #-} (Constructor c) => HasConName (C1 c f) where
  hasConName = (==) . pack $ conName (undefined :: C1 c f ())
  hasU1ConName _ = False

instance {-# OVERLAPS #-} (Constructor c) => HasConName (C1 c U1) where
  hasConName = (==) . pack $ conName (undefined :: C1 c U1 ())
  hasU1ConName = hasConName @(C1 c U1)

instance (HasConName f, HasConName g) => HasConName (f :+: g) where
  hasConName c = hasConName @f c || hasConName @g c
  hasU1ConName c = hasU1ConName @f c || hasU1ConName @g c

--------------------------------------------------

class EnumCtors (f :: * -> *) where
  enumCtors :: [Text]

instance EnumCtors f => EnumCtors (D1 c f) where
  enumCtors = enumCtors @f

instance (EnumCtors f, EnumCtors g) => EnumCtors (f :+: g) where
  enumCtors = enumCtors @f ++ enumCtors @g

instance (Constructor c) => EnumCtors (M1 C c f) where
  enumCtors = [pack $ conName (undefined :: M1 C c f ())]

getctors :: forall f. EnumCtors f => [Text]
getctors = enumCtors @f

--------------------------------------------------

class GetConName a where
  getConName :: a -> Text
  default getConName :: (Generic a, GetConNameG (Rep a)) => a -> Text
  getConName = getConNameG'

getConNameG' :: (Generic a, GetConNameG (Rep a)) => a -> Text
getConNameG' = getConNameG . from

class GetConNameG f where
  getConNameG :: f p -> Text

instance (GetConNameG f) => GetConNameG (D1 d f) where
  getConNameG = getConNameG . unM1

instance (GetConNameG f, GetConNameG g) => GetConNameG (f :+: g) where
  getConNameG (L1 f) = getConNameG f
  getConNameG (R1 g) = getConNameG g

instance (Constructor c) => GetConNameG (C1 c f) where
  getConNameG _ = pack $ conName (undefined :: C1 c f ())
