{-# LANGUAGE TypeFamilies
           , OverloadedStrings
           , DeriveGeneric
           , GeneralizedNewtypeDeriving
           , StandaloneDeriving
           , TypeApplications
           , FlexibleInstances #-}
module FakeRowsSpec (fakeRowsSpec) where

import Common
import Control.Applicative
import Control.Exception (bracket_)
import Database.PostgreSQL.Simple
import Database.PostgreSQL.Simple.Expr
import Database.PostgreSQL.Simple.FakeRows
import Database.PostgreSQL.Simple.ToField
import Database.PostgreSQL.Simple.FromField
import Fake
import GHC.Generics
import Test.Hspec

instance Fake String where
  fake = listUpTo 100 fakeLetter

data X = X { x :: Serial Int, y :: Int } deriving (Show, Generic)

instance HasFieldNames X
instance HasTable X where
  tableName _ = "X"
instance HasKey X where
  type Key X = Serial Int
  getKey (X k _) = k
  getKeyFieldNames _ = ["x"]

instance ToRow X
instance FromRow X
instance FakeRows X

newtype YKey = YKey { unYKey :: String }
  deriving (Show,Generic,Eq,Ord,FromField,ToField)

instance KeyField YKey

instance Fake YKey where
  fake = YKey <$> listUpTo1 10 fakeLetter

data Y = Y { a :: YKey, b :: Foreign X, c :: Int } deriving (Show,Generic)

instance Fake Y where
  fake = liftA3 Y fake fake fake

instance HasFieldNames Y
instance HasTable Y where
  tableName _ = "Y"
instance HasKey Y where
  type Key Y = YKey
  getKey (Y a _ _) = a
  getKeyFieldNames _ = ["a"]

instance ToRow Y
instance FromRow Y
instance FakeRows Y

data Z = Z { z1 :: Foreign X, z2 :: Foreign Y, z3 :: String }
  deriving (Show, Generic)

instance HasFieldNames Z
instance HasTable Z where
  tableName _ = "Z"
instance HasKey Z where
  type Key Z = (Serial Int, YKey)
  getKey (Z a b _) = (unForeign a, unForeign b)
  getKeyFieldNames _ = ["z1", "z2"]
instance ToRow Z
instance FromRow Z
instance FakeRows Z

mkTbls = do
  executeC "create table x (x serial primary key, y integer);" ()
  executeC "create table y (a text primary key, b integer references x(x), c integer);" ()
  executeC "create table z (z1 serial references x(x), z2 text references y(a), z3 text, primary key (z1, z2))" ()

dropTbls = do
  executeC "drop table z;" ()
  executeC "drop table y;" ()
  executeC "drop table x;" ()
  return ()

handleTbls go c = bracket_ (rr c mkTbls) (rr c dropTbls) (go c)

fakeRowsSpec :: SpecWith Connection
fakeRowsSpec = aroundWith handleTbls $ do
  describe "FakeRows" $ do
    it "should populate the database with fake rows" $ \c -> do
      (xs, ys, zs) <- rr c $ do
        populate @X 150
        populate @Y 100
        populate @Z 50
        xs <- selectFrom @X "x" ()
        ys <- selectFrom @Y "y" ()
        zs <- selectFrom @Z "z" ()
        return (xs, ys, zs)
      length xs `shouldBe` 150
      length ys `shouldBe` 100
      length zs `shouldBe` 50
