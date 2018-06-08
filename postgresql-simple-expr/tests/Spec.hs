{-# LANGUAGE TypeFamilies #-}
{-# LANGUAGE OverloadedStrings, DeriveGeneric, DeriveAnyClass #-}

module Main where

import Database.PostgreSQL.Simple
import Database.PostgreSQL.Simple.Connect
import Database.PostgreSQL.Simple.Expr
import Test.Hspec

import Common
import KeySpec
import FakeRowsSpec

basicSpec :: SpecWith Connection
basicSpec =
  describe "Expr.executeC" $ do
    it "should be able to create a table and destroy it" $ \c -> do
      rr c $ executeC "CREATE TABLE pgspec(id integer PRIMARY KEY, x text)" ()
      rr c $ executeC "INSERT INTO pgspec(id,x) values(1,'one')" ()
      [n] <- rr c $ queryC "select count(*) from pgspec" ()
      rr c $ executeC "DROP TABLE pgspec" ()
      (n :: Only Int) `shouldBe` Only 1

main :: IO ()
main = hspec $ beforeAll setupPG $ afterAll close $ do
  basicSpec
  keySpec
  fakeRowsSpec
