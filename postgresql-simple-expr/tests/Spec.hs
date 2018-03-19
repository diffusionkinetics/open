{-# LANGUAGE TypeFamilies #-}
{-# LANGUAGE OverloadedStrings, DeriveGeneric, DeriveAnyClass #-}

module Main where

import Control.Monad.Reader
import Data.Monoid ((<>))
import Data.Proxy
import Data.String
import Database.PostgreSQL.Simple
import Database.PostgreSQL.Simple.Connect
import Database.PostgreSQL.Simple.Expr
import GHC.Generics
import Test.Hspec

rr :: Connection -> ReaderT Connection IO a -> IO a
rr conn r = runReaderT r conn

setupPG :: IO Connection
setupPG = do
  cfg <- configFromEnv
  c <- createConn' cfg
  -- execute c ("drop table if exists " <> multiKeyTbl) ()
  -- execute c ("drop table if exists " <> singleKeyTbl) ()
  return c

teardownPG :: Connection -> IO ()
teardownPG c = do
  -- execute c ("drop table if exists " <> multiKeyTbl) ()
  -- execute c ("drop table if exists " <> singleKeyTbl) ()
  close c

data MultiKeyTbl = MultiKeyTbl {
    id1 :: Serial Int
  , id2 :: Serial Int
  , x   :: String
  } deriving (Eq, Show, Generic)

-- instance IsString (Serial String) where
  -- fromString = Serial
instance HasFieldNames MultiKeyTbl
instance ToRow MultiKeyTbl
instance FromRow MultiKeyTbl

instance HasTable MultiKeyTbl where
  tableName _ = "test_multikeytbl"

multiKeyTbl = fromString $ tableName (undefined :: Proxy MultiKeyTbl)

mkMultikeyTbl =
  "CREATE TABLE " <> multiKeyTbl
  <> "(id1 serial, id2 serial, x text, PRIMARY KEY(id1, id2))"

dropMultiKeyTbl :: Query
dropMultiKeyTbl = "DROP TABLE " <> multiKeyTbl

instance HasKey MultiKeyTbl where
  type Key MultiKeyTbl = (Serial Int, Serial Int)
  getKey (MultiKeyTbl k1 k2 _) = (k1, k2)
  getKeyFieldNames _ = ["id1", "id2"]

data SingleKeyTbl = SingleKeyTbl {
    singleKey :: Int
  , y :: String
  } deriving (Eq, Show, Generic)

instance HasFieldNames SingleKeyTbl
instance ToRow SingleKeyTbl
instance FromRow SingleKeyTbl
instance HasTable SingleKeyTbl where
  tableName _ = "test_singlekeytbl"

singleKeyTbl = fromString $ tableName (undefined :: Proxy SingleKeyTbl)
mkSingleKeyTbl = "create table " <> singleKeyTbl
  <> "(singleKey integer PRIMARY KEY, y text)"
dropSingleKeyTbl = "drop table " <> singleKeyTbl

instance HasKey SingleKeyTbl where
  type Key SingleKeyTbl = Only Int
  getKey (SingleKeyTbl k _) = Only k
  getKeyFieldNames _ = ["singleKey"]

pgSpec :: Spec
pgSpec =  beforeAll setupPG $ afterAll teardownPG $ do
  describe "Expr.executeC" $ do
    it "should be able to create a table and destroy it" $ \c -> do
      rr c $ executeC "CREATE TABLE pgspec(id integer PRIMARY KEY, x text)" ()
      rr c $ executeC "INSERT INTO pgspec(id,x) values(1,'one')" ()
      [n] <- rr c $ queryC "select count(*) from pgspec" ()
      rr c $ executeC "DROP TABLE pgspec" ()
      (n :: Only Int) `shouldBe` Only 1

  describe "Key support" $ do
    it "should support tables with a single key" $ \c -> do
      (k1, k2) <- rr c $ do
        executeC mkSingleKeyTbl ()
        k1 <- insert $ SingleKeyTbl 1 "one"
        k2 <- insert $ SingleKeyTbl 2 "two"
        update $ SingleKeyTbl 2 "TWO"
        return (k1, k2)
      val2 <- rr c $ getByKey k2
      total <- rr c $ countFrom singleKeyTbl ()
      newTotal <- rr c $ do
        delete (SingleKeyTbl 1 "one")
        deleteByKey (undefined :: Proxy SingleKeyTbl) k2
        newC <- countFrom singleKeyTbl ()
        executeC dropSingleKeyTbl ()
        return newC
      total `shouldBe` 2
      newTotal `shouldBe` 0
      val2 `shouldBe` Just (SingleKeyTbl 2 "TWO")
      (k1, k2) `shouldBe` (Only 1, Only 2)

    it "should support tables with multiple serial keys" $ \c -> do
      rr c $ executeC mkMultikeyTbl ()
      let item = MultiKeyTbl 10 100 "one" -- PG will ignore 10 & 100
      k <- rr c $ insert item
      x <- rr c $ getByKey k
      rr c $ executeC dropMultiKeyTbl ()
      k `shouldBe` (1, 1)
      (x :: Maybe MultiKeyTbl) `shouldBe` Just (MultiKeyTbl 1 1 "one")

  describe "update and delete" $ do
    it "should update and delete existing values" $ \c -> do
      (a, b, c, count) <- rr c $ do
        executeC mkMultikeyTbl ()
        k1 <- insert $ MultiKeyTbl 10 100 "one"
        k2 <- insert $ MultiKeyTbl 12 120 "two"
        k3 <- insert $ MultiKeyTbl 13 130 "three"
        update $ MultiKeyTbl 3 3 "THREE"
        update $ MultiKeyTbl 2 2 "TWO"
        deleteByKey (undefined :: Proxy MultiKeyTbl) (1, 1)
        one <- getByKey k1
        two <- getByKey k2
        three <- getByKey (3, 3)
        delete (MultiKeyTbl 2 2 "TWO")
        delete (MultiKeyTbl 4 4 "does not exist")
        total <- countFrom  multiKeyTbl ()
        executeC dropMultiKeyTbl ()
        return (one, two, three, total)
      count `shouldBe` 1
      (a :: Maybe MultiKeyTbl) `shouldBe` Nothing
      (b :: Maybe MultiKeyTbl) `shouldBe` Just (MultiKeyTbl 2 2 "TWO")
      (c :: Maybe MultiKeyTbl) `shouldBe` Just (MultiKeyTbl 3 3 "THREE")

main :: IO ()
main = hspec pgSpec
