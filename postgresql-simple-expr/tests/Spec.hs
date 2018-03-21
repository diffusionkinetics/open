{-# LANGUAGE TypeFamilies #-}
{-# LANGUAGE OverloadedStrings, DeriveGeneric, DeriveAnyClass #-}

module Main where

import Control.Exception (bracket_)
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
  createConn' cfg

data MultiKeyTbl = MultiKeyTbl {
    id1 :: Int
  , id2 :: String
  , x   :: Int
  } deriving (Eq, Show, Generic)

instance HasFieldNames MultiKeyTbl
instance ToRow MultiKeyTbl
instance FromRow MultiKeyTbl

instance HasTable MultiKeyTbl where
  tableName _ = "test_multikeytbl"

multiKeyTbl = fromString $ tableName (undefined :: Proxy MultiKeyTbl)

mkMultikeyTbl =
  "CREATE TABLE " <> multiKeyTbl
  <> "(id1 integer, id2 text, x integer, PRIMARY KEY(id1, id2))"

dropMultiKeyTbl :: Query
dropMultiKeyTbl = "DROP TABLE " <> multiKeyTbl

instance HasKey MultiKeyTbl where
  type Key MultiKeyTbl = (Int, String)
  getKey (MultiKeyTbl k1 k2 _) = (k1, k2)
  getKeyFieldNames _ = ["id1", "id2"]

data SingleKeyTbl = SingleKeyTbl {
    singleKey :: Serial Int
  , y :: String
  } deriving (Eq, Show, Generic)

instance HasFieldNames SingleKeyTbl
instance ToRow SingleKeyTbl
instance FromRow SingleKeyTbl
instance HasTable SingleKeyTbl where
  tableName _ = "test_singlekeytbl"

singleKeyTbl = fromString $ tableName (undefined :: Proxy SingleKeyTbl)
mkSingleKeyTbl = "create table " <> singleKeyTbl
  <> "(singleKey serial PRIMARY KEY, y text)"
dropSingleKeyTbl = "drop table " <> singleKeyTbl

instance HasKey SingleKeyTbl where
  type Key SingleKeyTbl = Serial Int
  getKey (SingleKeyTbl k _) = k
  getKeyFieldNames _ = ["singleKey"]

handleTables :: (Connection -> IO()) -> Connection -> IO()
handleTables go c = do
  bracket_ mkTables dropTables (go c)
  where mkTables = rr c $ do
          executeC mkSingleKeyTbl ()
          executeC mkMultikeyTbl ()
        dropTables = rr c $ do
          executeC dropSingleKeyTbl ()
          executeC dropMultiKeyTbl ()

pgSpec :: Spec
pgSpec =  beforeAll setupPG $ afterAll close $ aroundWith handleTables $ do
  describe "Expr.executeC" $ do
    it "should be able to create a table and destroy it" $ \c -> do
      rr c $ executeC "CREATE TABLE pgspec(id integer PRIMARY KEY, x text)" ()
      rr c $ executeC "INSERT INTO pgspec(id,x) values(1,'one')" ()
      [n] <- rr c $ queryC "select count(*) from pgspec" ()
      rr c $ executeC "DROP TABLE pgspec" ()
      (n :: Only Int) `shouldBe` Only 1

  describe "Key support" $ do
    it "should support tables with a single serial key" $ \c -> do
      (k1, k2) <- rr c $ do
        k1 <- insert $ SingleKeyTbl 10 "one"
        k2 <- insert $ SingleKeyTbl 20 "two"
        update $ SingleKeyTbl 2 "TWO"
        return (k1, k2)
      val2 <- rr c $ getByKey k2
      total <- rr c $ countFrom singleKeyTbl ()
      newTotal <- rr c $ do
        delete (SingleKeyTbl 1 "one")
        deleteByKey (undefined :: Proxy SingleKeyTbl) k2
        newC <- countFrom singleKeyTbl ()
        return newC
      total `shouldBe` 2
      newTotal `shouldBe` 0
      val2 `shouldBe` Just (SingleKeyTbl 2 "TWO")
      (k1, k2) `shouldBe` (1, 2)

    it "should support tables with multiple keys" $ \c -> do
      let item = MultiKeyTbl 10 "one" 100
      k <- rr c $ insert item
      x <- rr c $ getByKey k
      k `shouldBe` (10, "one")
      (x :: Maybe MultiKeyTbl) `shouldBe` Just (MultiKeyTbl 10 "one" 100)

  describe "update and delete" $ do
    it "should update and delete existing values" $ \c -> do
      (a, b, c, count) <- rr c $ do
        k1 <- insert $ MultiKeyTbl 10 "one" 100
        k2 <- insert $ MultiKeyTbl 12 "two" 120
        k3 <- insert $ MultiKeyTbl 13 "three" 130
        update $ MultiKeyTbl 13 "three" 3
        update $ MultiKeyTbl 12 "two" 2
        deleteByKey (undefined :: Proxy MultiKeyTbl) (10, "one")
        one <- getByKey k1
        two <- getByKey k2
        three <- getByKey (13, "three")
        delete (MultiKeyTbl 12 "two" 2)
        delete (MultiKeyTbl 4 "does not exist" 4)
        total <- countFrom  multiKeyTbl ()
        return (one, two, three, total)
      count `shouldBe` 1
      (a :: Maybe MultiKeyTbl) `shouldBe` Nothing
      (b :: Maybe MultiKeyTbl) `shouldBe` Just (MultiKeyTbl 12 "two" 2)
      (c :: Maybe MultiKeyTbl) `shouldBe` Just (MultiKeyTbl 13 "three" 3)

main :: IO ()
main = hspec pgSpec
