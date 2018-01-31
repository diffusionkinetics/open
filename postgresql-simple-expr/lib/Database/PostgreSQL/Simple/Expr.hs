{-# LANGUAGE TypeFamilies, GeneralizedNewtypeDeriving, DeriveGeneric, DefaultSignatures,
             PolyKinds, TypeOperators, ScopedTypeVariables, FlexibleContexts,
             FlexibleInstances, UndecidableInstances, OverloadedStrings, TypeApplications    #-}

module Database.PostgreSQL.Simple.Expr where

import Database.PostgreSQL.Simple
import Database.PostgreSQL.Simple.FromField
import Database.PostgreSQL.Simple.ToField
import Database.PostgreSQL.Simple.ToRow

import GHC.Generics
import Data.Proxy
import Data.String
import Data.List (intercalate, intersperse)
import Data.Monoid ((<>), mconcat)
import Data.Maybe (listToMaybe)
import Data.Aeson
import Data.Text (Text, pack)
import qualified Data.Text as T
class HasFieldNames a where
  getFieldNames :: Proxy a -> [String]

  default getFieldNames :: (Selectors (Rep a)) => Proxy a -> [String]
  getFieldNames proxy = selectors proxy

selectFrom :: forall r q. (ToRow q, FromRow r, HasFieldNames r) => Connection -> Query -> q -> IO [r]
selectFrom conn q1 args = do
  let fullq = "select " <> (fromString $ intercalate "," $ getFieldNames $ (Proxy :: Proxy r) ) <> " from " <> q1
  query conn fullq args

class HasFieldNames a => HasTable a where
  tableName :: Proxy a -> String

selectWhere :: forall r q. (ToRow q, FromRow r, HasTable r)
            => Connection -> Query -> q -> IO [r]
selectWhere conn q1 args = do
  let fullq = "select " <> (fromString $ intercalate "," $ getFieldNames $ (Proxy :: Proxy r) )
                        <> " from " <> fromString (tableName (Proxy :: Proxy r))
                        <> " where " <> q1
  query conn fullq args



--insert all fields
insertAll :: forall r. (ToRow r, HasTable r) => Connection  -> r -> IO ()
insertAll conn  val = do
  let fnms = getFieldNames $ (Proxy :: Proxy r)
  _ <- execute conn ("INSERT INTO " <> fromString (tableName (Proxy :: Proxy r)) <> " (" <>
                     (fromString $ intercalate "," fnms ) <>
                     ") VALUES (" <>
                     (fromString $ intercalate "," $ map (const "?") fnms) <> ")")
             val
  return ()

  --insert all fields
insertAllOrDoNothing :: forall r. (ToRow r, HasTable r) => Connection  -> r -> IO ()
insertAllOrDoNothing conn  val = do
  let fnms = getFieldNames $ (Proxy :: Proxy r)
  _ <- execute conn ("INSERT INTO " <> fromString (tableName (Proxy :: Proxy r)) <> " (" <>
                     (fromString $ intercalate "," fnms ) <>
                     ") VALUES (" <>
                     (fromString $ intercalate "," $ map (const "?") fnms) <> ") ON CONFLICT DO NOTHING")
             val
  return ()


class KeyField a where
   toFields :: a -> [Action]
   toText :: a -> Text
   autoIncrementing :: Proxy a -> Bool

   default toFields :: ToField a => a -> [Action]
   toFields = (:[]) . toField
   default toText :: Show a => a -> Text
   toText = pack . show

   autoIncrementing _ = False

instance KeyField Int
instance KeyField Text
instance KeyField String
instance (KeyField a, KeyField b) => KeyField (a,b) where
  toFields (x,y) = toFields x ++ toFields y
  toText (x,y) = T.concat [toText x, ",", toText y]
  autoIncrementing _ = autoIncrementing (undefined :: Proxy a)
                       && autoIncrementing (undefined :: Proxy b)

class HasTable a => HasKey a where
  type Key a
  getKey :: a -> Key a
  getKeyFieldNames :: Proxy a -> [String]

insert :: forall a . (HasKey a, KeyField (Key a), ToRow a, FromField (Key a)) => Connection -> a -> IO (Key a)
insert conn val = do
  if autoIncrementing (undefined :: Proxy (Key a))
     then ginsertSerial
     else do insertAll conn val
             return $ getKey val
   where ginsertSerial = do
           let [kName] = map fromString $ getKeyFieldNames (Proxy :: Proxy a)
               tblName = fromString $ tableName (Proxy :: Proxy a)
               fldNms = map fromString $ getFieldNames (Proxy :: Proxy a)
               fldNmsNoKey = filter (/=kName) fldNms
               qmarks = mconcat $ intersperse "," $ map (const "?") fldNmsNoKey
               fields = mconcat $ intersperse "," $ fldNmsNoKey
               qArgs = map snd $ filter ((/=kName) . fst) $ zip fldNms $ toRow val
               q = "insert into "<>tblName<>"("<>fields<>") values ("<>qmarks<>") returning "<>kName
           res <- query conn q qArgs
           case res of
             [] -> fail $ "no key returned from "++show tblName
             Only k : _ -> return k

insertOrDoNothing :: forall a . (HasKey a, KeyField (Key a), ToRow a, FromField (Key a)) => Connection -> a -> IO (Key a)
insertOrDoNothing conn val = do
  if autoIncrementing (undefined :: Proxy (Key a))
     then ginsertSerial
     else do insertAll conn val
             return $ getKey val
   where ginsertSerial = do
           let [kName] = map fromString $ getKeyFieldNames (Proxy :: Proxy a)
               tblName = fromString $ tableName (Proxy :: Proxy a)
               fldNms = map fromString $ getFieldNames (Proxy :: Proxy a)
               fldNmsNoKey = filter (/=kName) fldNms
               qmarks = mconcat $ intersperse "," $ map (const "?") fldNmsNoKey
               fields = mconcat $ intersperse "," $ fldNmsNoKey
               qArgs = map snd $ filter ((/=kName) . fst) $ zip fldNms $ toRow val
               q = "insert into "<>tblName<>"("<>fields<>") values ("<>qmarks<>") ON CONFLICT DO NOTHING returning "<>kName
           res <- query conn q qArgs
           case res of
             [] -> fail $ "no key returned from "++show tblName
             Only k : _ -> return k

update
  :: forall a . (HasKey a, KeyField (Key a), ToRow a)
  => Connection -> a -> IO ()
update conn val = do
  let [kName] = map fromString $ getKeyFieldNames (Proxy :: Proxy a)
      kval = getKey val
      tblName = fromString $ tableName (Proxy :: Proxy a)
      fldNms = map fromString $ getFieldNames (Proxy :: Proxy a)
      fldNmsNoKey = filter (/=kName) fldNms
      qArgs = map snd $ filter ((/=kName) . fst) $ zip fldNms $ toRow val
      fieldQ = mconcat $ intersperse ", " $ map (\f-> f <>" = ?") fldNmsNoKey
      (keyQ, keyA) = keyRestrict (Proxy @a) kval
      q = "update "<>tblName<>" set "<>fieldQ<>" where "<>keyQ
  execute conn q $ qArgs ++ keyA
  return ()

delete
  :: forall a . (HasKey a, KeyField (Key a), ToField (Key a))
  => Connection -> a -> IO ()
delete conn x = do
  let tblName = fromString $ tableName (Proxy :: Proxy a)
      [kName] = map fromString $ getKeyFieldNames (Proxy :: Proxy a)
      q = "delete from "<> tblName<>" where "<>kName<>" = ?"
  execute conn q (Only $ getKey x)
  return ()

deleteByKey
  :: forall a . (HasKey a, KeyField (Key a))
  => Connection -> Proxy a -> Key a -> IO ()
deleteByKey conn px k = do
  let tblName = fromString $ tableName px
      (keyQ, keyA) = keyRestrict (Proxy @a) k
      q = "delete from "<> tblName<>" where "<>keyQ
  execute conn q keyA
  return ()

conjunction :: [Query] -> Query
conjunction [] = "true"
conjunction (q1:[]) = q1
--conjunction (q1:q2:[]) = "("<>q1<>") and ("<>q2<>")" --needed?
conjunction (q1:qs) = "("<>q1<>") and "<>conjunction qs

keyRestrict :: (HasKey a, KeyField (Key a)) => Proxy a -> Key a -> (Query, [Action])
keyRestrict px key
  = let nms = getKeyFieldNames px
        q1 nm = fromString nm <> " = ? "
        q = conjunction $ map q1 nms
    in (q, toFields key)

-- |Fetch a row by its primary key

getByKey :: forall a . (HasKey a, KeyField (Key a), FromRow a) => Connection -> Key a -> IO (Maybe a)
getByKey conn key = do
  let (q, as) = keyRestrict (Proxy :: Proxy a) key
  ress <- selectWhere conn q as
  return $ listToMaybe ress

newtype Serial a = Serial { unSerial :: a }
  deriving (Num, Ord, Show, Read, Eq, Generic, ToField, FromField)

instance (ToField a, KeyField a) => KeyField (Serial a) where
  toFields (Serial x) = [toField x]
  toText (Serial x) = toText x
  autoIncrementing _ = True

instance ToJSON a => ToJSON (Serial a) where
  toJSON (Serial x) = toJSON x
instance FromJSON a => FromJSON (Serial a) where
  parseJSON mx = Serial <$> parseJSON mx

-- https://hackage.haskell.org/package/hpack-0.15.0/src/src/Hpack/GenericsUtil.hs
-- Copyright (c) 2014-2016 Simon Hengel <sol@typeful.net>

selectors :: (Selectors (Rep a)) => Proxy a -> [String]
selectors = f
  where
    f :: forall a. (Selectors (Rep a)) => Proxy a -> [String]
    f _ = selNames (Proxy :: Proxy (Rep a))


class Selectors a where
  selNames :: Proxy a -> [String]

instance Selectors f => Selectors (M1 D x f) where
  selNames _ = selNames (Proxy :: Proxy f)

instance Selectors f => Selectors (M1 C x f) where
  selNames _ = selNames (Proxy :: Proxy f)

instance Selector s => Selectors (M1 S s (K1 R t)) where
  selNames _ = [selName (undefined :: M1 S s (K1 R t) ())]

instance (Selectors a, Selectors b) => Selectors (a :*: b) where
  selNames _ = selNames (Proxy :: Proxy a) ++ selNames (Proxy :: Proxy b)

instance Selectors U1 where
  selNames _ = []