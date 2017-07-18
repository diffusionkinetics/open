module Dampf.Internal.ConfigFile.Pretty
  ( pShowDampfConfig
  ) where

import           Control.Lens
import           Data.List                          (intersperse)
import qualified Data.Map.Strict as Map
import           Data.Text                          (Text)
import qualified Data.Text as T
import           Text.PrettyPrint

import           Dampf.Internal.ConfigFile.Types


pShowDampfConfig :: DampfConfig -> String
pShowDampfConfig = render . hang (text "Config File:") 4 . pprDampfConfig


pprDampfConfig :: DampfConfig -> Doc
pprDampfConfig cfg = vcat
    [ text "Live Certificate:"
    , text ""
    , nest 4 (text l)
    , text ""
    , text "Database Servers:"
    , text ""
    , nest 4 (pprDatabaseServers ds)
    , text ""
    ]
  where
    l  = cfg ^. liveCertificate . non ""
    ds = cfg ^. databaseServers . to Map.toList


pprDatabaseServers :: [(Text, PostgresConfig)] -> Doc
pprDatabaseServers = vcat
    . intersperse (text "")
    . fmap (pprConfigs pprPostgresConfig)


pprPostgresConfig :: PostgresConfig -> Doc
pprPostgresConfig cfg = vcat
    [ text "host:" <+> text h
    , text "port:" <+> int p
    , text "users:"
    , nest 4 (pprMap u)
    ]
  where
    h = cfg ^. host
    p = cfg ^. port
    u = cfg ^. users . to Map.toList


pprConfigs :: (a -> Doc) -> (Text, a) -> Doc
pprConfigs f (n, c) = hang (text (T.unpack n) <> colon) 4 (f c)


pprMap :: (Show a) => [(a, a)] -> Doc
pprMap kvs = vcat
    $ fmap (\(k, v) -> text (show k) <> colon <+> text (show v)) kvs

