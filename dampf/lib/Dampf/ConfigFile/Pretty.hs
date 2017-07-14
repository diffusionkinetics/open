module Dampf.ConfigFile.Pretty where

import           Control.Lens
import qualified Data.Map.Strict as Map
import           Data.Maybe
import           Text.PrettyPrint

import           Dampf.ConfigFile.Types


pShowDampfConfig :: DampfConfig -> String
pShowDampfConfig = render
    . hang (text "Config File:") 4
    . pprDampfConfig


pprDampfConfig :: DampfConfig -> Doc
pprDampfConfig d = vcat
    [ text "liveCertificate:" <+> text vlc
    , text ""
    , text "postgres" <+> text (view name vp) <> colon
    , nest 4 (pprPostgresConfig vp)
    ]
  where
    mlc = view liveCertificate d
    vlc = fromMaybe "" mlc
    vp  = view postgres d


pprPostgresConfig :: PostgresConfig -> Doc
pprPostgresConfig p = vcat
    [ text "host:" <+> text vh
    , text "port:" <+> int vp
    , text "users:"
    , nest 4 (pprMap vu)
    , text ""
    ]
  where
    vh = view host p
    vp = view port p
    vu = Map.toList $ view users p


pprMap :: [(String, String)] -> Doc
pprMap kvs = vcat
    $ fmap (\(k, v) -> text k <> colon <+> text v) kvs

