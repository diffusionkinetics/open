{-# LANGUAGE OverloadedStrings #-}

module Dampf.Internal.AppFile.Pretty
  ( pShowDampfApp
  ) where

import           Control.Lens
import           Data.List                      (intersperse)
import qualified Data.Map.Strict as Map
import           Data.Text                      (Text)
import qualified Data.Text as T
import           Text.PrettyPrint

import           Dampf.Internal.AppFile.Types


pShowDampfApp :: DampfApp -> String
pShowDampfApp = render . hang (text "App File:") 4 . pprDampfApp


pprDampfApp :: DampfApp -> Doc
pprDampfApp a = vcat
    [ text "Docker Images:"
    , text ""
    , nest 4 (pprImages is)
    , text ""
    , text "Docker Containers:"
    , text ""
    , nest 4 (pprContainers cs)
    , text ""
    , text "Databases:"
    , text ""
    , nest 4 (pprDatabases bs)
    , text ""
    , text "Domains:"
    , text ""
    , nest 4 (pprDomains ds)
    , text ""
    ]
  where
    is = a ^. images . to Map.toList
    cs = a ^. containers . to Map.toList
    bs = a ^. databases . to Map.toList
    ds = a ^. domains . to Map.toList


pprImages :: [(Text, ImageSpec)] -> Doc
pprImages = vcat
    . intersperse (text "")
    . fmap (pprSpecs pprImageSpec)


pprImageSpec :: ImageSpec -> Doc
pprImageSpec spec = text "dockerFile:" <+> text df
  where
    df = spec ^. dockerFile


pprContainers :: [(Text, ContainerSpec)] -> Doc
pprContainers = vcat
    . intersperse (text "")
    . fmap (pprSpecs pprContainerSpec)


pprContainerSpec :: ContainerSpec -> Doc
pprContainerSpec spec = vcat
    [ text "image:"        <+> text i
    , text "expose:"       <+> pprList e
    , text "command:"      <+> text c
    , text "useDatabase:" <+> text d
    ]
  where
    i = spec ^. image
    e = spec ^. expose . non []
    c = spec ^. command . non ""
    d = spec ^. useDatabase . non "" . to T.unpack


pprDatabases :: [(Text, DatabaseSpec)] -> Doc
pprDatabases = vcat
    . intersperse (text "")
    . fmap (pprSpecs pprDatabaseSpec)


pprDatabaseSpec :: DatabaseSpec -> Doc
pprDatabaseSpec spec = vcat
    [ text "migrations:" <+> text m
    , text "user:"       <+> text u
    , text "extensions:" <+> pprList e
    ]
  where
    m = spec ^. migrations . non ""
    u = spec ^. user
    e = spec ^. extensions


pprDomains :: [(Text, DomainSpec)] -> Doc
pprDomains = vcat
    . intersperse (text "")
    . fmap (pprSpecs pprDomainSpec)


pprDomainSpec :: DomainSpec -> Doc
pprDomainSpec spec = vcat
    [ text "static:"         <+> text s
    , text "proxyContainer:" <+> text pc
    , text "letsEncrypt:"    <+> text le
    ]
  where
    s  = spec ^. static . non ""
    pc = spec ^. proxyContainer . non "" . to T.unpack
    le = spec ^. letsEncrypt . non False . to show


pprSpecs :: (a -> Doc) -> (Text, a) -> Doc
pprSpecs f (n, s) = hang (text (T.unpack n) <> colon) 4 (f s)


pprList :: (Show a) => [a] -> Doc
pprList = brackets . hsep . punctuate comma . fmap (text . show)

