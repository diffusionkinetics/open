{-# LANGUAGE OverloadedStrings #-}

module Dampf.AppFile.Pretty
  ( pShowDampfApp
  ) where

import           Control.Lens
import           Data.List                      (intersperse)
import qualified Data.Map.Strict as Map
import           Data.Text                      (Text)
import qualified Data.Text as T
import           Text.PrettyPrint

import           Dampf.AppFile.Types


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
    , text "Tests:"
    , text ""
    , nest 4 (pprTests ts)
    ]
  where
    is = a ^. images . to Map.toList
    cs = a ^. containers . to Map.toList
    bs = a ^. databases . to Map.toList
    ds = a ^. domains . to Map.toList
    ts = a ^. tests . to Map.toList


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
    [ text "image:"       <+> text i
    , text "expose:"      <+> pprList e
    , text "command:"     <+> text c
    , text "useDatabase:" <+> text d
    ]
  where
    i = spec ^. image . to T.unpack
    e = spec ^. expose . non []
    c = spec ^. command . non "" . to T.unpack
    d = spec ^. useDatabase . non "" . to T.unpack

pprTests :: [(Text, TestSpec)] -> Doc
pprTests = vcat
    . intersperse (text "")
    . fmap (pprSpecs pprTestSpec)


pprTestSpec :: TestSpec -> Doc
pprTestSpec spec = vcat
    [ text "when:"        <+> text w
    , text "units:"
    , nest 4 $ vcat (map pprTestUnit (spec ^. tsUnits))
    ]
    where
    w = spec ^. tsWhen . to show

pprTestUnit :: TestUnit -> Doc
pprTestUnit (TestRun i c) = text "Run" <+> ttext i <+> ttext c
pprTestUnit (TestGet i Nothing) = text "GET" <+> ttext i
pprTestUnit (TestGet i (Just reg)) = text "GET" <+> ttext i <+> text "=~" <+> ttext reg

pprDatabases :: [(Text, DatabaseSpec)] -> Doc
pprDatabases = vcat
    . intersperse (text "")
    . fmap (pprSpecs pprDatabaseSpec)


pprDatabaseSpec :: DatabaseSpec -> Doc
pprDatabaseSpec spec = vcat
    [ text "migrations:" <+> text m
    , text "user:"       <+> text u
    , text "extensions:" <+> pprTextList e
    ]
  where
    m = spec ^. migrations . non ""
    u = spec ^. user . to T.unpack
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
    , text "nowww:"          <+> text nw
    ]
  where
    s  = spec ^. static . non ""
    pc = spec ^. proxyContainer . non "" . to T.unpack
    le = spec ^. letsEncrypt . non False . to show
    nw = spec ^. nowww . non False . to show


pprSpecs :: (a -> Doc) -> (Text, a) -> Doc
pprSpecs f (n, s) = hang (text (T.unpack n) <> colon) 4 (f s)


pprList :: (Show a) => [a] -> Doc
pprList = brackets . hsep . punctuate comma . fmap (text . show)


pprTextList :: [Text] -> Doc
pprTextList = brackets . hsep . punctuate comma . fmap (text . T.unpack)

ttext :: Text -> Doc
ttext = text . T.unpack