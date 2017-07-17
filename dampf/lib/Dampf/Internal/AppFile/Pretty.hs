{-# LANGUAGE OverloadedStrings #-}

module Dampf.Internal.AppFile.Pretty
  ( pShowDampfs
  ) where

import           Control.Lens
import           Data.Maybe         (fromMaybe)
import qualified Data.Text as T
import           Text.PrettyPrint

import           Dampf.Internal.AppFile.Types


pShowDampfs :: Dampfs -> String
pShowDampfs = render . hang (text "AppFile:") 4 . pprDampfs


pprDampfs :: Dampfs -> Doc
pprDampfs = vcat . fmap pprDampf . view specs


pprDampf :: Dampf -> Doc
pprDampf (Image n s)      = text "image" <+> text n <> colon
    $+$ nest 4 (pprImageSpec s)
    $+$ text ""

pprDampf (Container n s)  = text "container" <+> text n <> colon
    $+$ nest 4 (pprContainerSpec s)
    $+$ text ""

pprDampf (Domain n s)     = text "domain" <+> text n <> colon
    $+$ nest 4 (pprDomainSpec s)
    $+$ text ""

pprDampf (PostgresDB n s) = text "postgresdb" <+> text n <> colon
    $+$ nest 4 (pprDBSpec s)
    $+$ text ""


pprImageSpec :: ImageSpec -> Doc
pprImageSpec spec = text "dockerFile:" <+> text df
  where
    df = spec ^. dockerFile


pprContainerSpec :: ContainerSpec -> Doc
pprContainerSpec spec = vcat
    [ text "image:"   <+> text i
    , text "expose:"  <+> pprList e
    , text "command:" <+> text c
    ]
  where
    i = spec ^. image
    e = spec ^. expose . non []
    c = spec ^. command . non ""


pprDomainSpec :: DomainSpec -> Doc
pprDomainSpec spec = vcat
    [ text "static:"         <+> text s
    , text "proxyContainer:" <+> text pc
    , text "letsencrypt:"    <+> text le
    ]
  where
    s  = spec ^. static . non ""
    pc = spec ^. proxyContainer ^. to (T.unpack . fromMaybe "")
    le = spec ^. letsencrypt ^. to (show . fromMaybe False)


pprDBSpec :: DBSpec -> Doc
pprDBSpec spec = vcat
    [ text "migrations:"   <+> text m
    , text "dbUser:"       <+> text u
    , text "dbExtensions:" <+> pprList e
    ]
  where
    m = spec ^. migrations . non ""
    u = spec ^. dbUser
    e = spec ^. dbExtensions


pprList :: (Show a) => [a] -> Doc
pprList = brackets . hsep . punctuate comma . fmap (text . show)

