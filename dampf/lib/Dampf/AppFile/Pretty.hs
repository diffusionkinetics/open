{-# LANGUAGE OverloadedStrings #-}

module Dampf.AppFile.Pretty
  ( pShowDampfs
  ) where

import           Data.Maybe         (fromMaybe)
import qualified Data.Text as T
import           Text.PrettyPrint

import           Dampf.AppFile.Types


pShowDampfs :: Dampfs -> String
pShowDampfs = render
    . hang (text "AppFile:") 4
    . pprDampfs


pprDampfs :: Dampfs -> Doc
pprDampfs = vcat . fmap pprDampf . unDampfs


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
pprImageSpec s = text "dockerFile:" <+> text (dockerFile s)


pprContainerSpec :: ContainerSpec -> Doc
pprContainerSpec s = vcat
    [ text "image:" <+> text vi
    , text "expose:" <+> pprList ve
    , text "command:" <+> text vc
    ]
  where
    vi = image s
    ve = fromMaybe [] (expose s)
    vc = fromMaybe "" (command s)


pprDomainSpec :: DomainSpec -> Doc
pprDomainSpec s = vcat
    [ text "static:" <+> text vs
    , text "proxyContainer:" <+> text vpc
    , text "letsencrypt:" <+> text vle
    ]
  where
    vs  = fromMaybe "" (static s)
    vpc = T.unpack $ fromMaybe "" (proxyContainer s)
    vle = show $ fromMaybe False (letsencrypt s)


pprDBSpec :: DBSpec -> Doc
pprDBSpec s = vcat
    [ text "migrations:" <+> text vm
    , text "dbUser:" <+> text vu
    , text "dbExtensions:" <+> pprList ve
    ]
  where
    vm = fromMaybe "" (migrations s)
    vu = dbUser s
    ve = dbExtensions s


pprList :: (Show a) => [a] -> Doc
pprList = brackets . hsep . punctuate comma . fmap (text . show)

