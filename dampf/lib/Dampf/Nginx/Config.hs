{-# LANGUAGE DeriveGeneric, OverloadedStrings #-}

module Dampf.Nginx.Config where

import Dampf.AppFile
import Data.Text (Text)
import qualified Data.Text as T
import Text.PrettyPrint.HughesPJClass


data Server = Server [ServerDecl]

data ServerDecl
  = Listen Int
  | ServerName [Text]
  | Location Text [(Text, Text)]

instance Pretty Server where
  pPrint (Server sds)
     = text "server {"
       $$ nest 2 (vcat (map pPrint sds))
       $$ char '}'

instance Pretty ServerDecl where
  pPrint (Listen p)
    = text "listen" <+> int p <> char ';'
  pPrint (ServerName nms)
    = text "server_name" <+> hsep (map ttext nms)<> char ';'
  pPrint (Location path kvs)
    = text "location" <+> ttext path
                      <+> char '{'
        $$ nest 2 (vcat (map ppKV kvs))
        $$ char '}'
          where ppKV (k,v) = ttext k <+> ttext v <> char ';'

ttext = text . T.unpack

domainToServer :: Text -> DomainSpec -> Server
domainToServer nm dspec
  = Server
      [ Listen 80
      , ServerName [nm, "www." `T.append` nm] --TODO: only if it is a root domain
      , Location "/" $ domainToLocation nm dspec
      ]

domainToLocation :: Text -> DomainSpec -> [(Text, Text)]
domainToLocation nm (DomainSpec mstatic mproxy)
  = concat [ maybe [] (const $ staticAttrs nm) mstatic
           , maybe [] proxyAttrs mproxy ]

staticAttrs nm = [ ("root",  "/var/www/" `T.append` nm)
                 , ("index", "index.html" )]

proxyAttrs cname
  = let port = last $ T.splitOn ":" cname in
    [ ("proxy_pass",       "http://127.0.0.1:" `T.append` port)
    , ("proxy_set_header", "Host $host")
    , ("proxy_set_header", "X-Real-IP $remote_addr")
    , ("proxy_set_header", "X-Forwarded-For $proxy_add_x_forwarded_for")
    , ("proxy_set_header", "X-Forwarded-Proto $scheme")
    ]

domainConfig :: Text -> DomainSpec -> Text
domainConfig nm spec = T.pack $ render $ pPrint $ domainToServer nm spec
