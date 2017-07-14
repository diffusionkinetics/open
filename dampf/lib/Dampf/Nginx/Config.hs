{-# LANGUAGE OverloadedStrings  #-}

module Dampf.Nginx.Config where

import           Control.Lens
import           Data.Text                      (Text)
import qualified Data.Text as T
import           Data.Maybe                     (fromMaybe)
import           System.FilePath
import           Text.PrettyPrint.HughesPJClass

import           Dampf.AppFile
import           Dampf.ConfigFile hiding        (port)


data Server = Server [ServerDecl]


data ServerDecl
  = Listen Int [String]
  | ServerName [Text]
  | Location Text [(Text, Text)]
  | Include FilePath
  | SSLCertificate FilePath
  | SSLCertificateKey FilePath


instance Pretty Server where
  pPrint (Server sds)
     = text "server {"
       $$ nest 2 (vcat (map pPrint sds))
       $$ char '}'


instance Pretty ServerDecl where
  pPrint (Listen p ss)
    = text "listen" <+> int p <+> vcat (map text ss) <> char ';'
  pPrint (ServerName nms)
    = text "server_name" <+> hsep (map ttext nms)<> char ';'
  pPrint (Location path kvs)
    = text "location" <+> ttext path
                      <+> char '{'
        $$ nest 2 (vcat (map ppKV kvs))
        $$ char '}'
          where ppKV (k,v) = ttext k <+> ttext v <> char ';'
  pPrint (Include fp)
    = text "include" <+> text fp <> char ';'
  pPrint (SSLCertificate fp)
    = text "ssl_certificate" <+> text fp <> char ';'
  pPrint (SSLCertificateKey fp)
    = text "ssl_certificate_key" <+> text fp <> char ';'


encryptDecls :: (HasDampfConfig c) => c -> [ServerDecl]
encryptDecls cfg = maybe [] f $ cfg ^. liveCertificate
  where
    f liveCert = [ Listen 443 ["ssl"]
                 , SSLCertificate $ liveCert</>"fullchain.pem"
                 , SSLCertificateKey $ liveCert</>"privkey.pem"
                 , Include "/etc/letsencrypt/options-ssl-nginx.conf"
                 ]


ttext :: Text -> Doc
ttext = text . T.unpack


domainToServer :: (HasDampfConfig c) => c -> Text -> DomainSpec -> Server
domainToServer cfg nm dspec
  = Server $
      [ Listen 80 []
      , ServerName [nm, "www." `T.append` nm] --TODO: only if it is a root domain
      , Location "/" $ domainToLocation nm dspec
      ] ++ if toEncrypt dspec then encryptDecls cfg else []


domainToLocation :: Text -> DomainSpec -> [(Text, Text)]
domainToLocation nm (DomainSpec mstatic mproxy _) =
  maybe [] (const $ staticAttrs nm) mstatic ++ maybe [] proxyAttrs mproxy


staticAttrs :: Text -> [(Text, Text)]
staticAttrs nm =
    [ ("root", "/var/www/" `T.append` nm)
    , ("index", "index.html")
    ]


toEncrypt :: DomainSpec -> Bool
toEncrypt ds = fromMaybe False $ letsencrypt ds


proxyAttrs :: Text -> [(Text, Text)]
proxyAttrs cname =
    [ ("proxy_pass",       "http://127.0.0.1:" `T.append` p)
    , ("proxy_set_header", "Host $host")
    , ("proxy_set_header", "X-Real-IP $remote_addr")
    , ("proxy_set_header", "X-Forwarded-For $proxy_add_x_forwarded_for")
    , ("proxy_set_header", "X-Forwarded-Proto $scheme")
    ]
  where
    p = last $ T.splitOn ":" cname

domainConfig :: (HasDampfConfig c) => c -> Text -> DomainSpec -> Text
domainConfig cfg nm spec = T.pack $ render $ pPrint $ domainToServer cfg nm spec

