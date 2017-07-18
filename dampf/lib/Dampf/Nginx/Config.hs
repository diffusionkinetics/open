{-# LANGUAGE OverloadedStrings  #-}

module Dampf.Nginx.Config where

import           Control.Lens
import           Data.Text                      (Text)
import qualified Data.Text as T
import           System.FilePath

import           Dampf.AppFile
import           Dampf.ConfigFile hiding        (port)
import           Dampf.Nginx.Types


encryptDecls :: (HasDampfConfig c) => c -> [ServerDecl]
encryptDecls cfg = maybe [] f $ cfg ^. liveCertificate
  where
    f liveCert = [ Listen 443 ["ssl"]
                 , SSLCertificate $ liveCert</>"fullchain.pem"
                 , SSLCertificateKey $ liveCert</>"privkey.pem"
                 , Include "/etc/letsencrypt/options-ssl-nginx.conf"
                 ]


domainToServer :: (HasDampfConfig c) => c -> Text -> DomainSpec -> Server
domainToServer cfg nm dspec
  = Server $
      [ Listen 80 []
      , ServerName [nm, "www." `T.append` nm] --TODO: only if it is a root domain
      , Location "/" $ domainToLocation nm dspec
      ] ++ if toEncrypt dspec then encryptDecls cfg else []


domainToLocation :: Text -> DomainSpec -> [(Text, Text)]
domainToLocation n spec = maybe [] (const $ staticAttrs n) s
    ++ maybe [] proxyAttrs p
  where
    s = spec ^. static
    p = spec ^. proxyContainer


staticAttrs :: Text -> [(Text, Text)]
staticAttrs nm =
    [ ("root", "/var/www/" `T.append` nm)
    , ("index", "index.html")
    ]


toEncrypt :: DomainSpec -> Bool
toEncrypt ds = ds ^. letsEncrypt . non False


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
domainConfig c t s = T.pack
    . pShowServer
    $ domainToServer c t s

