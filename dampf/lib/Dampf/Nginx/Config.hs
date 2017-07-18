{-# LANGUAGE OverloadedStrings  #-}

module Dampf.Nginx.Config where

import           Control.Lens
import           Control.Monad.IO.Class         (MonadIO)
import           Data.Text                      (Text)
import qualified Data.Text as T
import           System.FilePath

import           Dampf.AppFile
import           Dampf.ConfigFile hiding        (port)
import           Dampf.Nginx.Types
import           Dampf.Types


domainConfig :: (MonadIO m) => Text -> DomainSpec -> DampfT m Text
domainConfig name spec = T.pack . pShowServer <$> domainToServer name spec


domainToServer :: (MonadIO m) => Text -> DomainSpec -> DampfT m Server
domainToServer name spec
    | isSSL     = do
        decls <- (http ++) <$> sslDecls
        return (Server decls)

    | otherwise = return (Server http)
  where
    isSSL = spec ^. letsEncrypt . non False
    http  = httpDecls name spec


domainToLocation :: DomainSpec -> [(Text, Text)]
domainToLocation spec = maybe [] staticAttrs s ++ maybe [] proxyAttrs p
  where
    s = T.pack <$> spec ^. static
    p = spec ^. proxyContainer


sslDecls :: (MonadIO m) => DampfT m [ServerDecl]
sslDecls = do
    live <- view (config . liveCertificate)
    return (maybe [] f live)
  where
    f live =
        [ Listen 443 ["ssl"]
        , SSLCertificate $ live </> "fullchain.pem"
        , SSLCertificateKey $ live </> "privkey.pem"
        , Include "/etc/letsencrypt/options-ssl-nginx.conf"
        ]


httpDecls :: Text -> DomainSpec -> [ServerDecl]
httpDecls name spec =
    [ Listen 80 []
    , ServerName [name, "www." `T.append` name]
    , Location "/" $ domainToLocation spec
    ]


staticAttrs :: Text -> [(Text, Text)]
staticAttrs x =
    [ ("root", "/var/www/" `T.append` x)
    , ("index", "index.html")
    ]


proxyAttrs :: Text -> [(Text, Text)]
proxyAttrs x =
    [ ("proxy_pass",       "http://127.0.0.1:" `T.append` p)
    , ("proxy_set_header", "Host $host")
    , ("proxy_set_header", "X-Real-IP $remote_addr")
    , ("proxy_set_header", "X-Forwarded-For $proxy_add_x_forwarded_for")
    , ("proxy_set_header", "X-Forwarded-Proto $scheme")
    ]
  where
    p = last $ T.splitOn ":" x

