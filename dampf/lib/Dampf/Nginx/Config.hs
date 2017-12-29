{-# LANGUAGE OverloadedStrings  #-}

module Dampf.Nginx.Config where

import           Control.Lens
import           Control.Monad.IO.Class         (MonadIO)
import           Data.Text                      (Text)
import qualified Data.Text as T
import           System.FilePath

import           Dampf.Nginx.Types
import           Dampf.Types


domainConfig :: (MonadIO m) => Text -> DomainSpec -> DampfT m Text
domainConfig name spec = T.pack . pShowServer <$> domainToServer name spec


domainToServer :: (MonadIO m) => Text -> DomainSpec -> DampfT m Server
domainToServer name spec
    | isSSL     = do
        decls <- (http ++) <$> sslDecls name spec
        return (Server decls)

    | otherwise = return (Server http)
  where
    isSSL = spec ^. letsEncrypt . non False
    http  = httpDecls name spec


domainToLocation :: Text -> DomainSpec -> [(Text, Text)]
domainToLocation name spec =
    maybe [] staticAttrs s
    ++ cdnAttrs cdn
    ++ maybe [] proxyAttrs p
  where
    s :: Maybe Text
    s = const name <$> spec ^. static
    p = spec ^. proxyContainer
    cdn = spec ^. isCDN


sslDecls :: (MonadIO m) => Text -> DomainSpec -> DampfT m [ServerDecl]
sslDecls name spec = do
    return $ f $ "/etc/letsencrypt/live/"++ T.unpack name
  where
    f live =
        [ Listen 443 ["ssl"]
        , SSLCertificate $ live </> "fullchain.pem"
        , SSLCertificateKey $ live </> "privkey.pem"
        , SSLTrustedCertificate $ live </> "chain.pem"
        ]

httpDecls :: Text -> DomainSpec -> [ServerDecl]
httpDecls name spec =
    [ Listen 80 []
    , ServerName [name, "www." `T.append` name]
    , Location "/" $ domainToLocation name spec
    ]

cdnAttrs :: Maybe Bool -> [(Text, Text)]
cdnAttrs (Just True) =
    [ ("gzip_static","on")
    , ("expires","max")
    , ("log_not_found","off")
    , ("access_log","off")
    , ("add_header","Cache-Control public")
    , ("add_header","'Access-Control-Allow-Origin' '*'")
    , ("add_header","'Access-Control-Allow-Methods' 'GET, OPTIONS'")
    ]
cdnAttrs _ = []


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

