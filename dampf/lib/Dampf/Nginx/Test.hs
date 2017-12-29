{-# LANGUAGE OverloadedStrings, TupleSections  #-}

module Dampf.Nginx.Test 
  where

import           Control.Lens
import           Control.Monad.IO.Class         (MonadIO, liftIO)
import           Control.Monad                  (when)
import           Data.Text                      (Text)
import qualified Data.Text as T
import qualified Data.Text.IO as T
import qualified Data.Map.Strict as Map
import           Data.Monoid ((<>))
import           Data.Maybe (catMaybes)


import           System.Directory
import           System.FilePath
import           System.Posix.Files
import           System.Process

import           Dampf.Nginx.Types
import           Dampf.Types

pretendToDeployDomains :: (MonadIO m) => DampfT m [(FilePath, FilePath)]
pretendToDeployDomains = do
    ds  <- view $ app . domains

    let go = (\n -> (n, "/var/www" <> n))
        path = "/tmp/dampf/test-nginx"
    
    fl <- itraverse domainConfig ds <&> foldOf traverse

    liftIO $ do 
      createDirectoryIfMissing True path
      T.writeFile (path </> "nginx.conf") fl

    return $ ds ^.. traverse . static . _Just . to go
      <> [(path, "/etc/nginx")]
      <> let crt = "/etc/letsencrypt/live/" in [(crt,crt)]

domainConfig :: (MonadIO m) => Text -> DomainSpec -> DampfT m Text
domainConfig name spec = T.pack . pShowFakeServer <$> domainToServer name spec { _letsEncrypt = Just False }

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
    ++ maybe [] fakeProxyAttrs p
  where
    s :: Maybe Text
    s = const name <$> spec ^. static
    p = spec ^. proxyContainer
    cdn = spec ^. isCDN

sslDecls :: (MonadIO m) => Text -> DomainSpec -> DampfT m [ServerDecl]
sslDecls name spec = do
    return . f $ "/etc/letsencrypt/live/" ++ T.unpack name
  where
    f live =
        [ Listen 443 ["ssl"]
        , SSLCertificate $ live </> "fullchain.pem"
        , SSLCertificateKey $ live </> "privkey.pem"
        , SSLTrustedCertificate $ live </> "chain.pem"
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
httpDecls :: Text -> DomainSpec -> [ServerDecl]
httpDecls name spec =
    [ Listen 80 []
    , ServerName [name, "www." `T.append` name]
    , Location "/" $ domainToLocation name spec
    ]

staticAttrs :: Text -> [(Text, Text)]
staticAttrs x =
    [ ("root", "/var/www/" `T.append` x)
    , ("index", "index.html")
    ]

fakeProxyAttrs :: Text -> [(Text, Text)]
fakeProxyAttrs x =
    [ ("resolver", "127.0.0.11")
    , ("proxy_pass", "http://" <> x)
    , ("proxy_set_header", "Host $host")
    , ("proxy_set_header", "X-Real-IP $remote_addr")
    , ("proxy_set_header", "X-Forwarded-For $proxy_add_x_forwarded_for")
    , ("proxy_set_header", "X-Forwarded-Proto $scheme")
    ]
