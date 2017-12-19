{-# LANGUAGE OverloadedStrings, TupleSections  #-}

module Dampf.Nginx.Test 
  (pretendToDeployDomains)
  where

import           Control.Lens
import           Control.Monad.IO.Class         (MonadIO, liftIO)
import           Data.Text                      (Text)
import qualified Data.Text as T
import qualified Data.Text.IO as T
import qualified Data.Map.Strict as Map
import           Data.Monoid ((<>))


import           System.Directory
import           System.FilePath
import           System.Posix.Files
import           System.Process

import           Dampf.Nginx.Types
import           Dampf.Types


pretendToDeployDomains :: (MonadIO m) => DampfT m [(FilePath, FilePath)]
pretendToDeployDomains = do
    crt <- view $ config . liveCertificate 
    ds  <- view $ app . domains
    
    vols <- iforM ds $ \name spec -> do
        fl <- domainConfig name spec
        liftIO $ do
            let strName = T.unpack name

            T.writeFile        (".dampf-test-nginx/sites-available" </> strName) fl
            createSymbolicLink (".dampf-test-nginx/sites-available" </> strName)
                               (".dampf-test-nginx/sites-enabled" </> strName)
            
        return $ 
             spec ^.. static . _Just . to (\n -> (n, "/var/www" <> n))

    return . foldMap snd . Map.toList $ vols
          <> [(crt, crt)]
          <> [(".dampf-test-nginx/", "/etc/nginx")]

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
domainToLocation spec = maybe [] staticAttrs s ++ maybe [] fakeProxyAttrs p
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

fakeProxyAttrs :: Text -> [(Text, Text)]
fakeProxyAttrs x =
    [ ("proxy_pass", x)
    , ("proxy_set_header", "Host $host")
    , ("proxy_set_header", "X-Real-IP $remote_addr")
    , ("proxy_set_header", "X-Forwarded-For $proxy_add_x_forwarded_for")
    , ("proxy_set_header", "X-Forwarded-Proto $scheme")
    ]
