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
    crt <- view $ config . liveCertificate . _Just
    ds  <- view $ app . domains

    let go = (\n -> (n, "/var/www" <> n))
        path = "/tmp/dampf/test-nginx"
    
    fl <- itraverse domainConfig ds <&> foldOf traverse

    liftIO $ do 
      createDirectoryIfMissing True path
      T.writeFile (path </> "nginx.conf") fl

    return $ ds ^.. traverse . static . _Just . to go
      <> [(crt, crt)]
      <> [(path, "/etc/nginx")]

{-pretendToDeployDomains :: (MonadIO m) => DampfT m [(FilePath, FilePath)]-}
{-pretendToDeployDomains = do-}
    {-crt <- view $ config . liveCertificate . _Just-}
    {-ds  <- view $ app . domains-}

    {-let go = (\n -> (n, "/var/www" <> n))-}
        {-path = "/tmp/dampf/test-nginx"-}
    
    {-iforM_ ds $ \name spec -> do-}
        {-fl <- domainConfig name spec-}
        {-liftIO $ do-}
          {-let strName = T.unpack name-}
              {-savail  = path </> "sites-availible"-}
              {-senabl  = path </> "sites-enabled"  -}

          {-createDirectoryIfMissing True savail-}
          {-createDirectoryIfMissing True senabl-}

          {-T.writeFile (savail </> strName) fl-}

          {-exists <- fileExist (senabl </> strName)-}
          {-when (not exists) $ createSymbolicLink -}
            {-(savail </> strName) -}
            {-(senabl </> strName)-}

    {-return $ ds ^.. traverse . static . _Just . to go-}
      {-<> [(crt, crt)]-}
      {-<> [(path, "/etc/nginx")]-}

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
    {-, ("proxy_set_header", "Host $host")-}
    {-, ("proxy_set_header", "X-Real-IP $remote_addr")-}
    {-, ("proxy_set_header", "X-Forwarded-For $proxy_add_x_forwarded_for")-}
    {-, ("proxy_set_header", "X-Forwarded-Proto $scheme")-}
    ]
