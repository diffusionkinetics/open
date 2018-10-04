
{-# LANGUAGE OverloadedStrings  #-}

module Dampf.Nginx.Config where

import           Data.Monoid ((<>))
import           Control.Lens
import           Control.Monad.IO.Class         (MonadIO)
import           Data.Text                      (Text)
import qualified Data.Text as T
import           System.FilePath

import           Dampf.Nginx.Types
import           Dampf.Types

domainConfig :: (MonadIO m) => IsTest -> Text -> DomainSpec -> DampfT m Text
domainConfig isTest name spec = T.pack . pShowServers isTest <$> domainToServer isTest name spec

domainToServer :: (MonadIO m) => IsTest -> Text -> DomainSpec -> DampfT m [Server]
domainToServer isTest name spec = servers
  where 
    isHttpsOnly = spec ^. httpsOnly . non False
    isSSL = spec ^. letsEncrypt . non False

    addr 
      | isTest = "$server_addr" 
      | otherwise = "$host"

    redr_uri = "https://" <> addr <> "$request_uri" 

    redr = Server 
      [ Return 301 redr_uri
      , Listen 80 []
      , ServerName [name, "www." `T.append` name]
      ]
    
    decls = servDecls isTest isSSL isHttpsOnly name spec

    servers 
      | isHttpsOnly && isSSL = 
          pure $ redr : [ Server decls ]

      | isHttpsOnly && not isSSl = 
          warning "httpsOnly: encryption disabled" 
          *> pure $ Server decls

      | otherwise = 
          pure $ Server decls


domainToLocation :: IsTest -> Text -> DomainSpec -> [(Text, Text)]
domainToLocation isTest name spec =
    maybe [] staticAttrs s
    ++ cdnAttrs cdn
    ++ maybe [] (proxyAttrs isTest)  p
  where
    s :: Maybe Text
    s = const name <$> spec ^. static
    p = spec ^. proxyContainer
    cdn = spec ^. isCDN

servDecls :: IsTest -> IsSSL -> IsHttpsOnly -> Text -> DomainSpec -> [ServerDecl]
servDecls isTest isSSL isHttpsOnly name spec 
  |     isSSL && not isHttpsOnly =  httpDecls isTest name spec ++ sslDecls name spec
  |     isSSL &&     isHttpsOnly = (Listen 80  [] : httpDecls isTest name spec) ++ sslDecls name spec
  | not isSSL && not isHttpsOnly =  Listen 80  [] : httpDecls isTest name spec
  --not isSSL &&     isHttpsOnly 
  | otherwise                    =  Listen 443 [] : httpDecls isTest name spec

sslDecls :: Text -> DomainSpec -> [ServerDecl]
sslDecls name spec = f $ "/etc/letsencrypt/live/"++ T.unpack name
  where
    f live =
        [ Listen 443 ["ssl"]
        , SSLCertificate $ live </> "fullchain.pem"
        , SSLCertificateKey $ live </> "privkey.pem"
        , SSLTrustedCertificate $ live </> "chain.pem"
        ]

httpDecls :: IsTest -> Text -> DomainSpec -> [ServerDecl]
httpDecls isTest name spec =
    [ ServerName [name, "www." `T.append` name]
    , Location "/" $ domainToLocation isTest name spec
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


proxyAttrs :: IsTest -> Text -> [(Text, Text)]
proxyAttrs isTest x = pass ++
    [ ("proxy_set_header", "Host $host")
    , ("proxy_set_header", "X-Real-IP $remote_addr")
    , ("proxy_set_header", "X-Forwarded-For $proxy_add_x_forwarded_for")
    , ("proxy_set_header", "X-Forwarded-Proto $scheme")
    ]
  where
    p = last $ T.splitOn ":" x
    pass 
      | isTest = 
        [ ("resolver", "127.0.0.11")
        , ("proxy_pass", "http://" <> x)
        ]

      | otherwise = 
        [ ("proxy_pass", "http://127.0.0.1:" <> p) ]
