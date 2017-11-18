module Dampf.Nginx.Types
  ( Server(..)
  , ServerDecl(..)
  , pShowServer
  ) where

import           Data.Text          (Text)
import qualified Data.Text as T
import           Text.PrettyPrint


newtype Server
    = Server [ServerDecl]


data ServerDecl
    = Listen Int [String]
    | ServerName [Text]
    | Location Text [(Text, Text)]
    | Include FilePath
    | SSLCertificate FilePath
    | SSLCertificateKey FilePath
    | SSLTrustedCertificate FilePath



pShowServer :: Server -> String
pShowServer = render . pprServer


pprServer :: Server -> Doc
pprServer (Server ds) = text "server" <+> lbrace
    $+$ nest 4 (vcat $ fmap pprServerDecl ds)
    $+$ rbrace


pprServerDecl :: ServerDecl -> Doc
pprServerDecl (Listen p ss)         = text "listen"
    <+> int p <+> vcat (fmap text ss) <> semi

pprServerDecl (ServerName ns)       = text "server_name"
    <+> hsep (fmap (text . T.unpack) ns) <> semi

pprServerDecl (Location p kvs)      = text "location"
    <+> text (T.unpack p) <+> lbrace
    $+$ nest 4 (vcat (fmap ppMap kvs))
    $+$ rbrace

pprServerDecl (Include p)           = text "include"
    <+> text p <> semi

pprServerDecl (SSLCertificate p)    = text "ssl_certificate"
    <+> text p <> semi

pprServerDecl (SSLTrustedCertificate p)    = text "ssl_trusted_certificate"
    <+> text p <> semi


pprServerDecl (SSLCertificateKey p) = text "ssl_certificate_key"
    <+> text p <> semi


ppMap :: (Text, Text) -> Doc
ppMap (k, v) = text (T.unpack k) <+> text (T.unpack v) <> semi

