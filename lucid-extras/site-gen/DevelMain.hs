{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE ExtendedDefaultRules #-}

module DevelMain where
import Control.Monad

import Lucid.Mjml.Samples.Postgres

import System.Directory

import Lucid hiding (section_)

import Lucid.Mjml

html :: Monad m => HtmlT m ()
html = renderMjmlT (mjml_ undefined body)
--  where
    --go x = Identity $ runReaderT (evalState x _) _

  --evalState (runReaderT (renderer mjml) _) (MjmlState HM.empty) -- renderer _ _ -- (mjml exectatsst

update :: IO ()
update = do
  createDirectoryIfMissing True "sample-site"
  renderToFile "sample-site/index.html" html
