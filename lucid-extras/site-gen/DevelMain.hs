{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE ExtendedDefaultRules #-}

module DevelMain where
import Control.Monad

import System.Directory

import Lucid hiding (section_)

import Lucid.Mjml

body :: Monad m => ElementT m ()
body = container_ [] [sec]
  where
    col1 = column_ []
      [divider_ [borderColor_ "#F45E43"]
      , text_ [fontSize_ "20px", color_ "#F45E43"] "Hello Mjml"]
--    col2 = column_ [] [ElementT False $ toHtml "col2"]
    sec = section_ [] [col1]

html :: Monad m => HtmlT m ()
html = renderMjmlT (mjml_ undefined body)
--  where
    --go x = Identity $ runReaderT (evalState x _) _

  --evalState (runReaderT (renderer mjml) _) (MjmlState HM.empty) -- renderer _ _ -- (mjml exectatsst

update :: IO ()
update = do
  createDirectoryIfMissing True "sample-site"
  renderToFile "sample-site/index.html" html
