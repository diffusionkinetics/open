{-# LANGUAGE OverloadedStrings #-}

module Lucid.Mjml.Column(
  column_
  ) where

import Control.Monad

import Lucid.Html5
import Lucid.Base

import Lucid.Mjml.Component
import Lucid.Mjml.Unit
import Lucid.Base

import Control.Monad.Reader
import Control.Monad.State

import qualified Data.Text as T
import Text.Parsec

import qualified Blaze.ByteString.Builder.Html.Utf8 as Blaze
import           Blaze.ByteString.Builder (Builder)
import Data.Monoid

import qualified Data.HashMap.Strict as HM

-- Takes a width like '11.4px' and gives WithUnit Int
widthParser ::  T.Text -> Either ParseError (WithUnit Int)
widthParser = parse (WithUnit <$> ignoreDecimal <*> unitParser) "" where
  rd = read :: String -> Int
  integer = many1 (digit)
  decimal = optional $ (:) <$> (char '.') <*> integer
  ignoreDecimal = rd <$> (integer <* decimal)
  unitParser = (string "px" *> return PxUnit) <|> (string "%" *> return PercentUnit) <|> (return PxUnit)


parsedWidth :: Int -> Maybe T.Text -> Either ParseError (WithUnit Int)
parsedWidth sibling width =
  maybe (Right $ WithUnit computedWidth PercentUnit) widthParser width
  where
    computedWidth = 100 `div` sibling

throwError :: Either ParseError a -> a
throwError (Left t) = error (show t)
throwError (Right x) = x

renderBefore :: Int -> Maybe T.Text -> Builder
renderBefore sibling widthAttr = Blaze.fromString "<!--[if mso | IE]>"
                                 <> Blaze.fromString "<table role=\"presentation\" border=\"0\" cellpadding=\"0\" cellspacing=\"0\"><tr>"
                                 <> Blaze.fromString "<td style=\"vertical-align:top;width:"
                                 <> Blaze.fromShow width
                                 <> Blaze.fromString "\"><![endif]-->"
  where
    width = throwError (parsedWidth sibling widthAttr)

renderAfter :: Builder
renderAfter = Blaze.fromString "<!--[if mso | IE]></td></tr></table><![endif]-->"

columnClass :: Int -> Maybe T.Text -> Builder
columnClass sibling widthAttr = case (throwError w) of
  (WithUnit t PxUnit) -> Blaze.fromString "mj-column-px-" <> Blaze.fromShow t
  (WithUnit t PercentUnit) -> Blaze.fromString "mj-column-per-" <> Blaze.fromShow t
  where
    w = parsedWidth sibling widthAttr

renderChild :: Monad m => HM.HashMap T.Text T.Text -> ElementT m () -> (MjmlT (ReaderT ElementContext m)) ()
renderChild _ (ElementT True r) = r
renderChild _ (ElementT False r) = td_ $ tr_ r

render :: Monad m => HM.HashMap T.Text T.Text -> [ElementT m ()] -> MjmlT (ReaderT ElementContext m) ()
render attrs children = do
  ec <- ask
  let widthAttr = HM.lookup "width" attrs
      sib = case (sibling ec) of
        Nothing -> error "Needs sibling in context"
        Just t -> t

  -- addMediaQuery _
  build (renderBefore sib widthAttr)

  div_ [class_ "", class_ "", style_ ""] $
    table_ [border_ "0", cellpadding_ "0", cellspacing_ "0", role_ "presentation", width_ "100%"] $
    tbody_ $ forM_ children $ (\e -> local (\ec ->  ec {sibling = Just $ length children}) (renderChild attrs e))
  build renderAfter

column_ :: Monad m => [Attribute] -> [ElementT m ()] -> ElementT m ()
column_ attrs children = ElementT False (render attrMap children)
  where
     attrMap = HM.fromListWith (<>) (map toPair attrs)
