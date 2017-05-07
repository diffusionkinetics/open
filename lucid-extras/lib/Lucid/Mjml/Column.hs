module Lucid.Mjml.Column where

import Lucid.Mjml.Component
import Lucid.Base

import qualified Data.Text as T
import Text.Parsec

import qualified Blaze.ByteString.Builder.Html.Utf8 as Blaze
import           Blaze.ByteString.Builder (Builder)
import Data.Monoid

import qualified Data.HashMap.Strict as HM

data Unit = PercentUnit | PxUnit
data WithUnit a = WithUnit a Unit

instance Show Unit where
  show PercentUnit = "%"
  show PxUnit      = "px"

instance Show a => Show (WithUnit a) where
  show (WithUnit a u) = concat [show a, show u]

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

--render' :: (Monad m, Element c) => [c] -> MjmlT m ()
--render' children = HtmlT $ return _
