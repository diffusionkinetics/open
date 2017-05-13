module Lucid.Mjml.Unit where

data Unit = PercentUnit | PxUnit
data WithUnit a = WithUnit a Unit

instance Show Unit where
  show PercentUnit = "%"
  show PxUnit      = "px"

instance Show a => Show (WithUnit a) where
  show (WithUnit a u) = concat [show a, show u]
