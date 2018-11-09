{-# LANGUAGE DeriveGeneric, OverloadedStrings,FlexibleInstances, TemplateHaskell #-}

{-|

Re-exports the Simple interface, the grammar of grpahics 
interface and parts of the base interface.

-}

module Graphics.Plotly (
  module Base,
  module Simple,
  module GoG
) where

import Graphics.Plotly.Base as Base 
  hiding (x,y, z, _x, _y, _z, _size, _line, size, line)
import Graphics.Plotly.Simple as Simple
import Graphics.Plotly.GoG as GoG

