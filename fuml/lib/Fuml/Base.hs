{-# LANGUAGE FlexibleContexts #-}


module Fuml.Base where

import qualified Data.Vector.Generic as V

fdgrad :: V.Vector v Double => (v Double -> Double) -> v Double -> v Double
fdgrad f xv = V.imap g xv where
  g ix x = let h = if abs x > 1e-7
                      then abs (x) * 2e-5
                      else 1e-10
               plus = xv V.// [(ix, x+h)]
               minus = xv V.// [(ix, x-h)]
           in (f plus - f minus)/(2*h)

softMax :: [(a, Double)] -> [(a, Double)]
softMax axs = let d = sum $ map (exp . snd) axs
                  f (a,x) = (a, exp x / d)
              in map f axs
