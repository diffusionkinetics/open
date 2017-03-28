module Fuml.Base.LinearRegression where

import Numeric.LinearAlgebra

-- | Ordniary least squares
ols :: [(Vector Double, Double)] -> Vector Double
ols xys =
  let x = fromRows $ map fst xys
      y = col $ map snd xys
      betaMat = inv (tr' x <> x) <> tr' x <> y
  in betaMat ! 0

ridge :: Matrix Double -> [(Vector Double, Double)] -> Vector Double
ridge gamma xys =
  let x = fromRows $ map fst xys
      y = col $ map snd xys
      betaMat = inv (tr' x <> x + tr' gamma <> gamma) <> tr' x <> y
  in betaMat ! 0
