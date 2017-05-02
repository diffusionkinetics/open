module Fuml.Base.LinearRegression where

import Numeric.LinearAlgebra

-- | Ordinary least squares
ols :: [(Vector Double, Double)] -> Vector Double
ols xys =
  let x = fromRows $ map fst xys
      y = col $ map snd xys
      betaMat = inv (tr' x <> x) <> tr' x <> y
  in betaMat ! 0

-- | Weighted ordinary least squares
wols :: Vector Double -> [(Vector Double, Double)] -> Vector Double
wols wvs xys =
  let w = diag wvs
      x = fromRows $ map fst xys
      y = col $ map snd xys
      betaMat = inv (tr' x <> w <> x) <> tr' x <> w <> y
  in betaMat ! 0

-- | Ridge regression
ridge :: Matrix Double -> [(Vector Double, Double)] -> Vector Double
ridge gamma xys =
  let x = fromRows $ map fst xys
      y = col $ map snd xys
      betaMat = inv (tr' x <> x + tr' gamma <> gamma) <> tr' x <> y
  in betaMat ! 0
