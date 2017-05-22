{-# LANGUAGE OverloadedStrings #-}

module Stan.Schools where

import Stan.AST

schools :: [Stan]
schools = [
  Data [ Type (lower 0 Int) "J" []
       , Type Real "y" ["J"]
       , Type (lower 0 Real) "sigma" ["J"]
       ],
  Parameters [ Type Real "mu" []
             , Type (lower 0 Real) "tau" []
             , Type Real "eta" ["J"]
             ],
  TransformedParameters [ Type Real "theta" ["J"]
                        , For "j" 1 "J" [
                            ("theta",["j"]) := "mu" + "tau" * "eta"!["j"]
                            ]

                        ],
  Model [ "eta" :~ normal (0,1)
        , "y" :~ normal ("theta","sigma")
        ]
  ]

j :: Int
j = 8

y ::  [Double]
y = [28,  8, -3,  7, -1,  1, 18, 12]

sigma :: [Double]
sigma = [15, 10, 16, 11,  9, 11, 10, 18]
