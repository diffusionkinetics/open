{-# LANGUAGE OverloadedStrings #-}

module Stan.Schools where

import Stan.AST
import Stan.Data
import Data.Monoid ((<>))


schools :: [Stan]
schools = [
  Data [ lower 0 Int ::: "J"
       , Real ::: "y"!["J"]
       , lower 0 Real ::: "sigma"!["J"]
       ],
  Parameters [ Real ::: "mu"
             , lower 0 Real ::: "tau"
             , Real ::: "eta"!["J"]
             ],
  TransformedParameters [ Real ::: "theta"!["J"]
                        , For "j" 1 "J" [
                            "theta"!["j"] := "mu" + "tau" * "eta"!["j"]
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

schoolData :: StanData
schoolData =   "J" <~ j <>
               "y" <~ y <>
               "sigma" <~ sigma
