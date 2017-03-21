module Stan.Schools where

import Stan.AST

schools :: [Stan]
schools = [
  Data [ TypeDecl (Bounded (Just 0) Nothing Int) "J" []
       , TypeDecl Real "y" [Var "J"]
       , TypeDecl (Bounded (Just 0) Nothing Real) "sigma" [Var "J"]
       ],
  Parameters [ TypeDecl Real "mu" []
             , TypeDecl (Bounded (Just 0) Nothing Real) "tau" []
             , TypeDecl Real "eta" [Var "J"]
             ],
  TransformedParameters [ TypeDecl Real "theta" [Var "J"]
                        , For "j" 1 (Var "J")
                             [Assign ("theta",[Var "j"]) (Var "mu" + Var "tau" * (Ix (Var "eta") [Var "j"]))]

                        ],
  Model [ Distribute ("eta",[]) "normal" [0,1]
        , Distribute ("y",[]) "normal" [Var "theta",Var "sigma"]
        ]
  ]

j :: Int
j = 8

y ::  [Double]
y = [28,  8, -3,  7, -1,  1, 18, 12]

sigma :: [Double]
sigma = [15, 10, 16, 11,  9, 11, 10, 18]
