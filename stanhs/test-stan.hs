module Main where

import Stan.AST
import Stan.IO
import Stan.Run
import Stan.Schools

myExpr :: Expr
myExpr = sin $ Ix (Var "xs") [2+1*3]

myExpr1 :: Expr
myExpr1 = 2*(1+3)

myModel :: Stan
myModel = Model [
  TypeDecl Real "foo" [],
  Assign ("foo", []) myExpr
  ]

main = do
  putStrLn ""
--  putStrLn $ pp myExpr
--  putStrLn $ pp myModel
  let dataLines = [ dumpAs "J" j
                  , dumpAs "y" y
                  , dumpAs "sigma" sigma ]
  putStrLn $ ppStans schools
  putStrLn $ unlines dataLines
  _ <- runStan schools dataLines
  --print res
  return ()
