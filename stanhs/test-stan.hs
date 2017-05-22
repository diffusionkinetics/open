module Main where

import Stan.AST
import Stan.AST.Pretty
import Stan.Run
import Stan.Schools

myExpr :: Expr
myExpr = sin $ Ix (Var "xs") [2+1*3]

myExpr1 :: Expr
myExpr1 = 2*(1+3)

myModel :: Stan
myModel = Model [
  Type Real "foo" [],
  ("foo", []) := myExpr
  ]

main :: IO ()
main = do
  putStrLn ""
--  putStrLn $ pp myExpr
--  putStrLn $ pp myModel
  putStrLn $ ppStans schools
  res <- runStan schools schoolData sample {numSamples = 1000}
  putStrLn $ take 400 $ show res
  res1 <- runStan schools schoolData optimize
  putStrLn $ take 400 $ show res1
  return ()
