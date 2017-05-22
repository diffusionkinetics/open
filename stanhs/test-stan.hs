module Main where

import Stan.AST
import Stan.AST.Pretty
import Stan.Data
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

main :: IO ()
main = do
  putStrLn ""
--  putStrLn $ pp myExpr
--  putStrLn $ pp myModel
  let dataLines = [ dumpAs "J" j
                  , dumpAs "y" y
                  , dumpAs "sigma" sigma ]
  putStrLn $ ppStans schools
  putStrLn $ unlines dataLines
  res <- runStan schools dataLines sample {numSamples = 1000}
  putStrLn $ take 400 $ show res
  res1 <- runStan schools dataLines optimize
  putStrLn $ take 400 $ show res1
  return ()
