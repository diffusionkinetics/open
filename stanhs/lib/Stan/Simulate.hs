{-# LANGUAGE Rank2Types #-}

module Stan.Simulate where

import Stan.Data
import Stan.AST

eval :: StanEnv -> Expr -> StanValue
eval _ (LitInt x) = VInt x
eval _ (LitFloat x) = VDouble $ realToFrac x
eval e (BinOp "+" x y) = numOp2 (+) (eval e x) (eval e y)
eval e (BinOp "-" x y) = numOp2 (-) (eval e x) (eval e y)
eval e (BinOp "*" x y) = numOp2 (*) (eval e x) (eval e y)
--eval e (BinOp "/" x y) = numOp2 (/) (eval e x) (eval e y)

numOp2 :: (forall a . Num a => a -> a -> a) -> StanValue -> StanValue -> StanValue
numOp2 f (VDouble x) (VDouble y) = VDouble $ f x y

fracOp :: (forall a . Fractional a => a -> a) -> StanValue -> StanValue
fracOp f (VDouble x) = VDouble $ f x
