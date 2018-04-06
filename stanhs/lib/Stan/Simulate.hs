{-# LANGUAGE Rank2Types #-}

module Stan.Simulate where

import Stan.Data
import Stan.AST
import Control.Monad.State
import qualified Data.Map.Strict as Map
import qualified Data.Vector as V

eval :: StanEnv -> Expr -> StanValue
eval _ (LitInt x) = VInt x
eval _ (LitFloat x) = VDouble $ realToFrac x
eval e (BinOp "+" x y) = numOp2 (+) (eval e x) (eval e y)
eval e (BinOp "-" x y) = numOp2 (-) (eval e x) (eval e y)
eval e (BinOp "*" x y) = numOp2 (*) (eval e x) (eval e y)
--eval e (BinOp "/" x y) = numOp2 (/) (eval e x) (eval e y)
eval e (Apply "sqrt" [x]) = fracOp sqrt (eval e x)
eval e (Apply "exp" [x]) = fracOp exp (eval e x)
eval e (Var vnm) = case Map.lookup vnm e of
                     Nothing-> error $ "variable not found: "++vnm
                     Just v -> v

numOp2 :: (forall a . Num a => a -> a -> a) -> StanValue -> StanValue -> StanValue
numOp2 f (VDouble x) (VDouble y) = VDouble $ f x y
numOp2 f (VInt x) (VInt y) = VInt $ f x y

fracOp :: (forall a . Floating a => a -> a) -> StanValue -> StanValue
fracOp f (VDouble x) = VDouble $ f x

simulate :: Decl -> State StanEnv ()
simulate (_ ::: _) = return ()
simulate (Print _ _) = return ()
simulate (For vnm loe hie ds) = do
  env <- get
  let VInt lo = eval env loe
      VInt hi = eval env hie
      oldv = Map.lookup vnm env
  forM_ [lo..hi] $ \i -> do
    modify $ Map.insert vnm (VInt i)
    mapM_ simulate ds
  case oldv of
    Nothing -> modify $ Map.delete vnm 
    Just x -> modify $ Map.insert vnm x
simulate ((vnm,ixs) := e) = do
  env <- get
  let v = eval env e
  case ixs of
    [] -> modify $ Map.insert vnm v
    [eix] -> do
      let VInt ix =  eval env eix
      modify $ Map.adjust (vSet ix v) vnm

vSet :: Int -> StanValue -> StanValue -> StanValue
vSet ix newVal (VArray v) = VArray $ v V.// [(ix,newVal)]