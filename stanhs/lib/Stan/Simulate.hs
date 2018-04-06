{-# LANGUAGE Rank2Types #-}

module Stan.Simulate where

import Stan.Data
import Stan.AST hiding (normal)
import Control.Monad.State
import qualified Data.Map.Strict as Map
import qualified Data.Vector as V
import Data.Random
import Data.Random.Source.PureMT
import Data.Random.Distribution.Normal

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
  setVar vnm ixs $ eval env e
simulate ((vnm,ixs) :~ (distnm, argEs)) = do
  env <- get
  let argVs = map (eval env) argEs
      Just (VSeed oldSeed) = Map.lookup "__seed" env
  let (v, newSeed) = simDist distnm argVs oldSeed 
  modify $ Map.insert "__seed" (VSeed newSeed)
  setVar vnm ixs v

setVar :: Var -> [Expr] -> StanValue -> State StanEnv ()
setVar vnm [] v = modify $ Map.insert vnm v
setVar vnm [eix] v = do
  env <- get
  let VInt ix =  eval env eix
  modify $ Map.alter (vSet ix v) vnm
  
simDist :: String -> [StanValue] -> PureMT-> (StanValue, PureMT)
simDist "normal" [VDouble mu, VDouble sd] seed 
  = sampleState (fmap VDouble $ normal mu sd) seed 
  
vSet :: Int -> StanValue -> Maybe StanValue -> Maybe StanValue
vSet ix newVal (Just (VArray v)) = Just $ VArray $ v V.// [(ix,newVal)]

runSimulate :: [Decl] -> StanEnv -> StanEnv
runSimulate ds = execState (mapM_ simulate ds)


seedEnv :: PureMT -> StanEnv
seedEnv = Map.singleton "__seed" . VSeed

