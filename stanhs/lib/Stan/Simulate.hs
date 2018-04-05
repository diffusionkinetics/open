module Stan.Simulate where

import Stan.Data
import Stan.AST

eval :: StanEnv -> Expr -> StanValue
eval _ (LitInt x) = VInt x
eval _ (LitFloat x) = VDouble $ realToFrac x
