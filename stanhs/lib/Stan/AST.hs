{-# LANGUAGE DeriveAnyClass, DeriveGeneric, FlexibleInstances #-}

module Stan.AST where

import Data.Hashable
import GHC.Generics (Generic)
import Data.String

type Var = String

data Stan = Data [Decl]
          | TransformedData [Decl]
          | Parameters [Decl]
          | TransformedParameters [Decl]
          | Model [Decl]
          | GeneratedQuantities [Decl]
            deriving (Eq, Show, Generic, Hashable)

data Decl = Type ::: (Var,[Expr])
          | (Var,[Expr]) := Expr
          | (Var,[Expr]) :~ (String, [Expr])
          | For Var Expr Expr [Decl]
          | Print String [Expr]
            deriving (Eq, Show, Generic, Hashable)

data Type = Real
          | Int
          | Bounded (Maybe Expr)
                    (Maybe Expr)
                    Type
            deriving (Eq, Show, Generic, Hashable)

data Expr = LitInt Int
          | LitFloat Float
          | BinOp String Expr Expr
          | Ix Expr [Expr]
          | Apply String [Expr]
          | Var Var
            deriving (Eq, Show, Generic, Hashable)

infixl 1 :=
infixl 1 :::
class Indexable a where
  (!) :: a -> [Expr] -> a

instance Indexable Expr where
  (!) = Ix

instance Indexable (Var,[Expr]) where
  (v,exprs) ! es = (v,exprs++es)

instance Num Expr where
  e1 + e2 = BinOp "+" e1 e2
  e1 - e2 = BinOp "-" e1 e2
  e1 * e2 = BinOp "*" e1 e2
  negate e = Apply "-" [e]
  abs e = Apply "abs" [e]
  signum _ = error "stan: signum?"
  fromInteger x = LitInt (fromInteger x)

instance Fractional Expr where
  e1 / e2 = BinOp "/" e1 e2
  fromRational x = LitFloat $ fromRational x

instance Floating Expr where
  pi = LitFloat pi
  exp e = Apply "exp" [e]
  log e = Apply "log" [e]
  sqrt e = Apply "sqrt" [e]
  sin e = Apply "sin" [e]
  cos e = Apply "cos" [e]
  tan e = Apply "tan" [e]
  asin e = Apply "asin" [e]
  acos e = Apply "acos" [e]
  atan e = Apply "atan" [e]
  asinh e = Apply "asinh" [e]
  acosh e = Apply "acosh" [e]
  atanh e = Apply "atanh" [e]
  sinh e = Apply "sinh" [e]
  cosh e = Apply "cosh" [e]
  tanh e = Apply "tanh" [e]

instance IsString Expr where
  fromString s = Var s

instance IsString (Var,[Expr]) where
  fromString s = (s, [])

normal :: (Expr , Expr) -> (String, [Expr])
normal (mn, sd) = ("normal", [mn,sd])

gamma :: (Expr , Expr) -> (String, [Expr])
gamma (a, b) = ("gamma", [a,b])

exponential :: Expr -> (String, [Expr])
exponential mu = ("exponential", [mu])

dot :: Expr -> Expr -> Expr
dot e1 e2 = Apply "dot_product" [e1,e2]

lower :: Expr -> Type -> Type
lower lo ty = Bounded (Just lo) Nothing ty