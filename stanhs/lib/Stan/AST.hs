{-# LANGUAGE DeriveAnyClass, DeriveGeneric #-}

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

data Decl = TypeDecl Type Var [Expr]
          | Assign (Var,[Expr]) Expr
          | Distribute (Var,[Expr]) String [Expr]
          | For Var Expr Expr [Decl]
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

