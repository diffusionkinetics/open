{-# LANGUAGE DeriveAnyClass, DeriveGeneric #-}

module Stan.AST where

import Text.PrettyPrint.HughesPJClass
import Data.Hashable
import GHC.Generics (Generic)


type Var = String

data Stan = Data [Decl]
          | Parameters [Decl]
          | TransformedParameters [Decl]
          | Model [Decl]
            deriving (Eq, Show, Generic, Hashable)

data Decl = TypeDecl Type Var [Expr]
          | Assign (Var,[Expr]) Expr
          | Distribute (Var,[Expr]) String [Expr]
          | For Var Expr Expr [Decl]
            deriving (Eq, Show, Generic, Hashable)

instance Pretty Stan where
  pPrint (Model ds) = text "model {"
                      $$ nest 2 (ppDecls ds)
                      $$ char '}'
  pPrint (Parameters ds)
                    = text "parameters {"
                      $$ nest 2 (ppDecls ds)
                      $$ char '}'
  pPrint (TransformedParameters ds)
                    = text "transformed parameters {"
                      $$ nest 2 (ppDecls ds)
                      $$ char '}'
  pPrint (Data ds) = text "data {"
                      $$ nest 2 (ppDecls ds)
                      $$ char '}'

ppStans :: [Stan] -> String
ppStans = render . vcat . map pPrint

ppDecls :: [Decl] -> Doc
ppDecls = vcat . map ((<>(char ';')) . pPrint)

instance Pretty Decl where
  pPrint (TypeDecl t nm ixs) = pPrint t <+> text nm <> mcommasepBrackets (map pPrint ixs)
  pPrint (Assign (nm,ixes) e) = (text nm <> mcommasepBrackets (map pPrint ixes))
                                  <+> text "=" <+> pPrint e
  pPrint (Distribute (nm,ixes) dnm es) = (text nm <> mcommasepBrackets (map pPrint ixes))
                                  <+> text "~" <+> text dnm <> parens (commasep (map pPrint es))
  pPrint (For vnm elo ehi ds) = let range = pPrint elo <> char ':' <> pPrint ehi
                                in text "for" <+> parens (text vnm <+> text "in" <+> range) <+> char '{'
                                $$ nest 2 (ppDecls ds)
                                $$ char '}'

--          | For Var Expr Expr [Decl]

data Type = Real
          | Int
          | Bounded (Maybe Expr)
                    (Maybe Expr)
                    Type
            deriving (Eq, Show, Generic, Hashable)

instance Pretty Type where
  pPrint Real = text "real"
  pPrint Int = text "int"
  pPrint (Bounded Nothing Nothing t) = pPrint t
  pPrint (Bounded (Just e) Nothing t) = pPrint t <> angles ( ppLowerBound e)
  pPrint (Bounded Nothing (Just e) t) = pPrint t <> angles ( ppUpperBound e)
  pPrint (Bounded (Just e1) (Just e2) t) = pPrint t <> angles ( ppLowerBound e1 <> comma <> ppUpperBound e2)

ppLowerBound :: Expr -> Doc
ppLowerBound e = text "lower=" <> pPrint e
ppUpperBound :: Expr -> Doc
ppUpperBound e = text "upper=" <> pPrint e

angles :: Doc -> Doc
angles d = char '<' <> d <> char '>'

commasep :: [Doc] -> Doc
commasep = hcat . punctuate comma

mcommasepBrackets ::  [Doc] -> Doc
mcommasepBrackets [] = empty
mcommasepBrackets ds = brackets $ commasep ds

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

instance Pretty Expr where
  pPrintPrec _ _ (Var v) = text v
  pPrintPrec _ _ (LitInt x) = int x
  pPrintPrec _ _ (LitFloat x) = float x
  pPrintPrec l r (Ix e es) = pPrintPrec l r e <> brackets (commasep (map (pPrintPrec l r) es))
  pPrintPrec l r (BinOp s e1 e2)
    = let prec = opPrec s
      in maybeParens (r >= prec) $ pPrintPrec l prec e1 <> text s <> pPrintPrec l prec e2
  pPrintPrec l r (Apply fnm es) = text fnm <> parens (commasep (map (pPrintPrec l r) es))

pp :: Pretty a => a -> String
pp = render . pPrint

opPrec :: String -> Rational
opPrec "+" = 6
opPrec "-" = 6
opPrec "*" = 7
opPrec "/" = 7
opPrec _ = 1
