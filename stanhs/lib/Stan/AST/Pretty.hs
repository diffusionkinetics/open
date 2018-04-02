{-# LANGUAGE DeriveAnyClass, DeriveGeneric #-}

module Stan.AST.Pretty where

import Stan.AST
import Text.PrettyPrint.HughesPJClass

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
  pPrint (TransformedData ds)
                    = text "transformed data {"
                      $$ nest 2 (ppDecls ds)
                      $$ char '}'
  pPrint (GeneratedQuantities ds)
                    = text "generated quantities {"
                      $$ nest 2 (ppDecls ds)
                      $$ char '}'

ppStans :: [Stan] -> String
ppStans = render . vcat . map pPrint

ppDecls :: [Decl] -> Doc
ppDecls = vcat . map ((<>(char ';')) . pPrint)

instance Pretty Decl where
  pPrint (t ::: (nm, ixs)) = pPrint t <+> text nm <> mcommasepBrackets (map pPrint ixs)
  pPrint ((nm,ixes) := e) = (text nm <> mcommasepBrackets (map pPrint ixes))
                                  <+> text "=" <+> pPrint e
  pPrint ((nm,ixes) :~ (dnm, es)) = (text nm <> mcommasepBrackets (map pPrint ixes))
                                  <+> text "~" <+> text dnm <> parens (commasep (map pPrint es))
  pPrint (For vnm elo ehi ds) = let range = pPrint elo <> char ':' <> pPrint ehi
                                in text "for" <+> parens (text vnm <+> text "in" <+> range) <+> char '{'
                                $$ nest 2 (ppDecls ds)
                                $$ char '}'
  pPrint (Print s es) = text "print" <> parens (commasep $ text (show s) : map pPrint es)


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

