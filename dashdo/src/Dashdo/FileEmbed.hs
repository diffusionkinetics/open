module Dashdo.FileEmbed (embedFile) where

-- wrapper around fileEmbed that forces ghc to
-- recompile when dependent files change
-- see http://stackoverflow.com/questions/8570555

import qualified Data.FileEmbed as DFE

import Language.Haskell.TH.Syntax

embedFile :: FilePath -> Q Exp
embedFile path = do
  qAddDependentFile path
  DFE.embedFile path
