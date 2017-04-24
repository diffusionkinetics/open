{-# LANGUAGE OverloadedStrings, StandaloneDeriving #-}

module Inliterate where

import qualified Data.Text as T
import qualified Data.Text.Lazy as TL
import qualified Data.Text.Lazy.IO as TL
import Data.Text (Text)
import Data.List (isPrefixOf)
import qualified Data.Text.IO as T
import Cheapskate
import Cheapskate.Html
import Text.Read (readMaybe)
import Data.Foldable (toList)
import Data.Set (Set)
import qualified Data.Set as Set
import qualified Data.Sequence as Seq
import Text.Blaze.Html.Renderer.Text (renderHtml)
import Lucid
import Data.Monoid ((<>))
import Language.Haskell.Exts hiding (Do)

import Inliterate.Inspect
import Inliterate.Import

dumpDoc :: FilePath -> IO ()
dumpDoc fp = do
  t <- T.readFile fp
  let md = markdown def t
  print md
  mapM_ print $ codeBlocks md


parseCodeInfo :: Text -> Set CodeType
parseCodeInfo = Set.fromList . map parse1 . T.words where
  parse1 t = case readMaybe $ T.unpack $ T.toTitle t of
               Just ct -> ct
               Nothing -> error $ "unknown code type: "++T.unpack t

genHaskell :: Doc -> Text
genHaskell doc =
  let cbs = codeBlocks doc
      toplns = map snd $ filter ((Top `Set.member`) . fst) cbs
      mparsedTop = parseFileContents $ T.unpack $ T.unlines toplns
      newTop = case mparsedTop of
                 ParseFailed l s -> error $ "parse failure: "++s++" at "++show l
                 ParseOk m -> T.pack $ prettyPrint $ addTheImport m
      inDoBody (cb, _) = Do `Set.member` cb
      printDoBody (_,t) = T.lines $ chomp t
--      asks = map snd $ filter ((Eval `Set.member`) . fst) cbs
--      printAsk t = ["ask $ " `T.append` chomp t]
      doBody = concatMap printDoBody (filter inDoBody cbs) ++
               concatMap printBlock (getBlocks doc) ++
               ["return ()"]
      hdr = unlines $ map (T.unpack . codeBlockBody) $ filter isHtmlHeader $ getBlocks doc

  in T.unlines $ [newTop, "main = Inliterate.Import.wrapMain "<>T.pack (show hdr)<>" $ do"] ++ map ("  " `T.append`) doBody

addTheImport :: Module SrcSpanInfo -> Module SrcSpanInfo
addTheImport (Module l v1 v2 v6 decls) = Module l v1 v2 (m:v6) decls
  where m = ImportDecl {importAnn = l,
                        importModule = ModuleName l "Inliterate.Import",
                        importQualified = True,
                        importSrc = False,
                        importSafe = False,
                        importPkg = Nothing,
                        importAs = Just (ModuleName l "Inliterate.Import"),
                        importSpecs = Nothing}

printBlock :: Block -> [Text]
printBlock blk@(CodeBlock (CodeAttr "haskell" ci) t)
     | Eval `Set.member` ct = printAsk ct t
     | Hide `Set.member` ct = []
     | otherwise = printAnyBlock blk
  where ct = parseCodeInfo ci
printBlock blk = if isHtmlHeader blk then [] else printAnyBlock blk

isHtmlHeader (CodeBlock (CodeAttr "html_header" ci) t) = True
isHtmlHeader _ = False

codeBlockBody (CodeBlock (CodeAttr "html_header" ci) t) = t

printAsk :: Set CodeType -> Text -> [Text]
printAsk cts t
  = let showCts = map (T.pack . ("Inliterate.Import."++) . show) (Set.toList cts)
    in [T.concat ["Inliterate.Import.askInliterate ", escape $ chomp t, " ",
               "[",T.intercalate "," showCts, "]"  , " (", chomp t, ")"]]

printAnyBlock :: Block -> [Text]
printAnyBlock blk =
  map (("putStrLn " `T.append`) . escape)
  $ T.lines
  $ TL.toStrict
  $ renderHtml
  $ renderBlocks def (Seq.singleton blk)

escape :: Text -> Text
escape = T.pack . show . T.unpack

chomp :: Text -> Text
chomp = T.dropWhile (=='\n') . T.strip . T.dropWhileEnd (=='\n')

getBlocks :: Doc -> [Block]
getBlocks (Doc _ sblocks) = removeOptionsGhc $ toList sblocks

codeBlocks :: Doc -> [(Set CodeType, Text)]
codeBlocks d =
  [(parseCodeInfo ci, body)
      | CodeBlock (CodeAttr "haskell" ci) body
          <- getBlocks d]

removeOptionsGhc :: [Block] -> [Block]
removeOptionsGhc allBlks@(Para inls:blks)
    | [Str "{",Str "-",Str "#",Space,Str "OPTIONS",Str "_",Str "GHC"] `isPrefixOf` toList inls
         = removeOptionsGhc blks
    | [Str "{",Str "-",Str "#",Space,Str "LANGUAGE"] `isPrefixOf` toList inls
         = removeOptionsGhc blks
    | otherwise = allBlks
removeOptionsGhc blks = blks

deriving instance Eq Inline

  -- get extra headers
