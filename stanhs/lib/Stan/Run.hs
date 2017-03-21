module Stan.Run where

import Stan.AST
import System.Directory
import System.FilePath
import Data.Hashable
import System.Process


runStan :: [Stan] -> [String] -> IO ()
runStan ss dataLines = do
  tmp <- getTemporaryDirectory
  let stdir = tmp </> "stanhs"
      mdlNm = 's': (show $ abs $ hash ss)
      stanFile = stdir </> mdlNm <.> "stan"
      cppFile = stdir </> mdlNm <.> "cpp"
      cmd = "stanc --o="++(cppFile)++" "++(stanFile)
      cmd1 = "stanc "++(stanFile)
  createDirectoryIfMissing False stdir
  writeFile stanFile $ ppStans ss
  _ <- system cmd
  return ()
--     let callgpp = "g++ -DBOOST_RESULT_OF_USE_TR1 -DBOOST_NO_DECLTYPE -DBOOST_DISABLE_ASSERTS -I /opt/stan/src -I /opt/stan/stan/src -isystem /opt/stan/stan/lib/eigen_3.2.2 -isystem /opt/stan/stan/lib/boost_1.55.0 -Wall -pipe -DEIGEN_NO_DEBUG -Wno-unused-local-typedefs  -lpthread  -O3 -o "++(stanBins./mdlName)++" /opt/stan/src/cmdstan/main.cpp -include "++(stanBins./mdlName)++".cpp -L/opt/stan/bin -lstanc"
