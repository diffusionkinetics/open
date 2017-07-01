{-# LANGUAGE OverloadedStrings, ExtendedDefaultRules, FlexibleContexts, TemplateHaskell #-}

import System.Statgrab

import Dashdo
import Dashdo.Types
import Dashdo.Serve
import Dashdo.Elements
import Dashdo.Rdash (rdash, charts, controls, inDashboard)
import Control.Arrow ((&&&), second)
import Control.Concurrent (forkIO, threadDelay)
import Control.Concurrent.MVar
import Lucid
import Data.Text (Text, unpack, pack)
import Data.Text.Encoding (decodeUtf8)
import Lens.Micro.Platform
import System.Posix.User (UserEntry, userName, userID, getAllUserEntries)
import Lucid.Bootstrap3 (rowEven, Breakpoint( MD ))
import Lucid.Bootstrap (row_)

import Graphics.Plotly (plotly, layout, title, Trace, name, thinMargins, margin)
import Graphics.Plotly.Lucid
import Graphics.Plotly.GoG
import Graphics.Plotly.Simple
import Graphics.Plotly.Histogram (histogram)

-- System Load dashdo

data SysStats = SysStats
 { statsLoad :: Double
 , statsMem :: Integer
 , statsDiskIO :: (Integer, Integer)
 }

data Unused = Unused

loadDashdo mvStats = Dashdo Unused (const (readMVar mvStats)) load

load :: Unused -> [SysStats] -> SHtml Unused ()
load _ stats = do
  let theData = zip [1..] stats
      mkLine f = line (aes & x .~ fst & y .~ (f . snd)) theData
      cpuLoad = mkLine statsLoad
      memUsage = mkLine (fromIntegral . statsMem)
      diskRead = mkLine (fromIntegral . fst . statsDiskIO) & name ?~ "Read"
      diskWrite = mkLine (fromIntegral . snd . statsDiskIO) & name ?~ "Write"

  submitPeriodic 3
  charts
     [ ("CPU Load",     toHtml $ plotly "foo" [cpuLoad] & layout . margin ?~ thinMargins)
     , ("Memory Usage", toHtml $ plotly "bar" [memUsage] & layout . margin ?~ thinMargins)
     , ("Disk IO",      toHtml $ plotly "baz" [diskRead, diskWrite] & layout . margin ?~ thinMargins)]

-- end of System Load dashdo


-- Processes dashdo

data PsCtl = PsCtl
 { _processFilter :: Tag (Process -> Bool)
 , _processUser :: Text
 }

psActive, psAll :: Tag (Process -> Bool)
psActive = Tag "a" ((>0.1) . procCPUPercent)
psAll = Tag "b" (const True)

makeLenses ''PsCtl

psDashdo = Dashdo (PsCtl psAll "") (const getStats) process

process :: PsCtl -> ([Process], [UserEntry]) -> SHtml PsCtl ()
process ctl (ps, us) = do
  let user = map (fromIntegral . userID) $ filter ((== _processUser ctl) . pack . userName) us
      userFilter (uid:_) = ((== uid) . procUid)
      userFilter _ = const True
      processes = hbarChart . map (decodeUtf8 . procName &&& procCPUPercent)
        $ filter (_tagVal $ _processFilter ctl) $ filter (userFilter user) ps
      userCPU u = let uid = fromIntegral (userID u)
        in sum . map procCPUPercent . filter ((== uid) . procUid) $ ps
      users = hbarChart . filter ((> 0) . snd) $ map (pack . userName &&& userCPU) us

  inDashboard $
    checkbox "Hide inactive processes" psActive psAll processFilter

  charts
     [ ("CPU Usage by Process", toHtml $ plotly "ps" [processes] & layout . margin ?~ thinMargins)
     , ("CPU Usage by User",    plotlySelect (plotly "us" [users] & layout . margin ?~ thinMargins) "y" processUser)]

getStats :: IO ([Process], [UserEntry])
getStats = do
  ps <- runStats snapshots
  us <- getAllUserEntries
  return (ps, us)

statGrab :: MVar [SysStats] -> IO ()
statGrab mvStats = diskStats >>= forever
 where diskStats = ((sum . map fst &&& sum . map snd) . map (diskRead &&& diskWrite))
           <$> runStats snapshots
       grab f = f <$> runStats snapshot
       forever o = do
           n <- update o
           threadDelay 1000000
           forever n
       update (r', w') = do
           cpu <- grab load1
           mem <- grab memUsed
           (r, w) <- diskStats
           modifyMVar_ mvStats (return . take 60 . (SysStats cpu mem (r-r', w-w'):))
           return (r, w)

-- end of Processes dashdo

main = do
  stats <- newMVar ([] :: [SysStats])
  forkIO (statGrab stats)
  let dashdos = [ RDashdo "load" "System Load" $ loadDashdo stats
                , RDashdo "process" "Processes" psDashdo ]
  html <- rdash dashdos plotlyCDN
  runRDashdo html $ dashdos