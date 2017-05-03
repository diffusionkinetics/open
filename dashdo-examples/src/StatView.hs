{-# LANGUAGE OverloadedStrings, ExtendedDefaultRules, FlexibleContexts, TemplateHaskell #-}

import System.Statgrab

import Dashdo
import Dashdo.Types
import Dashdo.Serve
import Dashdo.Elements
import Control.Arrow ((&&&))
import Control.Concurrent (forkIO, threadDelay)
import Control.Concurrent.MVar
import Lucid
import Data.Text (Text, unpack, pack)
import Data.Text.Encoding (decodeUtf8)
import Lens.Micro.Platform
import System.Posix.User (UserEntry, userName, userID, getAllUserEntries)

import Graphics.Plotly (plotly, layout, title, Trace, name)
import Graphics.Plotly.Lucid
import Graphics.Plotly.GoG
import Graphics.Plotly.Simple
import Graphics.Plotly.Histogram (histogram)

data SysStats = SysStats
 { statsLoad :: Double
 , statsMem :: Integer
 , statsDiskIO :: (Integer, Integer)
 }

data Example = Example
 { _processFilter :: Tag (Process -> Bool) }

filterIdle, filterNone :: Tag (Process -> Bool)
filterIdle = Tag "a" ((>0.1) . procCPUPercent)
filterNone = Tag "b" (const True)

makeLenses ''Example

main = do
  stats <- newMVar ([] :: [SysStats])
  forkIO (statGrab stats)
  runDashdo (theDashdo stats)

theDashdo mvStats = Dashdo initv (const (getStats mvStats)) example

example :: Example -> ([SysStats], [Process], [UserEntry]) -> SHtml Example ()
example nm (stats, ps, us) = wrap plotlyCDN $ do
  let theData = zip [1..] stats
      mkLine f = line (aes & x .~ fst & y .~ (f . snd)) theData
      cpuLoad = mkLine statsLoad
      memUsage = mkLine (fromIntegral . statsMem)
      diskRead = mkLine (fromIntegral . fst . statsDiskIO) & name ?~ "Read"
      diskWrite = mkLine (fromIntegral . snd . statsDiskIO) & name ?~ "Write"
      processes = hbarChart . map (decodeUtf8 . procName &&& procCPUPercent)
        $ filter (_tagVal $ _processFilter nm) ps 
      userCPU u = let uid = fromIntegral (userID u)
        in sum . map procCPUPercent . filter ((== uid) . procUid) $ ps
      users = hbarChart . filter ((> 0) . snd) $ map (pack . userName &&& userCPU) us

  h2_ "Dashdo Load Monitor"
  checkbox "Hide inactive processes" filterIdle filterNone processFilter
  manualSubmit
  toHtml $ plotly "foo" [cpuLoad] & layout . title ?~ "CPU load"
  toHtml $ plotly "bar" [memUsage] & layout . title ?~ "Memory Usage"
  toHtml $ plotly "baz" [diskRead, diskWrite] & layout . title ?~ "Disk IO"
  toHtml $ plotly "ps" [processes] & layout . title ?~ "CPU Usage by Process"
  toHtml $ plotly "us" [users] & layout . title ?~ "CPU Usage by User"

initv = Example filterNone

getStats :: MVar [SysStats] -> IO ([SysStats], [Process], [UserEntry])
getStats mvStats = do
  ss <- readMVar mvStats
  ps <- runStats snapshots
  us <- getAllUserEntries
  return (ss, ps, us)

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
