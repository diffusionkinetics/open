{-# LANGUAGE OverloadedStrings, ExtendedDefaultRules #-}

import System.Statgrab

import Dashdo
import Dashdo.Types
import Dashdo.Serve
import Dashdo.Elements
import Control.Arrow ((&&&))
import Control.Concurrent (forkIO, threadDelay)
import Control.Concurrent.MVar (newMVar, readMVar, takeMVar, putMVar, MVar ())
import Lucid
import Data.Text (Text, unpack, pack)
import Lens.Micro.Platform

import Graphics.Plotly (plotly, layout, title, Trace)
import Graphics.Plotly.Lucid
import Graphics.Plotly.GoG
import Graphics.Plotly.Histogram (histogram)

data SysStats = SysStats
 { statsLoad :: Float
 , statsMem :: Integer
 , statsDiskIO :: (Integer, Integer)
 }

data Example = Example

main = do
  stats <- newMVar ([] :: [SysStats])
  forkIO (statGrab stats)
  runDashdo (theDashdo stats)

theDashdo mvStats = Dashdo initv (const (readMVar mvStats)) example

example :: Example -> [SysStats] -> SHtml Example ()
example _ stats = wrap plotlyCDN $ do
  let theData = zip [1..] stats
      mkLine f = line (aes & x .~ fst & y .~ (f . snd)) theData
      cpuLoad = mkLine statsLoad
      memUsage = mkLine (fromIntegral . statsMem)
      diskRead = mkLine (fromIntegral . fst . statsDiskIO)
      diskWrite = mkLine (fromIntegral . snd . statsDiskIO)

  h2_ "Dashdo Load Monitor"
  manualSubmit
  toHtml $ plotly "foo" [cpuLoad] & layout . title ?~ "CPU load"
  toHtml $ plotly "bar" [memUsage] & layout . title ?~ "Memory Usage"
  toHtml $ plotly "baz" [diskRead, diskWrite] & layout . title ?~ "Disk IO"

initv = Example

statGrab :: MVar [SysStats] -> IO ()
statGrab mvStats =  do
    drw <- (diskRead &&& diskWrite) <$> runStats (snapshot :: Stats DiskIO)
    forever drw $ \(diskR', diskW') -> do
        cpu <- (realToFrac . load1) <$> runStats (snapshot :: Stats Load)
        mem <- memUsed <$> runStats (snapshot :: Stats Memory)
        (diskR, diskW) <- (diskRead &&& diskWrite) <$> runStats (snapshot :: Stats DiskIO)
        stats <- takeMVar mvStats
        putMVar mvStats ((SysStats cpu mem (diskR - diskR', diskW - diskW')) : (take 59 stats))
        return (diskR, diskW)
 where forever o f = do
        n <- f o
        threadDelay 1000000
        forever n f
