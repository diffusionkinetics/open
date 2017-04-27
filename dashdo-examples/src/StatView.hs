{-# LANGUAGE OverloadedStrings, ExtendedDefaultRules, FlexibleContexts #-}

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
import Lens.Micro.Platform

import Graphics.Plotly (plotly, layout, title, Trace, name)
import Graphics.Plotly.Lucid
import Graphics.Plotly.GoG
import Graphics.Plotly.Histogram (histogram)

data SysStats = SysStats
 { statsLoad :: Double
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
      diskRead = mkLine (fromIntegral . fst . statsDiskIO) & name ?~ "Read"
      diskWrite = mkLine (fromIntegral . snd . statsDiskIO) & name ?~ "Write"

  h2_ "Dashdo Load Monitor"
  manualSubmit
  toHtml $ plotly "foo" [cpuLoad] & layout . title ?~ "CPU load"
  toHtml $ plotly "bar" [memUsage] & layout . title ?~ "Memory Usage"
  toHtml $ plotly "baz" [diskRead, diskWrite] & layout . title ?~ "Disk IO"

initv = Example

statGrab :: MVar [SysStats] -> IO ()
statGrab mvStats = diskStats >>= forever
 where diskStats = grab (diskRead &&& diskWrite)
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
