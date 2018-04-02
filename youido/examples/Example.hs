{-# LANGUAGE OverloadedStrings, ExistentialQuantification, ScopedTypeVariables,
   ExtendedDefaultRules, FlexibleContexts, TemplateHaskell,
   OverloadedLabels, TypeOperators, DataKinds #-}

import Youido.Serve
import Youido.Types
import Youido.Dashdo
import Lucid
import Lucid.Bootstrap
import Lucid.Bootstrap3
import Numeric.Datasets.Gapminder
import Numeric.Datasets
import Control.Monad.Reader
import Data.List (nub)
import Network.Wai
import Data.Text (Text, pack)
import Data.Monoid
import Lens.Micro.Platform
--import Graphics.Plotly.Lucid.hs

import Dashdo
import Dashdo.Types
import Dashdo.Elements
import Dashdo.FlexibleInput

data Countries = Countries
               | Country Text

instance FromRequest Countries where
  fromRequest (rq,_) = case pathInfo rq of
    "countries":_ -> Just Countries
    "country":cnm:_ -> Just $ Country cnm
    _ -> Nothing

instance ToURL Countries where
  toURL Countries = "/countries"
  toURL (Country cnm) = "/country/"<>cnm

countryH :: Monad m => [Gapminder] -> Countries -> ReaderT a m (Html ())
countryH gapM Countries = do
  let countries = nub $ map country gapM
  return $ ul_ $ forM_ countries $ \c -> li_ $ a_ [href_ $ toURL $ Country c] $ (toHtml c)
countryH gapM (Country c) = do
  let entries = filter ((==c) . country) gapM
  return $ ul_ $ forM_ entries $ \e ->
    li_ $ "year: " <> toHtml (show $ year e) <> "  population: "<> toHtml (show $ pop e)


data BubblesDD = BubblesDD { _selYear :: Int} deriving Show
makeLenses ''BubblesDD

bubblesDD gapM = do
  let years = nub $ map year gapM
  selYear <<~ select (map showOpt years)
  h2_ "hello world"
  BubblesDD y <- getValue
  p_ (toHtml $ show $ y)
  clickAttrs <- onClickDo $ \dd -> do
    liftIO $ putStrLn $ "current state: "++show dd
    return Reset
  button_ (clickAttrs) "Print state"



sidebar = mkSidebar
    [ "Bubbles"  *~ #bubbles :/ Initial
    , "Counties" *~ Countries
    ]

main :: IO ()
main = do
  gapM <- getDataset gapminder
  serveY () $ do
    dashdoGlobal
    dashdo #bubbles $ Dashdo (BubblesDD 1980) (bubblesDD gapM)
    port .= 3101
    wrapper .= const (stdWrapper (mempty) sidebar)
    handle $ countryH gapM

