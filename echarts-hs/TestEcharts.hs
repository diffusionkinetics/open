{-# LANGUAGE OverloadedStrings, ExtendedDefaultRules #-}

import qualified Data.Text.Lazy as T
import qualified Data.Text.Lazy.IO as T
import Lucid
import Lucid.Bootstrap3
import Graphics.Echarts
import Lens.Micro
import Numeric.Datasets.Anscombe
import Data.Aeson
import Data.List
import qualified Data.Text as Text
import Data.Monoid ((<>))
import Data.Text.Encoding (decodeUtf8)
import Data.ByteString.Lazy (toStrict)

main :: IO ()
main = do
  T.writeFile "echartstest.html" $ renderText testpage

testpage =  doctypehtml_ $ do
  head_ $ do
    meta_ [charset_ "utf-8"]
    cdnJqueryJS
    cdnBootstrapJS
    echartsCDN

  body_ $ do
     "Hello World"
     div_ [style_ "width: 600px; height: 600px", id_ "simple"] ""
     script_ $ runEcharts s_element s_options
     div_ [style_ "width: 800px; height: 800px", id_ "pie"] ""
     script_ $ runEcharts p_element p_options
     div_ [style_ "width: 800px; height: 800px", id_ "anscombe"] ""
     script_ $ anscombeData <> runEcharts ans_element ans_options


s_element = "simple"

s_options = mkOptions "simple" s_series

s_series = [mkGraph & series_symbolSize     ?~ 50
                    & series_roam           ?~ True
                    & series_label          ?~ (
                        defLabel & label_normal ?~ (
                            defNormalLabel & normal_show ?~ True))
                    -- & series_label . label_normal . normal_show ?~ True
                    & series_edgeSymbol     ?~ ["cirlce","arrow"]
                    & series_edgeSymbolSize ?~ [4,10]
                    & series_edgeLabel      ?~ (
                        defLabel & label_normal ?~ (
                            defNormalLabel & normal_textStyle ?~ TextStyle (Just 20)))
                    & series_lineStyle      ?~ (
                        defLineStyle & linestyle_normal ?~ (
                            defNormalLineStyle & normal_width     ?~ 2
                                                   & normal_opacity   ?~ 0.9
                                                   & normal_curveness ?~ 0))
                    & series_data   ?~ s_nodes
                    & series_links  ?~ s_edges]

s_nodes = [Data (Just "node1") Nothing (Just 300) (Just 300),
           Data (Just "node2") Nothing (Just 800) (Just 300),
           Data (Just "node3") Nothing (Just 550) (Just 100),
           Data (Just "node4") Nothing (Just 550) (Just 500)]

s_edges = [Link "node1" "node2" (Just $ Label (Just $ NormalLabel (Just True) Nothing Nothing) Nothing) (Just $ LineStyle (Just $ NormalLineStyle (Just 5) (Just 0.2) Nothing) Nothing),
           Link "node2" "node1" (Just $ Label (Just $ NormalLabel (Just True) Nothing Nothing) Nothing) (Just $ LineStyle (Just $ NormalLineStyle (Just 1) (Just 0.2) Nothing) Nothing),
           Link "node1" "node3" Nothing Nothing, Link "node2" "node3" Nothing Nothing,
           Link "node2" "node4" Nothing Nothing, Link "node1" "node4" Nothing Nothing]

{- options = EchartsOptions defTooltip
          $ Series "graph" 50 True (NormalLabel $ NormalLabelData (Just True) Nothing)
             ["circle","arrow"] [4,10] (NormalLabel $ NormalLabelData (Just True) (Just $ TextStyle 20)) (NormalLineStyle $ NormalLineStyle 2 0 0.9) nodes edges
-}

p_element = "pie"

p_options = mkOptions "pie" p_series
                            & options_tooltip ?~ p_tooltip
                            & options_legend  ?~ p_legend
      where
        p_tooltip = defTooltip & tooltip_trigger   ?~ "item"
                               & tooltip_formatter ?~ "{a} <br/>{b}: {c} ({d}%)"
        p_legend = defLegend & legend_orient  ?~ Vertical
                             & legend_data    ?~ map (LegendData) (map (_data_name) p_data)
                             & legend_x       ?~ XRight

p_series = [mkPie & series_name     ?~ "Pie Chart"
                  & series_radius   ?~ thinRadius
                  & series_data     ?~ p_data
                  & series_label    ?~ (
                      defLabel & label_normal   ?~ (defNormalLabel & normal_show ?~ False
                                                                 & normal_position ?~ Center)
                               & label_emphasis ?~ (defEmphasisLabel & emphasis_show ?~ True
                                                                     & emphasis_textStyle ?~ TextStyle (Just 30)))
                 & series_avoidLabelOverlap ?~ False]

p_data = [Data (Just "A") (Just $ PieValue 335) Nothing Nothing,
          Data (Just "B") (Just $ PieValue 310) Nothing Nothing,
          Data (Just "C") (Just $ PieValue 234) Nothing Nothing,
          Data (Just "D") (Just $ PieValue 135) Nothing Nothing,
          Data (Just "E") (Just $ PieValue 1548) Nothing Nothing]


ans_element = "anscombe"

ans_options = mkOptions "ans" ans_series
                              & options_tooltip ?~ (defTooltip & tooltip_formatter ?~ "Group {a}: ({c})" )
                              & options_xAxis   ?~ [Axis 0 (Just 0) (Just 20), Axis 1 (Just 0) (Just 20),
                                                    Axis 2 (Just 0) (Just 20), Axis 3 (Just 0) (Just 20)]
                              & options_yAxis   ?~ [Axis 0 (Just 0) (Just 15), Axis 1 (Just 0) (Just 15),
                                                    Axis 2 (Just 0) (Just 20), Axis 3 (Just 0) (Just 20)]
                              & options_grid    ?~ ans_grid
    where
      ans_grid = [ defGrid & grid_left ?~ "7%" & grid_top ?~ "7%" & grid_width ?~ "38%" & grid_height ?~ "38%"
                 , defGrid & grid_left ?~ "55%" & grid_top ?~ "7%" & grid_width ?~ "38%" & grid_height ?~ "38%"
                 , defGrid & grid_left ?~ "7%" & grid_top ?~ "55%" & grid_width ?~ "38%" & grid_height ?~ "38%"
                 , defGrid & grid_left ?~ "55%" & grid_top ?~ "55%" & grid_width ?~ "38%" & grid_height ?~ "38%"]

ans_series = [ mkScatter & series_name       ?~ "I"
                         & series_xAxisIndex ?~ 0
                         & series_yAxisIndex ?~ 0
                         & series_data       ?~ ans_data0
             , mkScatter & series_name       ?~ "II"
                         & series_xAxisIndex ?~ 1
                         & series_yAxisIndex ?~ 1
                         & series_data       ?~ ans_data1
             , mkScatter & series_name       ?~ "III"
                         & series_xAxisIndex ?~ 2
                         & series_yAxisIndex ?~ 2
                         & series_data       ?~ ans_data2
             , mkScatter & series_name       ?~ "IV"
                         & series_xAxisIndex ?~ 3
                         & series_yAxisIndex ?~ 3
                         & series_data       ?~ ans_data3]

ans_data0 = map mkData $ map (Just . ScatterValue) $ tupleToList anscombe1

ans_data1 = map mkData $ map (Just . ScatterValue) $ tupleToList anscombe2

ans_data2 = map mkData $ map (Just . ScatterValue) $ tupleToList anscombe3

ans_data3 = map mkData $ map (Just . ScatterValue) $ tupleToList anscombe4

anscombeData :: Text.Text
anscombeData = Text.unlines [
  "",
  "var ans_data0 = " <> (decodeUtf8 $ toStrict $ encode $ tupleToList anscombe1),
  "var ans_data1 = " <> (decodeUtf8 $ toStrict $ encode $ tupleToList anscombe2),
  "var ans_data2 = " <> (decodeUtf8 $ toStrict $ encode $ tupleToList anscombe3),
  "var ans_data3 = " <> (decodeUtf8 $ toStrict $ encode $ tupleToList anscombe4),
  ""
  ]

mkData :: Maybe DataValue -> Data
mkData x = Data Nothing x Nothing Nothing

tupleToList :: [(Double,Double)] -> [[Double]]
tupleToList xss =
  let xs = unzip xss
  in transpose [fst xs, snd xs]
