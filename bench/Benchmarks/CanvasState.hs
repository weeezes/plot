module Benchmarks.CanvasState
  (benchmarks
  ) where

import Criterion.Main
import qualified Data.Vector.Unboxed as V

import CanvasState
import Types (PlotType(..))

canvasState w h plotType pointCount =
  resize w h (initCanvasState)
  { points = V.generate pointCount (\i -> (fromIntegral i, fromIntegral i))
  , xMin = fromIntegral $ 0
  , xMax = fromIntegral $ pointCount-1
  , yMin = fromIntegral $ 0
  , yMax = fromIntegral $ pointCount-1
  , plotType = plotType
  }

benchmarks =
  [ bgroup "CanvasState_pointPlot" $
      [ bench "1000"    $ nf (plot . canvasState 100 100 PointPlot) 1000
      , bench "10000"   $ nf (plot . canvasState 100 100 PointPlot) 10000
      , bench "100000"  $ nf (plot . canvasState 100 100 PointPlot) 100000
      , bench "1000000" $ nf (plot . canvasState 100 100 PointPlot) 1000000
      ]
  , bgroup "CanvasState_areaPlot" $
      [ bench "1000"    $ nf (plot . canvasState 100 100 AreaPlot) 1000
      , bench "10000"   $ nf (plot . canvasState 100 100 AreaPlot) 10000
      , bench "100000"  $ nf (plot . canvasState 100 100 AreaPlot) 100000
      , bench "1000000" $ nf (plot . canvasState 100 100 AreaPlot) 1000000
      ]
  , bgroup "CanvasState_barPlot" $
      [ bench "1000"    $ nf (plot . canvasState 100 100 BarPlot) 1000
      , bench "10000"   $ nf (plot . canvasState 100 100 BarPlot) 10000
      , bench "100000"  $ nf (plot . canvasState 100 100 BarPlot) 100000
      , bench "1000000" $ nf (plot . canvasState 100 100 BarPlot) 1000000
      ]
  , bgroup "CanvasState_histogramPlot" $
      [ bench "1000"    $ nf (plot . canvasState 100 100 HistogramPlot) 1000
      , bench "10000"   $ nf (plot . canvasState 100 100 HistogramPlot) 10000
      , bench "100000"  $ nf (plot . canvasState 100 100 HistogramPlot) 100000
      , bench "1000000" $ nf (plot . canvasState 100 100 HistogramPlot) 1000000
      ]

  ]