module Types
  ( Canvas(..)
  , UiEvent(..)
  , Point(..)
  , Settings(..)
  , PlotType(..)
  ) where

import qualified Data.Vector.Unboxed as V

type Canvas = V.Vector Int

type Point = (Double,Double)
data UiEvent = Redraw (V.Vector Point) | Die
data PlotType = PointPlot | AreaPlot | BarPlot | HistogramPlot

data Settings = Settings
  { inputStream :: String
  , quitAfterDone :: Bool
  }

