module Types
  ( Canvas(..)
  , Point(..)
  , CanvasState(..)
  , Settings(..)
  ) where

import qualified Data.Vector.Unboxed as V

type Canvas = V.Vector Int

data Point = Point Double Double 

data CanvasState = CanvasState
  { canvas :: Canvas
  , points :: V.Vector (Double,Double)
  , mergedPoints :: V.Vector (Double,Double)
  , width :: Int
  , height :: Int
  , xMin :: Double
  , xMax :: Double
  , yMin :: Double
  , yMax :: Double
  }

data Settings = Settings
  { inputStream :: String
  }

