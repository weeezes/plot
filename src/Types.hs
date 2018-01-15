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
  , width :: Int
  , height :: Int
  , points :: [(Double,Double)]
  , xMin :: Double
  , xMax :: Double
  , yMin :: Double
  , yMax :: Double
  }

data Settings = Settings
  { inputStream :: String
  }

