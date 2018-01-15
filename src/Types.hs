module Types
  ( Canvas(..)
  , Tick(..)
  , Name(..)
  , CanvasState(..)
  , Settings(..)
  ) where

import qualified Data.Vector.Unboxed as V

type Canvas = V.Vector Int

data Tick = Tick Double Double 
type Name = ()

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

