module Types
  ( Canvas(..)
  , UiEvent(..)
  , Point(..)
  , Settings(..)
  ) where

import qualified Data.Vector.Unboxed as V

type Canvas = V.Vector Int

type Point = (Double,Double)
data UiEvent = Redraw (V.Vector Point)

data Settings = Settings
  { inputStream :: String
  }

