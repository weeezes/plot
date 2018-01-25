module Types
  ( Canvas(..)
  , UiEvent(..)
  , Point(..)
  , Settings(..)
  ) where

import qualified Data.Vector.Unboxed as V

type Canvas = V.Vector Int

data Point = Point Double Double
data UiEvent = Redraw [Point]

data Settings = Settings
  { inputStream :: String
  }

