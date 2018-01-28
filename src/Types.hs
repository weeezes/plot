module Types
  ( Canvas(..)
  , UiEvent(..)
  , Point(..)
  , Settings(..)
  ) where

import qualified Data.Vector.Unboxed as V
import qualified Data.Sequence as Seq

type Canvas = Seq.Seq Int

type Point = (Double,Double)
data UiEvent = Redraw (Seq.Seq Point)

data Settings = Settings
  { inputStream :: String
  }

