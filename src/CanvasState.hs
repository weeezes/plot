{-# LANGUAGE RecordWildCards #-}

module CanvasState
  ( CanvasState(..)
  , steps
  , resize
  , initCanvasState
  , setDots
  ) where

import qualified Data.Vector.Unboxed as V
import qualified Data.Sequence as Seq
import Data.Foldable (toList)

import Types
import Braille

data CanvasState = CanvasState
  { canvas :: Canvas
  , points :: Seq.Seq Point
  , width :: Int
  , height :: Int
  , xMin :: Double
  , xMax :: Double
  , yMin :: Double
  , yMax :: Double
  }

steps :: CanvasState -> Seq.Seq Point -> CanvasState
steps c@CanvasState{..} ps =
  let
    points' = points Seq.>< ps
    xMin' = foldl (\acc (x,_) -> min acc (prettyBounds x)) xMin ps
    xMax' = foldl (\acc (x,_) -> max acc (prettyBounds x)) xMax ps
    yMin' = foldl (\acc (_,y) -> min acc (prettyBounds y)) yMin ps
    yMax' = foldl (\acc (_,y) -> max acc (prettyBounds y)) yMax ps
  in
    c
    { points = points'
    , xMin = xMin'
    , xMax = xMax'
    , yMin = yMin'
    , yMax = yMax'
    }

resize :: Int -> Int -> CanvasState -> CanvasState
resize w h c =
  c { canvas = initCanvas w h, width = w*brailleWidth, height = h*brailleHeight }

initCanvasState :: Int -> Int -> CanvasState
initCanvasState w h = CanvasState { canvas = initCanvas w h, points = Seq.empty, width = w*brailleWidth, height = h*brailleHeight, xMin = 0.0, xMax = 0.0, yMin = 0.0, yMax = 0.0 }

setDot seq p =
  let
    (i,(x,y)) = p
    setBit' x y v = setBit v x y
  in
    Seq.adjust (setBit' x y) i seq

setDots :: CanvasState -> Canvas
setDots cs@CanvasState{..} =
  let
    dots (x,y) =
      let
        bx = round $ toBounds 0.0 (fromIntegral width) xMin xMax x :: Int
        by = height - (round $ toBounds 0.0 (fromIntegral height) yMin yMax y) :: Int
        (x',xDot)  = bx `quotRem` brailleWidth :: (Int,Int)
        (y',yDot)  = by `quotRem` brailleHeight :: (Int,Int)
        (x'', xDot') = if x' > (width `div` brailleWidth - 1) then
                         (width `div` brailleWidth- 1, brailleWidth -1)
                       else
                         (x', xDot)
        (y'', yDot') = if y' > (height `div` brailleHeight - 1) then
                         (height `div` brailleHeight - 1, brailleHeight -1)
                       else
                         (y', yDot)
        i = (width `div` brailleWidth)*y'' + x''
      in
        (i, (xDot', yDot'))
    --ds = map dots points -- :: Seq.Seq (Int, (Int, Int))
    --ds = foldl (\acc p -> acc Seq.|> dots p) Seq.empty points :: Seq.Seq (Int, (Int,Int))
    --canvas' = V.accum (\v (x,y) -> setBit v x y) canvas (toList ds)
    canvas' = foldl (\acc p -> setDot acc (dots p)) canvas points
  in
    if width > 0 && height > 0 then
      canvas'
    else
      canvas