{-# LANGUAGE RecordWildCards #-}

module CanvasState
  ( CanvasState(..)
  , steps
  , resize
  , initCanvasState
  , setDots
  ) where

import qualified Data.Vector.Unboxed as V

import Types
import Braille

data CanvasState = CanvasState
  { canvas :: Canvas
  , points :: V.Vector Point
  , mergedPoints :: V.Vector Point
  , width :: Int
  , height :: Int
  , xMin :: Double
  , xMax :: Double
  , yMin :: Double
  , yMax :: Double
  }

steps :: CanvasState -> V.Vector Point -> CanvasState
steps c@CanvasState{..} ps =
  let
    points' = V.concat [points, ps]
    xMin' = V.foldl (\acc (x,_) -> min acc (prettyBounds x)) xMin ps
    xMax' = V.foldl (\acc (x,_) -> max acc (prettyBounds x)) xMax ps
    yMin' = V.foldl (\acc (_,y) -> min acc (prettyBounds y)) yMin ps
    yMax' = V.foldl (\acc (_,y) -> max acc (prettyBounds y)) yMax ps
  in
    c
    { points = points'
    , mergedPoints = points'
    , xMin = xMin'
    , xMax = xMax'
    , yMin = yMin'
    , yMax = yMax'
    }

resize :: Int -> Int -> CanvasState -> CanvasState
resize w h c =
  c { canvas = initCanvas w h, width = w*brailleWidth, height = h*brailleHeight }

initCanvasState :: Int -> Int -> CanvasState
initCanvasState w h = CanvasState { canvas = initCanvas w h, points = V.empty, mergedPoints = V.empty, width = w*brailleWidth, height = h*brailleHeight, xMin = 0.0, xMax = 0.0, yMin = 0.0, yMax = 0.0 }

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
    ds = V.map dots mergedPoints
    canvas' = V.accum (\v (x,y) -> setBit v x y) canvas (V.toList ds)
  in
    if width > 0 && height > 0 then
      canvas'
    else
      canvas