{-# LANGUAGE RecordWildCards #-}

module CanvasState
  ( CanvasState(..)
  , steps
  , resize
  , initCanvasState
  , plot
  ) where

import qualified Data.Vector.Unboxed as V

import Types
import Braille

data CanvasState = CanvasState
  { canvas :: Canvas
  , points :: V.Vector Point
  , width :: Int
  , height :: Int
  , xMin :: Double
  , xMax :: Double
  , yMin :: Double
  , yMax :: Double
  , plotType :: PlotType
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
    , xMin = xMin'
    , xMax = xMax'
    , yMin = yMin'
    , yMax = yMax'
    }

resize :: Int -> Int -> CanvasState -> CanvasState
resize w h c =
  c { canvas = initCanvas w h, width = w*brailleWidth, height = h*brailleHeight }

initCanvasState :: Int -> Int -> CanvasState
initCanvasState w h = CanvasState { canvas = initCanvas w h, points = V.empty, width = w*brailleWidth, height = h*brailleHeight, xMin = 0.0, xMax = 0.0, yMin = 0.0, yMax = 0.0, plotType = PointPlot }

plot :: CanvasState -> Canvas
plot cs@CanvasState{..} =
  case plotType of
    PointPlot -> pointPlot cs
    AreaPlot -> areaPlot cs

pointPlot :: CanvasState -> Canvas
pointPlot cs@CanvasState{..} =
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
    ds = V.map dots points
    canvas' = V.accum (\v (x,y) -> setBit v x y) canvas (V.toList ds)
  in
    if width > 0 && height > 0 then
      canvas'
    else
      canvas

areaPlot :: CanvasState -> Canvas
areaPlot cs@CanvasState{..} =
  let
    bins = width `div` brailleWidth
    binSize = xMax / (fromIntegral bins)
    valuesToBins accvec acci accval vec =
      let
        (x,y) = vec V.! acci
        i = V.length accvec
        bin = (1 + fromIntegral i) * binSize
        accval' = max y accval
      in
        if acci < (V.length vec) then
          if x <= bin then
            valuesToBins accvec (acci+1) y vec
          else
            valuesToBins (V.snoc accvec (i,accval)) (acci+1) 0 vec
        else
          accvec

    bars (i,y) =
      let
        by = height - (round $ toBounds 0.0 (fromIntegral height) yMin yMax y) :: Int
        (y',yDot)  = by `quotRem` brailleHeight :: (Int,Int)
        (y'', yDot') = if y' > (height `div` brailleHeight - 1) then
                         (height `div` brailleHeight - 1, brailleHeight -1)
                       else
                         (y', yDot)
      in
        (i, (y'', yDot'))

    barChar yDot =
      if yDot == 0 then
        full
      else if yDot == 3 then
        base
      else
      foldl (\acc c -> setBit (setBit acc 0 c) 1 c) base [yDot..3]

    barIndexes i y yDot = V.fromList [ (i + x*(width `div` brailleWidth), if x == y then barChar yDot else full) | x <- [y..(height `div` brailleHeight)-1]]
    binValues = valuesToBins V.empty 0 (0 :: Double) points :: V.Vector (Int,Double)
    bs = V.concatMap (\(i, (y,yDot)) -> barIndexes i y yDot) $ V.map bars binValues
    canvas' = V.update canvas $ bs
  in
    if width > 0 && height > 0 then
      canvas'
    else
      canvas