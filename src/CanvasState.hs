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
steps c@CanvasState{..} ps = c
    { points = points V.++ ps
    , xMin = min xMin $ V.minimum xs
    , xMax = max xMax $ V.maximum xs
    , yMin = min yMin $ V.minimum ys
    , yMax = max yMax $ V.maximum ys
    }
  where
    (xs, ys) = V.unzip ps

resize :: Int -> Int -> CanvasState -> CanvasState
resize w h c =
  case initCanvas w h of
    Left e -> c
    Right canvas -> c
      { canvas = canvas
      , width = w * brailleWidth
      , height = h * brailleHeight
      }

initCanvasState :: CanvasState
initCanvasState =
  case initCanvas w h of
    Right canvas ->
      CanvasState
        { canvas = canvas
        , points = V.empty
        , width = w*brailleWidth
        , height = h*brailleHeight
        , xMin = 0.0
        , xMax = 0.0
        , yMin = 0.0
        , yMax = 0.0
        , plotType = PointPlot
        }
    Left e -> error "This never happens"
  where
    w = 5
    h = 5

plot :: CanvasState -> Canvas
plot cs@CanvasState{..} =
  case plotType of
    PointPlot -> pointPlot cs
    AreaPlot -> areaPlot cs
    BarPlot -> barPlot cs
    HistogramPlot -> histogramPlot cs

pointPlot :: CanvasState -> Canvas
pointPlot cs@CanvasState{..} =
  let
    yMin' = prettyBounds yMin
    yMax' = prettyBounds yMax
    dots (x,y) =
      let
        bx = round $ toBounds 0.0 (fromIntegral width) xMin xMax x :: Int
        by = height - (round $ toBounds 0.0 (fromIntegral height) yMin' yMax' y) :: Int
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

    yMin' = prettyBounds yMin
    yMax' = prettyBounds yMax
    bars (i,y) =
      let
        by = height - (round $ toBounds 0.0 (fromIntegral height) yMin' yMax' y) :: Int
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

barPlot :: CanvasState -> Canvas
barPlot cs@CanvasState{..} =
  let
    bins = width `div` brailleWidth
    binSize = xMax / (fromIntegral bins)
    valuesToBins accvec acci accval vec =
      let
        (x,y) = vec V.! acci
        i = V.length accvec
        bin = (1 + fromIntegral i) * binSize
        --accval' = max y accval
      in
        if acci < (V.length vec) then
          if x <= bin then
            valuesToBins accvec (acci+1) (accval+1) vec
          else
            valuesToBins (V.snoc accvec (i,accval)) (acci+1) 0 vec
        else
          accvec

    bars yMax (i,y) =
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

    binValues = valuesToBins V.empty 0 (0 :: Double) points :: V.Vector (Int,Double)
    yMax = prettyBounds $ snd $ V.maximumBy (\(i,y) (i',y') -> compare y y') binValues
    barIndexes i y yDot = V.fromList [ (i + x*(width `div` brailleWidth), if x == y then barChar yDot else full) | x <- [y..(height `div` brailleHeight)-1]]
    bs = V.concatMap (\(i, (y,yDot)) -> barIndexes i y yDot) $ V.map (bars yMax) binValues
    canvas' = V.update canvas $ bs
  in
    if width > 0 && height > 0 then
      canvas'
    else
      canvas

histogramPlot :: CanvasState -> Canvas
histogramPlot cs@CanvasState{..} =
  let
    bins = width `div` brailleWidth

    bars yMin yMax (i,y) =
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

    emptyBins = V.replicate bins 0 :: V.Vector Int
    binSize = (prettyBounds yMax - prettyBounds yMin) / (fromIntegral bins)
    flatBins = V.map (\(_,y) -> (floor $ (y - prettyBounds yMin) / binSize, 1)) points :: V.Vector (Int, Int)
    binValues = V.accumulate (\i i' -> i+1) emptyBins flatBins :: V.Vector Int

    yMax' = prettyBounds $ fromIntegral $ V.maximum binValues
    yMin' = prettyBounds yMin

    binValues' = V.imap (\i v -> (i,fromIntegral v :: Double)) binValues
    bs = V.concatMap (\(i, (y,yDot)) -> barIndexes i y yDot) $ V.map (bars yMin' yMax') binValues'
    canvas' = V.update canvas $ bs
  in
    if width > 0 && height > 0 then
      canvas'
    else
      canvas
