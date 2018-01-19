{-# LANGUAGE RecordWildCards #-}

module Braille where

import Data.Char
import Data.Bits (xor, (.|.))
import qualified Data.Vector.Unboxed as V

import Types

base = 0x2800
dots = [0x1,0x8,0x2,0x10,0x4,0x20,0x40,0x80]
brailleWidth = 2
brailleHeight = 4

toggleBit c x y =
  c `xor` dots !! (y*2+x)

setBit c x y =
  c .|. dots !! (y*2+x)

initCanvas :: Int -> Int -> Canvas
initCanvas w h =
  let
    canvas = V.replicate (w*h) base
  in
    canvas

toBounds :: Double -> Double -> Double -> Double -> Double -> Double
toBounds min max min' max' v =
  let
    l  = abs $ max - min
    l' = Prelude.max 1.0 (abs $ max' - min')
    r  = l / l'
  in
    abs $ ((abs min') + v) * r 

prettyBounds :: Double -> Double
prettyBounds v =
  let
    -- -3560
    s = signum v -- -
    v' = abs v -- 3560
    base = floor $ logBase 10.0 v' -- 3
    t = 10 ^ base :: Int -- 1000
    t' = 10 ^ (max 0 $ base - 1) :: Int -- 100
    m =  t * (floor $ v' / fromIntegral t) -- 3000
    m' = t' * (ceiling $ (v' - fromIntegral m) / fromIntegral t') -- 3560-3000 => 560/100 => 5.6 => 6
  in
    if v' < 1 then
      v
    else if v' > 2 && v' < 10 then
      s * 10
    else
      1.1 * s * fromIntegral (m + m')

setDots :: CanvasState -> Canvas
setDots (CanvasState canvas w h ps xmin xmax ymin ymax) =
  let
    dots (x,y) =
      let
        bx = round $ toBounds 0.0 (fromIntegral w) xmin xmax x :: Int
        by = h - (round $ toBounds 0.0 (fromIntegral h) ymin ymax y) :: Int
        (x',xDot)  = bx `quotRem` brailleWidth
        (y',yDot)  = by `quotRem` brailleHeight
        (x'', xDot') = if x' > (w `div` brailleWidth - 1) then
                         (w `div` brailleWidth- 1, brailleWidth -1)
                       else
                         (x', xDot)
        (y'', yDot') = if y' > (h `div` brailleHeight - 1) then
                         (h `div` brailleHeight - 1, brailleHeight -1)
                       else
                         (y', yDot)
        i = (w `div` brailleWidth)*y'' + x''
      in
        (i, (xDot', yDot'))
    ds = map dots ps
    canvas' = V.accum (\v (x,y) -> setBit v x y) canvas ds
  in
    if w > 0 && h > 0 then
      canvas'
    else
      canvas