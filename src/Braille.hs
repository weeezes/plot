module Braille where

import Data.Char
import Data.Bits (xor, (.|.))
import qualified Data.Vector.Unboxed as V
import qualified Data.Sequence as Seq

import Types

base = 0x2800
dots = [0x1,0x8,0x2,0x10,0x4,0x20,0x40,0x80]
brailleWidth = 2 :: Int
brailleHeight = 4 :: Int

toggleBit :: Int -> Int -> Int -> Int
toggleBit c x y =
  c `xor` dots !! (y*2+x)

setBit c x y =
  c .|. dots !! (y*2+x)

initCanvas :: Int -> Int -> Canvas
initCanvas w h =
  let
    canvas = Seq.fromList $ replicate (w*h) base
  in
    canvas

toBounds :: Double -> Double -> Double -> Double -> Double -> Double
toBounds min max min' max' v =
  let
    l  = max - min
    l' = max' - min'
    r  = l / l'
  in
    if isInfinite r || isNaN r then
      v - min'
    else
      r * (v - min')

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
