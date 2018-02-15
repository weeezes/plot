module Braille where

import Data.Either
import Data.Char
import Data.Bits (xor, (.|.))
import qualified Data.Vector.Unboxed as V

import Types

base = 0x2800 :: Int
full = 0x28FF :: Int
dots = [0x1,0x8,0x2,0x10,0x4,0x20,0x40,0x80] :: [Int]
brailleWidth = 2 :: Int
brailleHeight = 4 :: Int

toggleBit :: Int -> Int -> Int -> Int
toggleBit c x y =
  c `xor` dots !! (y*2+x)

setBit :: Int -> Int -> Int -> Int
setBit c x y =
  c .|. dots !! (y*2+x)

initCanvas :: Int -> Int -> Either String Canvas
initCanvas w h
  | w > 0 && h > 0 = Right $ V.replicate (w*h) base
  | otherwise = Left "Width and height need to be positive"

toBounds :: Double -> Double -> Double -> Double -> Double -> Double
toBounds min max min' max' v =
  let
    l  = max - min
    l' = max' - min'
    r  = l / l'
    v' = if isInfinite r || isNaN r then
           min + (v - min')
         else
           min + r * (v - min')
  in
    if v' > max then
      max
    else if v' < min then
      min
    else
      v'

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
      1.1 * v
    else if v' > 2 && v' < 10 then
      s * 10
    else
      1.1 * s * fromIntegral (m + m')
