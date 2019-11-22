{-# OPTIONS_GHC -F -pgmF htfpp #-}

module Tests.Braille
  ( htf_thisModulesTests
  ) where

import Test.Framework

import qualified Data.Vector.Unboxed as V
import Braille

prop_initCanvas_positives :: Positive Int -> Positive Int -> Bool
prop_initCanvas_positives w h = 
    case initCanvas w' h' of
      Right canvas -> V.length canvas == w'*h'
      Left _ -> False
  where
    w' = getPositive w
    h' = getPositive h

prop_initCanvas_negative_1 :: Positive Int -> Positive Int -> Bool
prop_initCanvas_negative_1 w h = 
    case initCanvas w' h' of
      Right canvas -> False
      Left _ -> True
  where
    w' = -1 * getPositive w
    h' = getPositive h

prop_initCanvas_negative_2 :: Positive Int -> Positive Int -> Bool
prop_initCanvas_negative_2 w h = 
    case initCanvas w' h' of
      Right canvas -> False
      Left _ -> True
  where
    w' = getPositive w
    h' = -1 * getPositive h

prop_initCanvas_negative_3 :: Positive Int -> Positive Int -> Bool
prop_initCanvas_negative_3 w h = 
    case initCanvas w' h' of
      Right canvas -> False
      Left _ -> True
  where
    w' = -1 * getPositive w
    h' = -1 * getPositive h

data Bounds = Bounds Double Double Double Double Double deriving (Show, Eq)
instance Arbitrary Bounds where
  arbitrary = do
    min <- arbitrary :: Gen Double
    max <- arbitrary `suchThat` (> min)

    min' <- arbitrary :: Gen Double
    max' <- arbitrary `suchThat` (> min')
    v <- arbitrary `suchThat` (\v -> v >= min' && v <= max')

    return (Bounds min max min' max' v)

prop_toBounds_full :: Bounds -> Bool
prop_toBounds_full (Bounds min max min' max' v) =
    v' >= min && v' <= max
  where
    v' = toBounds min max min' max' v

prop_toBounds_mid :: Bounds -> Bool
prop_toBounds_mid (Bounds min max min' max' _) =
    abs (v - m) < 0.01
  where
    v = toBounds min max min' max' $ min' + (max' - min')/2 
    m = min + (max - min)/2

prop_toBounds_max :: Bounds -> Bool
prop_toBounds_max (Bounds min max min' max' _) =
    abs (v - max) < 0.1 && v <= max
  where
    v = toBounds min max min' max' max'

prop_toBounds_min :: Bounds -> Bool
prop_toBounds_min (Bounds min max min' max' _) =
    abs (v - min) < 0.1 && v >= min
  where
    v = toBounds min max min' max' min'

prop_prettyBounds :: Double -> Bool
prop_prettyBounds v =
    (v > 0 && v' > v) || (v < 0 && v' < v) || (v == 0 && v' == 0)
  where
    v' = prettyBounds v

