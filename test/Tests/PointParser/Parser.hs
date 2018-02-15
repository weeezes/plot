{-# OPTIONS_GHC -F -pgmF htfpp #-}

module Tests.PointParser.Parser
  ( htf_thisModulesTests
  ) where

import Test.Framework

import Data.ByteString.Char8 (pack)
import qualified Data.Attoparsec.ByteString.Char8 as A
import qualified Data.Vector.Unboxed as V

import PointParser.Parser
import Types

data SinglePoints = SinglePoints Int [Double] String deriving (Show, Eq)
instance Arbitrary SinglePoints where
  arbitrary = do
    Positive length <- arbitrary :: Gen (Positive Int)
    points <- vectorOf length (arbitrary :: Gen Double)
    
    return $ SinglePoints length points (foldl (\acc p -> acc ++ (show p) ++ "\n") "" points)

prop_parsePoint_1 :: SinglePoints -> Bool
prop_parsePoint_1 (SinglePoints l points parseString) = 
  case A.parseOnly (A.many1 parsePoint) $ pack parseString of
    Left _  -> l == 0
    Right r -> 
        (length r) == l && (all (\(a, (x,y)) -> a == y) points')
      where
        points' = zip points (parsedPointsToPoints 0 r)
  
data XYPoints = XYPoints Int [Point] String deriving (Show, Eq)
instance Arbitrary XYPoints where
  arbitrary = do
    Positive length <- arbitrary :: Gen (Positive Int)
    xs <- vectorOf length (arbitrary :: Gen Double)
    ys <- vectorOf length (arbitrary :: Gen Double)

    delimiter <- elements ",;:|_"
    whitespaceBefore <- arbitrary `suchThat` (\i -> i <= 10 && i >= 0) :: Gen Int
    whitespaceAfter  <- arbitrary `suchThat` (\i -> i <= 10 && i >= 0) :: Gen Int

    let points = zip xs ys
    let pointRows = map (\(x,y) -> (show x) ++ (replicate whitespaceBefore ' ') ++ [delimiter] ++ (replicate whitespaceAfter ' ') ++ (show y)) points
    
    return $ XYPoints length points (foldl (\acc p -> acc ++ p ++ "\n") "" pointRows)

prop_parsePoint_2 :: XYPoints -> Bool
prop_parsePoint_2 (XYPoints l points parseString) = 
  case A.parseOnly (A.many1 parsePoint) $ pack parseString of
    Left _  -> l == 0
    Right r -> 
        (length r) == l && (all (\((x,y), (x',y')) -> y == y') points')
      where
        points' = zip points (parsedPointsToPoints 0 r)
