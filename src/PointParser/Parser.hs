{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE RecordWildCards #-}

module PointParser.Parser
  ( parsePoint
  , foldPoints
  , parsedPointsToPoints
  ) where


import qualified Data.ByteString.Lazy as BS
import qualified Data.ByteString.Lazy.Char8 as BSC
import qualified Data.Attoparsec.ByteString.Char8 as A
import qualified Data.Attoparsec.ByteString.Lazy as AL

import Control.Concurrent.STM.TQueue
import Control.Monad.STM (STM(..), atomically)
import Control.Monad (foldM)

import Types

data ParsedPoint = ParsedPoint Point | ParsedSingle Double deriving (Show)

parsePoint :: A.Parser ParsedPoint
parsePoint = do
  x <- A.signed A.double
  A.takeWhile (\c -> not (A.isDigit c || c == '.' || c == '-' || c == '\n' || c == '\r'))
  possiblyDigit <- A.peekChar
  p <- case possiblyDigit of
         Just c -> 
           if A.isDigit c || c == '-' then do
             y <- A.signed A.double
             return $ ParsedPoint (x,y)
           else
             return $ ParsedSingle x
         Nothing -> return $ ParsedSingle x
  A.takeTill (\c -> c == '\r' || c == '\n')
  A.endOfLine
  return p

parsedPointToPoint :: Int -> ParsedPoint -> Point
parsedPointToPoint _ (ParsedPoint p) = p
parsedPointToPoint x (ParsedSingle y) = (fromIntegral $ x,y)
parsedPointsToPoints startIndex ps = zipWith parsedPointToPoint [startIndex..] ps

foldPoints queue startIndex ps = do
  atomically $ writeTQueue queue $ parsedPointsToPoints startIndex ps
  return $ startIndex + (length ps)
