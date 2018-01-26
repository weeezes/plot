module PointParser
  ( loop
  ) where

import qualified Data.ByteString.Char8 as BS
import qualified Data.Attoparsec.ByteString.Char8 as A

import qualified Pipes.ByteString as PB
import qualified Pipes.Parse as PP
import qualified Pipes.Attoparsec as PA

import Control.Concurrent.STM.TQueue
import Control.Monad.STM (STM(..), atomically)

import Types

data ParsedPoint = ParsedPoint Double Double | ParsedSingle Double

parsePoint :: A.Parser ParsedPoint
parsePoint = do
  A.skipSpace
  x <- A.signed A.double
  A.takeWhile (\c -> not (A.isDigit c || c == '.' || c == '-' || c == '\n' || c == '\r'))
  possiblyDigit <- A.peekChar
  case possiblyDigit of
    Just c -> 
      if A.isDigit c || c == '-' then do
        y <- A.signed A.double
        return $ ParsedPoint x y
      else
        return $ ParsedSingle x
    Nothing -> return $ ParsedSingle x

foldPoints queue acc (ParsedPoint x y) = do
  atomically $ writeTQueue queue $ (x,y)
  return $ acc + 1
foldPoints queue acc (ParsedSingle y) = do
  atomically $ writeTQueue queue $ (acc+1, y)
  return $ acc + 1

loop queue h x = do
  PP.evalStateT (PP.foldAllM (foldPoints queue) (return $ 0 :: IO Double) pure) $ PA.parsed parsePoint (PB.fromHandle h)
  return ()
