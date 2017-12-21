{-# LANGUAGE GeneralizedNewtypeDeriving #-}
{-# LANGUAGE OverloadedStrings        #-}

module Type.Score (
  Score(..),
  scoreParser
) where

import Data.ByteString.Char8 (readInt)
import Data.Attoparsec.ByteString (Parser, parseOnly, takeWhile1) 
import Test.QuickCheck (Arbitrary, arbitrary, choose)

newtype Score = Score {
  score :: Int
} deriving (Eq, Ord, Num)

instance Show Score where
  show  = show . score 
  
instance Arbitrary Score where
  arbitrary = do
    n <- choose (0, 20000)
    return $ Score n

isDigit w = w >= 48 && w <= 57

scoreParser :: Parser Score  
scoreParser = do
  d <- takeWhile1 isDigit
  case readInt d of 
    Just (d', _) -> return $ Score d'
    Nothing -> fail "Not a score"
