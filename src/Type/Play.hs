{-# LANGUAGE GeneralizedNewtypeDeriving #-}
{-# LANGUAGE OverloadedStrings        #-}

module Type.Play (
  Play(..),
  Player(..),
  Score(..),
  isPlayOld,
  scoreParser,
  playerParser,
  playParser,
  playParserEOL
) where

import Type.Date
import Data.ByteString (ByteString)
import Data.ByteString.Char8 (readInt)
import Data.Text (Text, pack)
import Data.Text.Encoding (encodeUtf8)
import qualified Data.ByteString as B 
import Data.Attoparsec.ByteString (Parser, parseOnly, takeWhile1, word8) 
import Web.HttpApiData (FromHttpApiData, parseQueryParam)
import Control.Arrow (left)
import Test.QuickCheck (Gen, listOf1, elements, Arbitrary, arbitrary)
import Data.Char (toUpper)
import Control.Applicative (liftA2)

arbitraryName :: Gen String
arbitraryName = headUpper <$> listOf1 (elements "abcdefghijlmnopqrstuwxyz")
  where
  headUpper (x:xs) = toUpper x : xs

newtype Player = Player {
  player :: ByteString
} deriving (Eq, Ord)

instance Show Player where
  show  = show . player

instance Arbitrary Player where
  arbitrary = do
    p <- liftA2 (++) arbitraryName arbitraryName 
    return $ Player . encodeUtf8 . pack $ p

isAlpha w = w >= 65 && w <= 122

playerParser :: Parser Player
playerParser = Player <$> takeWhile1 isAlpha

newtype Score = Score {
  score :: Int
} deriving (Eq, Ord, Num)

instance Show Score where
  show  = show . score 

instance Show 

data Play = Play {
    pPlayer :: Player
  , pScore  :: Score  
  , pDate   :: Date 
} deriving (Eq)

instance Show Play where
  show (Play p s d) = show p ++ "," ++ show s ++ "," ++ show d

isDigit w = w >= 48 && w <= 57
eol = word8 10

isPlayOld :: Date -> Play -> Bool
isPlayOld dat (Play _ _ d) = d >= dat

scoreParser :: Parser Score  
scoreParser = do
  d <- takeWhile1 isDigit
  case readInt d of 
    Just (d', _) -> return $ Score d'
    Nothing -> fail "Not a score"

playParserEOL :: Parser Play
playParserEOL = do
  p <- playParser  
  _ <- eol 
  return $ p

playParser :: Parser Play
playParser = do
  p <- playerParser
  _ <- word8 44
  s <- scoreParser
  _ <- word8 44
  d <- dateParser
  return $ Play p s d
