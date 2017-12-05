{-# LANGUAGE GeneralizedNewtypeDeriving #-}

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
import Data.Attoparsec.ByteString (Parser, parseOnly, takeWhile1, word8) 
import Web.HttpApiData (FromHttpApiData, parseQueryParam)
import Control.Arrow (left)

newtype Player = Player {
  player :: ByteString
} deriving (Show, Eq, Ord)

newtype Score = Score {
  score :: Int
} deriving (Show, Eq, Ord, Num)

data Play = Play {
    pPlayer :: Player
  , pScore  :: Score  
  , pDate   :: Date 
} deriving (Show, Eq)

isDigit w = w >= 48 && w <= 57
isAlpha w = w >= 65 && w <= 122
eol = word8 10

isPlayOld :: Date -> Play -> Bool
isPlayOld dat (Play _ _ d) = d >= dat

scoreParser :: Parser Score  
scoreParser = do
  d <- takeWhile1 isDigit
  case readInt d of 
    Just (d', _) -> return $ Score d'
    Nothing -> fail "Not a score"
    
playerParser :: Parser Player
playerParser = Player <$> takeWhile1 isAlpha

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
