{-# LANGUAGE GeneralizedNewtypeDeriving #-}
{-# LANGUAGE OverloadedStrings        #-}

module Type.Play (
  Play(..),
  olderThan,
  playParser,
  playParserEOL
) where

import Type.Date
import Type.Player
import Type.Score
import Data.List (intersperse)
import Data.Attoparsec.ByteString (Parser, word8) 
import Test.QuickCheck (Gen, listOf1, elements, Arbitrary, arbitrary)

data Play = Play {
    pPlayer :: Player
  , pScore  :: Score  
  , pDate   :: Date 
} deriving (Eq)

instance Show Play where
  show (Play p s d) = concat $ intersperse ","  [show p, show s, show d]

instance Arbitrary Play where
  arbitrary = do
    p <- arbitrary 
    s <- arbitrary
    d <- arbitrary
    return $ Play p s d

eol = word8 10

olderThan :: Date -> Play -> Bool
olderThan dat (Play _ _ d) = d >= dat

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
