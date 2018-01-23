{-# LANGUAGE GeneralizedNewtypeDeriving #-}
{-# LANGUAGE OverloadedStrings        #-}
{-# LANGUAGE BangPatterns        #-}

module Type.Play (
  Play(..),
  olderThan,
  parsePlay,
  parsePlays,
  playParser,
  playsParser,
  playParserEOL
) where

import Type.Date
import Type.Player
import Type.Score
import Data.List (intersperse)
import Data.Attoparsec.ByteString (Parser, word8, sepBy1') 
import Test.QuickCheck (Gen, listOf1, elements, Arbitrary, arbitrary)
import Data.Maybe (fromJust)
import Control.Arrow (second)
import qualified Data.ByteString as B
import qualified Data.ByteString.Lazy as BL
import qualified Data.ByteString.Lazy.Char8 as BLC
import qualified Data.ByteString.Char8 as BC

data Play = Play {
    pPlayer :: !Player
  , pScore  :: !Score  
  , pDate   :: !Date 
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

breakComma = BLC.break (==',')

next f = second B.tail . fromJust . f

parsePlays :: BLC.ByteString -> [Play]
parsePlays s = if BLC.null r' 
  then []
  else p : (parsePlays r)
  where
  (p,r) = parseP s
  r' = BLC.tail r

parseP :: BLC.ByteString -> (Play, BLC.ByteString)
parseP s = (Play (Player a) (Score b) (dateFromGregorian y m d), ys)
  where
  !(a,as) = breakComma s
  as' = BLC.tail as
  !(b,bs) = fromJust $ BLC.readInt as'
  !(d,ds) = fromJust $ BLC.readInt (BLC.tail bs)
  !(m,ms) = fromJust $ BLC.readInt (BLC.tail ds)
  !(y,ys) = fromJust $ BLC.readInteger (BLC.tail ms)


parsePlay :: BLC.ByteString -> Play
parsePlay s = Play (Player a) (Score b) (dateFromGregorian y m d)
  where
  (a,as) = breakComma s
  as' = BLC.tail as
  (b,bs) = fromJust $ BLC.readInt as'
  (d,ds) = fromJust $ BLC.readInt (BLC.tail bs)
  (m,ms) = fromJust $ BLC.readInt (BLC.tail ds)
  (y,ys) = fromJust $ BLC.readInteger (BLC.tail ms)

playsParser :: Parser [Play]
playsParser = playParser `sepBy1'` eol

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
