{-# LANGUAGE OverloadedStrings        #-}
{-# LANGUAGE GeneralizedNewtypeDeriving #-}

module Type.Player (
  Player(..),
  playerParser,
  playerZero
) where

import Data.ByteString.Lazy.Char8 (ByteString, pack, unpack, fromStrict)
import Data.Attoparsec.ByteString (Parser, takeWhile1) 
import Test.QuickCheck (Gen, listOf1, elements, Arbitrary, arbitrary)
import Data.Char (toUpper)
import Data.Hashable (Hashable)
import Control.Applicative (liftA2)

arbitraryName :: Gen String
arbitraryName = headUpper <$> listOf1 (elements "abcdefghijlmnopqrstuwxyz")
  where
  headUpper (x:xs) = toUpper x : xs

newtype Player = Player {
  player :: ByteString
} deriving (Eq, Ord, Hashable)

instance Show Player where
  show = unpack . player

instance Arbitrary Player where
  arbitrary = do
    p <- liftA2 (++) arbitraryName arbitraryName 
    return $ Player . pack $ p

playerZero = Player ""    

isAlpha w = w >= 65 && w <= 122

playerParser :: Parser Player
playerParser = Player . fromStrict <$> takeWhile1 isAlpha
