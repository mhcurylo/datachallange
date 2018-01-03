{-# LANGUAGE GeneralizedNewtypeDeriving #-}

module Type.Date (
  Date,
  dateIndex,
  parseDate,
  dateParser,
  defaultDate,
  dateFromGregorian,
  testDate,
  addDays,
  removeDays
) where

import Data.Word (Word8)
import Prelude hiding (take)
import Data.Time (Day(..), fromGregorianValid, fromGregorian)
import Data.Text (Text, pack)
import Data.Text.Encoding (encodeUtf8)
import Data.Hashable
import Data.ByteString (ByteString)
import Data.ByteString.Char8 (readInteger, readInt)
import Data.Attoparsec.ByteString (Parser, parseOnly, take, word8) 
import Test.QuickCheck (Arbitrary)
import Web.HttpApiData (FromHttpApiData, parseQueryParam)
import Control.Arrow (left)

newtype Date = Date {
  date :: Integer 
} deriving (Eq, Ord, Arbitrary, Hashable, Enum)

printLogDate :: Date -> String
printLogDate (Date i) = logDate 
  where
  (y1:y2:y3:y4:_:m1:m2:_:d1:d2:[]) = show (ModifiedJulianDay i)
  logDate = d1:d2:'-':m1:m2:'-':y1:y2:y3:y4:[]

parseDate :: Text -> Either Text Date
parseDate = left pack . parseOnly dateParser . encodeUtf8

instance Show Date where
  show = printLogDate

dateFromGregorian :: Integer -> Int -> Int -> Date
dateFromGregorian y m d = Date $ i
  where 
  (ModifiedJulianDay i) = fromGregorian y m d

defaultDate :: Date
defaultDate = dateFromGregorian 2010 01 01

testDate :: Date
testDate = dateFromGregorian 2017 01 01

addDays :: Integer -> Date -> Date
addDays n (Date i) = Date $ i + n   

removeDays :: Integer -> Date -> Date
removeDays n (Date i) = Date $ i - n 

dateIndex :: Date -> Date -> Int
dateIndex (Date d) = fromIntegral . date  . removeDays d

takeInt :: Int -> Parser Int
takeInt n = do
  d <- readInt <$> take n 
  case d of
    Just (d', bs) -> return d'
    Nothing -> fail "Not a digit"

takeInteger :: Int -> Parser Integer
takeInteger n = do
  d <- readInteger <$> take n 
  case d of
    Just (d', bs) -> return d'
    Nothing -> fail "Not a digit"

dash :: Parser Word8
dash = word8 45 

dateParser :: Parser Date  
dateParser = do
  d <- takeInt 2
  _ <- dash 
  m <- takeInt 2
  _ <- dash 
  y <- takeInteger 4
  case fromGregorianValid y m d of
     Just (ModifiedJulianDay i) -> return $ Date i 
     Nothing  -> fail "Invalid date"

instance FromHttpApiData Date where
  parseQueryParam = parseDate
