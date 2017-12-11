
module Type.Date (
  Date(..),
  parseDate,
  dateParser,
  defaultDate,
  testDate,
  dateFromGregorian,
  addDays,
  removeDays
) where

import Data.Word (Word8)
import Prelude hiding (take)
import Data.Time (Day(..), fromGregorianValid, fromGregorian)
import qualified Data.Time as T
import Data.Text (Text, pack)
import Data.Text.Encoding (decodeUtf8, encodeUtf8)
import Data.ByteString (ByteString)
import Data.ByteString.Char8 (readInteger, readInt)
import Data.Attoparsec.ByteString (Parser, parseOnly, take, satisfy, word8) 
import Web.HttpApiData (FromHttpApiData, parseQueryParam)
import Control.Arrow (left)

newtype Date = Date {
  date :: Day 
} deriving (Eq, Ord, Show)

parseDate :: Text -> Either Text Date
parseDate = left pack . parseOnly dateParser . encodeUtf8

dateFromGregorian :: Integer -> Int -> Int -> Date
dateFromGregorian y m d = Date $ fromGregorian y m d

defaultDate :: Date
defaultDate = dateFromGregorian 2010 01 01

testDate :: Date
testDate = dateFromGregorian 2017 01 01

addDays :: Integer -> Date -> Date
addDays n (Date d) = Date $ T.addDays n d   

removeDays :: Integer -> Date -> Date
removeDays n (Date (ModifiedJulianDay jd)) = Date . ModifiedJulianDay $ (n + jd)

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
     Just day -> return . Date $ day
     Nothing  -> fail "Invalid date"

instance FromHttpApiData Date where
  parseQueryParam = parseDate
