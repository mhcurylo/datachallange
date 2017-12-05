{-# LANGUAGE OverloadedStrings #-}

module Type.DateSpec (main, spec) where

import Type.Date 
import Test.Hspec 
import Data.Time
import Data.Text
import Test.QuickCheck
import Test.QuickCheck.Instances

main :: IO ()
main = hspec spec

prop_rightOnProperDate :: Day -> Bool
prop_rightOnProperDate day = (parseDate urlDate) == (Right $ Date day) 
  where
  (y1:y2:y3:y4:_:m1:m2:_:d1:d2:[]) = show day
  urlDate = pack $ d1:d2:'-':m1:m2:'-':y1:y2:y3:y4:[]

prop_leftOnInvalidDate :: Day -> Bool
prop_leftOnInvalidDate day = case parseDate urlDate of
  Right _ -> False
  Left _ -> True
  where
  (y1:y2:y3:y4:_:m1:m2:_:d1:d2:[]) = show day
  urlDate = pack $ d1:d2:'-':'2':m2:'-':y1:y2:y3:y4:[]

prop_leftIfNotADate :: Text -> Bool
prop_leftIfNotADate day = case (parseDate day) of
  Right _ -> False
  Left _ -> True

spec :: Spec
spec = do
    it "Parses a legal date" $ property prop_rightOnProperDate
    it "Returns left on invalid date" $ property prop_leftOnInvalidDate
    it "Returns left if not a date" $ property prop_leftIfNotADate
