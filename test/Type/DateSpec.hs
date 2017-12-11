{-# LANGUAGE OverloadedStrings #-}

module Type.DateSpec (main, spec) where

import Type.Date 
import Test.Hspec 
import Data.Time (Day)
import Data.Text
import Test.QuickCheck
import Test.QuickCheck.Instances

main :: IO ()
main = hspec spec

prop_praseLogPrintLog_identity :: Date -> Bool
prop_praseLogPrintLog_identity d = (parseDate .  pack . show $ d) == (Right d)

prop_leftOnInvalidDate :: Date -> Bool
prop_leftOnInvalidDate d = case parseDate urlDate of
  Right _ -> False
  Left _ -> True
  where
  (d1:d2:'-':m1:m2:'-':y1:y2:y3:y4:[]) = show d
  urlDate = pack $ d1:d2:'-':'2':m2:'-':y1:y2:y3:y4:[]

prop_leftIfNotADate :: Text -> Bool
prop_leftIfNotADate d = case (parseDate d) of
  Right _ -> False
  Left _ -> True

prop_addDays_removeDays_identity :: Date -> Integer -> Bool
prop_addDays_removeDays_identity d i = d == (addDays i . removeDays i) d

spec :: Spec
spec = do
    it "Parses a legal date" $ property prop_praseLogPrintLog_identity
    it "Returns left on invalid date" $ property prop_leftOnInvalidDate
    it "Returns left if not a date" $ property prop_leftIfNotADate
    it "Adds and removes days" $ property prop_addDays_removeDays_identity
