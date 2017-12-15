{-# LANGUAGE OverloadedStrings #-}

module Type.PlaySpec (main, spec) where

import Test.Hspec 
import Test.QuickCheck

import Type.Play
import Type.Date
import Data.ByteString.Char8 (pack)
import Data.Attoparsec.ByteString (parseOnly) 

main :: IO ()
main = hspec spec

prop_parse_print_identity :: Play -> Bool
prop_parse_print_identity  p = (parseOnly playParser . pack . show $ p) == (Right p)

prop_parseEOL_print_concat_newline_identity :: Play -> Bool
prop_parseEOL_print_concat_newline_identity  p = (parseOnly playParserEOL . pack . (++ "\n") . show $ p) == (Right p)

prop_olderThan_checks_date_ord :: Date -> Play -> Bool
prop_olderThan_checks_date_ord d p = (pDate p >= d) == olderThan d p

spec :: Spec
spec = do
  it "Show and parse form indentity" $ property prop_parse_print_identity
  it "(++ \\n) . show and parseEOL form indentity" $ property prop_parse_print_identity
  it "olderThan checks if date is older than pDate" $ property prop_olderThan_checks_date_ord 
