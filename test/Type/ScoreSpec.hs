{-# LANGUAGE OverloadedStrings #-}

module Type.ScoreSpec (main, spec) where

import Test.Hspec 
import Test.QuickCheck

import Type.Score
import Data.Attoparsec.ByteString (parseOnly) 
import Data.ByteString.Char8 (pack)

main :: IO ()
main = hspec spec

prop_prase_print_identity:: Score -> Bool
prop_prase_print_identity  p = (parseOnly scoreParser . pack . show $ p) == (Right p)

spec :: Spec
spec = it "Show and parse form indentity" $ property prop_prase_print_identity
