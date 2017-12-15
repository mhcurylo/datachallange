{-# LANGUAGE OverloadedStrings #-}

module Type.PlayerSpec (main, spec) where

import Test.Hspec 
import Test.QuickCheck
import Type.Player
import Data.ByteString.Char8 (pack)
import Data.Attoparsec.ByteString (parseOnly) 

main :: IO ()
main = hspec spec

prop_prase_print_identity:: Player -> Bool
prop_prase_print_identity  p = (parseOnly playerParser . pack . show $ p) == (Right p)

spec :: Spec
spec = it "Show and parse form indentity" $ property prop_prase_print_identity
