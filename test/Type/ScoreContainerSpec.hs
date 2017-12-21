{-# LANGUAGE OverloadedStrings #-}

module Type.ScoreContainerSpec (main, spec) where

import Test.Hspec 
import Test.QuickCheck

import Type.ShortEvent
import Type.PlayerIds (PID, arbitraryPID)
import Type.Score

main :: IO ()
main = hspec spec

prop_compress_decompress_identity :: PID -> Score -> Bool
prop_compress_decompress_identity p s = (p, s) == (decompress $ compress p s)

prop_add_adds_score_values :: PID -> Score -> Score -> Bool
prop_add_adds_score_values p s1 s2 = s1 + s2 == (snd . decompress $ (add (compress p s1) (compress p s2)))

prop_add_emptyShortEvent_identity :: ShortEvent -> Bool
prop_add_emptyShortEvent_identity s = s == add emptyShortEvent s

spec :: Spec
spec = do
  it "Compress . decompress form identity" $ property $ forAll arbitraryPID prop_compress_decompress_identity 
  it "Add adds scores" $ property $ forAll arbitraryPID prop_add_adds_score_values
  it "Add emptyScoreEvent is identity" $ property $ forAll arbitraryShortEvent prop_add_emptyShortEvent_identity 
