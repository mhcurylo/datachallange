{-# LANGUAGE OverloadedStrings #-}

module Type.ScoreContainerSpec (main, spec) where

import Test.Hspec 
import Test.QuickCheck
import Test.QuickCheck.Monadic

import Type.ShortEvent
import Type.ScoreContainer
import Type.PlayerIds (PID, arbitraryPID)
import Type.Score

import Control.Monad
import qualified Data.Vector as V


main :: IO ()
main = hspec spec
prop_insert_sums_scores :: [Score] -> Property
prop_insert_sums_scores ss = monadicIO $ do
  pid <- pick arbitraryPID
  sc <- run $ baseSC
  run $ forM ss (\s ->
    insertPScore (pid, s) sc)
  res <- run $ V.freeze (vsc sc)
  assert $ (sum . map (Score . fromIntegral) $ V.toList res) == sum ss

prop_sumScores_sums_across_SCs :: [Score] -> Property
prop_sumScores_sums_across_SCs ss = monadicIO $ do
    pid <-  pick arbitraryPID
    scoreContainers <- run $ forM ss (\s -> do
      sc <- baseSC
      insertPScore (pid, s) sc
      return sc) 
    scores <- run $ sumScores scoreContainers
    assert $ (scores V.! fromIntegral pid) == sum ss

spec :: Spec
spec = do
  it "Insert scores sums the scores on single PIDs" $ property $ prop_insert_sums_scores
  it "SumScores sums scores across SCs" $ property $ prop_sumScores_sums_across_SCs
