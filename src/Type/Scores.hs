{-# LANGUAGE BangPatterns    #-}
{-# LANGUAGE OverloadedStrings #-}

module Type.Scores (
  Scores(..),
  emptyScores,
  emptyScoresFrom,
  scoresInsert,
  updateDate 
) where

import Type.Date
import Type.Score
import Type.Player 
import Type.Play
import Type.PlayerIds
import Type.ScoreIndex
import Data.ByteString (ByteString)
import Control.Monad

data Scores = Scores {
    sDate :: !Date
  , sPlayerIds :: PlayerIds
  , sScores:: ScoreIndex
} 

emptyScoresFrom :: Date -> IO Scores
emptyScoresFrom d = do
  epi <- emptyPlayerIds
  esi <- emptyScoreIndex
  return $ Scores d epi esi 

emptyScores = emptyScoresFrom defaultDate

updateDate :: Date -> Scores -> Scores
updateDate newDate scores = scores {sDate = removeDays 10 newDate } 

scoresInsert :: Scores -> Play -> IO ()
scoresInsert scores@(Scores dat ids scr) (Play p s d) 
  | d < dat = return () 
  | otherwise = do
    let i = dateIndex dat d
    pid <- playerId p ids
    insertPIDScoreAtIndex i pid s scr
