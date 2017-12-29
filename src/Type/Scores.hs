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
} deriving (Show) 

emptyScoresFrom :: Date -> Scores
emptyScoresFrom d = Scores d emptyPlayerIds emptyScoreIndex 
emptyScores = emptyScoresFrom defaultDate

updateDate :: Date -> Scores -> Scores
updateDate newDate scores = scores {sDate = removeDays 10 newDate } 

scoresInsert :: Scores -> Play -> IO Scores
scoresInsert scores@(Scores dat ids scr) (Play p s d) 
  | d < dat =return scores
  | otherwise = do
    let (newIds, pid) = playerId p ids
    newScrs <- insertPIDScoreAtDate d pid s scr
    return $ Scores dat newIds newScrs
