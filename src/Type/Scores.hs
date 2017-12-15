{-# LANGUAGE BangPatterns    #-}
{-# LANGUAGE OverloadedStrings #-}

module Type.Scores (
  Scores(..),
  PlayerScores(..),
  DayScores(..),
  PlayerIds(..),
  emptyScores,
  emptyScoresFrom,
  scoresInsert,
  updateDate 
) where

import Type.Date
import Type.Play
import Type.Score
import Type.Player 
import Data.ByteString (ByteString)
import Data.Monoid
import Control.Monad
import qualified Data.Vector as V
import qualified Data.Vector.Algorithms.Intro as V
import qualified Data.Map.Strict as M
import qualified Data.Set as S

maxPlayers = 100000
maxDays = 100

newtype PlayerScores = PlayerScores {
  playerScores :: V.Vector Score
} deriving (Show, Eq, Ord)

emptyPlayerScores = PlayerScores $ V.replicate maxPlayers 0

instance Monoid PlayerScores where
  mempty = emptyPlayerScores
  mappend (PlayerScores x) (PlayerScores y) = PlayerScores $ V.zipWith (+) x y

insertPlayerScore :: Int -> Score -> PlayerScores -> PlayerScores
insertPlayerScore i s (PlayerScores ps) = PlayerScores $ ps V.// [(i,s)]

newtype DayScores = DayScores {
  dayScores :: M.Map Date PlayerScores
} deriving (Show, Eq, Ord)

emptyDayScores = DayScores $ M.empty 
insertDayScore :: Date -> Int -> Score -> DayScores -> DayScores
insertDayScore d i s (DayScores m) = DayScores $ M.alter (insertPlayerScoreMaybe i s) d m 

insertPlayerScoreMaybe :: Int -> Score -> Maybe PlayerScores -> Maybe PlayerScores
insertPlayerScoreMaybe i s mps = Just $ insertPlayerScore i s $ case mps of
  Just ps -> ps 
  Nothing -> emptyPlayerScores

data PlayerIds = PlayerIds {
    playerToId :: M.Map Player Int
  , idToPlayer :: V.Vector Player
  , freeIds  :: [Int] 
} deriving (Show, Eq, Ord)

insertPlayerName :: Player -> PlayerIds -> (PlayerIds, Int)
insertPlayerName p ids@(PlayerIds pti itp (x:xs)) = case M.lookup p pti of
  Just id -> (ids, id)
  Nothing -> (PlayerIds (M.insert p x pti) (itp V.// [(x, p)]) xs, x) 

emptyPlayerIds = PlayerIds M.empty (V.replicate maxPlayers playerZero) [1..maxPlayers]

data Scores = Scores {
    sDate :: !Date
  , sPlayerIds :: PlayerIds
  , sScores:: DayScores
} deriving (Show, Eq) 

emptyScoresFrom :: Date -> Scores
emptyScoresFrom d = Scores d emptyPlayerIds emptyDayScores 
emptyScores = emptyScoresFrom defaultDate

updateDate :: Date -> Scores -> Scores
updateDate newDate scores = scores {sDate = removeDays 10 newDate } 

scoresInsert :: Scores -> Play -> Scores
scoresInsert scores@(Scores dat ids scr) (Play p s d) 
  | d < dat = scores
  | otherwise = Scores dat newIds newScrs
    where
    (newIds, playerId) = insertPlayerName p ids
    newScrs = insertDayScore d playerId s scr
