{-# LANGUAGE BangPatterns    #-}
{-# LANGUAGE OverloadedStrings #-}

module Type.Scores (
  SimpleScore,
  simpleScore,
  Scores,
  sScores,
  sDate,
  Top10,
  top10,
  emptyTop10,
  emptyScores,
  emptyScoresFrom,
  scoresInsert,
  latestScores,
  updateDate 
) where

import Type.Date
import Type.Play
import Data.List (foldl')
import Data.ByteString (ByteString)
import Data.Monoid
import Data.Functor
import Data.Text.Encoding (decodeUtf8)
import Control.Monad
import Data.Aeson (ToJSON, toJSON, object, (.=))
import qualified Data.Vector as V
import qualified Data.Vector.Algorithms.Intro as V
import qualified Data.Map.Strict as M
import qualified Data.Set as S

newtype SimpleScore = SimpleScore {
  simpleScore :: (Score, Player)
} deriving (Show, Eq, Ord)

instance ToJSON SimpleScore where 
  toJSON (SimpleScore ((Score s), (Player p))) = object [ decodeUtf8 p .= s ]

newtype Top10 = Top10 {
  top10 :: [SimpleScore]
} deriving (Show, Eq)

emptyTop10 :: Top10
emptyTop10 = Top10 $ []

instance ToJSON Top10 where
  toJSON (Top10 s) = object [ "top10" .= s]

latestScores :: Scores -> Top10
latestScores  (Scores dat (PlayerIds _ itp _) (DayScores dsm)) = Top10 $ V.toList . V.take 10 . V.map SimpleScore $  V.zip (V.map Score relevantVectors) (V.map Player itp)
  where
  relevantVectors = playerScores . foldl' mappend mempty . M.elems . fst  $ M.split (addDays 10 dat) dsm

maxPlayers = 100000
maxDays = 100

newtype PlayerScores = PlayerScores {
  playerScores :: V.Vector Int
} deriving (Show, Eq, Ord)

emptyPlayerScores = PlayerScores $ V.replicate maxPlayers 0

instance Monoid PlayerScores where
  mempty = emptyPlayerScores
  mappend (PlayerScores x) (PlayerScores y) = PlayerScores $ V.zipWith (+) x y

insertPlayerScore :: Int -> Score -> PlayerScores -> PlayerScores
insertPlayerScore i (Score s) (PlayerScores ps) = PlayerScores $ ps V.// [(i,s)]


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
    playerToId :: M.Map ByteString Int
  , idToPlayer :: V.Vector ByteString
  , freeIds  :: [Int] 
} deriving (Show, Eq, Ord)

insertPlayerName :: Player -> PlayerIds -> (PlayerIds, Int)
insertPlayerName (Player p) ids@(PlayerIds pti itp (x:xs)) = case M.lookup p pti of
  Just id -> (ids, id)
  Nothing -> (PlayerIds (M.insert p x pti) (itp V.// [(x, p)]) xs, x) 

emptyPlayerIds = PlayerIds M.empty (V.replicate maxPlayers ("" :: ByteString)) [1..maxPlayers]

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
