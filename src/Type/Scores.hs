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
  scorePlay,
  scoresEmpty,
  scoresEmptyDate,
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
import qualified Data.Map.Strict as M
import qualified Data.Set as S

newtype SimpleScore = SimpleScore {
  simpleScore :: (Score, Player)
} deriving (Show, Eq, Ord)

instance ToJSON SimpleScore where 
  toJSON (SimpleScore ((Score s), (Player p))) = object [ decodeUtf8 p .= s ]

newtype Top10 = Top10 {
  top10 :: S.Set SimpleScore
} deriving (Show, Eq)

emptyTop10 :: Top10
emptyTop10 = Top10 $ S.empty

instance ToJSON Top10 where
  toJSON (Top10 s) = object [ "top10" .= S.toDescList s]

instance Monoid Top10 where
  mempty = Top10 S.empty
  mappend (Top10 x) (Top10 y) = Top10 . S.fromDistinctAscList . take 10 . S.toAscList $ (S.union x y)

data Scores = Scores {
    sDate :: !Date
  , sScores:: M.Map Date Top10
} deriving (Show, Eq) 

scoresEmpty = Scores defaultDate M.empty
scoresEmptyDate d = Scores d M.empty

instance Monoid Scores where
  mempty = scoresEmpty 
  mappend (Scores dx mx) (Scores dy my) = Scores dd $ M.unionWith mappend (clean mx) (clean my)
    where
    dd = max dx dy
    clean = snd . M.split dd

updateDate :: Date -> Scores -> Scores
updateDate nd ss@(Scores d s) = Scores minDate (snd $ M.split minDate s)
  where
  minDate = removeDays 10 nd

top10insert :: SimpleScore -> Top10 -> Top10
top10insert  ss@(SimpleScore (s, p)) tt@(Top10 t) 
  | S.size t <= 10 = Top10 $ S.insert ss t 
  | S.size t > 10 && S.findMin t < ss = Top10 . S.insert ss . S.deleteMin $ t  
  | otherwise = tt

scoresInsert :: Scores -> Play -> Scores
scoresInsert ss@(Scores sd m) (Play p s d) 
  | d < sd = ss
  | otherwise = Scores sd $ newMap
    where
    newMap = case M.lookup d m of 
      Just t -> M.insert d (top10insert (SimpleScore (s, p)) t) m
      Nothing -> M.insert d (Top10 . S.singleton $ SimpleScore (s,p)) m

latestScores :: Scores -> Top10
latestScores (Scores d m) = foldl' mappend mempty . M.elems . fst $ M.split (addDays 10 d) m  

scorePlay :: Date -> Play -> Scores
scorePlay dat (Play p s d) = Scores dat . M.singleton d . Top10 . S.singleton $  SimpleScore (s, p)

strictScore :: Date -> Player -> Score -> Date -> Scores
strictScore minDate p s d = if d < minDate 
  then mempty 
  else Scores d $! M.singleton d . Top10 . S.singleton $ SimpleScore (s, p)
