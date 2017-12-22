{-# LANGUAGE BangPatterns    #-}
{-# LANGUAGE OverloadedStrings #-}

module Type.Top10 (
    Top10
  , top10
  , emptyTop10
  , latestScores
) where

import Type.Date
import Type.Play
import Type.Score
import Type.Player
import Type.PlayerIds
import Type.Scores
import Type.ShortEvent
import Type.ShortEvents
import Type.ScoreContainer
import Type.ScoreIndex
import Data.Tuple (swap)
import Data.List (foldl', sort)
import Data.Foldable (toList)
import Data.ByteString (ByteString)
import Data.Text.Encoding (decodeUtf8)
import Control.Monad
import Control.Monad.Primitive
import Data.Aeson (ToJSON, toJSON, object, (.=))
import qualified Data.Vector as V
import qualified Data.Vector.Algorithms.Intro as V
import qualified Data.Map.Strict as M
import qualified Data.Set as S
import Control.Arrow ((***), first)
import Control.Applicative
import Data.Ord 
import Data.List (sort)

newtype SimpleScore = SimpleScore {
  simpleScore :: (Score, Player)
} deriving (Show, Eq, Ord)

instance ToJSON SimpleScore where 
  toJSON (SimpleScore (s, p)) = object [ decodeUtf8 (player p) .= (score s) ]

newtype Top10 = Top10 {
  top10 :: [SimpleScore]
} deriving (Show, Eq)

emptyTop10 :: Top10
emptyTop10 = Top10 $ []
 
instance ToJSON Top10 where
  toJSON (Top10 s) = object [ "top10" .= s]

lastTenDays :: Date -> ScoreIndex -> V.Vector Score
lastTenDays d si = concatToScoreVector . map (flip getScores $ si) $ [d..(addDays 10 d)]

latestScores :: Scores -> Top10 
latestScores  (Scores d pid si) = Top10 . take 10 . sort . V.toList . V.map SimpleScore $ V.zip (lastTenDays d si) (toPlayerVector pid)
