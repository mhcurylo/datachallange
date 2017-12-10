{-# LANGUAGE BangPatterns    #-}
{-# LANGUAGE OverloadedStrings #-}

module Type.Top10 (
  Top10,
  top10,
  emptyTop10,
  latestScores,
) where

import Type.Date
import Type.Play
import Type.Scores
import Data.List (foldl')
import Data.ByteString (ByteString)
import Data.Text.Encoding (decodeUtf8)
import Control.Monad
import Control.Monad.Primitive
import Data.Aeson (ToJSON, toJSON, object, (.=))
import qualified Data.Vector as V
import qualified Data.Vector.Unboxed.Mutable as VM
import qualified Data.Vector.Algorithms.Intro as V
import qualified Data.Map.Strict as M
import qualified Data.Set as S
import Control.Arrow ((***))
import Data.Ord 

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

lastTenDays :: Scores -> V.Vector Int
lastTenDays  (Scores d (PlayerIds _ itp _) (DayScores dsm)) = playerScores . foldl' mappend mempty . M.elems . fst  $ M.split (addDays 1000 d) dsm

latestScores :: PrimMonad m => Scores -> m Top10
latestScores  scores@(Scores _ (PlayerIds _ itp _) _) = do
  relevantScores <- V.thaw $ V.zip (lastTenDays scores) itp
  V.partialSortBy (flip (comparing fst)) relevantScores 10
  v <- V.freeze relevantScores
  return $ Top10 . V.toList . V.map (SimpleScore . (Score *** Player)) . V.take 10$ v 
--  return $ Top10 . V.toList . V.take 10 $ top10
