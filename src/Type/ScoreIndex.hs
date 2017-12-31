module Type.ScoreIndex (
     ScoreIndex
   , insertPIDScoreAtIndex 
   , emptyScoreIndex
   , getScores
) where

import Type.PlayerIds
import Type.Date
import Type.Score
import Type.Player 
import Type.ScoreContainer
import Data.ByteString (ByteString)
import Data.Monoid
import Data.Maybe
import Control.Monad
import Control.Applicative
import qualified Data.Vector as V
import qualified Data.Vector.Mutable as VM
import qualified Data.Sequence as S
import qualified Data.HashMap.Strict as H

newtype ScoreIndex = ScoreIndex {
  scoreDay :: V.Vector ScoreContainer 
} deriving (Show)

emptyScoreIndex :: IO ScoreIndex
emptyScoreIndex = ScoreIndex <$> V.replicateM 100 baseScoreContainer

insertPIDScoreAtIndex :: Int -> PID -> Score -> ScoreIndex -> IO () 
insertPIDScoreAtIndex i p s (ScoreIndex si) = insertPScore (p, s) (si V.! i)

getScores :: Int -> ScoreIndex -> ScoreContainer
getScores i (ScoreIndex si) = si V.! i   
