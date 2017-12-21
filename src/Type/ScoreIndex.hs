module Type.ScoreIndex (
     ScoreIndex
   , insertPIDScoreAtDate  
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
import qualified Data.Vector as V
import qualified Data.Sequence as S
import qualified Data.HashMap.Strict as H

bufferLength = 4096

newtype ScoreIndex = ScoreIndex {
  scoreDay :: H.HashMap Date ScoreContainer 
} deriving (Show, Eq)

emptyScoreIndex = ScoreIndex H.empty

insertPIDScoreAtDate :: Date -> PID -> Score -> ScoreIndex -> ScoreIndex
insertPIDScoreAtDate d p s (ScoreIndex sd) = ScoreIndex $ H.alter updateWithPlayScore d sd
  where
  updateWithPlayScore = Just . insertPIDScore (p, s) . fromMaybe emptyScoreContainer 

getScores :: Date -> ScoreIndex -> ScoreContainer
getScores d (ScoreIndex si) = fromMaybe emptyScoreContainer (H.lookup d si)
