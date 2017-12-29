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
import Control.Applicative
import qualified Data.Vector as V
import qualified Data.Sequence as S
import qualified Data.HashMap.Strict as H

bufferLength = 4096

newtype ScoreIndex = ScoreIndex {
  scoreDay :: H.HashMap Date ScoreContainer 
} deriving (Show)

emptyScoreIndex = ScoreIndex H.empty

insertPIDScoreAtDate :: Date -> PID -> Score -> ScoreIndex -> IO ScoreIndex
insertPIDScoreAtDate d p s si@(ScoreIndex sd) = case H.lookup d sd of
    Just sc -> do 
      insertPScore (p, s) sc
      return si 
    Nothing -> do
      sc <- baseScoreContainer 
      insertPScore (p, s) sc
      return $ ScoreIndex $ H.insert d sc sd
  
getScores :: Date -> ScoreIndex -> IO ScoreContainer
getScores d (ScoreIndex si) = case H.lookup d si of
  Just sc -> return sc
  Nothing -> baseScoreContainer
