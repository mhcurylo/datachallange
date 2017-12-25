module Type.ScoreContainer (
    ScoreContainer
  , emptyScoreContainer
  , insertPIDScore
  , concatToScoreVector
) where

import Type.PlayerIds
import Type.Score
import Type.ShortEvent
import Type.ShortEvents
import Control.Arrow ((&&&))
import Data.Foldable (toList)
import Data.Monoid
import Data.List (concatMap)
import qualified Data.Vector as V
import qualified Data.Sequence as S

bufferLength = 100000
maxPlayers = 100000

data ScoreContainer = ScoreContainer { 
    selBuffer :: ShortEvents 
  , selShortEvents :: ShortEvents 
} deriving (Show, Eq)

emptyScoreContainer = ScoreContainer emptyShortEvents emptyShortEvents

insertPIDScore :: (PID, Score) -> ScoreContainer -> ScoreContainer
insertPIDScore (p, s) (ScoreContainer b e) = if S.length (shortEvents b') > bufferLength 
  then ScoreContainer emptyShortEvents (b' <> e) 
  else ScoreContainer b' e 
  where 
  b' = insertShortEvent (compress p s) b 

toShortEvents :: ScoreContainer -> ShortEvents
toShortEvents (ScoreContainer b e) = b <> e

concatToScoreVector :: [ScoreContainer] -> V.Vector Score 
concatToScoreVector = toVectorScore . mconcat . uncurry (++) . unzip . map (selBuffer &&& selShortEvents)
