module Type.ScoreContainer (
    ScoreContainer
  , emptyScoreContainer
  , insertPIDScore
  , concatToShortEvents
) where

import Type.PlayerIds
import Type.Score
import Type.ShortEvent
import Type.ShortEvents
import Control.Arrow ((&&&))
import Data.Monoid
import qualified Data.Sequence as S

bufferLength = 4096
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

concatToShortEvents :: [ScoreContainer] -> ShortEvents
concatToShortEvents = mconcat . uncurry (++) . unzip . map (selBuffer &&& selShortEvents)
