module Type.ScoreContainer (
    ScoreContainer
  , emptyScoreContainer
  , insertPIDScore
  , concatToScoreVector
  , baseSC
  , insertPScore
  , sumScores
  , SC(..)
) where

import Type.PlayerIds
import Type.Score
import Type.ShortEvent
import Type.ShortEvents
import Control.Arrow ((&&&))
import Control.Monad
import Control.Applicative ((<$>))
import Data.Foldable (toList)
import Data.Monoid
import Data.List (concatMap)
import Data.Int
import qualified Data.Vector as V
import qualified Data.Vector.Mutable as VM
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

newtype SC = SC {
  vsc :: VM.IOVector Int32 
}

baseSC :: IO SC
baseSC = SC <$> VM.replicate maxPlayers 0

insertPScore :: (PID, Score) -> SC -> IO ()
insertPScore (p, s) sc = VM.modify (vsc sc) (+ (fromIntegral . score $ s)) (fromIntegral p)  

sumScores :: [SC] -> IO (V.Vector Score)
sumScores ss = do
  acc <- VM.replicate maxPlayers (Score 0) 
  forM_ [0..maxPlayers-1] $ \j -> do
    r <- foldM (\p c -> (p +) <$> VM.read (vsc c) j) 0 ss
    VM.write acc j (Score . fromIntegral $ r)
  V.freeze acc
