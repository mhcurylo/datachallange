module Type.ScoreContainer (
    baseScoreContainer
  , insertPScore
  , sumScores
  , ScoreContainer(..)
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

maxPlayers = 100000

newtype ScoreContainer = ScoreContainer {
  vsc :: VM.IOVector Int32 
}

instance Show ScoreContainer where
  show _ = "Score Container IO"

baseScoreContainer :: IO ScoreContainer
baseScoreContainer = ScoreContainer <$> VM.replicate maxPlayers 0

insertPScore :: (PID, Score) -> ScoreContainer -> IO ()
insertPScore (p, s) sc = VM.modify (vsc sc) (+ (fromIntegral . score $ s)) (fromIntegral p)  

sumScores :: [ScoreContainer] -> IO (V.Vector Score)
sumScores ss = do
  acc <- VM.replicate maxPlayers (Score 0) 
  forM_ [0..maxPlayers-1] $ \j -> do
    r <- foldM (\p c -> (p +) <$> VM.read (vsc c) j) 0 ss
    VM.write acc j (Score . fromIntegral $ r)
  V.unsafeFreeze acc
