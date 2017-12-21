module Type.ShortEvent (
    ShortEvent
  , compress
  , decompress
  , add
  , withIndex
  , emptyShortEvent
  , arbitraryShortEvent 
) where

import Type.Score (Score(..)) 
import Type.PlayerIds (PID, arbitraryPID)
import Data.Int (Int32)
import Data.Word (Word64)
import Control.Applicative (liftA2)
import Control.Arrow ((&&&))
import Test.QuickCheck (Gen, arbitrary)

mx :: Word64
mx = fromIntegral (maxBound :: Int32) 

type ShortEvent = Word64

emptyShortEvent :: ShortEvent
emptyShortEvent = (0 :: Word64)

arbitraryShortEvent :: Gen ShortEvent
arbitraryShortEvent = fromIntegral <$> liftA2 compress arbitraryPID arbitrary

compress :: PID -> Score -> ShortEvent
compress p (Score s) = (fromIntegral s) + (fromIntegral p * mx)

decompress :: ShortEvent -> (PID, Score)
decompress = decodePid &&& decodeScore

decodePid :: ShortEvent -> PID
decodePid = fromIntegral . (`div` mx)

decodeScore :: ShortEvent -> Score
decodeScore = Score . fromIntegral . (`mod` mx)

withIndex :: ShortEvent -> (Int, ShortEvent)
withIndex = (fromIntegral . (`div` mx)) &&& id

add :: ShortEvent -> ShortEvent -> ShortEvent
add s1 s2 = (s1 `mod` mx) + s2
