module Type.PlayerIds (
  PID,
  PlayerIds,
  emptyPlayerIds,
  insertPlayer,
  playerId,
  toPlayerIdsVector,
  arbitraryPID
) where

import Type.Player 
import Data.ByteString (ByteString)

import Data.Word (Word32)
import Data.Tuple (swap)
import Control.Arrow (second)
import Test.QuickCheck
import qualified Data.Vector as V
import qualified Data.HashMap.Strict as H

maxPlayers = 100000
maxDays = 100

type PID = Word32

arbitraryPID :: Gen PID
arbitraryPID = choose (0, 100000)

data PlayerIds = PlayerIds {
    playerToId :: H.HashMap Player PID
  , freeIds  :: PID
} deriving (Show, Eq)

playerId :: Player -> PlayerIds -> (PlayerIds, PID)
playerId p ids@(PlayerIds pti i) = case H.lookup p pti of
  Just id -> (ids, id)
  Nothing -> (PlayerIds (H.insert p i pti) (i + 1), i) 

insertPlayer :: Player -> PlayerIds -> PlayerIds
insertPlayer p = fst . playerId p

emptyPlayerIds = PlayerIds H.empty 0

toPlayerIdsVector :: PlayerIds -> V.Vector Player 
toPlayerIdsVector (PlayerIds pti i) = (V.replicate (fromIntegral i) playerZero) V.// (map (first fromIntegral . swap) $ H.toList pti)
