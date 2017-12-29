module Type.PlayerIds (
  PID,
  PlayerIds,
  emptyPlayerIds,
  insertPlayer,
  playerId,
  toPlayerVector,
  arbitraryPID
) where

import Type.Player 
import Data.ByteString (ByteString)

import Data.Word (Word32)
import Data.Tuple (swap)
import Control.Arrow (second, first)
import Test.QuickCheck
import qualified Data.Vector as V
import qualified Data.Vector.Mutable as VM
import qualified Data.HashTable.IO as H

maxPlayers = 100000
maxDays = 100

type PID = Word32

arbitraryPID :: Gen PID
arbitraryPID = choose (0, 100000)

data PlayerIds = PlayerIds {
    playerToId :: H.BasicHashTable Player PID
  , freeIds  :: PID
} deriving (Show)

playerId :: Player -> PlayerIds -> IO (PlayerIds, PID)
playerId p ids@(PlayerIds pti i) = do
  v <- H.lookup pti p
  case v of
    Just id -> return (ids, id)
    Nothing -> do
      H.insert pti p i 
      return (PlayerIds pti (i + 1), i) 

insertPlayer :: Player -> PlayerIds -> IO PlayerIds
insertPlayer p pids = do
  (pids, pid) <- playerId p pids
  return pids

emptyPlayerIds :: IO PlayerIds
emptyPlayerIds = do
  hashTable <- H.newSized maxPlayers
  return $ PlayerIds hashTable 0

toPlayerVector :: PlayerIds -> IO (V.Vector Player )
toPlayerVector (PlayerIds pti i) = do
  vec <- VM.replicate (fromIntegral i) playerZero 
  H.mapM_ (\(k, v) -> VM.write vec (fromIntegral v) k) pti
  V.freeze vec
