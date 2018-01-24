module Type.PlayerIds (
  PID,
  PlayerIds,
  emptyPlayerIds,
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

maxPlayers :: Int
maxPlayers = 100000
maxDays = 100

type PID = Word32

arbitraryPID :: Gen PID
arbitraryPID = choose (0, ((fromIntegral maxPlayers) - 1))

data PlayerIds = PlayerIds {
    playerToId :: H.BasicHashTable Player PID
  , lastId :: VM.IOVector PID
} 

playerId :: Player -> PlayerIds -> IO PID
playerId p ids@(PlayerIds pti vi) = do
  v <- H.lookup pti p
  case v of
    Just id -> return id
    Nothing -> do
      i <- VM.read vi 0
      H.insert pti p i 
      VM.write vi 0 (i + 1)
      return i 

emptyPlayerIds :: IO PlayerIds
emptyPlayerIds = do
  hashTable <- H.newSized maxPlayers
  vi <- VM.replicate 1 (fromIntegral 0)
  return $ PlayerIds hashTable vi

toPlayerVector :: PlayerIds -> IO (V.Vector Player )
toPlayerVector (PlayerIds pti vi) = do
  i <- VM.read vi 0
  vec <- VM.replicate (fromIntegral i) playerZero 
  H.mapM_ (\(k, v) -> VM.write vec (fromIntegral v) k) pti
  V.unsafeFreeze vec
