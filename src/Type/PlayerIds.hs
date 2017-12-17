module Type.PlayerIds (
  PlayerIds,
  emptyPlayerIds,
  insertPlayer,
  playerId,
  toPlayerIdsVector
) where

import Type.Player 
import Data.ByteString (ByteString)

import Data.Tuple (swap)
import qualified Data.Vector as V
import qualified Data.HashMap.Strict as H

maxPlayers = 100000
maxDays = 100

data PlayerIds = PlayerIds {
    playerToId :: H.HashMap Player Int
  , freeIds  :: Int
} deriving (Show, Eq)

playerId :: Player -> PlayerIds -> (PlayerIds, Int)
playerId p ids@(PlayerIds pti i) = case H.lookup p pti of
  Just id -> (ids, id)
  Nothing -> (PlayerIds (H.insert p i pti) (i + 1), i) 

insertPlayer :: Player -> PlayerIds -> PlayerIds
insertPlayer p = fst . playerId p

emptyPlayerIds = PlayerIds H.empty 0

toPlayerIdsVector :: PlayerIds -> V.Vector Player 
toPlayerIdsVector (PlayerIds pti i) = (V.replicate i playerZero) V.// (map swap $ H.toList pti)
