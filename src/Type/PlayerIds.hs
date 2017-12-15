module Type.PlayerIds (
  PlayerIds,
  emptyPlayerIds,
  insertPlayer,
  playerId
) where

import Type.Player 
import Data.ByteString (ByteString)

import Data.Monoid
import Control.Monad
import qualified Data.Vector as V
import qualified Data.Vector.Algorithms.Intro as V
import qualified Data.HashMap.Strict as H

maxPlayers = 100000
maxDays = 100

data PlayerIds = PlayerIds {
    playerToId :: H.HashMap Player Int
  , freeIds  :: Int
} deriving (Show)

playerId :: Player -> PlayerIds -> (PlayerIds, Int)
playerId p ids@(PlayerIds pti i) = case H.lookup p pti of
  Just id -> (ids, id)
  Nothing -> (PlayerIds (H.insert p i' pti) i', i') 
  where
  i' = i + 1

insertPlayer :: Player -> PlayerIds -> PlayerIds
insertPlayer p = fst . playerId p

emptyPlayerIds = PlayerIds H.empty 0
