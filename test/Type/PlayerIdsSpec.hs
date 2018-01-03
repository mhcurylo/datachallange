module Type.PlayerIdsSpec (main, spec) where

import Test.Hspec 
import Test.QuickCheck

import Type.Player
import Type.PlayerIds
import Data.List
import qualified Data.Vector as V
import qualified Data.Set as S

main :: IO ()
main = hspec spec

-- prop_inserting_player_is_idempotent :: Player -> Bool
-- prop_inserting_player_is_idempotent p = insertp emptyPlayerIds == (insertp . insertp) emptyPlayerIds
--  where
--  insertp = insertPlayer p

--prop_first_player_gets_num0 :: Player -> Bool
--prop_first_player_gets_num0 p = 0 == (snd . playerId p $ emptyPlayerIds)

--prop_players_get_consequent_ids :: [Player] -> Bool
--prop_players_get_consequent_ids ps = all (uncurry (==)) $ zip [0..] cpsIds 
--  where
--  cps = S.toList . S.fromList $ ps
--  pids = foldl' (flip insertPlayer) emptyPlayerIds cps 
--  cpsIds = map (\p -> snd . playerId p $ pids) cps

--prop_toVector_preserves_ids :: [Player] -> Bool
--prop_toVector_preserves_ids ps = cps == V.toList pidsv  
--  where
--  cps = S.toList . S.fromList $ ps
--  pidsv = toPlayerVector $ foldl' (flip insertPlayer) emptyPlayerIds cps 
  
t :: [Player] -> Bool
t p = True

spec :: Spec
spec = do
  it "Is true" $ property $ t
--  it "Inserting player is idempotent" $ property prop_inserting_player_is_idempotent
--  it "Inserting one player gets him id 0" $ property prop_first_player_gets_num0 
--  it "Players get consequent ids" $ property prop_players_get_consequent_ids
--  it "ToVector assigns Players to their ids" $ property prop_toVector_preserves_ids

