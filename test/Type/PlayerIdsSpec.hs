module Type.PlayerIdsSpec (main, spec) where

import Test.Hspec 
import Test.QuickCheck

import Type.Player
import Type.PlayerIds
import Data.List

main :: IO ()
main = hspec spec

prop_inserting_player_is_idempotent :: Player -> Bool
prop_inserting_player_is_idempotent p = insertp emptyPlayerIds == (insertp . insertp) emptyPlayerIds
  where
  insertp = insertPlayer p

prop_first_player_gets_num0 :: Player -> Bool
prop_first_player_gets_num0 p = 0 == (snd . playerId p $ emptyPlayerIds)

spec :: Spec
spec = do
  it "Inserting player is idempotent" $ prop_inserting_player_is_idempotent
  it "Inserting one player gets him id 0" $ prop_first_player_gets_num0 
