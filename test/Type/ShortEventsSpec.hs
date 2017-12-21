
module Type.ShortEventsSpec (main, spec) where

import Test.Hspec 
import Test.QuickCheck

import Type.ShortEvent
import Type.ShortEvents
import Data.Monoid
import Data.Foldable (toList)
import Data.List (sort, nub, foldl')
import qualified Data.Sequence as S

main :: IO ()
main = hspec spec

prop_compressShortEvents_is_idempotent :: ShortEvents -> Bool
prop_compressShortEvents_is_idempotent se = sc == compressShortEvents sc 
  where
  sc = compressShortEvents se

prop_compressShortEvents_makes_players_unique :: ShortEvent -> ShortEvents -> Bool
prop_compressShortEvents_makes_players_unique se ss = indexes == nub indexes
  where
  ss' = insertShortEvent se . insertShortEvent se $ ss
  indexes = map (fst . withIndex) .  toList . shortEvents $ compressShortEvents ss' :: [Int]

prop_compressShortEvents_accumulates_values :: ShortEvent -> Bool
prop_compressShortEvents_accumulates_values se = doubleSe == doubleSe' 
  where
  doubleSe = add se se 
  ss = insertShortEvent se . insertShortEvent se $ emptyShortEvents
  doubleSe' = head . toList . shortEvents . compressShortEvents $ ss

prop_ShortEvents_Eq_compares_compressed :: ShortEvent -> Bool
prop_ShortEvents_Eq_compares_compressed se = ss == ss'
  where
  ss = insertShortEvent se . insertShortEvent se $ emptyShortEvents
  ss' = compressShortEvents ss

prop_monoid_identity :: ShortEvents -> Bool
prop_monoid_identity se = se == se <> mempty

prop_monoid_associativity :: ShortEvents -> ShortEvents -> ShortEvents -> Bool
prop_monoid_associativity s1 s2 s3 = s1 <> (s2 <> s3) == (s1 <> s2) <> s3

prop_communicative_monoid :: ShortEvents -> ShortEvents -> Bool
prop_communicative_monoid s1 s2 = s1 <> s2 == s2 <> s1

prop_ShortEvents_mconcat_is_correct :: [ShortEvents] -> Bool
prop_ShortEvents_mconcat_is_correct ses = mconcat ses == foldl' mappend mempty ses

spec :: Spec
spec = do
  it "compressShortEvents is idempotent" $ property $ prop_compressShortEvents_is_idempotent
  it "compressShortEvents limits the ShortEvents to one entry per player" $ property $ forAll arbitraryShortEvent prop_compressShortEvents_makes_players_unique 
  it "compressShortEvents accumulates values" $ property $ forAll arbitraryShortEvent prop_compressShortEvents_accumulates_values
  it "Eq ShortEvents compares compressed events" $ property $ forAll arbitraryShortEvent prop_ShortEvents_Eq_compares_compressed
  it "has monoid identity" $ property $ prop_monoid_identity
  it "has monoid associativity" $ property $ prop_monoid_associativity
  it "is communicative monoid" $ property $ prop_communicative_monoid 
  it "mconcat is correct" $ property $ prop_ShortEvents_mconcat_is_correct 
