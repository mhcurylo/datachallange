module Type.ShortEvents (
    ShortEvents
  , shortEvents  
  , toVectorScore
  , insertShortEvent
  , compressShortEvents
  , emptyShortEvents
) where 

import Type.Score
import Type.ShortEvent 
import Data.Monoid
import Data.ByteString (ByteString)
import Data.Foldable (toList)
import Test.QuickCheck
import qualified Data.Vector as V
import qualified Data.Sequence as S

maxPlayers = 100000
accumVector = V.replicate (maxPlayers + 1) emptyShortEvent

newtype ShortEvents = ShortEvents {
  shortEvents :: S.Seq ShortEvent
} deriving (Show)

emptyShortEvents = ShortEvents S.empty

insertShortEvent :: ShortEvent -> ShortEvents -> ShortEvents
insertShortEvent se (ShortEvents s) = ShortEvents $ s S.|> se 

toVector :: ShortEvents -> V.Vector ShortEvent
toVector = V.accum add accumVector . map withIndex . toList . shortEvents

toVectorScore :: ShortEvents -> V.Vector Score
toVectorScore = V.map decodeScore . toVector

compressShortEvents :: ShortEvents -> ShortEvents
compressShortEvents = ShortEvents . S.fromList . filter (/= emptyShortEvent) . V.toList . toVector

instance Eq ShortEvents where
  (==) sa sb = cleanSeq sa == cleanSeq sb 
    where
    cleanSeq = shortEvents . compressShortEvents

instance Arbitrary ShortEvents where
  arbitrary = ShortEvents . S.fromList <$> listOf arbitraryShortEvent

instance Monoid ShortEvents where
  mappend (ShortEvents s1) (ShortEvents s2) = compressShortEvents (ShortEvents $ s1 <> s2)
  mempty = emptyShortEvents
  mconcat ms = compressShortEvents . ShortEvents $ foldMap shortEvents ms 
