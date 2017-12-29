{-# LANGUAGE OverloadedStrings        #-}
{-# LANGUAGE ExtendedDefaultRules     #-}

module Process.File where

import           Conduit
import           Data.Conduit.Combinators 
import           Data.Conduit.Attoparsec (conduitParserEither, PositionRange, ParseError)
import           Data.Conduit.Foldl
import           Data.ByteString
import           Data.Text
import           Data.Void (Void)
import           Type.Date (Date)
import           Type.Play (Play, playParser, playParserEOL, olderThan)
import           Type.Scores (Scores(..), scoresInsert, emptyScoresFrom) 
import           Network.HTTP.Simple
import           Control.Monad.IO.Class (liftIO)

parseFile :: MonadThrow m => ConduitM ByteString Play m ()
parseFile = conduitParserEither playParserEOL =$= awaitForever nextPlay
              where
              nextPlay (Left s) = error $ show s
              nextPlay (Right (_, p)) = yield $ p

processHttp :: (MonadResource m, MonadThrow m) => Scores -> [Request] -> ConduitM () Void m Scores
processHttp scores fs = sequenceSources (fmap (\f -> httpSource f getResponseBody .| parseFile) fs) 
               .| filterE (olderThan (sDate scores))
               .| concatC
               .| foldM (\s p -> liftIO (scoresInsert s p)) scores

scoreFiles :: (MonadResource m, MonadThrow m) => Date -> [String] -> ConduitM () Void m Scores
scoreFiles d fs = sequenceSources (fmap (\f -> sourceFileBS f .| parseFile) fs) 
               .| filterE (olderThan d)
               .| concatC
               .| foldM (\s p -> liftIO (scoresInsert s p)) (emptyScoresFrom d)
