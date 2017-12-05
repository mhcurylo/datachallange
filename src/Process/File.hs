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
import           Type.Play (Play, playParser, playParserEOL, isPlayOld)
import           Type.Scores (Scores(..), scorePlay, scoresInsert, scoresEmptyDate, latestScores, Top10) 
import           Network.HTTP.Simple
import           Control.Monad.IO.Class (liftIO)

parseFile :: MonadThrow m => ConduitM ByteString Play m ()
parseFile = conduitParserEither playParserEOL =$= awaitForever nextPlay
              where
              nextPlay (Left s) = error $ show s
              nextPlay (Right (_, p)) = yield $ p

processHttp :: (MonadResource m, MonadThrow m) => Scores -> [Request] -> ConduitM () Void m Scores
processHttp scores fs = sequenceSources (fmap (\f -> httpSource f getResponseBody .| parseFile) fs) 
               .| filterE (isPlayOld (sDate scores))
               .| concatC
               .| sinkFold scoresInsert scores id


scoreFiles :: (MonadResource m, MonadThrow m) => Date -> [String] -> ConduitM () Void m Top10
scoreFiles d fs = sequenceSources (fmap (\f -> sourceFileBS f .| parseFile) fs) 
               .| filterE (isPlayOld d)
               .| concatC
               .| sinkFold scoresInsert (scoresEmptyDate d) latestScores
