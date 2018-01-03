{-# LANGUAGE OverloadedStrings        #-}
{-# LANGUAGE ExtendedDefaultRules     #-}

module Process.File where

import           Prelude hiding (mapM_)
import           Conduit
import           Data.Conduit.Combinators 
import           Data.Conduit.Attoparsec (conduitParserEither, PositionRange, ParseError)
import qualified Data.Conduit.Binary as DC
import           Data.Conduit.Foldl
import           Data.Conduit.Async
import           Data.ByteString
import qualified Data.ByteString.Lazy.Char8 as BLC
import           Data.Text
import           Data.Void (Void)
import           Type.Date (Date)
import           Type.Play (Play, parsePlay, playParserEOL, olderThan)
import           Type.Scores (Scores(..), scoresInsert, emptyScoresFrom) 
import           Network.HTTP.Simple
import           Control.Monad.IO.Class (liftIO)

parseFile :: MonadThrow m => ConduitM ByteString Play m ()
parseFile = awaitForever $ yield . parsePlay . BLC.fromStrict

processHttp :: (MonadResource m, MonadThrow m) => Scores -> [Request] -> ConduitM () Void m ()
processHttp scores fs = sequenceSources (fmap (\f -> httpSource f getResponseBody .| DC.lines .| parseFile) fs) 
               .| filterE (olderThan (sDate scores))
               .| concatC
               .| mapM_ (\p -> liftIO $ scoresInsert scores p)

scoreFiles :: (MonadResource m, MonadThrow m) => Scores -> [String] -> CConduit () Void m ()
scoreFiles scores fs = sequenceSources (fmap (\f -> sourceFileBS f .| DC.lines) fs) 
               =$=& concatC 
               =$=& parseFile
               =$=& mapM_ (\p -> liftIO $ scoresInsert scores p)
