{-# LANGUAGE OverloadedStrings        #-}
{-# LANGUAGE ExtendedDefaultRules     #-}
{-# LANGUAGE FlexibleContexts     #-}

module Process.File where

import           Prelude hiding (mapM_)
import           Conduit
import           Data.Conduit.Combinators 
import           Data.Conduit.Attoparsec (conduitParserEither, PositionRange, ParseError)
import qualified Data.Conduit.Binary as C
import           Data.Conduit.Foldl
import           Data.Conduit.TMChan
import           Data.ByteString
import           Data.Text
import           Data.Void (Void)
import           Type.Play (parsePlay, Play)
import           Type.Scores (Scores(..), scoresInsert, emptyScoresFrom) 
import           Network.HTTP.Simple
import           Control.Monad.IO.Class (liftIO)

processFile :: (MonadResource m) => ConduitM ByteString Play m () 
processFile = C.lines .| mapC (parsePlay 0)

sourceFiles :: (MonadIO mo) => [String] -> mi (Source mo Play)
sourceFiles fs = mergeSources (fmap (\f -> sourceFileBS f .| processFile) fs) 100

sourceHttp :: (MonadResource mi, MonadIO mo, MonadBaseControl IO mi) => [Request] -> mi (Source mo Play)
sourceHttp fs = mergeSources (fmap (\f -> httpSource f getResponseBody .| processFile) fs) 100

processHttp :: (MonadResource mi, MonadIO mo, MonadBaseControl IO mi) => Scores -> [Request] -> mi (ConduitM () Void mo ())
processHttp scores fs = sourceHttp fs .|
               return (source .| mapM_ (\p -> liftIO $ scoresInsert scores p))

scoreFiles ::(MonadResource mi, MonadIO mo, MonadBaseControl IO mi) => Scores -> [String] -> mi (ConduitM () Void mo ())
scoreFiles scores fs =  sourceFiles fs .|
               return (source .| mapM_ (\p -> liftIO $ scoresInsert scores p))
