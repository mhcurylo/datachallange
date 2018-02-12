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

sourceFiles fs = sequenceSources (fmap (\f -> sourceFileBS f .| processFile) fs)

sourceHttp fs = sequenceSources (fmap (\f -> httpSource f getResponseBody .| processFile) fs)

processHttp :: (MonadResource mi, MonadBaseControl IO mi) => Scores -> [Request] -> ConduitM () Void mi ()
processHttp scores fs = sourceHttp fs .| mapM_E (\p -> liftIO $ scoresInsert scores p)

scoreFiles ::(MonadResource mi, MonadBaseControl IO mi) => Scores -> [String] -> ConduitM () Void mi ()
scoreFiles scores fs = sourceFiles fs .| mapM_E (\p -> liftIO $ scoresInsert scores p)
