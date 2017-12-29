{-# LANGUAGE DataKinds                #-}
{-# LANGUAGE OverloadedStrings        #-}
{-# LANGUAGE TypeOperators            #-}
{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE RankNTypes #-}

module Http.Server (
  app
) where

import           Http.Servant
import           Type.API
import           Type.Scores
import           Type.Top10
import           Process.File
import           Data.Proxy
import           Data.Text                  (Text)
import           GHC.Generics
import           Network.Wai
import           Network.Wai.Handler.Warp
import           Network.HTTP.Client
import           Servant
import           Servant.API
import           Control.Concurrent.MVar (MVar, modifyMVar_, newMVar, takeMVar, putMVar)
import           Control.Monad.IO.Class (liftIO)
import           Conduit
import           Data.Conduit.Async (runCConduit)

type ServerState = MVar Scores

server :: IO (Server DevDataChallengeAPI)
server = do
  serverState <- liftIO $ newMVar emptyScores
  return $inputEP serverState
       :<|> dateEP serverState

inputEP :: ServerState -> Files -> Handler Top10
inputEP state fs = do 
  requests <- mapM parseRequest $ files fs
  scores <- liftIO $ takeMVar state 
  newScores <- runResourceT $ runCConduit $ processHttp scores requests 
  liftIO $ putMVar state newScores
  liftIO $ latestScores newScores

dateEP :: ServerState -> Maybe Date -> Handler Top10
dateEP state current = case current of
  Just d -> do
    scores <- liftIO $ takeMVar state 
    let newScores = updateDate d scores
    liftIO $ putMVar state newScores
    liftIO $ latestScores newScores
  Nothing -> fail "I need a date, OK?"

app :: IO Application
app = serve devDataChallengeAPI <$> server
