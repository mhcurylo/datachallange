{-# LANGUAGE DataKinds                #-}
{-# LANGUAGE OverloadedStrings        #-}
{-# LANGUAGE TypeOperators            #-}

module Http.Servant (
  DevDataChallengeAPI,
  devDataChallengeAPI
) where

import           Type.API
import           Data.Proxy
import           Data.Text                  (Text)
import           Servant

type DevDataChallengeAPI = "input" :> ReqBody '[JSON] Files :> Post '[JSON] Top10
                       :<|> "date" :> QueryParam "current" Date :> Post '[JSON] Top10 

devDataChallengeAPI :: Proxy DevDataChallengeAPI
devDataChallengeAPI = Proxy
