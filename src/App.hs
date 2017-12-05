{-# LANGUAGE DataKinds                #-}
{-# LANGUAGE TypeOperators            #-}

module App where

import           Http.Server
import           Network.Wai
import           Network.Wai.Handler.Warp

runApp = flip run <$> app
