{-# LANGUAGE OverloadedStrings #-}

module Http.ServerSpec (main, spec) where

import Http.Server
import Test.Hspec 
import Test.Hspec.Wai 

main :: IO ()
main = hspec spec

spec :: Spec
spec = with (return app) $ do
  describe "POST /date" $ do
    it "responds with 200" $ do
      post "/date?current=10-12-2011" "" `shouldRespondWith` 200
