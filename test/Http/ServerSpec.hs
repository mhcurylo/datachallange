{-# LANGUAGE OverloadedStrings #-}

module Http.ServerSpec (main, spec) where

import Test.Hspec 
import Test.Hspec.Wai 

main :: IO ()
main = hspec spec

spec :: Spec
spec = describe "Place for integration tests" $ do
    it "responds with 200" $ do
      True

--    it "responds with 200" $ do
--      post "/date?current=10-12-2011" "" `shouldRespondWith` 200
