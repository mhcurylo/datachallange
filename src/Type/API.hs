{-# LANGUAGE TemplateHaskell #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE GeneralizedNewtypeDeriving #-}

module Type.API (
  Files,
  files,
  Date,
  Top10,
  emptyTop10
) where

import Type.Play (Date)
import Type.Top10 (Top10, emptyTop10)
import Data.Aeson (FromJSON, parseJSON)
import Data.Aeson.TH (deriveJSON, defaultOptions)

data Files = Files {
  files :: [String]
} deriving (Eq, Ord, Show)

deriveJSON defaultOptions ''Files
