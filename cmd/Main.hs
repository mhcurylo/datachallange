{-# LANGUAGE ExtendedDefaultRules #-}
{-# LANGUAGE OverloadedStrings    #-}

module Main where

import Process.File
import Type.Play
import Type.Acc
import Type.Scores
import Type.Top10
import System.Environment (getArgs)
import Data.Attoparsec.Text (parseOnly)
import Conduit
import Data.List (foldl')
import Data.Monoid (mappend)
import Data.Conduit.Async
import Data.Conduit.Combinators (foldlE)
import qualified Data.Text as T
import qualified Data.Map as M
import qualified Data.ByteString.Char8 as B

main :: IO ()
main = do
  s <- emptyScoresFrom sDate
  a <- getArgs
  runResourceT $ runCConduit $ scoreFiles s a 
  t10 <- latestScores s
  print . show $ t10
