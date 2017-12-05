{-# LANGUAGE ExtendedDefaultRules #-}
{-# LANGUAGE OverloadedStrings    #-}

module Main where

import Process.File
import Type.Date
import Type.Play
import Type.Scores
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
  a <- getArgs
  r <- runResourceT $ runCConduit $ scoreFiles testDate a 
  print . show $ r
