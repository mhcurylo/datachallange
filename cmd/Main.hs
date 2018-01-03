{-# LANGUAGE ExtendedDefaultRules #-}
{-# LANGUAGE OverloadedStrings    #-}

module Main where

import Control.Parallel
import Process.File
import Type.Date
import Type.Play
import Type.Player
import Type.Scores
import Type.Score
import Type.Top10
import System.Environment (getArgs)
import Data.Attoparsec.ByteString (parseOnly)
import Conduit
import Data.List (foldl')
import Data.Monoid (mappend)
import Data.Either 
import Data.Conduit.Async
import Data.Conduit.Combinators (foldlE)
import Control.Arrow (right)
import Control.Monad (forM_)
import qualified Data.Text as T
import qualified Data.Map.Strict as M
import qualified Data.ByteString.Lazy as B
import qualified Data.ByteString.Lazy.Char8 as BC

type State = M.Map Date (M.Map Player Score) 

insertPlay :: State -> Play -> State
insertPlay m (Play p s d) = M.alter insertScore d m
  where
  insertScore ms = case ms of
    Just sm -> Just $ M.insertWith (+) p s sm
    Nothing -> Just $ M.singleton p s
main :: IO ()
main = do
  a <- getArgs
  f <- BC.readFile (head a)
  let p = parsePlays $ f  
  let m = foldl' insertPlay M.empty p
  print . show $ take 10 $  M.elems m 
      
