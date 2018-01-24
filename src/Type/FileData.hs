{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE BangPatterns #-}
{-# LANGUAGE DeriveGeneric, DeriveAnyClass #-}

module Type.FileData where

import qualified Data.ByteString.Char8 as B
import qualified Data.Vector as V
import qualified Data.Vector.Unboxed.Mutable as VUM
import qualified Data.Vector.Mutable as VM
import Data.List (foldl')
import Control.Monad.IO.Class (liftIO)
import Control.Monad (when)
import Conduit as CON
import Type.Play

import qualified Data.HashMap.Strict as HM
import qualified Data.HashTable.IO as HT

type FDV = VM.IOVector (VUM.IOVector Int)

fdvValue = do
  v <- VUM.replicate 2 0
  VUM.write v 0 99999
  return v

emptyFDV :: IO FDV
emptyFDV = VM.replicateM 102 fdvValue 

sDate = dayHM "01-01-2016"

laterThan :: Int -> Play -> Bool
laterThan cd (Play _ _ _ d) = d > (cd + 99)

fdvUpdate :: Int -> Int -> FDV -> IO ()
fdvUpdate fdid d v = do
  vv <- VM.read v (fdid)
  f <- VUM.read vv 0
  l <- VUM.read vv 1
  when (f > d) (VUM.write vv 0 d)
  when (l < d) (VUM.write vv 1 d)

fdvSink fdv = CON.mapM_C (\(Play f d _ _) -> liftIO $ fdvUpdate f d fdv)

