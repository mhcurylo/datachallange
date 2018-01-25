{-# LANGUAGE BangPatterns    #-}
{-# LANGUAGE OverloadedStrings #-}

module Type.Scores (
  Scores(..),
  emptyScores,
  emptyScoresFrom,
  scoresInsert,
  updateDate 
) where

import Type.Play
import Type.Id
import Type.FileData
import Type.Acc
import Data.ByteString (ByteString)
import Control.Monad
import Control.Applicative ((<*>), (<$>))

import qualified Data.Vector.Mutable as VM
import qualified Data.Vector.Unboxed.Mutable as VUM

data Scores = Scores {
    scDate :: Int
  , scPlayerIds :: Id 
  , scFileIds :: Id 
  , scFileData :: FDV 
  , scScores :: VV 
} 

emptyScoresFrom :: Int -> IO Scores
emptyScoresFrom d = Scores d <$> emptyPid <*> emptyFid <*> emptyFDV <*> emptyVector

emptyScores = emptyScoresFrom 0

updateDate :: Int -> Scores -> IO Scores
updateDate nd s = do
  v <- emptyVector
  return $ s {scDate = (nd - 10), scScores = v} 

scoresInsert :: Scores -> Play -> IO ()
scoresInsert (Scores cd pid _ fdv vv) pl@(Play f d p s) = if (d < cd)
  then return ()
  else do
    fdvUpdate f d fdv
    !dps <- getPid pid pl
    addDPSV cd vv dps 
