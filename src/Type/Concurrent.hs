{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE BangPatterns #-}
{-# LANGUAGE DeriveGeneric, DeriveAnyClass #-}

module Type.Concurrent where

import qualified Data.ByteString.Char8 as B
import qualified Data.Conduit.Binary as C
import Control.Monad (forM_, forever)
import qualified Data.Conduit.List as CL
import qualified Data.Vector as V
import qualified Data.Vector.Unboxed.Mutable as VUM
import qualified Data.Vector.Mutable as VM
import Data.List (foldl')
import Control.Monad.IO.Class (liftIO)
import Control.Concurrent (forkIO)
import Conduit as CON
import Data.Conduit.Async

import qualified Data.IntMap.Strict as IM
import qualified Data.HashMap.Strict as HM
import qualified Data.HashTable.IO as HT

import Type.Id 
import Type.Play
import Type.FileData
import Type.Acc

vvSink pid v = CON.mapM_C (\c -> liftIO $ do
        !dps <- getPid pid c
        addDPSV sDate v dps 
      )

v2Sink fdv pid v =  CON.mapM_C (\c@(Play f d _ _) -> liftIO $ do
        fdvUpdate f d fdv
        !dps <- getPid pid c
        addDPSV sDate v dps 
      )

readFileM :: String -> FDV -> Id -> VV -> IO (VV, FDV)
readFileM file fdv pid v = do
  runConduitRes $ (C.sourceFile file 
      .| C.lines 
      .| mapC (parsePlay 0) 
      .| v2Sink fdv pid v
      )
  return $ (v, fdv)


