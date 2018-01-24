{-# LANGUAGE BangPatterns #-}
{-# LANGUAGE OverloadedStrings #-}

module Type.Acc where

import qualified Data.Vector.Unboxed.Mutable as VUM
import qualified Data.Vector.Mutable as VM
import Control.Monad.IO.Class (liftIO)
import Conduit as CON
import Type.Play
import Type.Id 

type VV = VUM.IOVector Int

emptyVector :: IO VV
emptyVector = VUM.replicate 100000 0 

sDate = dayHM "01-01-2017"

addDPSV :: Int -> VV -> DPS -> IO ()
addDPSV cd v (d, p, s) = if (d > cd + 10) || (d < cd)
  then return ()
  else VUM.write v p s

