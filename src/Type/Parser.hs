{-# LANGUAGE OverloadedStrings #-}

module Type.Parse where

import Data.Text (Text, pack)
import Data.Attoparsec.Text (Parser, parseOnly, decimal, char, endOfInput) 


