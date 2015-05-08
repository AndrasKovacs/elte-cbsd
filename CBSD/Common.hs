{-# LANGUAGE GeneralizedNewtypeDeriving #-}

module CBSD.Common where

import CBSD.Search

import Control.Applicative
import Control.Monad
import Data.Aeson
import Data.Aeson.Types

data Result = Win Player | Draw | Continue deriving (Eq, Show)
newtype Score = Score {_unScore :: Int} deriving (Eq, Show, Ord, Num, Integral, Enum, Real)

instance Bounded Score where
  maxBound = Score (maxBound - 1)
  minBound = Score (minBound + 2)  
