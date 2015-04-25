{-# LANGUAGE
  GeneralizedNewtypeDeriving, LambdaCase,
  ScopedTypeVariables, OverloadedStrings #-}

module CBSD.Common where

import Control.Applicative
import Control.Monad
import Data.Aeson
import Data.Aeson.Types

import CBSD.Search

data Result = Win Player | Draw | Continue deriving (Eq, Show)
newtype Score = Score Int deriving (Eq, Show, Ord, Num)

instance Bounded Score where
  maxBound = Score (maxBound - 1)
  minBound = Score (minBound + 2)  