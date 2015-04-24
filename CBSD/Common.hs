{-# LANGUAGE GeneralizedNewtypeDeriving #-}

module CBSD.Common where

import Control.Applicative
import Control.Monad
import Data.Aeson

import CBSD.Search

data Cell   = Empty | Filled Player | Block deriving (Eq, Show)
data Result = Win Player | Draw | Continue deriving (Eq, Show)

newtype Score = Score Int deriving (Eq, Show, Ord, Num)

instance Bounded Score where
  maxBound = Score (maxBound - 1)
  minBound = Score (minBound + 2)
  

instance ToJSON Player where
  toJSON = \case PMax -> Number 1; _ -> Number 2

instance FromJSON Player where
  parseJSON = withScientific "" $ \case
    1 -> pure PMax
    2 -> pure PMin
    _ -> empty

instance ToJSON Cell where
  toJSON = \case
    Filled PMax -> Number 1
    Filled PMin -> Number 2
    Empty       -> Number 0  -- We conflate Empty and Blocked!!!
    Block       -> Number 0

instance FromJSON Cell where
  parseJSON v =
        (Filled <$> parseJSON v)
    <|> (Empty  <$ withScientific "" (guard . (==0)) v)
