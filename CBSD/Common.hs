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

data HeuMsg state
  = Close
  | Eval state Player
  | EvalRe Int
  deriving (Eq, Show)

heuToJSON :: ToJSON strep => (state -> strep) -> HeuMsg state -> Value
heuToJSON stateToRep = \case
  Close -> object [
    "messageType" .= String "CLOSE"
    ]
  Eval state player -> object [
    "messageType" .= String "EVAL",
    "state" .= object [
      "board"      .= stateToRep state,
      "nextPlayer" .= player
      ]
    ]
  EvalRe score -> object [
    "messageType" .= String "EVAL_RE",
    "stateValue"  .= score
    ]

parseHeu ::
  forall state strep. FromJSON strep =>
  (strep -> state) -> Value -> Parser (HeuMsg state)
parseHeu convState = withObject "" $ \obj -> do
  (tag :: String) <- obj .: "messageType"
  case tag of 
    "CLOSE"   -> pure Close
    "EVAL"    -> do
      state <- obj .: "state"
      Eval <$> (convState <$> (state .: "board")) <*> state .: "nextPlayer"
    "EVAL_RE" -> EvalRe <$> obj .: "stateValue"
    _         -> empty
