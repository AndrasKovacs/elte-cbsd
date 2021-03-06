
{-# LANGUAGE LambdaCase, RankNTypes, ScopedTypeVariables, FlexibleContexts #-}

module CBSD.Components.Logic where

import CBSD.Messages.SocketComm
import CBSD.Messages.Types
import CBSD.Search

import Data.Aeson
import Network
import System.IO
import Text.Printf
import Control.Monad
import Data.Function

main ::
     forall state move.
     (FromJSON state, ToJSON state,
      FromJSON move, ToJSON move)
  => IO PortNumber                        -- ^ Port of center
  -> (Player -> state -> [move])          -- ^ Possible moves
  -> state                                -- ^ Start state
  -> (Player -> state -> move -> state)   -- ^ Update state with move
  -> String                               -- ^ Name of component
  -> GameType                             -- ^ Game type
  -> IO ()
main getCenterOutPort moves startState makeMove name gameType = withSocketsDo $ do

  (centerInPort, hCenterIn, centerOutPort, hCenterOut) <-
    registerAtCenter
      getCenterOutPort
      name
      [gameType]
      GAMELOGIC
    
  forever $ respond hCenterIn $ \(msg :: CenterLogic state move) -> do
    case msg of
    
      (CL_GET_START_STATE :: CenterLogic state move) ->
        pure $ Just $ LC_GET_START_STATE $
          State 0 ONGOING startState PMax
      
      CL_EVALUATE_MOVE (ReqEvaluateMove strec move) -> do
        let State id status state player = strec
        pure $ Just $ LC_EVALUATE_MOVE $ ResEvaluateMove $
          State id status (makeMove player state move) (switch player)
      
      CL_POSSIBLE_MOVES (ReqPossibleMoves (State _ _ state player)) -> do
        pure $ Just $ LC_POSSIBLE_MOVES $ ResPossibleMoves (moves player state)

