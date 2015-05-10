
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

  (centerOutPort, hCenterOut, centerInPort, hCenterIn) <-
    registerAtCenter
      getCenterOutPort
      name
      [gameType]
      GAMELOGIC
    
  forever $ respond hCenterIn $ \case
    
    SEC (Req_GET_START_STATE :: CenterLogic state move) ->
      pure $ Just $ SEC $ Res_GET_START_STATE $
        StateRec 0 ONGOING startState PMax

    SEC (Req_EVALUATE_MOVE (ReqEvaluateMove strec move)) -> do
      let StateRec id status state player = strec
      pure $ Just $ SEC $ Res_EVALUATE_MOVE $ ResEvaluateMove $
        StateRec id status (makeMove player state move) (switch player)

    SEC (Req_POSSIBLE_MOVES (ReqPossibleMoves (StateRec _ _ state player))) -> do
      pure $ Just $ SEC $ Res_POSSIBLE_MOVES $ ResPossibleMoves (moves player state)

    other -> error $
      printf (
        "expected EVALUATE_MOVE, GET_START_STATE or POSSIBLE_MOVES request, "
        ++ "but received %s\n") (show $ encode other)

