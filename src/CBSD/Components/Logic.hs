
{-# LANGUAGE
  LambdaCase, RankNTypes, ScopedTypeVariables,
  FlexibleContexts, MonoLocalBinds #-}

module CBSD.Components.Logic where

import CBSD.Messages.SocketComm
import CBSD.Messages.Types
import CBSD.Search

import Data.Aeson hiding (Result)
import Network
import System.IO
import Text.Printf
import Control.Monad
import Data.Function
import Data.Maybe
 
main ::
     forall state move.
     (FromJSON state, ToJSON state,
      FromJSON move, ToJSON move)
  => IO PortNumber                                          -- ^ Port of center
  -> (Player -> state -> [move])                            -- ^ Possible moves
  -> state                                                  -- ^ Start state
  -> (Player -> state -> move -> (Maybe state, TurnStatus)) -- ^ Update state with move
  -> String                                                 -- ^ Name of component
  -> GameType                                               -- ^ Game type
  -> IO ()
main getCenterOutPort moves startState makeMove name gameType = do

  (centerInPort, centerInSock, centerOutPort, hCenterOut) <-
    registerAtCenter
      getCenterOutPort
      name
      [gameType]
      GAMELOGIC

  forever $ do
    (hCenterIn, _, _) <- accept centerInSock    
    respond hCenterIn $ \(msg :: CenterLogic state move) -> do
      case msg of
      
        (CL_GET_START_STATE :: CenterLogic state move) -> do
          printf "LOGIC: received GET_START_STATE\n"
          let resp = LC_GET_START_STATE $ StateWrap $ State 0 ONGOING startState PMax
          printf "Sending response: %s\n" (show $ encode resp)
          pure $ Just $ resp
        
        CL_EVALUATE_MOVE (ReqEvaluateMove strec move) -> do
          let State id status state player = strec
          printf "LOGIC: received EVALUATE_MOVE %s\n" (show $ encode msg)
          
          let (maybeSt, status) = makeMove player state move
          let state' = fromMaybe state maybeSt
                              
          let resp = LC_EVALUATE_MOVE $ ResEvaluateMove $
               State id status state' (switch player)
          printf "Result status %s\n" (show status)              
          printf "Sending response: %s\n" (show $ encode resp)                       
          pure $ Just resp
        
        CL_POSSIBLE_MOVES (ReqPossibleMoves (State _ _ state player)) -> do
          pure $ Just $ LC_POSSIBLE_MOVES $ ResPossibleMoves (moves player state)
    hClose hCenterIn

