{-# LANGUAGE LambdaCase, RankNTypes, ScopedTypeVariables, FlexibleContexts #-}

module CBSD.Components.Tree where

import CBSD.Messages.SocketComm
import CBSD.Messages.Types
import CBSD.Search

import Data.Aeson
import Network
import System.IO
import Text.Printf
import Control.Monad
import Data.Function

setupHeuristic :: (PortNumber -> IO ()) -> IO (PortNumber, Handle)
setupHeuristic startHeu = do
  (heuPort, heuSock) <- listenOnUnusedPort
  printf "starting heuristic\n"
  startHeu heuPort
  printf "listening for heuristic on port %s\n" (show heuPort)
  (hHeu, _, _) <- accept heuSock
  hSetBuffering hHeu LineBuffering
  printf "accepted heuristic\n"
  pure (heuPort, hHeu)  

main ::
     forall state move.
     (FromJSON state, ToJSON state,
      FromJSON move, ToJSON move)
  => IO PortNumber                      -- ^ Port of central component
  -> (PortNumber -> IO ())              -- ^ Start heuristic 
  -> Search IO Score state move         -- ^ Search algorithm
  -> String                             -- ^ Name of component
  -> [GameType]                         -- ^ Game types
  -> Int                                -- ^ Search timeout
  -> (Player -> state -> move -> state) -- ^ Update state with move
  -> IO ()
main getCenterOutPort startHeu
     searchAlg name gameTypes timeout makeMove = withSocketsDo $ do

  -- Register at center
  (portCenterIn, hCenterIn, portCenterOut, hCenterOut) <-
    registerAtCenter
      getCenterOutPort
      name
      gameTypes
      GAMETREE

  -- Set up heuristic
  (hPortNumber, hHeu) <- setupHeuristic startHeu

  -- Set up search
  let
    heu :: state -> IO Score
    heu state = do
      request hHeu (TH_EVAL state) >>= \case
        HT_EVAL_RE score -> pure $ Score score
        other -> 
          error $
            printf "expected EVAL_RE message, got %s\n"
              (show $ encode other)

    moves :: Player -> state -> IO [(state, move)]
    moves player state = do
      res <- request hCenterOut $ (TC_POSSIBLE_MOVES $
               ReqPossibleMoves $ State 0 ONGOING state player :: TreeCenter state move)
      case res of
        (CT_POSSIBLE_MOVES (ResPossibleMoves moves) :: CenterTree state move) -> do
          pure $ map (\m -> (makeMove player state m, m)) moves
        other ->
          error $
            printf "expected POSSIBLE_MOVES response, go %s\n"
              (show $ encode other)

    search :: Player -> state -> IO (Maybe move)
    search = nextMove True moves heu searchAlg timeout maxBound    

  -- Main loop
  forever $ respond hCenterIn $ \case    
    (CT_TURN (ReqTurn gameId (State _ _ state player) _ _) :: CenterTree state move) -> do
      Just move <- search player state
      pure $ Just $ (TC_TURN $ ResTurn gameId move :: TreeCenter state move)
      
