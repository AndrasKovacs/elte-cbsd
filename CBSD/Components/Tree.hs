{-# LANGUAGE LambdaCase, RankNTypes, ScopedTypeVariables, FlexibleContexts #-}

module CBSD.Components.Tree where

import CBSD.Messages.SocketComm
import CBSD.Messages.Types
import CBSD.Search

import Data.Aeson
import Network
import Control.Exception
import Control.Concurrent
import System.IO
import Text.Printf
import Control.Monad
import Data.Function

main ::
     forall state move.
     (FromJSON state, ToJSON state,
      FromJSON move, ToJSON move)
  => IO PortNumber              -- ^ Port of central component
  -> (PortNumber -> IO ())      -- ^ Start heuristic 
  -> Search IO Score state move -- ^ Search algorithm
  -> String                     -- ^ Name of component
  -> [GameType]                 -- ^ Game types
  -> Int                        -- ^ Search timeout
  -> (move -> state -> state)   -- ^ Update state with move
  -> IO ()
main getCenterOutPort startHeu
     searchAlg name gameTypes timeout updWithMove = withSocketsDo $ do       
  hSetBuffering stdout LineBuffering

  -- Get center port number
  printf "getting port number of center\n"
  centerOutPort <- getCenterOutPort
  printf "acquired port number of center: %s\n" (show centerOutPort)  

  -- Connect to center
  hCenterOut <- fix $ \again -> do
    printf "trying to connect to center at port %s\n" (show centerOutPort)    
    catch (connectTo "localhost" (PortNumber centerOutPort))
      (\(_ :: IOException) -> do
           printf "failed to connect\n"
           threadDelay 1000000
           again)
  hSetBuffering hCenterOut LineBuffering      
  printf "connected to center\n"

  -- Accept center
  (centerInPort, centerInSock) <- listenOnUnusedPort
  printf "listening for center on port %s\n" (show centerInPort)
  
  putMessage hCenterOut (SEC (ReqTC_CONNECT $
    ReqConnect gameTypes name GAMETREE (fromIntegral centerInPort)
    :: TreeCenter state move))
                             
  printf "CONNECT message sent to center\n"  
  (hCenterIn, _, _) <- accept centerInSock
  hSetBuffering hCenterIn LineBuffering
  printf "accepted center\n"

  -- Accept heuristic
  (heuPort, heuSock) <- listenOnUnusedPort
  printf "starting heuristic\n"
  startHeu heuPort
  printf "listening for heuristic on port %s\n" (show heuPort)
  (hHeu, _, _) <- accept heuSock
  hSetBuffering hHeu LineBuffering
  printf "accepted heuristic\n"

  -- Set up search
  let
    heu :: state -> IO Score
    heu state = do
      request hHeu (SEC $ ReqTH_EVAL state) >>= \case
        SEC (ResTH_EVAL_RE score) -> pure $ Score score
        other -> 
          error $
            printf "expected EVAL_RE message, got %s\n"
              (show $ encode other)

    moves :: Player -> state -> IO [(state, move)]
    moves player state = do
      res <- request hCenterOut $ SEC $ ReqTC_POSSIBLE_MOVES $
               ReqPossibleMoves $ StateRec 0 ONGOING state player
      case res of
        SEC (ResTC_POSSIBLE_MOVES (ResPossibleMoves moves)) -> do
          pure $ map (\m -> (updWithMove m state, m)) moves
        other ->
          error $
            printf "expected POSSIBLE_MOVES response, go %s\n"
              (show $ encode other)

    search :: Player -> state -> IO (Maybe move)
    search = nextMove True moves heu searchAlg timeout maxBound
    

  -- Main loop
  forever $ respond hCenterIn $ \case
    SEC (
      ReqCT_TURN (
          ReqTurn gameId (StateRec _ _ state player) _ _)) -> do
      Just move <- search player state
      pure $ Just $ SEC $ ResCT_TURN $ ResTurn gameId move
      
