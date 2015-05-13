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
import Data.List
import Data.List.Split
import System.Environment
import Text.Read
import Data.Word
import Data.Maybe

setupHeuristic :: (PortNumber -> IO ()) -> IO (PortNumber, Handle)
setupHeuristic startHeu = do
  (heuPort, heuSock) <- listenOnUnusedPort
  printf "GAMETREE: starting heuristic\n"
  startHeu heuPort
  printf "GAMETREE: listening for heuristic on port %s\n" (show heuPort)
  (hHeu, _, _) <- accept heuSock
  hSetBuffering hHeu LineBuffering
  printf "GAMETREE: accepted heuristic\n"
  pure (heuPort, hHeu)

main ::
     forall state move.
     (FromJSON state, ToJSON state,
      FromJSON move, ToJSON move)
  => IO PortNumber                  -- ^ Port of central component
  -> (PortNumber -> IO ())          -- ^ Start heuristic 
  -> Search IO Score state move     -- ^ Search algorithm
  -> String                         -- ^ Name of component
  -> [GameType]                     -- ^ Game types
  -> Int                            -- ^ Search timeout
  -> (Player -> state -> move -> (Maybe state, TurnStatus)) -- ^ Update state with move
  -> IO ()
main getCenterOutPort startHeu searchAlg name gameTypes timeout makeMove = do

  -- Set up heuristic
  (hPortNumber, hHeu) <- setupHeuristic startHeu  

  -- Register at center
  (portCenterIn, centerInSock, portCenterOut, hCenterOut) <-
    registerAtCenter
      getCenterOutPort
      name
      gameTypes
      GAMETREE

  -- Set up search
  let
    heu :: state -> Player -> IO Score
    heu state player = do
      request hHeu (TH_EVAL state player :: TreeHeu state) >>= \case
        (TH_EVAL_RE score :: TreeHeu state) -> pure $ Score score
        other -> 
          error $
            printf "GAMETREE: expected EVAL_RE message, got %s\n"
              (show $ encode other)

    moves :: Player -> state -> IO [(state, move)]
    moves player state = do
      res <- request hCenterOut $ (TC_POSSIBLE_MOVES $
               ReqPossibleMoves $ State 0 ONGOING state player :: TreeCenter state move)
      case (res :: CenterTree state move) of
        CT_POSSIBLE_MOVES (ResPossibleMoves moves) -> do
          pure $ map (\m -> (fromJust $ fst $ makeMove player state m, m)) moves
        other ->
          error $
            printf "GAMETREE: expected POSSIBLE_MOVES response, got %s\n"
              (show $ encode other)

    search :: Player -> state -> IO (Maybe move)
    search = nextMove True moves heu searchAlg timeout maxBound    

  -- Main loop
  forever $ do
    (hCenterIn, _, _) <- accept centerInSock
    respond hCenterIn $ \case    
      (CT_TURN (ReqTurn gameId (State _ _ state player) _ _) :: CenterTree state move) -> do
        Just move <- search player state
        pure $ Just $ (TC_TURN $ ResTurn gameId move :: TreeCenter state move)
    hClose hCenterIn
      

-- parseArgs :: IO (PortNumber, [(GameType, FilePath)])
-- parseArgs = 
--   getArgs >>= \case
--     cport : heus -> do
--       heus <- forM heus $ \heu -> case splitOn "=" heu of
--         ["-ataxxHeu",    path] -> pure (Ataxx, path)
--         ["-agarakHeu",   path] -> pure (Agarak, path)
--         ["-potyogosHeu", path] -> pure (Potyogos, path)
--         splits -> error $ printf "can't recognize game type in %s argument\n" (intercalate "=" splits)
--       portNum <- maybe
--         (error "usage: tree CENTER_PORT [-ataxxHeu=FILE] [-agarakHeu=FILE] [-potyogosHeu=FILE]")
--         pure
--         (readMaybe cport :: Maybe Word16)
--       pure (fromIntegral portNum, heus)

                    
-- generalMain ::
--      IO PortNumber                                   -- ^ Port of central component

--   -> (forall state move. Search IO Score state move) -- ^ Search algorithm
--   -> String                                          -- ^ Name of component
--   -> Int                                             -- ^ Search timeout
--   -> IO ()
-- generalMain searchAlg name timeout = do

--   (portCenterOut, heus) <- parseArgs

  
  -- Register at center
  -- (portCenterIn, centerInSock, portCenterOut, hCenterOut) <-
  --   registerAtCenter
  --     (pure portCenterOut)
  --     name
  --     gameTypes
  --     GAMETREE
  -- -- Set up search
  -- let
  --   heu :: state -> Player -> IO Score
  --   heu state player = do
  --     request hHeu (TH_EVAL state player :: TreeHeu state) >>= \case
  --       (TH_EVAL_RE score :: TreeHeu state) -> pure $ Score score
  --       other -> 
  --         error $
  --           printf "expected EVAL_RE message, got %s\n"
  --             (show $ encode other)

  --   moves :: Player -> state -> IO [(state, move)]
  --   moves player state = do
  --     res <- request hCenterOut $ (TC_POSSIBLE_MOVES $
  --              ReqPossibleMoves $ State 0 ONGOING state player :: TreeCenter state move)
  --     case (res :: CenterTree state move) of
  --       CT_POSSIBLE_MOVES (ResPossibleMoves moves) -> do
  --         pure $ map (\m -> (makeMove player state m, m)) moves
  --       other ->
  --         error $
  --           printf "expected POSSIBLE_MOVES response, go %s\n"
  --             (show $ encode other)

  --   search :: Player -> state -> IO (Maybe move)
  --   search = nextMove True moves heu searchAlg timeout maxBound    

  -- -- Main loop
  -- forever $ respond hCenterIn $ \case    
  --   (CT_TURN (ReqTurn gameId (State _ _ state player) _ _) :: CenterTree state move) -> do
  --     Just move <- search player state
  --     pure $ Just $ (TC_TURN $ ResTurn gameId move :: TreeCenter state move)
      
