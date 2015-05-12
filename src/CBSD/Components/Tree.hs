{-# LANGUAGE LambdaCase, RankNTypes, ScopedTypeVariables, FlexibleContexts, OverloadedStrings #-}

module CBSD.Components.Tree where

import CBSD.Messages.SocketComm
import CBSD.Messages.Types
import CBSD.Search
import qualified CBSD.Ataxx as Ataxx
import qualified CBSD.Potyogos as Potyogos

import Data.Maybe
import Control.Lens
import Data.Aeson
import Data.Aeson.Lens
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
import System.Process
import qualified Data.ByteString as B

-- NOTE: AGARAK and ATTAX have exactly the same message formats!

-- specific main
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
  -> IO ()
main getCenterOutPort startHeu searchAlg name gameTypes timeout = do

  -- Set up heuristic
  (hPortNumber, hHeu) <- do
    (heuPort, heuSock) <- listenOnUnusedPort
    printf "GAMETREE: starting heuristic\n"
    startHeu heuPort
    printf "GAMETREE: listening for heuristic on port %s\n" (show heuPort)
    (hHeu, _, _) <- accept heuSock
    hSetBuffering hHeu LineBuffering
    printf "GAMETREE: accepted heuristic\n"
    pure (heuPort, hHeu)          
  
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
        CT_POSSIBLE_MOVES (ResPossibleMoves moves) ->
          pure $ map (\(MoveAndBoard s m) -> (s, m)) moves
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
    


-- GENERIC MAIN
genericMain ::
     (forall state move. Search IO Score state move) -- ^ Search algorithm
  -> String                                          -- ^ Name of component
  -> Int                                             -- ^ Search timeout
  -> IO ()
genericMain searchAlg name timeout = do

  -- Parse arguments
  (portCenterIn, heuristics) <- do
    let usage =
          error "usage: tree CENTER_PORT [-ataxxHeu=COMMAND] [-agarakHeu=COMMAND] [-potyogosHeu=COMMAND]"
            
    getArgs >>= \case
      cport : heus -> do
  
        heus <- forM heus $ \heu -> case splitOn "=" heu of
          ["-ataxxHeu",    path] -> pure (Ataxx, path)
          ["-agarakHeu",   path] -> pure (Agarak, path)
          ["-potyogosHeu", path] -> pure (Potyogos, path)
          splits -> error $ printf "can't recognize game type in %s argument\n" (intercalate "=" splits)
        
        portNum <- maybe
          usage                   
          pure
          (readMaybe cport :: Maybe Word16)
        
        when (length heus > 3) $ do
          error "There can be at most a single heuristic for a game type."
        
        pure (fromIntegral portNum, heus)
      _ -> usage
        

  -- setup all the heuristics
  (heuristics :: [(GameType, Handle)]) <- forM heuristics $ \(gtype, heuCmd) -> do    
    (heuPort, heuSock) <- listenOnUnusedPort    
    printf "GAMETREE: starting heuristic\n"
    let cmd = heuCmd ++ " " ++ show heuPort
    printf "GAMETREE: starting heuristic with command: %s\n" cmd
    spawnCommand cmd    
    printf "GAMETREE: listening for heuristic on port %s\n" (show heuPort)
    (hHeu, _, _) <- accept heuSock
    hSetBuffering hHeu LineBuffering
    printf "GAMETREE: accepted heuristic\n"
    pure (gtype, hHeu)    


  -- Register at center
  (portCenterIn, centerInSock, portCenterOut, hCenterOut) <-
    registerAtCenter
      (pure portCenterIn)
      name
      (map fst heuristics)
      GAMETREE

  let heu :: forall state. (FromJSON state, ToJSON state) => Handle -> state -> Player -> IO Score
      heu hHeu state player = do
        request hHeu (TH_EVAL state player :: TreeHeu state) >>= \case
          (TH_EVAL_RE score :: TreeHeu state) -> pure $ Score score
          other -> 
            error $
              printf "GAMETREE: expected EVAL_RE message, got %s\n"
                (show $ encode other)

      moves ::
           forall state move .
           (FromJSON state, FromJSON move, ToJSON state, ToJSON move) 
        => Player -> state -> IO [(state, move)]
      moves player state = do
        res <- request hCenterOut $ (TC_POSSIBLE_MOVES $
                 ReqPossibleMoves $ State 0 ONGOING state player :: TreeCenter state move)
        case (res :: CenterTree state move) of
          CT_POSSIBLE_MOVES (ResPossibleMoves moves) ->
            pure $ map (\(MoveAndBoard s m) -> (s, m)) moves
          other ->
            error $
              printf "GAMETREE: expected POSSIBLE_MOVES response, got %s\n"
                (show $ encode other)                      
                
      ataxxHeu :: Ataxx.GStateJSON -> Player -> IO Score
      ataxxHeu = heu (fromMaybe (error "no heuristic available for Ataxx") $ lookup Ataxx heuristics)

      -- We use Ataxx messages for this too!!!
      agarakHeu :: Ataxx.GStateJSON -> Player -> IO Score
      agarakHeu = heu (fromMaybe (error "no heuristic available for Agarak") $ lookup Agarak heuristics)

      potyogosHeu :: Potyogos.GStateJSON -> Player -> IO Score
      potyogosHeu = heu (fromMaybe (error "no heuristic available for Potyogos") $ lookup Potyogos heuristics)

      ataxxSearch :: Player -> Ataxx.GStateJSON -> IO (Maybe Ataxx.MoveJSON)
      ataxxSearch = nextMove True moves ataxxHeu searchAlg timeout maxBound

      agarakSearch :: Player -> Ataxx.GStateJSON -> IO (Maybe Ataxx.MoveJSON)
      agarakSearch = nextMove True moves agarakHeu searchAlg timeout maxBound

      potyogosSearch :: Player -> Potyogos.GStateJSON -> IO (Maybe Potyogos.MoveJSON)
      potyogosSearch = nextMove True moves potyogosHeu searchAlg timeout maxBound

  forever $ do
    (hCenterIn, _, _) <- accept centerInSock    
    line <- B.hGetLine hCenterIn
    let invalidLine = error $ printf "GAMETREE: expected TURN request, got %s\n" (show line)

    -- WE'RE FUCKING DETERMINING THE GAME TYPE BY LOOKING AT THE BOARD DIMENSIONS :(((
    -- AND DID I MENTION THIS IS UGLY AS FUCK
    case line^? _Value . key "content" . key "state" . key "board" . _JSON :: Maybe [[Int]] of
      Just board -> case (length board, length $ head board) of
        (6  , 7)  -> case decodeStrict line :: Maybe (CenterTree Potyogos.GStateJSON Potyogos.MoveJSON) of
          Just (CT_TURN (ReqTurn gameId (State _ _ state player) _ _)) -> do
            move <- fromJust <$> potyogosSearch player state
            putMessage hCenterIn (TC_TURN $ ResTurn gameId move :: TreeCenter Potyogos.GStateJSON Potyogos.MoveJSON)
          _ -> invalidLine
        (7  , 7)  -> case decodeStrict line :: Maybe (CenterTree Ataxx.GStateJSON Ataxx.MoveJSON) of
          Just (CT_TURN (ReqTurn gameId (State _ _ state player) _ _)) -> do
            move <- fromJust <$> ataxxSearch player state
            putMessage hCenterIn (TC_TURN $ ResTurn gameId move :: TreeCenter Ataxx.GStateJSON Ataxx.MoveJSON)
          _ -> invalidLine
        (10 , 10) -> case decodeStrict line :: Maybe (CenterTree Ataxx.GStateJSON Ataxx.MoveJSON) of
          Just (CT_TURN (ReqTurn gameId (State _ _ state player) _ _)) -> do
            move <- fromJust <$> ataxxSearch player state
            putMessage hCenterIn (TC_TURN $ ResTurn gameId move :: TreeCenter Ataxx.GStateJSON Ataxx.MoveJSON)
          _ -> invalidLine
      _ -> invalidLine

    hClose hCenterIn
