{-# LANGUAGE LambdaCase, RankNTypes, ScopedTypeVariables, FlexibleContexts, OverloadedStrings #-}

module CBSD.Components.Tree where

import CBSD.Messages.SocketComm
import CBSD.Messages.Types
import CBSD.Search

import Control.Concurrent
import Control.Exception
import Control.Lens
import Control.Monad
import Data.Aeson
import Data.Aeson.Lens
import Data.Function
import Data.List
import Data.List.Split
import Data.Maybe
import Data.Word
import Network
import System.Environment
import System.IO
import System.Process
import Text.Printf
import Text.Read
import qualified Data.ByteString as B
    

main ::
     (forall state move. Search IO Score state move) -- ^ Search algorithm
  -> String                                          -- ^ Name of component
  -> Int                                             -- ^ Search timeout
  -> Int                                             -- ^ Maximum depth
  -> IO ()
main searchAlg name timeout maxDepth = do

  let usage =
        "usage: tree CENTER_PORT HOME_PORT [-ataxxHeu=COMMAND] " ++
        "[-agarakHeu=COMMAND] [-potyogosHeu=COMMAND]"  

  -- Parse arguments
  (centerPort, homePort, heuristics) <- do            
    getArgs >>= \case
      cport : hport : heus -> do
  
        heus <- forM heus $ \heu -> case splitOn "=" heu of
          ["-ataxxHeu",    path] -> pure (Ataxx, path)
          ["-agarakHeu",   path] -> pure (Agarak, path)
          ["-potyogosHeu", path] -> pure (Potyogos, path)
          splits -> error usage
        
        when (length heus > 3) $ do
          error "There can be at most a single heuristic for a game type."

        (cport, hport) <- maybe (error usage) pure $ do
          cport <- readMaybe cport :: Maybe Word16
          hport <- readMaybe hport :: Maybe Word16
          pure (fromIntegral cport, fromIntegral hport)

        pure (cport, hport, heus)
          
      _ -> error usage

  -- Start listening
  homeSock <- listenOn (PortNumber homePort)      

  -- Connect to all heuristics
  heuristics <- forM heuristics $ \(gtype, heuCmd) -> do    
    let cmd = heuCmd ++ " " ++ show homePort
    printf "GAMETREE: starting heuristic with command: %s\n" cmd
    spawnCommand cmd    
    printf "GAMETREE: listening for heuristic on port %s\n" (show homePort)
    (hHeu, _, _) <- accept homeSock
    hSetBuffering hHeu LineBuffering
    printf "GAMETREE: accepted heuristic\n"
    pure (gtype, hHeu)
    
  -- Connnect to center
  hCenterOut <- fix $ \again -> do
    printf
      "GAMETREE: trying to connect to center at port %s\n"
      (show centerPort)    
    catch (connectTo "localhost" (PortNumber centerPort))
      (\(_ :: IOException) -> do
           printf "GAMETREE: failed to connect\n"
           threadDelay 1000000
           again)
  hSetBuffering hCenterOut LineBuffering      
  printf "GAMETREE: connected\n"

  -- Exchange CONNECT messages
  putMessage hCenterOut (Req_CONNECT $
    ReqConnect (map fst heuristics) name GAMETREE (fromIntegral homePort))
  printf "GAMETREE: CONNECT request sent to center\n"

  resConnect <- getMessage hCenterOut
  case resConnect of
    Res_CONNECT (ResConnect res _) -> case res of
      OK      -> pure ()
      FAILURE -> error $
        printf "GAMETREE: received FAILURE code in CONNECT response from center\n"
    other -> error $
      printf
        "GAMETREE: expected CONNECT response, got %s\n" (show $ encode other)
  printf "GAMETREE: CONNECT response OK\n"


  -- set up heu and moves (NOTE THE MASKING!)           
  let heu :: Handle -> Value -> Player -> IO Score
      heu hHeu state player = uninterruptibleMask_ $ do
        request hHeu (TH_EVAL state player :: TreeHeu Value) >>= \case
          (TH_EVAL_RE score :: TreeHeu Value) -> pure $ Score score
          other -> 
            error $
              printf "GAMETREE: expected EVAL_RE message, got %s\n"
                (show $ encode other)
  
      moves :: Int -> Player -> Value -> IO [(Value, Value)]
      moves gameId player state = uninterruptibleMask_ $ do
        
        let msg :: TreeCenter Value Value
            msg = TC_POSSIBLE_MOVES $ ReqPossibleMoves $ State gameId ONGOING state player
                  
        printf "GAMETREE: requesting possible moves from center: %s\n" (show $ encode msg)        
        res <- request hCenterOut msg
        case (res :: CenterTree Value Value) of
          CT_POSSIBLE_MOVES (ResPossibleMoves moves) ->
            pure $ map (\(MoveAndBoard s m) -> (s, m)) moves
          other ->
            error $
              printf "GAMETREE: expected POSSIBLE_MOVES response, got %s\n"
                (show $ encode other)

      getHeuHandle :: GameType -> Handle
      getHeuHandle game =
        fromMaybe (error $ printf "no heuristic available for %s\n" (show game))
        $ lookup game heuristics

      search gameId gameType =
        nextMove True (moves gameId) (heu $ getHeuHandle gameType) alphaBeta timeout maxDepth
        
  -- Main loop
  forever $ do
    (hCenterIn, _, _) <- accept homeSock   
    line <- B.hGetLine hCenterIn
    
    let invalidLine :: a
        invalidLine = error $ printf "GAMETREE: expected TURN request, got %s\n" (show line)
    
    case line^? _Value . key "content" . key "state" . key "board" . _JSON :: Maybe [[Int]] of
      Just board -> let        
        gameType = case (length board, length $ head board) of
          (7, 6)   -> Potyogos
          (7, 7)   -> Ataxx
          (10, 10) -> Agarak
          _        -> invalidLine
        in case decodeStrict line :: Maybe (CenterTree Value Value) of
             Just (CT_TURN (ReqTurn gameId (State _ _ state player) _ _)) -> do
               move <- fromJust <$> search gameId gameType player state
               putMessage hCenterIn (TC_TURN $ ResTurn gameId move :: TreeCenter Value Value)
             _ -> invalidLine 
      _ -> invalidLine
    hClose hCenterIn
