{-# LANGUAGE TemplateHaskell #-}

module CBSD.Messages.Types where

import CBSD.Search
import CBSD.Messages.TH
import Data.Aeson.TH

{-
GameType, ConnectResult egységesítés?
TreeHeu egységesítés?
Általános hibaüzenet.
Opcionális mezők: nuill érték vagy kihagyás?
-}


data TurnStatus    = ONGOING | DRAW | PLAYER_1_WON | PLAYER_2_WON deriving (Eq, Show)
data ComponentType = GAMETREE | GAMELOGIC | GUI deriving (Eq, Show)
data GameType      = Ataxx | Foxes | DropAmoeba deriving (Eq, Show)
data ConnectResult = OK | FAIL deriving (Eq, Show)

$(deriveJSON defaultOptions ''TurnStatus)                      
$(deriveJSON defaultOptions ''ComponentType)           
$(deriveJSON defaultOptions ''GameType)           
$(deriveJSON defaultOptions ''ConnectResult)

data ReqConnect = ReqConnect {
  reqc_games :: [GameType],
  reqc_name  :: String,
  reqc_type  :: ComponentType,
  reqc_port  :: Int
  } deriving (Eq, Show)
$(deriveJSON messageOptions ''ReqConnect)             

data ResConnect = ResConnect {
  resc_result :: ConnectResult,
  resc_id     :: Int
  } deriving (Eq, Show)
$(deriveJSON messageOptions ''ResConnect)             

data StateRec state = StateRec {
  st_id         :: Int,
  st_status     :: TurnStatus,
  st_board      :: state,
  st_nextPlayer :: Player
  } deriving (Eq, Show)
$(deriveJSON messageOptions ''StateRec)

data ReqTurn state move = ReqTurn {
  reqt_gameId :: Int,
  reqt_state  :: StateRec state,
  reqt_next   :: Maybe Player,
  reqt_moves  :: Maybe [move]
  } deriving (Eq, Show)
$(deriveJSON messageOptions ''ReqTurn)              

data ResTurn move = ResTurn {
  rest_gameId :: Int,
  rest_step   :: [move]
  } deriving (Eq, Show)
$(deriveJSON messageOptions ''ResTurn)

data ReqPossibleMoves state = ReqPossibleMoves {
  reqmoves_state :: StateRec state
  } deriving (Eq, Show)
$(deriveJSON messageOptions ''ReqPossibleMoves)             

data ResPossibleMoves move = ResPossibleMoves {
  resmoves_moves :: [move]
  } deriving (Eq, Show)
$(deriveJSON messageOptions ''ResPossibleMoves)

data TreeCenter
  = ReqTC_CONNECT ReqConnect
  | ResTC_CONNECT ResConnect
  deriving (Eq, Show)
$(deriveJSON taggingOptions ''TreeCenter)           
  
data CenterTree state move
  = ReqCT_TURN (ReqTurn state move)
  | ResCT_TURN (ResTurn move)
  | ReqCT_POSSIBLE_MOVES (ReqPossibleMoves state)    
  | ResCT_POSSIBLE_MOVES (ResPossibleMoves move)    
  deriving (Eq, Show)
$(deriveJSON taggingOptions ''CenterTree)

data TreeHeu state
  = ReqTH_CLOSE    
  | ReqTH_EVAL {
    theu_state :: StateRec state }
  | ResTH_EVAL_RE {
    theu_stateValue :: Int }
  deriving (Eq, Show)
$(deriveJSON taggingOptions ''TreeHeu)

data CenterLogic state move
  = ReqCL_POSSIBLE_MOVES (ReqPossibleMoves state)
  | ResCL_POSSIBLE_MOVES (ResPossibleMoves move)
  | ReqCL_GET_START_STATE
  | ResCL_GET_START_STATE (StateRec state)
  deriving (Eq, Show)
$(deriveJSON taggingOptions ''CenterLogic)

data LogicCenter
  = ReqLC_CONNECT ReqConnect
  | ResLC_CONNECT ResConnect
  deriving (Eq, Show)
$(deriveJSON taggingOptions ''LogicCenter)

