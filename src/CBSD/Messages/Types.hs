{-# LANGUAGE TemplateHaskell, LambdaCase #-}

module CBSD.Messages.Types where

import CBSD.Messages.TH
import CBSD.Search
import Control.Lens
import Data.Aeson
import Data.Aeson.Lens
import Data.Aeson.TH
import qualified Data.Text as T

{-
GameType, ConnectResult egységesítés?
TreeHeu egységesítés?

Általános hibaüzenet?
Opcionális mezők: null érték vagy kihagyás?

Potyogós: column 0-tól vagy 1-től kezdve számozva?

GET_START_STATE: Hol határozzuk meg, hogy melyik player kezd?
  -> jelenleg: GAMELOGIC-ba égettem, hogy PLAYER_1 kezd.
-}

------------------------------------------------------------

newtype StripEmptyContent a = SEC a

stripEmptyContent :: Value -> Value
stripEmptyContent = _Object . at (T.pack contentField) %~ \case
  Just (Array arr) | null arr -> Nothing
  other -> other

instance (FromJSON a) => FromJSON (StripEmptyContent a) where
  parseJSON val = SEC <$> parseJSON (stripEmptyContent val)

instance (ToJSON a) => ToJSON (StripEmptyContent a) where
  toJSON (SEC a) = stripEmptyContent $ toJSON a

------------------------------------------------------------    

data TurnStatus    = ONGOING | DRAW | PLAYER_1_WON | PLAYER_2_WON deriving (Eq, Show)
data ComponentType = GAMETREE | GAMELOGIC | GUI deriving (Eq, Show)
data GameType      = Ataxx | Fox | Amoeba deriving (Eq, Show)
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
  rest_step   :: move
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
  
data CenterTree state move
  = Req_TURN (ReqTurn state move)
  | Res_TURN (ResTurn move)
  deriving (Eq, Show)
$(deriveJSON taggingOptions ''CenterTree)

data TreeHeu state
  = Req_CLOSE    
  | Req_EVAL {
    theu_state :: state }
  | Res_EVAL_RE {
    theu_stateValue :: Int }
  deriving (Eq, Show)
$(deriveJSON taggingOptions ''TreeHeu)

data ReqEvaluateMove state move = ReqEvaluateMove {
  reqEval_state :: StateRec state,
  reqEval_move  :: move
  } deriving (Eq, Show)
$(deriveJSON messageOptions ''ReqEvaluateMove)

data ResEvaluateMove state = ResEvaluateMove {
  resEval_state :: StateRec state
  } deriving (Eq, Show)
$(deriveJSON messageOptions ''ResEvaluateMove)             

data CenterLogic state move
  = Req_POSSIBLE_MOVES  (ReqPossibleMoves state)
  | Res_POSSIBLE_MOVES  (ResPossibleMoves move)
  | Req_EVALUATE_MOVE   (ReqEvaluateMove state move)
  | Res_EVALUATE_MOVE   (ResEvaluateMove state)
  | Req_GET_START_STATE 
  | Res_GET_START_STATE (StateRec state)
  deriving (Eq, Show)
$(deriveJSON taggingOptions ''CenterLogic)

data Connect
  = Res_CONNECT ResConnect  
  | Req_CONNECT ReqConnect
  deriving (Eq, Show)
$(deriveJSON taggingOptions ''Connect)           

