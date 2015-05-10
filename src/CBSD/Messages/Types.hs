{-# LANGUAGE TemplateHaskell, LambdaCase #-}

module CBSD.Messages.Types where
import CBSD.Messages.TH
import CBSD.Search
import Control.Lens
import Data.Aeson
import Data.Aeson.Lens
import Data.Aeson.TH
import qualified Data.Text as T

------------------------------------------------------------

newtype StripEmptyContent a = StripEmptyContent {unStripEmptyContent :: a}
  deriving (Eq, Show)

stripEmptyContent :: Value -> Value
stripEmptyContent = _Object . at (T.pack contentField) %~ \case
  Just (Array arr) | null arr -> Nothing
  other -> other

addEmptyContent :: Value -> Value
addEmptyContent = _Object . at (T.pack contentField) %~ \case
  Nothing -> Just (Array mempty)
  other   -> other

instance (FromJSON a) => FromJSON (StripEmptyContent a) where
  parseJSON val = StripEmptyContent <$> parseJSON (addEmptyContent val)

instance (ToJSON a) => ToJSON (StripEmptyContent a) where
  toJSON (StripEmptyContent a) = stripEmptyContent $ toJSON a

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

data Res_Connect
  = Res_CONNECT ResConnect
  | Res_CONNECT_DUMMY   -- HACK!!!!!!!!!!
  deriving (Eq, Show)
$(deriveJSON taggingOptions ''Res_Connect)

data Req_Connect
  = Req_CONNECT ReqConnect
  | Req_CONNECT_DUMMY   -- HACK!!!!!!!!!!
  deriving (Eq, Show)
$(deriveJSON taggingOptions ''Req_Connect)                      

data State state = State {
  st_id         :: Int,
  st_status     :: TurnStatus,
  st_board      :: state,
  st_nextPlayer :: Player
  } deriving (Eq, Show)
$(deriveJSON messageOptions ''State)

data ReqTurn state move = ReqTurn {
  reqt_gameId :: Int,
  reqt_state  :: State state,
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
  reqmoves_state :: State state
  } deriving (Eq, Show)
$(deriveJSON messageOptions ''ReqPossibleMoves)             

data ResPossibleMoves move = ResPossibleMoves {
  resmoves_moves :: [move]
  } deriving (Eq, Show)
$(deriveJSON messageOptions ''ResPossibleMoves)

data CenterTree state move
  = CT_TURN (ReqTurn state move)
  | CT_POSSIBLE_MOVES (ResPossibleMoves move)
  | CT_DUMMY             -- HACK!!!!!!!!!!
  deriving (Eq, Show)
$(deriveJSON taggingOptions ''CenterTree)

data TreeCenter state move
  = TC_TURN (ResTurn move)
  | TC_POSSIBLE_MOVES (ReqPossibleMoves state)    
  | TC_DUMMY             -- HACK!!!!!!!!!!    
  deriving (Eq, Show)
$(deriveJSON taggingOptions ''TreeCenter)

data TreeHeu state
  = TH_CLOSE    
  | TH_EVAL {
    theu_state :: state }
  | TH_EVAL_RE {
    theu_stateValue :: Int}
  deriving (Eq, Show)
$(deriveJSON taggingOptions ''TreeHeu)

data HeuTree
  = HT_EVAL_RE {
    heut_stateValue :: Int}
  | HT_DUMMY             -- HACK!!!!!!!!!!
  deriving (Eq, Show)
$(deriveJSON taggingOptions ''HeuTree)           

data ReqEvaluateMove state move = ReqEvaluateMove {
  reqEval_state :: State state,
  reqEval_move  :: move
  } deriving (Eq, Show)
$(deriveJSON messageOptions ''ReqEvaluateMove)

data ResEvaluateMove state = ResEvaluateMove {
  resEval_state :: State state
  } deriving (Eq, Show)
$(deriveJSON messageOptions ''ResEvaluateMove)

data CenterLogic state move
  = CL_POSSIBLE_MOVES  (ReqPossibleMoves state)
  | CL_EVALUATE_MOVE   (ReqEvaluateMove state move)
  | CL_GET_START_STATE
  deriving (Eq, Show)
$(deriveJSON taggingOptions ''CenterLogic)

data LogicCenter state move
  = LC_POSSIBLE_MOVES  (ResPossibleMoves move)
  | LC_EVALUATE_MOVE   (ResEvaluateMove state)
  | LC_GET_START_STATE (State state)
  deriving (Eq, Show)
$(deriveJSON taggingOptions ''LogicCenter)

