{-# LANGUAGE TemplateHaskell, LambdaCase, OverloadedStrings, ScopedTypeVariables #-}

module CBSD.Messages.Types where
import CBSD.Messages.TH
import CBSD.Search
import Data.Aeson
import Data.Aeson.Types
import Control.Applicative
import Data.Aeson.TH
import qualified Data.Text as T
 
-- POSSIBLE MOVES KORREKCIÓ: [(state, move)] !!!!!

import CBSD.Ataxx


data TurnStatus    = ONGOING | DRAW | PLAYER_1_WON | PLAYER_2_WON deriving (Eq, Show)
data ComponentType = GAMETREE | GAMELOGIC | GUI deriving (Eq, Show)
data GameType      = Ataxx | Agarak | Potyogos deriving (Eq, Show)
data ConnectResult = OK | FAILURE deriving (Eq, Show)

$(deriveJSON defaultOptions ''TurnStatus)                      
$(deriveJSON defaultOptions ''ComponentType)           
$(deriveJSON defaultOptions ''ConnectResult)

instance FromJSON GameType where
  parseJSON = withText "" $ \case
    "ataxx"  -> pure Ataxx
    "fox"    -> pure Agarak
    "amoeba" -> pure Potyogos
    _        -> empty

instance ToJSON GameType where
  toJSON Ataxx    = String "ataxx"
  toJSON Agarak   = String "fox"
  toJSON Potyogos = String "amoeba"

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

data StateWrap state = StateWrap {
  wrap_state :: State state
  } deriving (Eq, Show)
$(deriveJSON messageOptions ''StateWrap)

data LogicCenter state move
  = LC_POSSIBLE_MOVES  (ResPossibleMoves move)
  | LC_EVALUATE_MOVE   (ResEvaluateMove state)
  | LC_GET_START_STATE (StateWrap state)

    
  deriving (Eq, Show)
$(deriveJSON taggingOptions ''LogicCenter)


-- TreeHeu is parsed manually
------------------------------------------------------------

data TreeHeu state
  = TH_CLOSE    
  | TH_EVAL {
    theu_state :: state,
    theu_nextPlayer :: Player}
  | TH_EVAL_RE {
    theu_stateValue :: Int}
  deriving (Eq, Show)

pToInt :: Player -> Int
pToInt = \case PMax -> 1; _ -> 2

intToP :: Value -> Parser Player
intToP = withScientific "" $ \case
  1 -> pure PMax
  2 -> pure PMin
  _ -> empty

instance (ToJSON state) => ToJSON (TreeHeu state) where
  toJSON TH_CLOSE =
    object [
      "@type"       .= T.pack "komp_common.MessageFactory$MSG_Close",
      "messageType" .= T.pack "CLOSE"
      ]
  toJSON (TH_EVAL state player) =
    object [
      "@type" .= T.pack "komp_common.MessageFactory$MSG_Eval",
      "state" .= object [
        "board" .= state,
        "nextPlayer" .= pToInt player
        ],
      "messageType" .= T.pack "EVAL"
      ]
  toJSON (TH_EVAL_RE stateVal) =
    object [
      "@type" .= T.pack "komp_common.MessageFactory$MSG_EvalRe",
      "stateValue" .= stateVal,
      "messageType" .= T.pack "EVALRE"
      ]

instance (FromJSON state) => FromJSON (TreeHeu state) where
  parseJSON = withObject "" $ \obj -> do
    (ty :: String) <- obj .: "messageType"
    case ty of
     "CLOSE" -> pure TH_CLOSE
     "EVAL"  -> do
       stateObj <- obj .: "state"
       TH_EVAL <$> (stateObj .: "board") <*> (intToP =<< (stateObj .: "nextPlayer"))
     "EVALRE" -> TH_EVAL_RE <$> (obj .: "stateValue")
     
