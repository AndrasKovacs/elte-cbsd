
{-# LANGUAGE TemplateHaskell #-}

module CBSD.Messages (
    Center_GL(..)
  , Center_GT(..)
  , GTH_State(..)
  , GT_Heu(..)
  , Wrap(..) ) where

import Data.Aeson.TH
import CBSD.Search
import CBSD.MessageGen

data Center_GL state move
  = CGL_REQ_GET_START_STATE
  | CGL_RES_GET_START_STATE {
    cgl_state :: state }
    
  | CGL_REQ_CONNECT {
    cgl_name  :: String,
    cgl_type  :: String,
    cgl_games :: [String],
    cgl_port  :: Int }
  | CGL_RES_CONNECT {
    cgl_result :: String,
    cgl_id     :: Int}
    
  | CGL_REQ_POSSIBLE_MOVES {
    cgl_state :: state,
    cgl_next  :: Player }
  | CGL_RES_GET_POSSIBLE_MOVES {
    cgl_moves :: [(state, move)] }
    
  | CGL_REQ_NEXT_STATE {
    cgl_state :: state,
    cgl_move  :: move }
  | CGL_RES_GET_NEXT_STATE {
    cgl_state :: state }    
  deriving (Eq, Show)

data Center_GT state move
  = CGT_REQ_CONNECT {
    cgt_name  :: String,
    cgt_typye :: String,
    cgt_games :: [String],
    cgt_port  :: Int }
  | CGT_RES_CONNECT {
    cgt_result :: String,
    cgt_id     :: Int }
    
  | CGT_REQ_TURN {
    cgt_gameId :: Int,
    cgt_state  :: state,
    cgt_next   :: Maybe Player,
    cgt_moves  :: Maybe [(state, move)] }
  | CGT_RES_TURN {
    cgt_gameId :: Int,
    cgt_move   :: move }

data GTH_State state
  = GTH_State {
    gth_board      :: state,
    gth_nextPlayer :: Player } -- Fölösleges a Player!
  deriving (Eq, Show)

data GT_Heu state
  = GTH_REQ_CLOSE    
  | GTH_REQ_EVAL {
    gth_state :: GTH_State state }
  | GTH_RES_EVAL_RE {
    gth_stateValue :: Int }
  deriving (Eq, Show)

$(deriveJSON genOptions ''Center_GL)
$(deriveJSON genOptions ''Center_GT)
$(deriveJSON genOptions ''GTH_State)
$(deriveJSON genOptions ''GT_Heu)           
