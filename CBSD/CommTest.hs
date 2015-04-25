
{-# LANGUAGE OverloadedStrings, TemplateHaskell, LambdaCase #-}

module CBSD.CommTest where

import Control.Lens
import Control.Lens.Extras
import Network

import Data.Aeson
import Data.Aeson.TH
import Data.Aeson.Lens

import CBSD.Common
import CBSD.Search


-- Dodgy hack to remove empty content fields from TH parsers
----------------------------------------------------------------------

newtype Wrap a = Wrap {_unWrap :: a}
  deriving (Eq, Show)           
           
instance ToJSON a => ToJSON (Wrap a) where
  toJSON = removeEmptyContent . toJSON . _unWrap
  
instance FromJSON a => FromJSON (Wrap a) where
  parseJSON = (Wrap <$>) . parseJSON . addEmptyContent

removeEmptyContent :: Value -> Value
removeEmptyContent = _Object . at "content" %~ \case
  Just (Array arr) | is _Empty arr -> Nothing
  other -> other

addEmptyContent :: Value -> Value
addEmptyContent = _Object . at "content" %~ \case
  Nothing -> Just (Array mempty)
  other   -> other


-- MESSAGES
----------------------------------------------------------------------

data Center_GL state move
  = CGL_REQ_GET_START_STATE
  | CGL_REQ_CONNECT {
    cgl_name  :: String,
    cgl_type  :: String,
    cgl_games :: [String],
    cgl_port  :: Int }
  | CGL_REQ_POSSIBLE_MOVES {
    cgl_state :: state,
    cgl_next  :: Player }
  | CGL_REQ_NEXT_STATE {
    cgl_state :: state,
    cgl_move  :: move }
  | CGL_RES_CONNECT {
    cgl_result :: String,
    cgl_id     :: Int}
  | CGL_RES_GET_START_STATE {
    cgl_state :: state }
  | CGL_RES_GET_POSSIBLE_MOVES {
    cgl_moves :: [(state, move)] }
  | CGL_RES_GET_NEXT_STATE {
    cgl_state :: state }
  deriving (Eq, Show)           

$(deriveJSON (
      defaultOptions {
          fieldLabelModifier     = drop 4,
          constructorTagModifier = drop 8,
          sumEncoding = defaultTaggedObject {
            tagFieldName      = "messageType",
            contentsFieldName = "content"
            }
          }) ''Center_GL)

data Center_GT state move
  = CGT_REQ_CONNECT {
    cgt_name  :: String,
    cgt_typye :: String,
    cgt_games :: [String],
    cgt_port  :: Int }
  | CGT_REQ_TURN {
    cgt_gameId :: Int,
    cgt_state  :: state,
    cgt_next   :: Maybe Player,
    cgt_moves  :: Maybe [(state, move)] }
  | CGT_RES_CONNECT {
    cgt_result :: String,
    cgt_id     :: Int }
  | CGT_RES_TURN {
    cgt_gameId :: Int,
    cgt_move   :: move }

$(deriveJSON (
      defaultOptions {
          fieldLabelModifier     = drop 4,
          constructorTagModifier = drop 8,
          sumEncoding = defaultTaggedObject {
            tagFieldName      = "messageType",
            contentsFieldName = "content"
            }
          }) ''Center_GT)

data GTH_State state
  = GTH_State {
    gth_board      :: state,
    gth_nextPlayer :: Player }
  deriving (Eq, Show)

$(deriveJSON (
      defaultOptions {
          fieldLabelModifier     = drop 4,
          sumEncoding = defaultTaggedObject {
            tagFieldName      = "messageType",
            contentsFieldName = "content"
            }
          }) ''GTH_State)           

data GT_Heu state
  = GTH_REQ_CLOSE
  | GTH_REQ_EVAL {
    gth_state :: GTH_State state }
  | GTH_RES_EVAL_RE {
    gth_stateValue :: Int }
  deriving (Eq, Show)

$(deriveJSON (
      defaultOptions {
          fieldLabelModifier     = drop 4,
          constructorTagModifier = drop 8,
          sumEncoding = defaultTaggedObject {
            tagFieldName      = "messageType",
            contentsFieldName = "content"
            }
          }) ''GT_Heu)           















-- portNum :: PortNumber
-- portNum = 6456

-- client :: IO ()
-- client = bracket
--   (connectTo "localhost" (PortNumber portNum))
--   hClose
--   $ \handle -> do
--     hSetBuffering handle LineBuffering
--     printf "client: connected\n"
--     printf "client: sending\n"
--     hPutStrLn handle "fooooooobaaaaaaar"

-- server :: IO ()
-- server = do
--   sock <- listenOn $ PortNumber portNum
--   printf "server: listening on %s\n" (show portNum)
--   bracket (accept sock) (hClose . (^._1)) $ \(handle, host, num) -> do
    
--     hSetBuffering handle LineBuffering
--     printf "server: accepted connection from %s\n" (show (host, num))
--     line <- hGetLine handle
--     printf "server: recieved input: %s\n" line
    

-- main :: IO ()
-- main = withSocketsDo $ do
--   hSetBuffering stdout LineBuffering
  
--   forkIO server
--   forkIO client
  
--   getLine
--   return ()
