{-# LANGUAGE ScopedTypeVariables, NoMonomorphismRestriction #-}

module Execs where
  
import CBSD.Search
import CBSD.Utils.GetPortArg
import CBSD.Messages.Types

import qualified CBSD.Ataxx as Ataxx
import qualified CBSD.Potyogos as Potyogos

import qualified CBSD.Components.Heuristic as Heu
import qualified CBSD.Components.Tree as Tree
import qualified CBSD.Components.Logic as Logic

import Control.Concurrent
import Data.Coerce
import Network
import Data.Maybe
import Data.Aeson
import Text.Printf

import System.Environment
import System.Process

mkMove moves p s m  = fromMaybe
   (error $ printf "EVALUATE_MOVE: invalid move: %s\n" (show $ encode m))
   (moves p s m)

startHeu :: PortNumber -> IO ()
startHeu port = 
  getArgs >>= \case
    _:path:_ -> do
      callProcess path [show port]
    _ -> error "usage: tree [port number of center] [file path of heuristic]"


potyogosHeu :: IO ()
potyogosHeu = withSocketsDo $ Heu.main getPortArg Potyogos.publicSmarterHeu

ataxxHeu :: IO ()
ataxxHeu = withSocketsDo $ Heu.main getPortArg Ataxx.publicHeu

-- tree :: IO ()
-- tree = withSocketsDo $
--   Tree.main
--     getPortArg
--     startHeu
--     (orderWith 0 minimax alphaBeta)
--     "Alpha-beta tree"
--     [Amoeba, Foxes, Ataxx]
--     1000000
    

potyogosLogic :: IO ()
potyogosLogic = withSocketsDo $
  Logic.main
    getPortArg
    Potyogos.publicMoves
    Potyogos.publicStart
    (mkMove Potyogos.publicMakeMove)
    "PotyogosLogic"
    Amoeba

potyogosTreeWithHeu :: IO ()
potyogosTreeWithHeu = withSocketsDo $ do
  Tree.main
    getPortArg
    (\port -> Heu.main (pure port) (Potyogos.publicSmarterHeu))
    (orderWith 0 minimax alphaBeta)
    "PotyogsTree"
    [Amoeba]
    1000000
    (mkMove Potyogos.publicMakeMove)

ataxxLogic :: IO ()
ataxxLogic = withSocketsDo $ do
  Logic.main
    getPortArg
    Ataxx.publicMoves
    Ataxx.publicStart
    (mkMove Ataxx.publicMakeMove)
    "AtaxxLogic"
    Ataxx

ataxxTreeWithHeu :: IO ()
ataxxTreeWithHeu = withSocketsDo $ do
  Tree.main
    getPortArg
    (\port -> Heu.main (pure port) (Ataxx.publicHeu))
    (orderWith 0 minimax alphaBeta)
    "AtaxxTree"
    [Ataxx]
    1000000
    (mkMove Ataxx.publicMakeMove)
