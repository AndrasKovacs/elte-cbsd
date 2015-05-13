{-# LANGUAGE ScopedTypeVariables, NoMonomorphismRestriction, LambdaCase #-}

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


{- HIBÁK
1.
ember-ember Ataxx-logic EVALUATE_MOVE: center:
   "unrecognized field: STATUS" --> pedig benne van a doksiban...

2.
ember-gép Ataxx-logic Ataxx-Tree-With-Heu:
   GUI null exceptionnel meghal

-}




startHeu :: PortNumber -> IO ()
startHeu port = 
  getArgs >>= \case
    _:heuCmd:_ -> do
      let cmd = heuCmd ++ " " ++ show port
      printf "GAMETREE: starting heuristic with command: %s\n" cmd
      spawnCommand cmd
      pure ()
    _ -> error specificTreeErr    

mkMove moves p s m  = fromMaybe
   (error $ printf "EVALUATE_MOVE: invalid move: %s\n" (show $ encode m))
   (moves p s m)

logicArgErr     = "usage: logic PORT_OF_CENTER"
specificTreeErr = "usage: tree PORT_OF_CENTER HEURISTIC_CMD"
heuArgErr       = "usage: heuristic PORT_OF_GAMETREE"

potyogosHeu :: IO ()
potyogosHeu = withSocketsDo $ Heu.main (getPortArg heuArgErr) Potyogos.publicSmarterHeu

ataxxHeu :: IO ()
ataxxHeu = withSocketsDo $ Heu.main (getPortArg heuArgErr) Ataxx.publicHeu    

-- potyogosLogic :: IO ()
-- potyogosLogic = withSocketsDo $
--   Logic.main
--     (getPortArg logicArgErr)
--     Potyogos.publicMoves
--     Potyogos.publicStart
--     (mkMove Potyogos.publicMakeMove)
--     "PotyogosLogic"
--     Potyogos
 
-- potyogosTree :: IO ()
-- potyogosTree = withSocketsDo $ do
--   Tree.main
--     (getPortArg specificTreeErr)
--     startHeu
--     (orderWith 0 minimax alphaBeta)
--     "PotyogsTree"
--     [Potyogos]
--     1000000
--     (mkMove Potyogos.publicMakeMove)    

ataxxLogic :: IO ()
ataxxLogic = withSocketsDo $ do
  Logic.main
    (getPortArg specificTreeErr)
    Ataxx.publicMoves
    Ataxx.publicStart
    Ataxx.publicMakeMove
    "AtaxxLogic"
    Ataxx

-- ataxxTree :: IO ()
-- ataxxTree = withSocketsDo $ do
--   Tree.main
--     (getPortArg logicArgErr)
--     startHeu
--     (orderWith 0 minimax alphaBeta)
--     "AtaxxTree"
--     [Ataxx]
--     1000000
--     (mkMove Ataxx.publicMakeMove)

-- potyogosTreeWithHeu :: IO ()
-- potyogosTreeWithHeu = withSocketsDo $ do
--   Tree.main
--     (getPortArg specificTreeErr)
--     (\port -> Heu.main (pure port) (Potyogos.publicSmarterHeu))
--     (orderWith 0 minimax alphaBeta)
--     "PotyogsTree"
--     [Potyogos]
--     1000000
--     (mkMove Potyogos.publicMakeMove)    

-- ataxxTreeWithHeu :: IO ()
-- ataxxTreeWithHeu = withSocketsDo $ do
--   Tree.main
--     (pure 1234)
--     (\port -> Heu.main (pure port) (Ataxx.publicHeu))
--     (orderWith 0 minimax alphaBeta)
--     "AtaxxTree"
--     [Ataxx]
--     1000000
--     (mkMove Ataxx.publicMakeMove)
