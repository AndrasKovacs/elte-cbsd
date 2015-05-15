{-# LANGUAGE
  ScopedTypeVariables, NoMonomorphismRestriction, LambdaCase, FlexibleContexts #-}

module Execs where
  
import CBSD.Search
import CBSD.Utils.GetPortArgs
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

-- startHeu :: PortNumber -> IO ()
-- startHeu port = 
--   getArgs >>= \case
--     _:heuCmd:_ -> do
--       let cmd = heuCmd ++ " " ++ show port
--       printf "GAMETREE: starting heuristic with command: %s\n" cmd
--       spawnCommand cmd
--       pure ()
--     _ -> error specificTreeErr    

mkMove moves p s m  = fromMaybe
   (error $ printf "EVALUATE_MOVE: invalid move: %s\n" (show $ encode m))
   (moves p s m)

logicArgErr     = "usage: logic CENTER_PORT"
specificTreeErr = "usage: tree CENTER_PORT HEURISTIC_COMMAND"
heuArgErr       = "usage: heuristic GAMETREE_PORT"

potyogosHeu :: IO ()
potyogosHeu = withSocketsDo $ Heu.main (getOnePort heuArgErr) Potyogos.publicSmarterHeu

ataxxHeu :: IO ()
ataxxHeu = withSocketsDo $ Heu.main (getOnePort heuArgErr) Ataxx.publicHeu    

potyogosLogic :: IO ()
potyogosLogic = withSocketsDo $
  Logic.main
    (getTwoPorts logicArgErr)
    Potyogos.publicMoves
    Potyogos.publicStart
    Potyogos.publicMakeMove
    "PotyogosLogic"
    Potyogos


ataxxLogic :: IO ()
ataxxLogic = withSocketsDo $ do
  Logic.main
    (getTwoPorts logicArgErr)    
    Ataxx.publicMoves
    Ataxx.publicStart
    Ataxx.publicMakeMove
    "AtaxxLogic"
    Ataxx



alphaBetaTree :: IO ()
alphaBetaTree = withSocketsDo $ do
  Tree.genericMain (orderWith 0 minimax alphaBeta) "AlphaBetaTree" 1000000






-- ataxxTree :: IO ()
-- ataxxTree = withSocketsDo $ do
--   Tree.main
--     (getPortArgs specificTreeErr)
--     startHeu
--     (orderWith 0 minimax alphaBeta :: Search IO Score Ataxx.GStateJSON Ataxx.MoveJSON)
--     "AtaxxTree"
--     [Ataxx]
--     1000000
  
-- potyogosTreeWithHeu :: IO ()
-- potyogosTreeWithHeu = withSocketsDo $ do
--   Tree.main
--     (getPortArgs specificTreeErr)
--     (\port -> Heu.main (pure port) (Potyogos.publicSmarterHeu))
--     (orderWith 0 minimax alphaBeta)
--     "PotyogsTree"
--     [Potyogos]
--     1000000
--     (mkMove Potyogos.publicMakeMove)    

-- ataxxTreeWithHeu :: IO ()
-- ataxxTreeWithHeu = withSocketsDo $ do
--   Tree.main
--     getPortArgs
--     (\port -> Heu.main (pure port) (Ataxx.publicHeu))
--     (orderWith 0 minimax alphaBeta)
--     "AtaxxTree"
--     [Ataxx]
--     1000000
--     (mkMove Ataxx.publicMakeMove)


-- potyogosTree :: IO ()
-- potyogosTree = withSocketsDo $ do
--   Tree.main
--     (getPortArgs specificTreeErr)
--     startHeu
--     (orderWith 0 minimax alphaBeta :: Search IO Score Potyogos.GStateJSON Potyogos.MoveJSON)
--     "PotyogsTree"
--     [Potyogos]
--     1000000
