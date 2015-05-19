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

import Network
import Data.Maybe
import Data.Aeson
import Text.Printf

mkMove moves p s m  = fromMaybe
   (error $ printf "EVALUATE_MOVE: invalid move: %s\n" (show $ encode m))
   (moves p s m)

logicArgErr     = "usage: logic CENTER_PORT HOME_PORT"
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
ataxxLogic = withSocketsDo $
  Logic.main
    (getTwoPorts logicArgErr)    
    Ataxx.publicMoves
    Ataxx.publicStart
    Ataxx.publicMakeMove
    "AtaxxLogic"
    Ataxx

alphaBetaTree3 :: IO ()
alphaBetaTree3 = withSocketsDo $ 
  Tree.main alphaBeta "AlphaBetaTree" 3000000 maxBound

alphaBetaTree5 :: IO ()
alphaBetaTree5 = withSocketsDo $ 
  Tree.main alphaBeta "AlphaBetaTree" 5000000 maxBound

alphaBetaTree10 :: IO ()
alphaBetaTree10 = withSocketsDo $ 
  Tree.main alphaBeta "AlphaBetaTree" 10000000 maxBound  
  
