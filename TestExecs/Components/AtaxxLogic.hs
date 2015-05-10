{-# LANGUAGE OverloadedStrings, NoMonomorphismRestriction #-}
module Main where

import qualified CBSD.Components.Logic as Logic
import qualified CBSD.Ataxx as Ataxx
import CBSD.Messages.Types
import CBSD.Utils.GetPortArg
import CBSD.Search

import Data.Coerce
import Data.Maybe
import Data.Aeson
import Text.Printf

moves :: Player -> Ataxx.GStateJSON -> [Ataxx.MoveJSON]
moves p s = coerce (map snd $ Ataxx.moves p (coerce s))

makeMove :: Player -> Ataxx.GStateJSON -> Ataxx.MoveJSON -> Ataxx.GStateJSON
makeMove p s m =
  fromMaybe
    (error $ printf "EVALUATE_MOVE: invalid move: %s\n" (show $ encode m))
    (coerce Ataxx.makeMove p s m)

main :: IO ()
main = withSocketsDo $ do
  Logic.main
    getPortArg
    moves
    (coerce Ataxx.start :: Ataxx.GStateJSON)
    makeMove
    "AtaxxLogic"
    Ataxx

