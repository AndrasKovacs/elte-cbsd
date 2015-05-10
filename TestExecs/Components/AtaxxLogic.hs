{-# LANGUAGE OverloadedStrings, NoMonomorphismRestriction #-}

module Main where

import qualified CBSD.Components.Logic as Logic
import qualified CBSD.Ataxx as Ataxx
import CBSD.Messages.Types
import CBSD.Messages.SocketComm
import CBSD.Utils.GetPortArg
import CBSD.Search
import Data.Coerce
import Data.Maybe
import Data.Aeson


moves :: Player -> Ataxx.GStateJSON -> [Ataxx.MoveJSON]
moves p s = coerce (map snd $ Ataxx.moves p (coerce s))

makeMove :: Player -> Ataxx.GStateJSON -> Ataxx.MoveJSON -> Ataxx.GStateJSON
makeMove p s m = fromJust $ coerce Ataxx.makeMove p s m

main :: IO ()
main =
  Logic.main
    (pure 1234)
    moves
    (coerce Ataxx.start :: Ataxx.GStateJSON)
    makeMove
    "AtaxxLogic"
    Ataxx      


connectIn = "{\"content\":{\"result\":\"OK\",\"id\":1},\"messageType\":\"CONNECT\"}"
