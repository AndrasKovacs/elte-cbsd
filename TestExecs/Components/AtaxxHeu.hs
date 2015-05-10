
module Main where

import CBSD.Search
import CBSD.Utils.GetPortArg
import qualified CBSD.Ataxx as Ataxx
import qualified CBSD.Components.Heuristic as Heu

import Data.Coerce
import Network

main :: IO ()
main = withSocketsDo $
  Heu.main
    getPortArg
    (coerce Ataxx.heu :: Ataxx.GStateJSON -> Int)

