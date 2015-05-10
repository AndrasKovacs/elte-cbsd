
module Main where

import qualified CBSD.Components.Heuristic as Heu
import qualified CBSD.Ataxx as Ataxx
import CBSD.Utils.GetPortArg
import CBSD.Search
import Data.Coerce

main :: IO ()
main = 
  Heu.main
    getPortArg
    (coerce Ataxx.heu :: Ataxx.GStateJSON -> Int)

