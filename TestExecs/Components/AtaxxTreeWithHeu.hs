
module Main where

import Network
import Control.Concurrent
import Data.Coerce
import Data.Maybe

import CBSD.Search
import CBSD.Messages.Types
import CBSD.Utils.GetPortArg

import qualified CBSD.Ataxx as Ataxx
import qualified CBSD.Components.Tree as Tree
import qualified CBSD.Components.Heuristic as Heu

makeMove :: Player -> Ataxx.GStateJSON -> Ataxx.MoveJSON -> Ataxx.GStateJSON
makeMove p s m = fromJust $ coerce Ataxx.makeMove p s m

startHeu :: PortNumber -> IO ()
startHeu port = do
  forkIO $ Heu.main (pure port) (coerce Ataxx.heu :: Ataxx.GStateJSON -> Int)
  pure ()

main :: IO ()
main = withSocketsDo $ do
  Tree.main
    getPortArg
    startHeu
    (orderWith 0 minimax alphaBeta)
    "AtaxxTree"
    [Ataxx]
    1000000
    makeMove
