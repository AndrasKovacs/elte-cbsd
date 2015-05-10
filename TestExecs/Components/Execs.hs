
module Execs where
  
import CBSD.Search
import CBSD.Utils.GetPortArg
import CBSD.Messages.Types
import qualified CBSD.Ataxx as Ataxx
import qualified CBSD.Components.Heuristic as Heu
import qualified CBSD.Components.Tree as Tree
import qualified CBSD.Components.Logic as Logic

import Control.Concurrent
import Data.Coerce
import Network
import Data.Maybe
import Data.Aeson
import Text.Printf


ataxxHeu :: IO ()
ataxxHeu = withSocketsDo $
  Heu.main
    getPortArg
    (coerce Ataxx.heu :: Ataxx.GStateJSON -> Int)

ataxxLogic :: IO ()
ataxxLogic = withSocketsDo $ do
  Logic.main
    getPortArg
    moves
    (coerce Ataxx.start :: Ataxx.GStateJSON)
    makeMove
    "AtaxxLogic"
    Ataxx
  where
    moves :: Player -> Ataxx.GStateJSON -> [Ataxx.MoveJSON]
    moves p s = coerce (map snd $ Ataxx.moves p (coerce s))
    
    makeMove :: Player -> Ataxx.GStateJSON -> Ataxx.MoveJSON -> Ataxx.GStateJSON
    makeMove p s m =
      fromMaybe
        (error $ printf "EVALUATE_MOVE: invalid move: %s\n" (show $ encode m))
        (coerce Ataxx.makeMove p s m)    

ataxxTreeWithHeu :: IO ()
ataxxTreeWithHeu = withSocketsDo $ do
  Tree.main
    getPortArg
    startHeu
    (orderWith 0 minimax alphaBeta)
    "AtaxxTree"
    [Ataxx]
    1000000
    makeMove
  where
    makeMove :: Player -> Ataxx.GStateJSON -> Ataxx.MoveJSON -> Ataxx.GStateJSON
    makeMove p s m =
      fromMaybe
         (error $ printf "EVALUATE_MOVE: invalid move: %s\n" (show $ encode m))
         (coerce Ataxx.makeMove p s m)
    
    startHeu :: PortNumber -> IO ()
    startHeu port = do
      forkIO $ Heu.main (pure port) (coerce Ataxx.heu :: Ataxx.GStateJSON -> Int)
      pure ()    
