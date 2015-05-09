{-# LANGUAGE LambdaCase #-}

module CBSD.Components.Heuristic where

import CBSD.Messages.SocketComm
import CBSD.Messages.Types

import Data.Aeson
import Network
import Control.Exception hiding (Handler)
import System.IO
import Text.Printf
import Control.Monad
import System.Exit
import Data.Coerce

type TreeHeu' state = StripEmptyContent (TreeHeu state)
type Handler  state = TreeHeu state -> IO (Maybe (TreeHeu state))
type Handler' state = TreeHeu' state -> IO (Maybe (TreeHeu' state))

main ::
     forall state.
     (ToJSON state, FromJSON state)
  => IO PortNumber
  -> (state -> Int)
  -> IO ()
main getPort heu = withSocketsDo $ do  
  portNum <- getPort  
  bracket
    (connectTo "localhost" (PortNumber portNum))
    hClose
    $ \handle -> do
      hSetBuffering handle LineBuffering
      forever $ respond handle $ (coerce :: Handler state -> Handler' state) $ \case
         ReqTH_CLOSE -> do
           printf "received CLOSE message from game tree\n"
           printf "closing now\n"
           exitSuccess
         ReqTH_EVAL (StateRec _ _ board _) ->
           pure $ Just $ ResTH_EVAL_RE (heu board)
         _ -> pure Nothing          
            
