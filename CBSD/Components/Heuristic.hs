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

main ::
     (ToJSON state, FromJSON state)
  => IO PortNumber
  -> (state -> Int)
  -> IO ()
main getPort heu = withSocketsDo $ do  
  portNum <- getPort  
  handle <- connectTo "localhost" (PortNumber portNum)  
  hSetBuffering handle LineBuffering
  forever $ respond handle $ \case
     SEC Req_CLOSE -> do
       printf "received CLOSE message from game tree\n"
       printf "closing now\n"
       exitSuccess
     SEC (Req_EVAL (StateRec _ _ board _)) ->
       pure $ Just $ SEC $ Res_EVAL_RE (heu board)
     _ -> pure Nothing          
            
