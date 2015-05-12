{-# LANGUAGE LambdaCase, ScopedTypeVariables #-}

module CBSD.Components.Heuristic where

import CBSD.Messages.SocketComm
import CBSD.Messages.Types
import CBSD.Search

import Data.Aeson
import Network
import Control.Exception hiding (Handler)
import Control.Concurrent
import Data.Function
import System.IO
import Text.Printf
import Control.Monad
import System.Exit

main ::
     forall state.
     (ToJSON state, FromJSON state)
  => IO PortNumber
  -> (state -> Player -> Int)
  -> IO ()
main getPort heu = do
  hSetBuffering stdout LineBuffering
  printf "HEURISTIC: acquiring game tree port number\n"
  port <- getPort
  printf "HEURISTIC: acquired port %s\n" (show port)
  
  handle <- fix $ \again -> do
    printf "HEURISTIC: trying to connect to game tree at port %s\n" (show port)
    catch (connectTo "localhost" (PortNumber port))
      (\(_ :: IOException) -> do
           printf "HEURISTIC: failed to connect\n"
           threadDelay 1000000
           again)     
  hSetBuffering handle LineBuffering
  printf "HEURISTIC: connected\n"
  
  forever $ respond handle $ \case
     TH_CLOSE -> do
       printf "HEURISTIC: received CLOSE message from game tree\n"
       printf "HEURISTIC: closing now\n"
       exitSuccess
     TH_EVAL board player ->
       pure $ Just $ (TH_EVAL_RE (heu board player) :: TreeHeu state)
     _ -> pure Nothing          
            
