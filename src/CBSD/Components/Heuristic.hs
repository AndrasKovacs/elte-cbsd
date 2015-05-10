{-# LANGUAGE LambdaCase, ScopedTypeVariables #-}

module CBSD.Components.Heuristic where

import CBSD.Messages.SocketComm
import CBSD.Messages.Types

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
     (ToJSON state, FromJSON state)
  => IO PortNumber
  -> (state -> Int)
  -> IO ()
main getPort heu = withSocketsDo $ do
  hSetBuffering stdout LineBuffering
  printf "acquiring game tree port number\n"
  port <- getPort
  printf "acquired port %s\n" (show port)
  
  handle <- fix $ \again -> do
    printf "trying to connect to game tree at port %s\n" (show port)
    catch (connectTo "localhost" (PortNumber port))
      (\(_ :: IOException) -> do
           printf "failed to connect\n"
           threadDelay 1000000
           again)     
  hSetBuffering handle LineBuffering
  
  forever $ respond handle $ \case
     TH_CLOSE -> do
       printf "received CLOSE message from game tree\n"
       printf "closing now\n"
       exitSuccess
     TH_EVAL (State _ _ board _) ->
       pure $ Just $ HT_EVAL_RE (heu board)
     _ -> pure Nothing          
            
