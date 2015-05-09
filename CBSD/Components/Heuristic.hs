{-# LANGUAGE LambdaCase #-}

module CBSD.Components.Heuristic where

import CBSD.Messages.SocketComm
import CBSD.Messages.Types

import Network
import Control.Exception
import System.IO
import System.Environment
import Text.Printf
import Text.Read
import Control.Monad
import System.Exit
import Data.Word

main ::
     Codec (TreeHeu state)
  -> IO PortNumber
  -> (state -> Int)
  -> IO ()
main codec getPort heu = withSocketsDo $ do
  portNum <- getPort
  bracket
    (connectTo "localhost" (PortNumber portNum))
    hClose
    $ \handle -> do
      hSetBuffering handle LineBuffering
      forever $ do
        respond codec handle $ \case
          ReqTH_CLOSE -> do
            printf "received CLOSE message from game tree\n"
            printf "closing now\n"
            exitSuccess
          ReqTH_EVAL (StateRec _ _ board _) ->
            pure $ Just $ ResTH_EVAL_RE (heu board)
          _ -> pure Nothing
            
