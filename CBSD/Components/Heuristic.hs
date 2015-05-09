{-# LANGUAGE LambdaCase #-}

module CBSD.Components.Heuristic where

import CBSD.Messages.SocketComm

import Network
import Control.Exception
import System.Environment
import Text.Printf
import Text.Read
import Control.Monad
import System.Exit

import Data.Word
import qualified Data.ByteString as B
import qualified Data.ByteString.Char8 as CB
import qualified Data.ByteString.Lazy as LB


getPortNum :: IO PortNumber
getPortNum = getArgs >>= \case
  port:_ -> maybe
    (error "can't parse port number argument")
    (pure . fromIntegral)
    (readMaybe port :: Maybe Word16)
  _ -> error "missing port number argument"
  

makeMain ::
  forall state. Show state
  => (B.ByteString -> Maybe a)
  -> (a -> B.ByteString)
  -> IO PortNumber
  -> (state -> Int)
  -> IO ()
makeMain decode encode getPortNum heu = withSocketsDo $ do
  portNum <- getPortNum
  bracket
    (connectTo "localhost" (PortNumber portNum))
    hClose
    $ \handle -> do
      hSetBuffering handle LineBuffering
      forever $ do
        respond decode encode handle $ \case
          ReqTH_CLOSE -> do
            printf "received CLOSE message\n"
            printf "closing\n"
            exitSuccess
          ReqTH_EVAL
            
          
          
        


          

