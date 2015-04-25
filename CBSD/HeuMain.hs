
{-# LANGUAGE LambdaCase, ScopedTypeVariables, RankNTypes #-}

module CBSD.HeuMain where

import Control.Exception
import Control.Monad
import Data.Aeson
import Data.Function
import Data.Word
import Network
import System.Environment
import System.Exit
import System.IO
import Text.Printf
import Text.Read

import qualified Data.ByteString as B
import qualified Data.ByteString.Char8 as CB
import qualified Data.ByteString.Lazy as LB

import CBSD.Messages


getPortNum :: IO PortNumber
getPortNum = getArgs >>= \case
  port:_ -> maybe
    (error "can't parse port number argument")
    (pure . fromIntegral)
    (readMaybe port :: Maybe Word16)
  _ -> error "missing port number argument"  

getMsg :: FromJSON state => Handle -> IO (GT_Heu state)
getMsg handle = fix $ \loop ->
  maybe
    (printf "Received invalid message\n" >> loop)
    (pure . _unWrap)
    =<< decodeStrict <$> B.hGetLine handle

putMsg :: ToJSON state => Handle -> GT_Heu state -> IO ()
putMsg = (.(LB.toStrict . encode . Wrap)) . CB.hPutStrLn

heuMain ::
     forall state.
     (FromJSON state, ToJSON state, Show state)
  => IO PortNumber
  -> (state -> Int)
  -> IO ()
heuMain getPortNum heu = withSocketsDo $ do  
  portNum <- getPortNum  
  bracket
    (connectTo "localhost" (PortNumber portNum))
    hClose
    $ \handle -> do
      hSetBuffering handle LineBuffering
      forever $ do        
        getMsg handle >>= \case          
          (GTH_REQ_CLOSE :: GT_Heu state) -> do
            printf "Received CLOSE\n"
            printf "Shutting down\n"
            exitSuccess            
          GTH_REQ_EVAL state@(GTH_State board player) -> do
            printf "Received EVAL with state %s\n" (show state)
            let score = heu board
            printf "Sending heuristic score %d\n" score
            putMsg handle (GTH_RES_EVAL_RE score :: GT_Heu state)
