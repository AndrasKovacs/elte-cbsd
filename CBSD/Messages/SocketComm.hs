{-# LANGUAGE ScopedTypeVariables, TupleSections, RankNTypes, ScopedTypeVariables #-}

module CBSD.Messages.SocketComm where

import CBSD.Messages.Types

import Network
import System.IO
import Control.Exception
import Control.Concurrent
import Data.Function
import Data.Aeson
import Text.Printf

import qualified Data.ByteString.Lazy as LB
import qualified Data.ByteString as B
import qualified Data.ByteString.Char8 as CB
 
-- | Try grabbing ports until there's a unused one.
listenOnUnusedPort :: IO (PortNumber, Socket)
listenOnUnusedPort = ($ 2000) $ fix $ \go port ->
  catch ((port,) <$> listenOn (PortNumber port))
        (\(_ :: IOException) -> go (port + 1))

-- | If the message isn't valid, try reading again
getMessage :: FromJSON a => Handle -> IO a
getMessage handle = fix $ \again -> do
  line <- B.hGetLine handle
  maybe
    (do printf "received invalid message: %s\n" (show line)
        again)
    pure
    (decodeStrict line)

putMessage :: ToJSON a => Handle -> a -> IO ()
putMessage handle = CB.hPutStrLn handle . LB.toStrict . encode
  
-- | Send request, then get response
request :: (FromJSON a, ToJSON a) => Handle -> a -> IO a
request handle a = do
  putMessage handle a
  getMessage handle

-- | Get request, make response and send it
--   If the request isn't valid, try reading again
respond :: (FromJSON a, ToJSON a) => Handle -> (a -> IO (Maybe a)) -> IO ()
respond handle makeResponse = fix $ \again -> do
  req <- getMessage handle
  maybe
    (do printf "received invalid request: %s\n" (show (encode req))
        again)        
    (putMessage handle)
    =<< makeResponse req

registerAtCenter ::
     IO PortNumber     -- ^ Port of center
  -> String            -- ^ Component name
  -> [GameType]        
  -> ComponentType     
  -> IO (
    PortNumber, Handle, -- ^ Input port and handle
    PortNumber, Handle) -- ^ Output port and handle
registerAtCenter getCenterOutPort name gameTypes componentType = do
  hSetBuffering stdout LineBuffering

  -- Get center port number
  printf "getting port number of center\n"
  centerOutPort <- getCenterOutPort
  printf "acquired port number of center: %s\n" (show centerOutPort)  

  -- Connect to center
  hCenterOut <- fix $ \again -> do
    printf "trying to connect to center at port %s\n" (show centerOutPort)    
    catch (connectTo "localhost" (PortNumber centerOutPort))
      (\(_ :: IOException) -> do
           printf "failed to connect\n"
           threadDelay 1000000
           again)
  hSetBuffering hCenterOut LineBuffering      
  printf "connected to center\n"

  -- Accept center
  (centerInPort, centerInSock) <- listenOnUnusedPort
  printf "listening for center on port %s\n" (show centerInPort)
  
  putMessage hCenterOut (SEC (Req_CONNECT $
    ReqConnect gameTypes name componentType (fromIntegral centerInPort)))
                             
  printf "CONNECT message sent to center\n"  
  (hCenterIn, _, _) <- accept centerInSock
  hSetBuffering hCenterIn LineBuffering
  printf "accepted center\n"
  pure (centerInPort, hCenterIn, centerOutPort, hCenterOut)    

