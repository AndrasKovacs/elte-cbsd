{-# LANGUAGE ScopedTypeVariables, TupleSections #-}

module CBSD.Messages.SocketComm where

import Network
import System.IO
import Control.Exception
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

