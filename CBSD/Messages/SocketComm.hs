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

type Codec a = (B.ByteString -> Maybe a, a -> LB.ByteString)

-- | Try grabbing ports until there's a unused one.
listenOnUnusedPort :: IO (PortNumber, Socket)
listenOnUnusedPort = ($ 49152) $ fix $ \go port ->
  catch ((port,) <$> listenOn (PortNumber port))
        (\(_ :: IOException) -> go (port + 1))

-- | If the message isn't valid, try reading again
getMessage :: (B.ByteString -> Maybe a) -> Handle -> IO a
getMessage decode handle = fix $ \again -> do
  line <- B.hGetLine handle
  maybe
    (do printf "received invalid message: %s\n" (show line)
        again)
    pure
    (decode line)

putMessage :: (a -> LB.ByteString) -> Handle -> a -> IO ()
putMessage encode handle =
  CB.hPutStrLn handle . LB.toStrict . encode
  
-- | Send request, then get response
request :: Codec a -> Handle -> a -> IO a
request (decode, encode) handle a = do
  putMessage encode handle a
  getMessage decode handle

-- | Get request, make response and send it
--   If the request isn't valid, try reading again
respond :: Codec a -> Handle -> (a -> IO (Maybe a)) -> IO ()
respond (decode, encode) handle makeResponse = fix $ \again -> do
  req <- getMessage decode handle
  maybe
    (do printf "received invalid request: %s\n" (show (encode req))
        again)        
    (putMessage encode handle)
    =<< makeResponse req

