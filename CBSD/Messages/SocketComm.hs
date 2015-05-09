
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

-- Ugly. I'm not a network expert. 
listenOnUnusedPort :: IO (PortNumber, Socket)
listenOnUnusedPort = ($ 49152) $ fix $ \go port ->
  catch ((port,) <$> listenOn (PortNumber port))
        (\(_ :: IOException) -> go (port + 1))

getMessage :: (B.ByteString -> Maybe a) -> Handle -> IO a
getMessage decode handle = fix $ \go -> do
  line <- B.hGetLine handle
  maybe
    (printf "received invalid message: %s\n" (show line) >> go)
    pure
    (decode line)

putMessage :: (a -> LB.ByteString) -> Handle -> a -> IO ()
putMessage encode handle =
  CB.hPutStrLn handle . LB.toStrict . encode
  

-- | Send request, then get response
request :: (B.ByteString -> Maybe a) -> (a -> LB.ByteString) -> Handle -> a -> IO a
request decode encode handle a = do
  putMessage encode handle a
  getMessage decode handle

-- | Get request, make response and send it
--   If the request isn't valid, try reading again
respond ::
  (B.ByteString -> Maybe a) -> (a -> LB.ByteString) -> Handle -> (a -> Maybe (IO a)) -> IO ()
respond decode encode handle makeResponse =
  getMessage decode handle >>=
  makeResponse >>=
  putMessage encode handle

