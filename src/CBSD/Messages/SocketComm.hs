{-# LANGUAGE
  ScopedTypeVariables, TupleSections, LambdaCase,
  RankNTypes, ScopedTypeVariables #-}

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

-- Comm primitives (Note the StripEmptyContent wrapping!)
------------------------------------------------------------  

-- | Try reading until the handle becomes non-empty
getMessage :: forall a. FromJSON a => Handle -> IO a
getMessage handle = do
  line <- B.hGetLine handle
--  printf "getMessage: %s\n" (show line)
  maybe
    (error $ printf "received invalid message: %s\n" (show line))
    pure
    (decodeStrict line)

putMessage :: ToJSON a => Handle -> a -> IO ()
putMessage handle a = do
  let line = encode a
--  printf "putMessage: %s\n" (show line)
  CB.hPutStrLn handle $ LB.toStrict line
  
-- | Send request, then get response
request :: (ToJSON a, FromJSON b) => Handle -> a -> IO b
request handle a = do
  putMessage handle a
  getMessage handle

-- | Get request, make response and send it
--   If the request isn't valid, try reading again
respond ::
  (FromJSON a, ToJSON a, ToJSON b) => Handle -> (a -> IO (Maybe b)) -> IO ()
respond handle makeResponse = do
  req <- getMessage handle
  maybe
    (error $ printf "received invalid request: %s\n" (show $ encode req))
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
  printf "acquired port number: %s\n" (show centerOutPort)  

  -- Connect to center
  hCenterOut <- fix $ \again -> do
    printf "trying to connect to center at port %s\n" (show centerOutPort)    
    catch (connectTo "localhost" (PortNumber centerOutPort))
      (\(_ :: IOException) -> do
           printf "failed to connect\n"
           threadDelay 1000000
           again)
  hSetBuffering hCenterOut LineBuffering      
  printf "connected\n"

  -- Accept center
  (centerInPort, centerInSock) <- listenOnUnusedPort
  printf "listening for center on port %s\n" (show centerInPort)
  
  putMessage hCenterOut (Req_CONNECT $
    ReqConnect gameTypes name componentType (fromIntegral centerInPort))
  printf "CONNECT request sent to center\n"      

  resConnect <- getMessage hCenterOut
  case resConnect of
    Res_CONNECT (ResConnect res _) -> case res of
      OK      -> pure ()
      FAILURE -> error "received FAILURE code in CONNECT response from center\n"
    other -> error $ printf "expected CONNECT response, got %s\n" (show $ encode other)
  printf "CONNECT response OK\n"
                                 
  (hCenterIn, _, _) <- accept centerInSock
  hSetBuffering hCenterIn LineBuffering
  printf "accepted center\n"
  pure (centerInPort, hCenterIn, centerOutPort, hCenterOut)    
