
{-# LANGUAGE OverloadedStrings #-}


module CBSD.CommTest where

import CBSD.Search
import CBSD.Ataxx

import Control.Applicative
import Network
import System.Environment
import System.IO 
import Control.Concurrent
import Control.Exception
import Control.Lens hiding ((.=))
import Text.Printf


portNum :: PortNumber
portNum = 6456



             
  

-- For Ataxx!!
-- data Msg
--   = EVAL {state :: 





-- client :: IO ()
-- client = bracket
--   (connectTo "localhost" (PortNumber portNum))
--   hClose
--   $ \handle -> do
--     hSetBuffering handle LineBuffering
--     printf "client: connected\n"
--     printf "client: sending\n"
--     hPutStrLn handle "fooooooobaaaaaaar"

-- server :: IO ()
-- server = do
--   sock <- listenOn $ PortNumber portNum
--   printf "server: listening on %s\n" (show portNum)
--   bracket (accept sock) (hClose . (^._1)) $ \(handle, host, num) -> do
    
--     hSetBuffering handle LineBuffering
--     printf "server: accepted connection from %s\n" (show (host, num))
--     line <- hGetLine handle
--     printf "server: recieved input: %s\n" line
    

-- main :: IO ()
-- main = withSocketsDo $ do
--   hSetBuffering stdout LineBuffering
  
--   forkIO server
--   forkIO client
  
--   getLine
--   return ()
