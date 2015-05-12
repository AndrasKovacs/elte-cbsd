{-# LANGUAGE LambdaCase #-}

module CBSD.Utils.GetPortArg where

import Network
import Data.Word
import Text.Read
import System.Environment

getPortArg :: String -> IO PortNumber
getPortArg errMsg = getArgs >>= \case
  port:_ -> maybe
    (error errMsg)
    (pure . fromIntegral)
    (readMaybe port :: Maybe Word16)
  _ -> error errMsg
