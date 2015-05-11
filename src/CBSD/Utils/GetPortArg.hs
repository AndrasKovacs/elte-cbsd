{-# LANGUAGE LambdaCase #-}

module CBSD.Utils.GetPortArg where

import Network
import Data.Word
import Text.Read
import System.Environment

getPortArg :: IO PortNumber
getPortArg = getArgs >>= \case
  port:_ -> maybe
    (error "expected port number as first argument")
    (pure . fromIntegral)
    (readMaybe port :: Maybe Word16)
  _ -> error "expected port number as first argument"
