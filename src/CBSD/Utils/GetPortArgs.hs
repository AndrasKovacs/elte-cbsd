{-# LANGUAGE LambdaCase #-}

module CBSD.Utils.GetPortArgs where

import Network
import Data.Word
import Text.Read
import System.Environment
import Data.Maybe

getOnePort :: String -> IO PortNumber
getOnePort errMsg = getArgs >>= \case
  port:_ -> maybe
    (error errMsg)
    (pure . fromIntegral)
    (readMaybe port :: Maybe Word16)

getTwoPorts :: String -> IO (PortNumber, PortNumber)
getTwoPorts errMsg = getArgs >>= \case
  cport:hport:_ -> maybe (error errMsg) pure $ do
    cport <- (readMaybe cport :: Maybe Word16)
    hport <- (readMaybe hport :: Maybe Word16)
    pure (fromIntegral cport, fromIntegral hport)
  _ -> error errMsg
