{-|
Touching up the TH-generated parsers so that they conforms to the CBSD formats
-}
{-# LANGUAGE LambdaCase #-}

module CBSD.Messages.MakeCodec (
  makeCodec
  ) where

import CBSD.Messages.SocketComm
import CBSD.Messages.TH

import Control.Lens
import Data.Aeson.Lens
import Data.Aeson
import qualified Data.Text as T

newtype Wrap a = Wrap {unWrap :: a}

removeEmptyContent :: Value -> Value
removeEmptyContent = _Object . at (T.pack contentField) %~ \case
  Just (Array arr) | null arr -> Nothing
  other -> other

instance (FromJSON a) => FromJSON (Wrap a) where
  parseJSON val = Wrap <$> parseJSON (removeEmptyContent val)

instance (ToJSON a) => ToJSON (Wrap a) where
  toJSON (Wrap a) = removeEmptyContent $ toJSON a

makeCodec :: (ToJSON a, FromJSON a) => Codec a
makeCodec = (fmap unWrap . decodeStrict, encode . Wrap)

