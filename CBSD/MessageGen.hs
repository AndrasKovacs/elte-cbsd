{-# LANGUAGE
  OverloadedStrings, LambdaCase, NoMonomorphismRestriction #-}

module CBSD.MessageGen where  

import Control.Lens
import Data.Aeson
import Data.Aeson.TH
import Data.Aeson.Lens

newtype Wrap a = Wrap {_unWrap :: a}
  deriving (Eq, Show)
           
instance ToJSON a => ToJSON (Wrap a) where
  toJSON = removeEmptyContent . toJSON . _unWrap
    where
    removeEmptyContent = _Object . at content %~ \case
      Just (Array arr) | null arr -> Nothing
      other -> other    
  
instance FromJSON a => FromJSON (Wrap a) where
  parseJSON = (Wrap <$>) . parseJSON . addEmptyContent
    where
    addEmptyContent = _Object . at content %~ \case
      Nothing -> Just (Array mempty)
      other   -> other  
  
stripFields = drop 4
stripTags   = drop 8

messageType = "messageType"
content     = "content"

genOptions :: Options
genOptions =
  defaultOptions {
    fieldLabelModifier     = drop 4,
    constructorTagModifier = drop 8,
    sumEncoding            = defaultTaggedObject {
      tagFieldName      = messageType,
      contentsFieldName = content
      }
    }

  
