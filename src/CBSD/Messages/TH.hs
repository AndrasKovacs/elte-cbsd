
module CBSD.Messages.TH where
       
import Data.Aeson.TH

stripPrefix :: String -> String
stripPrefix = drop 1 . dropWhile (/='_')

messageOptions :: Options
messageOptions = defaultOptions { fieldLabelModifier = stripPrefix }

messageTypeField :: String
messageTypeField = "messageType"

contentField :: String
contentField = "content"
  
taggingOptions :: Options
taggingOptions =
  defaultOptions {
    fieldLabelModifier     = stripPrefix,
    constructorTagModifier = stripPrefix,
    sumEncoding            = defaultTaggedObject {
      tagFieldName      = messageTypeField,
      contentsFieldName = contentField
      }
    }
