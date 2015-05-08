
module CBSD.Messages.TH (
    stripPrefix
  , messageOptions
  , taggingOptions
    ) where
       
import Data.Aeson.TH

stripPrefix :: String -> String
stripPrefix = drop 1 . dropWhile (/='_')

messageOptions :: Options
messageOptions = defaultOptions { fieldLabelModifier = stripPrefix }
  
taggingOptions :: Options
taggingOptions =
  defaultOptions {
    fieldLabelModifier     = stripPrefix,
    constructorTagModifier = stripPrefix,
    sumEncoding            = defaultTaggedObject {
      tagFieldName      = "messageType",
      contentsFieldName = "content"
      }
    }
