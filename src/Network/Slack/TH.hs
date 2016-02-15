module Network.Slack.TH (jsonOptionsWithPrefix
                        ,jsonOptionsEvent) where

import Text.CaseConversion
import Data.Aeson.TH



jsonOptionsWithPrefix :: String -> Options
jsonOptionsWithPrefix prefix =
  defaultOptions {fieldLabelModifier = convertCase Camel Snake . drop (length prefix)
                 ,constructorTagModifier = toCamelCase . (prefix:) . fromSnakeCase
                 }

jsonOptionsEvent :: Options
jsonOptionsEvent =
  defaultOptions {fieldLabelModifier = toSnakeCase . drop 1 . fromCamelCase
                 ,constructorTagModifier = toSnakeCase . init . fromCamelCase
                 ,sumEncoding = TaggedObject {tagFieldName = "type"
                                             ,contentsFieldName = "contents"}}
