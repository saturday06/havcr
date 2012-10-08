{-# LANGUAGE OverloadedStrings #-}

module HAVCR where

import Data.Yaml
import Data.Functor
import Control.Applicative
import Network.HTTP
import Network.URI
import Data.Text
import Data.Maybe (fromJust)
import Data.HashMap.Strict (toList)

instance FromJSON URI where
    parseJSON (String u) = pure $ fromJust $ parseURI $ unpack u

instance FromJSON RequestMethod where
    parseJSON (String m) = pure GET -- TODO
                                    -- Y U NO EXPORT "rqMethodMap"???
                                    -- pure $ Prelude.lookup (unpack m) rqMethodMap

instance FromJSON Header where
    parseJSON (Object h) = Header <$>
                           pure hName <*>
                           parseJSON v
                           where (k, v) = Prelude.head $ toList h
                                 hName = fromJust $ Prelude.lookup (unpack k) headerMap

instance FromJSON r => FromJSON (Request r) where
    parseJSON (Object v) = Request <$>
                           v .: "uri" <*>
                           v .: "method" <*>
                           v .: "headers" <*>
                           v .: "body"

instance ToJSON URI where
    toJSON uri = String $ pack $ uriToString id uri ""

instance ToJSON RequestMethod where
    toJSON meth = String $ pack $ show meth

instance ToJSON Header where
    toJSON (Header hName hValue) = object [(pack $ show hName) .= hValue]

instance ToJSON r => ToJSON (Request r) where
    toJSON (Request uri meth heads body) =
           object ["uri" .= uri, "method" .= meth, "headers" .= heads, "body" .= body]
