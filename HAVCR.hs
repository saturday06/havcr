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
import qualified Data.Vector as V

instance FromJSON URI where
    parseJSON (String u) = pure $ fromJust $ parseURI $ unpack u
    parseJSON _ = fail "could not parse"

instance FromJSON RequestMethod where
    parseJSON (String m) = pure $ Custom (unpack $ toUpper m) -- TODO
                                    -- Y U NO EXPORT "rqMethodMap"???
                                    -- pure $ Prelude.lookup (unpack m) rqMethodMap
    parseJSON _ = fail "could not parse"

-- instance FromJSON Header where
--     parseJSON (Object h) = Header <$>
--                            pure hName <*>
--                            parseJSON v
--                            where (k, v) = Prelude.head $ toList h
--                                  hName = fromJust $ Prelude.lookup (unpack k) headerMap
--     parseJSON _ = fail "could not parse"

instance FromJSON r => FromJSON (Request r) where
    parseJSON (Object v) = Request <$>
                           v .: "uri" <*>
                           v .: "method" <*>
                           -- remap {a: [1, 2], b: [3]} to [(a, 1), (a, 2), (b, 3)]
                           (fmap mkHeaders $ v .: "headers") <*>
                           v .: "body"
    parseJSON _ = fail "could not parse"

mkHeaders :: Value -> [Header]
mkHeaders (Object hs) = Prelude.concat $ Prelude.map hNameToHdrs $ toList hs
    where hNameToHdrs (k, Array vs) = Prelude.map (hNameToHdr k) (V.toList vs)
          hNameToHdr k (String v) = mkHeader (fromJust $ Prelude.lookup (unpack k) headerMap) (unpack v)

--------------------------------------

instance ToJSON URI where
    toJSON uri = String $ pack $ uriToString id uri ""

instance ToJSON RequestMethod where
    toJSON meth = String $ pack $ show meth -- TODO
                                    -- Y U NO EXPORT "rqMethodMap"???

instance ToJSON Header where -- TODO
    toJSON (Header hName hValue) = object []
    -- toJSON (Header hName hValue) = object [(pack $ show hName) .= hValue]

instance ToJSON r => ToJSON (Request r) where
    toJSON (Request uri meth heads body) =
           object ["uri" .= uri, "method" .= meth, "headers" .= heads, "body" .= body]
