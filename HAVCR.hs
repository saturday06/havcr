{-# LANGUAGE OverloadedStrings #-}

module HAVCR where

import Data.Yaml
import Data.Functor
import Control.Applicative
import Network.HTTP
import Network.URI
import qualified Data.Text as T
import Data.Maybe (fromJust)
import Data.HashMap.Strict (toList)
import qualified Data.Vector as V

instance FromJSON URI where
    parseJSON (String u) = pure $ fromJust $ parseURI $ T.unpack u
    parseJSON _ = fail "could not parse"

instance FromJSON RequestMethod where
    parseJSON (String m) = pure $ Custom (T.unpack $ T.toUpper m) -- TODO
                                    -- Y U NO EXPORT "rqMethodMap"???
                                    -- pure $ lookup (T.unpack m) rqMethodMap
    parseJSON _ = fail "could not parse"

-- Simple Header serialization is incompatibe with original VCR files
--
-- instance FromJSON Header where
--     parseJSON (Object h) = Header <$>
--                            pure hName <*>
--                            parseJSON v
--                            where (k, v) = head $ toList h
--                                  hName = fromJust $ lookup (T.unpack k) headerMap
--     parseJSON _ = fail "could not parse"

instance FromJSON r => FromJSON (Request r) where
    parseJSON (Object v) = Request <$>
                           v .: "uri" <*>
                           v .: "method" <*>
                           (fmap toHeaders $ v .: "headers") <*>
                           v .: "body"
    parseJSON _ = fail "could not parse"

toHeaders :: Value -> [Header]
toHeaders (Object hs) = concat $ map hNameToHdrs $ toList hs
    where hNameToHdrs (k, Array vs) = map (hNameToHdr k) (V.toList vs)
          hNameToHdr k (String v) = mkHeader (fromJust $ lookup (T.unpack k) headerMap) (T.unpack v)

--------------------------------------

instance ToJSON URI where
    toJSON uri = String $ T.pack $ uriToString id uri ""

instance ToJSON RequestMethod where
    toJSON meth = String $ T.pack $ show meth -- TODO
                                    -- Y U NO EXPORT "rqMethodMap"???

-- Simple Header serialization is incompatibe with original VCR files
--
-- instance ToJSON Header where -- TODO
--     toJSON (Header hName hValue) = fail "not implemented"

instance ToJSON r => ToJSON (Request r) where
    toJSON (Request uri meth heads body) =
           object ["uri" .= uri, "method" .= meth, "headers" .= (fromHeaders heads), "body" .= body]

fromHeaders :: [Header] -> Value
fromHeaders hs = object $ map h2js $ foldr h2h [] hs
    where h2js h    = (T.pack $ show $ fst h) .= snd h
          h2h h1 h2 = case lookup (hdrName h1) h2 of
                      Just sh -> (hdrName h1, (hdrValue h1):sh):(filter (\n -> fst n /= hdrName h1) h2)
                      Nothing -> (hdrName h1, [hdrValue h1]):h2
