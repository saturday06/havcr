module Data.Yaml.HAVCR where

import Data.Char (toUpper)
import Data.Yaml
import Data.Functor ((<$>))
import Control.Applicative (pure, (<*>))
import Network.HTTP
import Network.URI (URI, parseURI, uriToString)
import qualified Data.Text as T (Text, pack, unpack)
import Data.Maybe (fromJust)
import Data.HashMap.Strict (toList)
import qualified Data.Vector as V (toList)
import Data.Time.Format (parseTime, formatTime)
import Data.Time.Clock (UTCTime)
import System.Locale (defaultTimeLocale)


-- Request (from/to JSON)

instance FromJSON r => FromJSON (Request r) where
    parseJSON (Object v) = Request <$>
                           v .: "uri" <*>
                           v .: "method" <*>
                           (toHeaders <$> v .: "headers") <*>
                           v .: "body"
    parseJSON _ = fail "could not parse"

instance FromJSON URI where
    parseJSON (String u) = pure $ fromJust $ parseURI $ T.unpack u
    parseJSON _ = fail "could not parse"

instance FromJSON RequestMethod where
    parseJSON (String m) = pure $ fromJust $ lookup (map toUpper $ T.unpack m) rqMethodMap
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

toHeaders :: Value -> [Header]
toHeaders (Object hs) = concat $ map hNameToHdrs $ toList hs
    where hNameToHdrs (k, Array vs) = map (hNameToHdr k) (V.toList vs)
          hNameToHdr k (String v) = mkHeader (fromJust $ lookup (T.unpack k) headerMap) (T.unpack v)
toHeaders _ = [] -- TODO: Can also be (Array hs), when it is written as "[...]"

--------------------------------------

instance ToJSON r => ToJSON (Request r) where
    toJSON (Request uri meth heads body) =
           object ["uri" .= uri, "method" .= meth, "headers" .= (fromHeaders heads), "body" .= body]

instance ToJSON URI where
    toJSON uri = String $ T.pack $ uriToString id uri ""

instance ToJSON RequestMethod where
    toJSON meth = String $ T.pack $ show meth

-- Simple Header serialization is incompatibe with original VCR files
--
-- instance ToJSON Header where -- TODO
--     toJSON (Header hName hValue) = fail "not implemented"

fromHeaders :: [Header] -> Value
fromHeaders hs = object $ map h2js $ foldr h2h [] hs
    where h2js h    = (T.pack $ show $ fst h) .= snd h
          h2h h1 h2 = case lookup (hdrName h1) h2 of
                      Just sh -> (hdrName h1, (hdrValue h1):sh):(filter (\n -> fst n /= hdrName h1) h2)
                      Nothing -> (hdrName h1, [hdrValue h1]):h2


-- Response (from/to JSON)

instance FromJSON r => FromJSON (Response r) where
    parseJSON (Object v) = Response <$>
                           toResponseCode <$> ((v .: "status") >>= (.: "code")) <*>
                           ((v .: "status") >>= (.: "message")) <*>
                           (toHeaders <$> v .: "headers") <*>
                           v .: "body"
    parseJSON _ = fail "could not parse"

toResponseCode :: Value -> ResponseCode
toResponseCode (Number c) = toTripleInt $ show c
                            where toTripleInt (x:y:z:[]) = (read [x], read [y], read [z])
                                  toTripleInt _ = error "could not parse"

instance ToJSON r => ToJSON (Response r) where
    toJSON (Response code reason heads body) =
           object [ "status" .= object ["code" .= fromResponseCode code, "message" .= reason]
                  , "headers" .= (fromHeaders heads)
                  , "body" .= body
                  ]

fromResponseCode :: ResponseCode -> Value
fromResponseCode (x, y, z) = Number $ fromIntegral $ x*100 + y*10 + z

deriving instance Eq Header
deriving instance Eq a => Eq (Request a)
deriving instance Eq a => Eq (Response a)

--- Episode

data Episode = Episode { epsRequest :: Request T.Text
                       , epsResponse :: Response T.Text
                       , epsTime :: UTCTime
                       }
               deriving (Show, Eq)

instance FromJSON Episode where
    parseJSON (Object v) = Episode <$>
                           v .: "request" <*>
                           v .: "response" <*>
                           (toTime <$> v .: "recorded_at")

toTime :: Value -> UTCTime
toTime (String t) = fromJust $ parseTime defaultTimeLocale "%a, %e %b %Y %T %Z" $ T.unpack t

instance ToJSON Episode where
    toJSON (Episode req res recordedAt) =
           object [ "request" .= req, "response" .= res, "recorded_at" .= (fromTime recordedAt) ]

fromTime :: UTCTime -> Value
fromTime t = String $ T.pack $ formatTime defaultTimeLocale "%a, %e %b %Y %T %Z" t

-- Cassette

data Cassette = Cassette { casEpisodes :: [Episode]
                         , casRecorder ::T.Text
                         }

instance FromJSON Cassette where
    parseJSON (Object v) = Cassette <$>
                           v .: "http_interactions" <*>
                           v .: "recorded_with"

instance ToJSON Cassette where
    toJSON (Cassette eps recordedWith) =
           object [ "http_interactions" .= eps, "recorded_with" .= recordedWith ]

-------------------- AUXILIARY

-- using a copy of rqMethodMap from Network.HTTP.Base
-- until it is properly exported from that module
rqMethodMap :: [(String, RequestMethod)]
rqMethodMap = [("HEAD",    HEAD),
	       ("PUT",     PUT),
	       ("GET",     GET),
	       ("POST",    POST),
               ("DELETE",  DELETE),
	       ("OPTIONS", OPTIONS),
	       ("TRACE",   TRACE),
	       ("CONNECT", CONNECT)]