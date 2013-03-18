{-# LANGUAGE ScopedTypeVariables #-}

-- HAVCR module providing a "simulated server" for testing
module Network.HAVCR.Proxy (
  mockedServer
) where

import Control.Proxy
import Network.HTTP
import Network.Stream
import Data.String (IsString)
import Data.List (find)
import Data.Maybe (fromJust)
import Data.Time.Clock (UTCTime, getCurrentTime)

import Data.Yaml
import Data.Yaml.HAVCR

mockedServer :: forall ty p r. (HStream ty, Eq ty, FromJSON ty, ToJSON ty, IsString ty, Proxy p) =>
                FilePath -> Request ty -> Server p (Request ty) (Result (Response ty)) IO r
mockedServer fname = runIdentityK $ foreverK $ \req ->
  do result <- lift $ simulatedHTTP req fname
     respond result

simulatedHTTP :: forall ty. (HStream ty, Eq ty, FromJSON ty, ToJSON ty, IsString ty) =>
                 Request ty -> FilePath -> IO (Result (Response ty))
simulatedHTTP req fname = do
  cas <- decodeFile fname
  eps <- return $ episodes $ fromJust cas
  case findRecordedResponse req eps of
    Just r -> return $ Right r
    Nothing -> getRealResponse req eps fname

findRecordedResponse :: Eq ty =>
                        Request ty -> [Episode ty] -> Maybe (Response ty)
findRecordedResponse req eps = fmap epsResponse findResponse
  where findResponse = find (\es -> req == epsRequest es) eps

getRealResponse :: (HStream ty, ToJSON ty) =>
                   Request ty -> [Episode ty] -> FilePath -> IO (Result (Response ty))
getRealResponse req eps fname = do
  res <- simpleHTTP req
  case res of
    Left r -> fail $ "Connection error: " ++ (show r)
    Right r -> do
      t <- getCurrentTime
      saveEpisodes (eps ++ [Episode req r t]) fname
  return res

saveEpisodes :: ToJSON ty => [Episode ty] -> FilePath -> IO ()
saveEpisodes eps fname = encodeFile fname cas
                         where cas = Cassette eps "havcr-0.0.1"
