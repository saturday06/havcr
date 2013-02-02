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

import Data.Yaml
import Data.Yaml.HAVCR

mockedServer :: forall ty p r. (HStream ty, Eq ty, FromJSON ty, IsString ty, Proxy p) =>
                Cassette ty -> Request ty -> Server p (Request ty) (Result (Response ty)) IO r
mockedServer cas = runIdentityK $ foreverK $ \req ->
  do result <- lift $ simulatedHTTP req cas
     respond result

simulatedHTTP :: forall ty. (HStream ty, Eq ty, FromJSON ty, IsString ty) =>
                 Request ty -> Cassette ty -> IO (Result (Response ty))
simulatedHTTP req cas = do
  case findRecordedResponse req cas of
    Just r -> return $ Right r
    Nothing -> getRealResponse req

findRecordedResponse :: forall ty. (FromJSON ty, Eq ty) =>
                        Request ty -> Cassette ty -> Maybe (Response ty)
findRecordedResponse req cas = fmap epsResponse findResponse
  where findResponse = find (\es -> req == epsRequest es) (episodes cas)

getRealResponse :: HStream ty =>
                   Request ty -> IO (Result (Response ty))
getRealResponse req = simpleHTTP req -- TODO: and record it to the cassette
