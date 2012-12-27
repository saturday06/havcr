{-# LANGUAGE ScopedTypeVariables #-}

-- HAVCR module providing a "simulated server" for testing
module Network.HAVCR.Proxy (
  mockedServer
) where

import Control.Proxy
import Control.Monad
import Network.HTTP
import Network.Stream
import Network.BufferType
import Data.Text
import Data.String (IsString)

mockedServer :: forall ty p. (HStream ty, IsString ty, Proxy p) => Request ty -> Server p (Request ty) (Result (Response ty)) IO (Request ty)
mockedServer = runIdentityK $ foreverK $ \req ->
    -- do result <- lift $ simpleHTTP req
    do result <- lift $ simulatedHTTP req
       respond result

simulatedHTTP :: forall ty. (HStream ty, IsString ty) => Request ty -> IO (Network.Stream.Result (Response ty))
simulatedHTTP req = return $ Right $ Response (2,0,0) "OK" [] ("hello" :: ty)
