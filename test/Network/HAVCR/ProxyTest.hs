{-# LANGUAGE ScopedTypeVariables #-}

module Network.HAVCR.ProxyTest where

import Network.HAVCR.Proxy

import Test.HUnit
import Test.Framework (testGroup)
import Test.Framework.Providers.HUnit (testCase)

import Control.Proxy
import Control.Monad
import Network.HTTP
import Network.URI
import Network.Stream
import Network.BufferType
import Data.Text
import Data.Maybe (fromJust)
import Data.String (IsString)

tests = [test_SimpleMockedResponse]

testClient :: forall ty p r. (HStream ty, IsString ty, Proxy p) => () -> Client p (Request ty) (Result (Response ty)) IO r
testClient () = runIdentityP $ forever $ do
    request req
    where req = Request (fromJust $ parseURI "http://www.google.com") GET [] "test"

test_SimpleMockedResponse =
    let expectedResponse = Response (2,0,0) "OK" [] "hello"
    in do assertEqual "response" expectedResponse actualResponse
          where actualResponse = runProxy proxy
                proxy :: (Proxy p) => () -> Session p IO r
                proxy = mockedServer >-> testClient
