{-# LANGUAGE ScopedTypeVariables #-}

module Network.HAVCR.ProxyTest where

import Network.HAVCR.Proxy
import Data.Yaml.HAVCR

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
import Data.Yaml (decodeFile)

tests = [ testCase "mocked response" test_SimpleMockedResponse
        ]

testClient :: forall ty p r. (HStream ty, IsString ty, Proxy p) =>
              () -> Client p (Request String) (Result (Response ty)) IO (Result (Response ty))
testClient () = runIdentityP $ do
    request req
    where req = Request (fromJust $ parseURI testURI) testMethod testHeaders testBody :: Request String
          testURI = "http://example.com/result?a=true&b=0"
          testMethod = POST
          testHeaders = [
            Header HdrContentType "application/x-www-form-urlencoded",
            Header HdrAccept "*/*",
            Header HdrUserAgent "Ruby"
            ]
          testBody = "HELLO"

testProxy :: Proxy p => Cassette String -> () -> Session p IO (Result (Response String))
testProxy cas = mockedServer cas >-> testClient

test_SimpleMockedResponse = do
  cas <- decodeFile "test/fixtures/sample01.yml"
  actualResponse <- runProxy $ testProxy $ fromJust cas :: IO (Result (Response String))
  assertEqual "response" (Right $ expectedResponse) actualResponse
    where expectedResponse = Response (2,0,0) "OK" headers "RESULT" :: Response String
          headers = [
            Header HdrContentType "text/plain; charset=iso-8859-2",
            Header HdrServer "Apache",
            Header HdrDate "Wed, 28 Dec 2011 15:08:44 GMT",
            Header HdrContentLength "12"
            ]
