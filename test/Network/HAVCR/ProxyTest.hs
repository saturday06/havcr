module Network.HAVCR.ProxyTest where

import Network.HAVCR.Proxy

import Test.HUnit
import Test.Framework (testGroup)
import Test.Framework.Providers.HUnit (testCase)

import Control.Proxy
import Control.Monad
import Network.HTTP
import Network.Stream
import Network.BufferType
import Data.Text
import Data.String (IsString)

tests = []

testClient :: Proxy p => () -> Client p (Request ty) (Result (Response ty)) IO ()
testClient = undefined