{-# LANGUAGE OverloadedStrings #-}

module Main where

import Test.HUnit
import Test.Framework
import Test.Framework.Providers.HUnit

import HAVCR
import Network.HTTP
import Network.URI
import Data.Text
import Data.Yaml
import Data.Maybe (fromJust)

main :: IO ()
main =
     do ropts <- interpretArgsOrExit []
        defaultMainWithOpts tests ropts

tests = [ testCase "1" test_parseRequest ]

test_parseRequest =
  do req <- decodeFile "test/fixtures/sample02.yml" :: IO (Maybe (Request Text))
     assertEqual "URI" (fromJust $ parseURI "http://example.com/result?a=true&b=0") (rqURI (fromJust req))
     assertEqual "method" "GET" (show $ rqMethod (fromJust req))
     assertEqual "headers" expectedHeaders (Prelude.map (\h -> show (hdrName h) ++ ": " ++ hdrValue h) $ rqHeaders (fromJust req))
     assertEqual "body" "HELLO" (rqBody (fromJust req))
     where expectedHeaders = [ "Content-Type: application/x-www-form-urlencoded",
                               "Accept: */*",
                               "User-Agent: Ruby",
                               "User-Agent: Haskell" ]
