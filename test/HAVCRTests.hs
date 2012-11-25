{-# LANGUAGE OverloadedStrings #-}

module Main where

import Test.HUnit
import Test.Framework
import Test.Framework.Providers.HUnit

import HAVCR
import Network.HTTP
import Network.URI
import Data.Text
import Data.Text.Encoding
import Data.Yaml
import Data.Maybe (fromJust)

main :: IO ()
main =
     do ropts <- interpretArgsOrExit []
        defaultMainWithOpts tests ropts

tests = [ testCase "parse Request" test_parseRequest
        , testCase "serialize Request" test_serializeRequest
        ]

test_parseRequest =
  do req <- decodeFile "test/fixtures/sample02.yml" :: IO (Maybe (Request Text))
     assertEqual "URI" (fromJust $ parseURI "http://example.com/result?a=true&b=0") (rqURI (fromJust req))
     assertEqual "method" "POST" (show $ rqMethod (fromJust req))
     assertEqual "headers" expectedHeaders (Prelude.map (\h -> show (hdrName h) ++ ": " ++ hdrValue h) $ rqHeaders (fromJust req))
     assertEqual "body" "HELLO" (rqBody (fromJust req))
     where expectedHeaders = [ "Content-Type: application/x-www-form-urlencoded",
                               "Accept: */*",
                               "User-Agent: Ruby",
                               "User-Agent: Haskell" ]

test_serializeRequest =
  let reqIn = Request (fromJust $ parseURI "http://example.com/result?a=true&b=0") POST headers ("HELLO" :: Text)
      headers = [   Header HdrContentType "application/x-www-form-urlencoded"
                  , Header HdrAccept "*/*"
                  , Header HdrUserAgent "Ruby"
                  , Header HdrUserAgent "Haskell"
                  ]
      req = fromJust $ decode $ encode reqIn :: Request Text
  in do assertEqual "URI" (fromJust $ parseURI "http://example.com/result?a=true&b=0") (rqURI req)
        assertEqual "method" "POST" (show $ rqMethod req)
        assertEqual "headers" expectedHeaders (Prelude.map (\h -> show (hdrName h) ++ ": " ++ hdrValue h) $ rqHeaders req)
        assertEqual "body" "HELLO" (rqBody req)
        where expectedHeaders = [ "Content-Type: application/x-www-form-urlencoded",
                                  "Accept: */*",
                                  "User-Agent: Ruby",
                                  "User-Agent: Haskell" ]
