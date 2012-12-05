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
        , testCase "parse Response" test_parseResponse
        ]

test_parseRequest =
  do req <- decodeFile "test/fixtures/sample02.yml" :: IO (Maybe (Request Text))
     assertRequestComponents $ fromJust req

test_serializeRequest =
  let reqIn = Request (fromJust $ parseURI "http://example.com/result?a=true&b=0") POST headers ("HELLO" :: Text)
      headers = [ Header HdrContentType "application/x-www-form-urlencoded"
                , Header HdrAccept "*/*"
                , Header HdrUserAgent "Ruby"
                , Header HdrUserAgent "Haskell"
                ]
      req = fromJust $ decode $ encode reqIn :: Request Text
  in assertRequestComponents req

test_parseResponse =
  do rsp <- decodeFile "test/fixtures/sample03.yml" :: IO (Maybe (Response Text))
     assertResponseComponents $ fromJust rsp

assertRequestComponents req =
  do assertEqual "URI" (fromJust $ parseURI "http://example.com/result?a=true&b=0") (rqURI req)
     assertEqual "method" "POST" (show $ rqMethod req)
     assertEqual "headers" expectedHeaders (headerStrings $ rqHeaders req)
     assertEqual "body" "HELLO" (rqBody req)
     where expectedHeaders = [ "Content-Type: application/x-www-form-urlencoded"
                             , "Accept: */*"
                             , "User-Agent: Ruby"
                             , "User-Agent: Haskell"
                             ]

assertResponseComponents rsp =
  do assertEqual "code" (2,0,0) (rspCode rsp)
     assertEqual "reason" "OK" (rspReason rsp)
     assertEqual "headers" expectedHeaders (headerStrings $ rspHeaders rsp)
     assertEqual "body" "HELLO" (rspBody rsp)
     where expectedHeaders = [ "Content-Type: text/plain; charset=iso-8859-2"
                             , "Server: Apache"
                             , "Content-Length: 12"
                             ]

headerStrings :: [Header] -> [String]
headerStrings = Prelude.map (\h -> show (hdrName h) ++ ": " ++ hdrValue h)
