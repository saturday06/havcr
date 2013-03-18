module Data.Yaml.HAVCRTest (tests) where

import Data.Yaml.HAVCR

import Test.HUnit (assertEqual)
import Test.Framework.Providers.HUnit (testCase)

import Network.HTTP
import Network.URI
import Data.Text
import Data.Yaml (decode, encode, decodeFile)
import Data.Maybe (fromJust)
import Data.Time.Format (parseTime)
import System.Locale (defaultTimeLocale)

tests = [ testCase "parse Request" test_parseRequest
        , testCase "serialize Request" test_serializeRequest
        , testCase "parse Response" test_parseResponse
        , testCase "serialize Response" test_serializeResponse
        , testCase "parse Cassette" test_parseCassette
        , testCase "serialize Cassette" test_serializeCassette
        , testCase "compare Responses" test_compareResponses
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

test_serializeResponse =
  let resIn = Response (2,0,0) "OK" headers ("HELLO" :: Text)
      headers = [ Header HdrContentType "text/plain; charset=iso-8859-2"
                , Header HdrServer "Apache"
                , Header HdrContentLength "12"
                ]
      res = fromJust $ decode $ encode resIn :: Response Text
  in assertResponseComponents res

test_parseCassette =
  do cas <- decodeFile "test/fixtures/sample01.yml" :: IO (Maybe (Cassette Text))
     assertCassetteComponents $ fromJust cas

test_serializeCassette =
  let casIn = Cassette episodes "VCR 2.0.0.rc1"
      episodes = [Episode req res (fromJust $ parseTime defaultTimeLocale "%a, %e %b %Y %T %Z" "Wed, 28 Dec 2011 15:08:44 GMT")]
      req = Request (fromJust $ parseURI "http://example.com/result?a=true&b=0") POST reqHeaders ("HELLO" :: Text)
      reqHeaders = [ Header HdrContentType "application/x-www-form-urlencoded"
                   , Header HdrAccept "*/*"
                   , Header HdrUserAgent "Ruby"
                   ]
      res = Response (2,0,0) "OK" resHeaders ("RESULT" :: Text)
      resHeaders = [ Header HdrDate "Wed, 28 Dec 2011 15:08:44 GMT"
                   , Header HdrServer "Apache"
                   , Header HdrContentLength "12"
                   , Header HdrContentType "text/plain; charset=iso-8859-2"
                   ]
      cas = fromJust $ decode $ encode casIn :: Cassette Text
  in assertCassetteComponents cas

test_compareResponses =
  let res1 = Response (2,0,0) "OK" resHeaders1 ("RESULT" :: Text)
      resHeaders1 = [ Header HdrDate "Wed, 28 Dec 2011 15:08:44 GMT"
                    , Header HdrServer "Apache"
                    , Header HdrContentLength "12"
                    , Header HdrContentType "text/plain; charset=iso-8859-2"
                    ]
      res2 = Response (2,0,0) "OK" resHeaders2 ("RESULT" :: Text)
      resHeaders2 = [ Header HdrDate "Wed, 28 Dec 2011 15:08:44 GMT"
                    , Header HdrContentLength "12"
                    , Header HdrServer "Apache"
                    , Header HdrContentType "text/plain; charset=iso-8859-2"
                    ]
  in assertEqual "responses" res1 res2
  
test_compareRequests =
  let req1 = Request (fromJust $ parseURI "http://example.com/result?a=true&b=0") POST reqHeaders1 ("HELLO" :: Text)
      reqHeaders1 = [ Header HdrContentType "application/x-www-form-urlencoded"
                    , Header HdrAccept "*/*"
                    , Header HdrUserAgent "Ruby"
                    ]
      req2 = Request (fromJust $ parseURI "http://example.com/result?a=true&b=0") POST reqHeaders2 ("HELLO" :: Text)
      reqHeaders2 = [ Header HdrContentType "application/x-www-form-urlencoded"
                    , Header HdrUserAgent "Ruby"
                    , Header HdrAccept "*/*"
                    ]
  in assertEqual "responses" req1 req2

--

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

assertCassetteComponents (Cassette eps recorder) =
  do assertEqual "episodes" expectedEpisodes eps
     assertEqual "recorder" "VCR 2.0.0.rc1" recorder
     where expectedEpisodes = [ Episode req res recordedAt ]
           req = Request (fromJust $ parseURI "http://example.com/result?a=true&b=0") POST reqHeaders ("HELLO" :: Text)
           reqHeaders = [ Header HdrContentType "application/x-www-form-urlencoded"
                        , Header HdrAccept "*/*"
                        , Header HdrUserAgent "Ruby"
                        ]
           res = Response (2,0,0) "OK" resHeaders ("RESULT" :: Text)
           resHeaders = [ Header HdrContentType "text/plain; charset=iso-8859-2"
                        , Header HdrServer "Apache"
                        , Header HdrDate "Wed, 28 Dec 2011 15:08:44 GMT"
                        , Header HdrContentLength "12"
                        ]
           recordedAt = fromJust $ parseTime defaultTimeLocale "%a, %e %b %Y %T %Z" "Wed, 28 Dec 2011 15:08:44 GMT"

headerStrings :: [Header] -> [String]
headerStrings = Prelude.map (\h -> show (hdrName h) ++ ": " ++ hdrValue h)
