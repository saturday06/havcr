module Main where

import Test.Framework

import HAVCR.ProxyTest

main :: IO ()
main =
     do ropts <- interpretArgsOrExit []
        defaultMainWithOpts tests ropts
        where tests = HAVCR.ProxyTest.tests
