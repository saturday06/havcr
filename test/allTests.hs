module Main where

import Test.Framework.Runners.Console (interpretArgsOrExit, defaultMainWithOpts)

import qualified Data.Yaml.HAVCRTest as T1
import qualified Network.HAVCR.ProxyTest as T2

tests = T1.tests ++ T2.tests

main :: IO ()
main = do ropts <- interpretArgsOrExit []
          defaultMainWithOpts tests ropts
