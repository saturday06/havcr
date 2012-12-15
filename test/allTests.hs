module Main where

import Test.Framework.Runners.Console (interpretArgsOrExit, defaultMainWithOpts)

import qualified Data.Yaml.HAVCRTests as T1

tests = T1.tests

main :: IO ()
main = do ropts <- interpretArgsOrExit []
          defaultMainWithOpts tests ropts
