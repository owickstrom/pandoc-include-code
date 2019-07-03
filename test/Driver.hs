module Main where

import           Test.Tasty

import qualified FilterTest

main :: IO ()
main = defaultMain FilterTest.tests
