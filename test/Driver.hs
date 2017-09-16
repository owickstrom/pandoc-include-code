module Main where

import Test.Tasty
import Test.Tasty.HUnit

import qualified FilterTest

main :: IO ()
main = defaultMain FilterTest.tests
