{-# LANGUAGE LambdaCase #-}
module Main where

import System.Environment

import           Text.Pandoc.JSON
import           Text.Pandoc.Filter.IncludeCode

import Paths_pandoc_include_code
import qualified Data.Version          as Version

main :: IO ()
main =
  getArgs >>=
    \case
      (arg:_)
        | arg == "-V" -> showVersion
        | arg == "--version" -> showVersion
      _ -> toJSONFilter includeCode
  where
    showVersion = putStrLn (Version.showVersion version)
