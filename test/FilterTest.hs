{-# LANGUAGE TupleSections #-}

module FilterTest where

import           Test.Tasty
import           Test.Tasty.HUnit

import qualified Text.Pandoc.Filter.IncludeCode as Filter
import           Text.Pandoc.JSON

(@@?=) a b = a >>= (@?= b)

type Formatted = Bool

includeCode ::
     Format
  -> String
  -> Maybe String
  -> (Maybe Word, Maybe Word)
  -> Formatted
  -> IO Block
includeCode fmt fixtureName snippet (startLine, endLine) formatted =
  Filter.includeCode
    (Just fmt)
    (CodeBlock
       ( ""
       , []
       , mconcat
           [ [("include", "test/fixtures/" ++ fixtureName)]
           , maybe [] ((: []) . ("snippet", )) snippet
           , maybe [] ((: []) . ("startLine", ) . show) startLine
           , maybe [] ((: []) . ("endLine", ) . show) endLine
           , [("formatted", "true") | formatted]
           ])
       "")

includeCodeForHtml ::
     String -> Maybe String -> (Maybe Word, Maybe Word) -> Formatted -> IO Block
includeCodeForHtml = includeCode (Format "html5")

noRange = (Nothing, Nothing)

codeBlock :: String -> Block
codeBlock = CodeBlock ("", [], [])

tests =
  testGroup
    "Text.Pandoc.Filter.IncludeCode"
    [ testCase "includes snippet" $
      includeCodeForHtml "foo-snippet.txt" (Just "foo") noRange False @@?=
      codeBlock "foo\n"
    , testCase "includes snippet within start and end line" $
      includeCodeForHtml "foo-snippet.txt" (Just "foo") (Just 1, Just 3) False @@?=
      codeBlock "foo\n"
    , testCase "excludes snippet outside start and end line" $
      includeCodeForHtml "foo-snippet.txt" (Just "foo") (Just 2, Just 3) False @@?=
      codeBlock ""
    , testCase "includes only lines between start and end line" $
      includeCodeForHtml "some-text.txt" Nothing (Just 2, Just 3) False @@?=
      codeBlock "world\nthis\n"
    ]
