{-# LANGUAGE TupleSections #-}

module FilterTest where

import           Test.Tasty
import           Test.Tasty.HUnit

import qualified Text.Pandoc.Filter.IncludeCode as Filter
import           Text.Pandoc.JSON

(@@?=) a b = a >>= (@?= b)

type Formatted = Bool

includeCode ::
  String
  -> [(String, String)]
  -> IO Block
includeCode fixtureName attrs =
  Filter.includeCode
    (Just (Format "html5"))
    (CodeBlock
       ( ""
       , []
       , mconcat
           [ [("include", "test/fixtures/" ++ fixtureName)]
           , attrs
           ])
       "")

noRange = (Nothing, Nothing)

codeBlock :: String -> Block
codeBlock = CodeBlock ("", [], [])

tests =
  testGroup
    "Text.Pandoc.Filter.IncludeCode"
    [ testCase "includes snippet" $
      includeCode "foo-snippet.txt" [("snippet", "foo")] @@?=
      codeBlock "foo\n"
    , testCase "includes snippet within start and end line" $
      includeCode "foo-snippet.txt" [("snippet", "foo"), ("startLine", "1"), ("endLine", "3")] @@?=
      codeBlock "foo\n"
    , testCase "includes snippet by exact name, not prefix" $
      includeCode "foo-snippets.txt" [("snippet", "foo")] @@?=
      codeBlock "foo\n"
    , testCase "excludes snippet outside start and end line" $
      includeCode "foo-snippet.txt" [("snippet", "foo"), ("startLine", "2"), ("endLine", "3")] @@?=
      codeBlock ""
    , testCase "includes only lines between start and end line" $
      includeCode "some-text.txt" [("startLine", "2"), ("endLine", "3")] @@?=
      codeBlock "world\nthis\n"
    , testCase "dedents lines as much as specified" $
      includeCode "indents.txt" [("dedent", "1")] @@?=
      codeBlock "zero\n two\none\n  three\n       eight\n"
    , testCase "dedents lines as much as possible without removing non-whitespace" $
      includeCode "indents.txt" [("dedent", "8")] @@?=
      codeBlock "zero\ntwo\none\nthree\neight\n"
    ]
