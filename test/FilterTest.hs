{-# LANGUAGE LambdaCase #-}
{-# LANGUAGE TupleSections #-}

module FilterTest where

import Test.Tasty
import Test.Tasty.HUnit

import qualified Text.Pandoc.Filter.IncludeCode as Filter
import Text.Pandoc.JSON

fails a =
  a >>= \case
    Left _ -> return ()
    Right x -> fail ""

a `failsWith` b = a >>= (@?= Left b)

a `succeedsWith` b = a >>= (@?= Right b)

type Formatted = Bool

includeCode ::
     String
  -> [String]
  -> [(String, String)]
  -> IO (Either Filter.InclusionError Block)
includeCode fixtureName classes attrs =
  Filter.includeCode'
    (CodeBlock
       ( ""
       , classes
       , mconcat [[("include", "test/fixtures/" ++ fixtureName)], attrs])
       "")

noRange = (Nothing, Nothing)

codeBlock :: String -> Block
codeBlock = CodeBlock ("", [], [])

tests =
  testGroup
    "Text.Pandoc.Filter.IncludeCode"
    [ testCase "includes snippet" $
      includeCode "foo-snippet.txt" [] [("snippet", "foo")] `succeedsWith`
      codeBlock "foo\n"
    , testCase "includes snippet by exact name, not prefix" $
      includeCode "foo-snippets.txt" [] [("snippet", "foo")] `succeedsWith`
      codeBlock "foo\n"
    , testCase "disallows conflicting modes" $
      fails $
      includeCode
        "foo-snippet.txt"
        []
        [("snippet", "foo"), ("startLine", "2"), ("endLine", "3")]
    , testCase "includes only lines between start and end line" $
      includeCode "some-text.txt" [] [("startLine", "2"), ("endLine", "3")] `succeedsWith`
      codeBlock "world\nthis\n"
    , testCase "dedents lines as much as specified" $
      includeCode "indents.txt" [] [("dedent", "1")] `succeedsWith`
      codeBlock "zero\n two\none\n  three\n       eight\n"
    , testCase
        "dedents lines as much as possible without removing non-whitespace" $
      includeCode "indents.txt" [] [("dedent", "8")] `succeedsWith`
      codeBlock "zero\ntwo\none\nthree\neight\n"
    , testCase "sets startFrom attribute in range mode if numberLines is present" $
      includeCode
        "some-text.txt"
        ["numberLines"]
        [("startLine", "2"), ("endLine", "3")] `succeedsWith`
      CodeBlock ("", ["numberLines"], [("startFrom", "2")]) "world\nthis\n"
    , testCase "sets startFrom attribute in snippet mode if numberLines is present" $
      includeCode
        "foo-snippet.txt"
        ["numberLines"]
        [("snippet", "foo")] `succeedsWith`
      CodeBlock ("", ["numberLines"], [("startFrom", "2")]) "foo\n"
    , testCase "doesn't include nested snippet comments" $
      includeCode "nested-snippets.txt" [] [("snippet", "global")] `succeedsWith`
      codeBlock "global text\nnested text\n"
    ]
