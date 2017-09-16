module FilterTest where

import Test.Tasty
import Test.Tasty.HUnit

import Text.Pandoc.Filter.IncludeCode
import Text.Pandoc.JSON

tests =
  testGroup
    "Filter"
    [ testCase "includes snippet" $ do
        out <-
          includeCode
            (Just (Format "html5"))
            (CodeBlock
               ( ""
               , []
               , [ ("include", "test/fixtures/foo-snippet.txt")
                 , ("snippet", "foo")
                 ])
               "")
        out @?= CodeBlock ("", [], []) "foo\n"
    ]
