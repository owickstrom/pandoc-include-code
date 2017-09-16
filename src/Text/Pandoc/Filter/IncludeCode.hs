{-# LANGUAGE FlexibleContexts #-}

module Text.Pandoc.Filter.IncludeCode where

import qualified Data.Map              as Map

import           Data.Function         ((&))
import           Data.List             (isInfixOf)
import           Text.Pandoc.JSON
import           Text.Read

getRange :: Map.Map String String -> Maybe (Int, Int)
getRange attrs = do
  start <- Map.lookup "startLine" attrs >>= readMaybe
  end <- Map.lookup "endLine" attrs >>= readMaybe
  if start <= end
    then return (start, end)
    else Nothing

withinLines :: Maybe (Int, Int) -> String -> String
withinLines range =
  case range of
    Just (start, end) ->
      unlines . take (end - startIndex) . drop startIndex . lines
      where startIndex = pred start
    Nothing -> id

onlySnippet :: Map.Map String String -> String -> String
onlySnippet attrs =
  case Map.lookup "snippet" attrs of
    Just name ->
      unlines .
      takeWhile (not . isSnippetEnd) .
      drop 1 . dropWhile (not . isSnippetStart) . lines
      where isSnippetTag tag line =
              (tag ++ " snippet " ++ name) `isInfixOf` line
            isSnippetStart = isSnippetTag "start"
            isSnippetEnd = isSnippetTag "end"
    Nothing -> id

includeCode :: Maybe Format -> Block -> IO Block
includeCode (Just fmt) cb@(CodeBlock (id', classes, attrs) _) = do
  let attrs' = Map.fromList attrs
  case Map.lookup "include" attrs' of
    Just f -> do
      fileContents <- readFile f
      let filteredLines =
            fileContents & withinLines (getRange attrs') & onlySnippet attrs'
          paramNames =
            ["include", "startLine", "endLine", "snippet"]
          filteredAttrs = foldl (flip Map.delete) attrs' paramNames
          classes' = unwords classes
      return (CodeBlock (id', classes, Map.toList filteredAttrs) filteredLines)
    Nothing -> return cb
includeCode _ x = return x
