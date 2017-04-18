{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE QuasiQuotes      #-}

module Main where

import qualified Data.Map              as Map

import           Data.Function         ((&))
import           Data.List             (isInfixOf)
import           Text.Pandoc.JSON
import           Text.Read
import           Text.Regex.PCRE.Heavy

type IsEscaped = Bool

encloseInListingEscape :: IsEscaped -> String -> String
encloseInListingEscape True s  = s
encloseInListingEscape False s = "@" ++ s ++ "@"

escapeForLatex :: IsEscaped -> String -> String
escapeForLatex isEscaped = concatMap escape
  where
    escape '$' =
      if isEscaped
        then "\\$"
        else "$"
    escape c = [c]

replaceDashWithLatex :: IsEscaped -> String -> String
replaceDashWithLatex isEscaped = gsub ([re|(\--)|]) toLatex
  where
    toLatex ("--":_) = encloseInListingEscape isEscaped "-{}-"
    toLatex _        = ""

replaceTagWithLatex :: IsEscaped -> String -> String
replaceTagWithLatex isEscaped s = foldl replaceWith s replacements
  where
    replacements =
      [ ([re|<strong>(.*?)</strong>|], "\\texttt{\\textbf{", "}}")
      , ([re|<em>(.*?)</em>|], "\\texttt{\\textit{", "}}")
      , ([re|<sub>(.*?)</sub>|], "\\textsubscript{", "}")
      ]
    replaceWith s' (r, pre, post) = gsub r (toLatex pre post) s'
    toLatex pre post (contents:_) =
      let replacedContents = replaceWithLatex True contents
          command = pre ++ replacedContents ++ post
      in encloseInListingEscape isEscaped command
    toLatex _ _ [] = ""

replaceWithLatex :: Bool -> String -> String
replaceWithLatex isEscaped =
  replaceDashWithLatex isEscaped .
  replaceTagWithLatex isEscaped . escapeForLatex isEscaped

postProcess :: Format -> String -> String
postProcess fmt contents
  | fmt == Format "latex" =
    unlines $ map (replaceWithLatex False) $ lines contents
  | otherwise = contents

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
      let isFormatted = "formatted" `Map.member` attrs'
      fileContents <-
        if isFormatted
          then postProcess fmt <$> readFile f
          else readFile f
      let filteredLines =
            fileContents & withinLines (getRange attrs') & onlySnippet attrs'
          paramNames =
            ["include", "formatted", "startLine", "endLine", "snippet"]
          filteredAttrs = foldl (flip Map.delete) attrs' paramNames
          classes' = unwords classes
      case fmt of
        Format "html5" | isFormatted ->
          return
            (RawBlock
               (Format "html")
               ("<pre class=" ++
                classes' ++ "><code>" ++ filteredLines ++ "</code></pre>"))
        _ ->
          return
            (CodeBlock (id', classes, Map.toList filteredAttrs) filteredLines)
    Nothing -> return cb
includeCode _ x = return x

main :: IO ()
main = toJSONFilter includeCode
