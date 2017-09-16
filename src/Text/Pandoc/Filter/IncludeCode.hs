{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE NamedFieldPuns   #-}
{-# LANGUAGE RecordWildCards  #-}

module Text.Pandoc.Filter.IncludeCode where

import           Control.Monad.Reader
import           Data.Char            (isSpace)
import           Data.Function        ((&))
import           Data.HashMap.Strict  (HashMap)
import qualified Data.HashMap.Strict  as HM
import           Data.List            (isInfixOf)
import           Text.Pandoc.JSON
import           Text.Read            (readMaybe)

data InclusionSpec = InclusionSpec
  { include :: FilePath
  , snippet :: Maybe String
  , range   :: Maybe (Int, Int)
  , dedent  :: Maybe Int
  }

type Inclusion = ReaderT InclusionSpec IO

runInclusion :: InclusionSpec -> Inclusion a -> IO a
runInclusion = flip runReaderT

parseInclusion :: HashMap String String -> Maybe InclusionSpec
parseInclusion attrs = do
  include <- HM.lookup "include" attrs
  return InclusionSpec {..}
  where
    snippet = HM.lookup "snippet" attrs
    dedent = HM.lookup "dedent" attrs >>= readMaybe
    range = do
      start <- HM.lookup "startLine" attrs >>= readMaybe
      end <- HM.lookup "endLine" attrs >>= readMaybe
      if start <= end
        then return (start, end)
        else Nothing

readIncluded :: Inclusion String
readIncluded = do
  path <- asks include
  lift (readFile path)

filterLineRange :: String -> Inclusion String
filterLineRange contents = do
  r <- asks range
  case r of
    Just (start, end) ->
      return
        (unlines (take (end - startIndex) (drop startIndex (lines contents))))
      where startIndex = pred start
    Nothing -> return contents

onlySnippet :: String -> Inclusion String
onlySnippet contents = do
  s <- asks snippet
  case s of
    Just name ->
      lines contents
      & dropWhile (not . isSnippetStart)
      & takeWhile (not . isSnippetEnd)
      & drop 1
      & unlines
      & return
      where isSnippetTag tag line =
              (tag ++ " snippet " ++ name) `isInfixOf` line
            isSnippetStart = isSnippetTag "start"
            isSnippetEnd = isSnippetTag "end"
    Nothing -> return contents

dedentLines :: String -> Inclusion String
dedentLines contents = do
  d <- asks dedent
  case d of
    Just n  -> lines contents & map (dedentLine n) & unlines & return
    Nothing -> return contents
  where
    dedentLine 0 line = line
    dedentLine _ "" = ""
    dedentLine n (c:cs)
      | isSpace c = dedentLine (pred n) cs
      | otherwise = c : cs

filterAttributes :: [(String, String)] -> [(String, String)]
filterAttributes = filter nonFilterAttribute
  where
    nonFilterAttribute (key, _) = key `notElem` attributeNames
    attributeNames = ["include", "startLine", "endLine", "snippet", "dedent"]

includeCode :: Maybe Format -> Block -> IO Block
includeCode _ cb@(CodeBlock (id', classes, attrs) _) =
  case parseInclusion (HM.fromList attrs) of
    Just i -> do
      contents <-
        runInclusion i $
        readIncluded >>= filterLineRange >>= onlySnippet >>= dedentLines
      return (CodeBlock (id', classes, filterAttributes attrs) contents)
    Nothing -> return cb
includeCode _ x = return x
