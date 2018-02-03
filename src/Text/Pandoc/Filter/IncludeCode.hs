{-# LANGUAGE CPP                        #-}
{-# LANGUAGE FlexibleContexts           #-}
{-# LANGUAGE GeneralizedNewtypeDeriving #-}
{-# LANGUAGE LambdaCase                 #-}
{-# LANGUAGE NamedFieldPuns             #-}
{-# LANGUAGE OverloadedStrings          #-}
{-# LANGUAGE RecordWildCards            #-}

module Text.Pandoc.Filter.IncludeCode
  ( InclusionMode(..)
  , InclusionError(..)
  , includeCode
  , includeCode'
  ) where
#if MIN_VERSION_base(4,8,0)
import           Control.Applicative      ((<|>))
#else
import           Control.Applicative
import           Data.Monoid
#endif
import           Control.Monad.Except
import           Control.Monad.Reader
import           Data.Char                (isSpace)
import           Data.HashMap.Strict      (HashMap)
import qualified Data.HashMap.Strict      as HM
import           Data.List                (isInfixOf)
import           Data.Maybe               (catMaybes)
import           Data.Text                (Text)
import qualified Data.Text                as Text
import qualified Data.Text.IO             as Text
import           Text.Pandoc.JSON
import           Text.Read                (readMaybe)

import           Text.Pandoc.Filter.Range (Range, mkRange, rangeEnd, rangeStart)

data InclusionMode
  = SnippetMode Text
  | RangeMode Range
  | EntireFileMode
  deriving (Show, Eq)

data InclusionSpec = InclusionSpec
  { include :: FilePath
  , mode    :: InclusionMode
  , dedent  :: Maybe Int
  }

data MissingRangePart
  = Start
  | End
  deriving (Show, Eq)

data InclusionError
  = InvalidRange Int
                 Int
  | IncompleteRange MissingRangePart
  | ConflictingModes [InclusionMode]
  deriving (Show, Eq)

newtype Inclusion a = Inclusion
  { runInclusion :: ReaderT InclusionSpec (ExceptT InclusionError IO) a
  } deriving ( Functor
             , Applicative
             , Monad
             , MonadIO
             , MonadReader InclusionSpec
             , MonadError InclusionError
             )

runInclusion' :: InclusionSpec -> Inclusion a -> IO (Either InclusionError a)
runInclusion' spec action = runExceptT (runReaderT (runInclusion action) spec)

parseInclusion ::
     HashMap String String -> Either InclusionError (Maybe InclusionSpec)
parseInclusion attrs =
  case HM.lookup "include" attrs of
    Just include -> do
      rangeMode <- parseRangeMode
      mode <-
        case catMaybes [rangeMode, snippetMode] of
          []  -> return EntireFileMode
          [m] -> return m
          ms  -> throwError (ConflictingModes ms)
      return (Just InclusionSpec {..})
    Nothing -> return Nothing
  where
    lookupInt name = HM.lookup name attrs >>= readMaybe
    snippetMode = SnippetMode . Text.pack <$> HM.lookup "snippet" attrs
    dedent = lookupInt "dedent"
    parseRangeMode =
      case (lookupInt "startLine", lookupInt "endLine") of
        (Just start, Just end) ->
          maybe
            (throwError (InvalidRange start end))
            (return . Just . RangeMode)
            (mkRange start end)
        (Nothing, Just _) -> throwError (IncompleteRange Start)
        (Just _, Nothing) -> throwError (IncompleteRange End)
        (Nothing, Nothing) -> return Nothing

type Lines = [Text]

readIncluded :: Inclusion Text
readIncluded = liftIO . Text.readFile =<< asks include

isSnippetTag :: Text -> Text -> Text -> Bool
isSnippetTag tag name line =
  mconcat [tag, " snippet ", name] `Text.isSuffixOf` Text.strip line

isSnippetStart, isSnippetEnd :: Text -> Text -> Bool
isSnippetStart = isSnippetTag "start"

isSnippetEnd = isSnippetTag "end"

includeByMode :: Lines -> Inclusion Lines
includeByMode ls =
  asks mode >>= \case
    SnippetMode name ->
      return $
      drop 1 $
      takeWhile (not . isSnippetEnd name) $
      dropWhile (not . isSnippetStart name) ls
    RangeMode range ->
      return (take (rangeEnd range - startIndex) (drop startIndex ls))
      where startIndex = pred (rangeStart range)
    EntireFileMode -> return ls

dedentLines :: Lines -> Inclusion Lines
dedentLines ls = do
  d <- asks dedent
  case d of
    Just n  -> return (map (dedentLine n) ls)
    Nothing -> return ls
  where
    dedentLine 0 line = line
    dedentLine n line =
      case Text.uncons line of
        Just (c, cs)
          | isSpace c -> dedentLine (pred n) cs
          | otherwise -> Text.cons c cs
        Nothing -> ""

filterAttributes :: [(String, String)] -> [(String, String)]
filterAttributes = filter nonFilterAttribute
  where
    nonFilterAttribute (key, _) = key `notElem` attributeNames
    attributeNames = ["include", "startLine", "endLine", "snippet", "dedent"]

printAndFail :: InclusionError -> IO a
printAndFail = fail . formatError
  where
    formatError =
      \case
        InvalidRange start end ->
          "Invalid range: " ++ show start ++ " to " ++ show end
        IncompleteRange Start -> "Incomplete range: \"startLine\" is missing"
        IncompleteRange End -> "Incomplete range: \"endLine\" is missing"
        ConflictingModes modes -> "Conflicting modes: " ++ show modes

splitLines :: Text -> Inclusion Lines
splitLines = return . Text.lines

joinLines :: Lines -> Inclusion Text
joinLines = return . Text.unlines

allSteps :: Inclusion Text
allSteps =
  readIncluded >>= splitLines >>= includeByMode >>= dedentLines >>= joinLines

includeCode' :: Block -> IO (Either InclusionError Block)
includeCode' cb@(CodeBlock (id', classes, attrs) _) =
  case parseInclusion (HM.fromList attrs) of
    Right (Just spec) ->
      runInclusion' spec allSteps >>= \case
        Left err -> return (Left err)
        Right contents ->
          return
            (Right
               (CodeBlock
                  (id', classes, filterAttributes attrs)
                  (Text.unpack contents)))
    Right Nothing -> return (Right cb)
    Left err -> return (Left err)
includeCode' x = return (Right x)

-- | A Pandoc filter that includes code snippets from external files.
includeCode :: Maybe Format -> Block -> IO Block
includeCode _ = includeCode' >=> either printAndFail return
