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
import           Control.Monad.State
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

import           Text.Pandoc.Filter.Range (LineNumber, Range, mkRange, rangeEnd,
                                           rangeStart)

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
  = InvalidRange LineNumber
                 LineNumber
  | IncompleteRange MissingRangePart
  | ConflictingModes [InclusionMode]
  deriving (Show, Eq)

newtype InclusionState = InclusionState
  { startLineNumber :: Maybe LineNumber
  }

newtype Inclusion a = Inclusion
  { runInclusion :: ReaderT InclusionSpec (StateT InclusionState (ExceptT InclusionError IO)) a
  } deriving ( Functor
             , Applicative
             , Monad
             , MonadIO
             , MonadReader InclusionSpec
             , MonadError InclusionError
             , MonadState InclusionState
             )

runInclusion' ::
     InclusionSpec
  -> Inclusion a
  -> IO (Either InclusionError (a, InclusionState))
runInclusion' spec action =
  runExceptT (runStateT (runReaderT (runInclusion action) spec) initialState)
  where
    initialState = InclusionState {startLineNumber = Nothing}

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

setStartLineNumber :: LineNumber -> Inclusion ()
setStartLineNumber n = modify (\s -> s {startLineNumber = Just n})

readIncluded :: Inclusion Text
readIncluded = liftIO . Text.readFile =<< asks include

makeSnippetPrefix :: Text -> Text
makeSnippetPrefix tag = mconcat [tag, " snippet "]

isSnippetTag :: Text -> Text -> Text -> Bool
isSnippetTag tag name line =
  mconcat [prefix, name] `Text.isSuffixOf` Text.strip line
  where prefix = makeSnippetPrefix tag

isSnippetStart, isSnippetEnd :: Text -> Text -> Bool
isSnippetStart = isSnippetTag "start"

isSnippetEnd = isSnippetTag "end"

includeByMode :: Lines -> Inclusion Lines
includeByMode ls =
  asks mode >>= \case
    SnippetMode name -> do
      let (before, start) = break (isSnippetStart name) ls
          -- index +1 for line number, then +1 for snippet comment line, so +2:
          startLine = length before + 2
      setStartLineNumber startLine
      return (takeWhile (not . isSnippetEnd name) (drop 1 start))
    RangeMode range -> do
      setStartLineNumber (rangeStart range)
      return (take (rangeEnd range - startIndex) (drop startIndex ls))
      where startIndex = pred (rangeStart range)
    EntireFileMode -> return ls

removeSnippetComments :: Lines -> Inclusion Lines
removeSnippetComments ls =
  return $ filter (not . hasSnippetPrefix) ls
  where
    hasSnippetPrefix line = any (`Text.isInfixOf` line) snippetPrefixes
    snippetPrefixes = map makeSnippetPrefix ["start", "end"]

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

modifyAttributes ::
     InclusionState -> [String] -> [(String, String)] -> [(String, String)]
modifyAttributes InclusionState {startLineNumber} classes =
  (++) extraAttrs . filter nonFilterAttribute
  where
    nonFilterAttribute (key, _) = key `notElem` attributeNames
    attributeNames = ["include", "startLine", "endLine", "snippet", "dedent"]
    extraAttrs =
      case startLineNumber of
        Just n
          | "numberLines" `elem` classes -> [("startFrom", show n)]
        _ -> []

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
  readIncluded >>= splitLines >>= includeByMode >>= removeSnippetComments >>= dedentLines >>= joinLines

includeCode' :: Block -> IO (Either InclusionError Block)
includeCode' cb@(CodeBlock (id', classes, attrs) _) =
  case parseInclusion (HM.fromList attrs) of
    Right (Just spec) ->
      runInclusion' spec allSteps >>= \case
        Left err -> return (Left err)
        Right (contents, state) ->
          return
            (Right
               (CodeBlock
                  (id', classes, modifyAttributes state classes attrs)
                  (Text.unpack contents)))
    Right Nothing -> return (Right cb)
    Left err -> return (Left err)
includeCode' x = return (Right x)

-- | A Pandoc filter that includes code snippets from external files.
includeCode :: Maybe Format -> Block -> IO Block
includeCode _ = includeCode' >=> either printAndFail return
