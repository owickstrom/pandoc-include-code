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
import           Data.Text                (Text,splitOn)
import qualified Data.Text                as Text
import qualified Data.Text.IO             as Text
import           Text.Pandoc.JSON
import           Text.Read                (readMaybe)

import           Text.Pandoc.Filter.Range (LineNumber, Range, mkRange, rangeEnd,
                                           rangeStart)

 -- Added: 
import           Data.Either              (partitionEithers)
import           Data.Map                 (delete)
import           System.FilePath 

data InclusionMode
  = SnippetMode Text
  | RangeMode Range
  | EntireFileMode
  deriving (Show, Eq)

data InclusionSpec = InclusionSpec
  { include :: FilePath
  , mode    :: InclusionMode
  , dedent  :: Maybe Int
  , base    :: Maybe FilePath
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

-- newtype -> data (added constructor for link)
data InclusionState = InclusionState --add link
  { startLineNumber :: Maybe LineNumber,
    link :: Maybe FilePath
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
    initialState = InclusionState {startLineNumber = Nothing, link = Nothing}

parseInclusion ::
     HashMap Text Text -> Either InclusionError (Maybe InclusionSpec)
parseInclusion attrs =
  case HM.lookup "include" attrs of
    Just tinclude -> do
      let include = Text.unpack tinclude
      rangeMode <- parseRangeMode
      base <- getBase --get the base link specifed as an attribute in the CodeBlock
      mode <-
        case catMaybes [rangeMode, snippetMode] of
          []  -> return EntireFileMode
          [m] -> return m
          ms  -> throwError (ConflictingModes ms)
      return (Just InclusionSpec {..})
    Nothing -> return Nothing
  where
    lookupInt name = HM.lookup name attrs >>= readMaybe . Text.unpack
    snippetMode = SnippetMode <$> HM.lookup "snippet" attrs
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
    getBase = case (HM.lookup "base" attrs) of -- find value of base attribute if specified
      Nothing -> return Nothing 
      Just b -> return (Just (Text.unpack b))

type Lines = [Text]

setStartLineNumber :: LineNumber -> Inclusion ()
setStartLineNumber n = modify (\s -> s {startLineNumber = Just n})

setLink :: Maybe [Char] -> [Char] -> Inclusion () 
setLink (Just b) n = modify (\s -> s {link = Just (b ++ n)}) -- add the specified base to the link used in 'include'
setLink Nothing n = modify (\s -> s {link = Just n}) -- set the link to the same path used in 'include'

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
     InclusionState -> [Text] -> [(Text ,Text)] -> [(Text, Text)]
modifyAttributes InclusionState {startLineNumber} classes =
  (++) extraAttrs . filter nonFilterAttribute
  where
    nonFilterAttribute (key, _) = key `notElem` attributeNames
    attributeNames = ["include", "startLine", "endLine", "snippet", "dedent", "base"] --added "base" to list of attributes to remove
    extraAttrs =
      case startLineNumber of
        Just n
          | "numberLines" `elem` classes -> [("startFrom", Text.pack (show n))]
        _ -> []

modifyClasses :: [Text] -> [Text] -- also need to remove the ".includelink" class from the CodeBlock
modifyClasses classes = filter (\x -> x `notElem` ["includeLink"]) classes

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

includeLink :: Inclusion ()
includeLink =  do
    links <- asks include 
    bases <- asks base 
    setLink bases links 

allSteps :: Inclusion Text
allSteps =
    includeLink >> readIncluded >>= splitLines >>= includeByMode >>= dedentLines >>= joinLines

modifiedCodeBlock :: Block -> (Text, InclusionState) -> FilePath -> Either InclusionError [Block]
modifiedCodeBlock cb@(CodeBlock (id', classes, attrs) _) (contents, state) base =
  case link state of
      Just path | "includeLink" `elem` classes  -> Right [CodeBlock (id', modifyClasses classes, modifyAttributes state classes attrs) contents, Plain [Link ("",["includeCodeLink"],[]) [Str $ Text.pack (takeFileName  path)] ( addBase (Text.pack path), "")]]
      _ -> Right [CodeBlock (id', classes, modifyAttributes state classes attrs) contents]
    where
      addBase link
          | or $ map ($ link) (map Text.isPrefixOf ["C:", "/", "\\", "file:", "http:", "https:"]) = link
          | otherwise =  Text.append (Text.pack base) link

includeCode' :: Text -> Block -> IO (Either InclusionError [Block])
includeCode' base cb@(CodeBlock (id', classes, attrs) _) =  
    case parseInclusion (HM.fromList attrs) of
      Right (Just spec) ->
        runInclusion' spec allSteps >>= \case
          Left err -> return (Left err)
          Right out-> return (modifiedCodeBlock cb out (Text.unpack base))
      Right Nothing -> return (Right [cb]) 
      Left err -> return (Left err)
includeCode' _ x = return (Right [x])

-- | A Pandoc filter that includes code snippets from external files.
includeCode :: Maybe Format -> Pandoc -> IO Pandoc
includeCode _ (Pandoc m@(Meta list) bs) = do
  b <- sequence $ map (includeCode' $ getBaseURL (lookupMeta "base" m) ) bs
  return (Pandoc modMeta (modBlocks b))
  where modMeta = Meta (Data.Map.delete "base" list)
        modBlocks b = concat $ snd $ partitionEithers b
        getBaseURL base = case base of 
                    Just (MetaInlines [Str baseURL]) -> baseURL
                    Just (MetaString baseURL) -> baseURL
                    Nothing-> ""
