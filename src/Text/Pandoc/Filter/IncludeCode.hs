{-# LANGUAGE CPP                        #-}
{-# LANGUAGE FlexibleContexts           #-}
{-# LANGUAGE GeneralizedNewtypeDeriving #-}
{-# LANGUAGE LambdaCase                 #-}
{-# LANGUAGE NamedFieldPuns             #-}
{-# LANGUAGE RecordWildCards            #-}

module Text.Pandoc.Filter.IncludeCode
  ( includeCode
  ) where
#if MIN_VERSION_base(4,8,0)
#else
import           Control.Applicative
import           Data.Monoid
#endif
import           Control.Monad.Except
import           Control.Monad.Reader
import           Data.Char            (isSpace)
import           Data.HashMap.Strict  (HashMap)
import qualified Data.HashMap.Strict  as HM
import           Data.List            (isInfixOf)
import           Text.Pandoc.JSON
import           Text.Read            (readMaybe)

data Range = Range Int Int

mkRange :: Int -> Int -> Maybe Range
mkRange s e
  | s > 0 && e > 0 && s <= e = Just (Range s e)
  | otherwise = Nothing

data InclusionSpec = InclusionSpec
  { include :: FilePath
  , snippet :: Maybe String
  , range   :: Maybe Range
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
      range <- getRange
      return (Just InclusionSpec {..})
    Nothing -> return Nothing
  where
    lookupInt name = HM.lookup name attrs >>= readMaybe
    snippet = HM.lookup "snippet" attrs
    dedent = lookupInt "dedent"
    getRange =
      case (lookupInt "startLine", lookupInt "endLine") of
        (Just start, Just end) ->
          maybe
            (throwError (InvalidRange start end))
            (return . Just)
            (mkRange start end)
        (Nothing, Just _) -> throwError (IncompleteRange Start)
        (Just _, Nothing) -> throwError (IncompleteRange End)
        (Nothing, Nothing) -> return Nothing

type Lines = [String]

readIncluded :: Inclusion String
readIncluded = liftIO . readFile =<< asks include

filterLineRange :: Lines -> Inclusion Lines
filterLineRange ls =
  asks range >>= \case
    Just (Range start end) ->
      return (take (end - startIndex) (drop startIndex ls))
      where startIndex = pred start
    Nothing -> return ls

onlySnippet :: Lines -> Inclusion Lines
onlySnippet ls = do
  s <- asks snippet
  case s of
    Just name ->
      return $
      drop 1 $
      takeWhile (not . isSnippetEnd) $ dropWhile (not . isSnippetStart) ls
      where isSnippetTag tag line =
              (tag ++ " snippet " ++ name) `isInfixOf` line
            isSnippetStart = isSnippetTag "start"
            isSnippetEnd = isSnippetTag "end"
    Nothing -> return ls

dedentLines :: Lines -> Inclusion Lines
dedentLines ls = do
  d <- asks dedent
  case d of
    Just n  -> return (map (dedentLine n) ls)
    Nothing -> return ls
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

printAndFail :: InclusionError -> IO Block
printAndFail = fail . formatError
  where
    formatError =
      \case
        InvalidRange start end ->
          "Invalid range: " ++ show start ++ " to " ++ show end
        IncompleteRange Start -> "Incomplete range: \"startLine\" is missing"
        IncompleteRange End -> "Incomplete range: \"endLine\" is missing"

splitLines :: String -> Inclusion Lines
splitLines = return . lines

joinLines :: Lines -> Inclusion String
joinLines = return . unlines

-- | A Pandoc filter that includes code snippets from external files.
includeCode :: Maybe Format -> Block -> IO Block
includeCode _ cb@(CodeBlock (id', classes, attrs) _) =
  case parseInclusion (HM.fromList attrs) of
    Right (Just spec) ->
      runInclusion'
        spec
        (readIncluded >>= splitLines >>= filterLineRange >>= onlySnippet >>=
         dedentLines >>=
         joinLines) >>= \case
        Left err -> printAndFail err
        Right contents ->
          return (CodeBlock (id', classes, filterAttributes attrs) contents)
    Right Nothing -> return cb
    Left err -> printAndFail err
includeCode _ x = return x
