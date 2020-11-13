Pandoc-Include-Code
===================

## Direct Link
Add the class `.includeLink`  to the `CodeBlock`

````
```{.haskell .includeLink include=source/sample.hs snippet=animals }
```
````

This adds a hyperlink to the filepath specified in the
`include` attribute. This example links to `source/sample.hs` :

```
hello
world
this
is
text
```

[some-text.txt](test/fixtures/some-text.txt)

## Adding Base url for entire file
A base url will be appended to all relative paths specified in the
`include` attribute of each `CodeBlock` . It does not affect paths beginning with `file:` , `C:`, `\`, `/`,.... This can be done
two option:

### Option 1: YAML

Specify a base key along with the base url as the attribute in
the YAML header:

``` {.md}
    ---
    title:    All About Wonderland
    author:   Alice
    date:     November 2020
    base:     http://localhost:8000/
    ---
```

### Option 2: Command Line

Add the base as a metavalue ` -M base=<base url>` or 
` --metavalue base=<base url>` in the command line when
calling pandoc.

``` {.md fontsize="\\small"}
pandoc --filter pandoc-include-code -M base=http://localhost:8000/  in.md -o out.html
```

NOTE: If the base url is specified in the metadata block, then
by specifying a different base in the command line, it will override
the original base.

Both of these options will add a hyperlink to the filepath defined
in the `include`  attribute linking to
`http://localhost:8000/source/sample.hs`:

## Overriding a specified base url

Adding a base attribute in the metadata block or the command line
will affect all relative links in the `CodeBlocks`. To add an alternative base for a
specific link, add the base as an attribute `base=https....`  to
the`CodeBlock`:

```` {.md fontsize="\\small"}
```{.haskell .includeLink include=source/sample.hs snippet=animals base=<path>}
```
````

This adds a hyperlink to the filepath specified in the
`include` attribute linking to `../source/sample.hs` :


Changelog
---------

Added `containers`  to build-dependencies in
    `pandoc-include-code.cabal` 

``` haskell
build-depends:   base                 >= 4        && < 5
                , unordered-containers >= 0.2      && < 0.3
                , process
                , filepath
                , text                 >= 1.2      && < 2
                , mtl                  >= 2.2      && < 3
                , pandoc-types         >= 1.21     && <= 1.21
                , containers
```

Added the following imports to `IncludeCode.hs` 

``` haskell
import Data.Either      (partitionEithers)
import Data.Map         (delete)
```

 Added a field for the base url attribute in
    `InclusionSpec` for when the base url is specified in the
    `CodeBlock` 

``` haskell
data InclusionSpec = InclusionSpec
{ include :: FilePath
, mode    :: InclusionMode
, dedent  :: Maybe Int
, base :: Maybe [Char] 
}
```

Changed `InclusionState` from `newtype` to
    `data` in order to add the filepath specified in the
    include attribute of the `CodeBlock` 

``` haskell
data InclusionState = InclusionState
    { startLineNumber :: Maybe LineNumber,
    link :: Maybe FilePath
    }
```

Modified `ParseInclusion` to retrieve the base url
    specified as an attribute in the `CodeBlock`  (which may be
    `Nothing`) and add it to the base field in
    `InclusionSpec`

```haskell
base <- getBase
.
.
.
getBase = case (HM.lookup "base" attrs) of
            Nothing -> return Nothing 
            Just b -> return $ Just (Text.unpack b)
```

Added a function to set the filepath in `InclusionState`.
    If a base url is specified through the meta block or command-line,
    append it to the link specified in the `include`  attribute.
    Otherwise, set the link to the same path defined in `include` 

``` haskell
setLink :: Maybe [Char] -> [Char] -> Inclusion () 
setLink (Just b) n = modify (\s -> s {link = Just (b ++ n)})
setLink Nothing n = modify (\s -> s {link = Just n})
```

Added the `base`  attribute to the list of items to remove from
    the `CodeBlock` 

``` haskell
attributeNames = ["include", "startLine", "endLine", "snippet", "dedent", "base"] 
```

 Added function to remove the `.includeLink`  class from the
    `CodeBlock` 

``` haskell
modifyClasses :: [Text] -> [Text] 
modifyClasses classes = filter (\x -> x `notElem` ["includeLink"]) classes
```

 Added function to retrieve the `include`  and `base` 
attribute from the respective fields in `InclusionSpec` to
then be used to create the complete link

``` haskell
includeLink :: Inclusion ()
includeLink =  do
    links <- asks include
    bases <- asks base
    setLink bases links 
```

Added includeLink to steps to be performed

``` haskell
allSteps :: Inclusion Text
allSteps =
    includeLink >> readIncluded >>= splitLines >>= includeByMode >>= dedentLines >>= joinLines
```

Added function to extract the filename from the FilePath

``` haskell
getName :: Text -> Text
getName filepath = last $ splitOn (Text.pack "/") filepath
```

Added function to create the a `Block`  list containing either a
    `CodeBlock`  or a `CodeBlock`  and `Plain`  text with a
    `Link` 

``` haskell
modifiedCodeBlock :: Block -> (Text, InclusionState) -> Text -> Either InclusionError [Block]
modifiedCodeBlock cb@(CodeBlock (id', classes, attrs) _) (contents, state) base =
  case link state of
      Just path | "includeLink" `elem` classes  -> Right [(CodeBlock (id', modifyClasses classes, modifyAttributes state classes attrs) contents), Plain [Link nullAttr [Str (getName path)] ( addBase path, "")]]
      _ -> Right [CodeBlock (id', classes, modifyAttributes state classes attrs) contents]
    where
      addBase link
          | or $ map ($ link) (map Text.isPrefixOf ["C:", "/", "\\", "file:", "http:", "https:"]) = link
          | otherwise =  Text.append base link
```

Modified `includeCode' to add the `base`  url if the
    user specified it in the metadata block or command line

``` haskell
includeCode' :: Text -> Block -> IO (Either InclusionError [Block])
includeCode' base cb@(CodeBlock (id', classes, attrs) _) =  
    case parseInclusion (HM.fromList attrs) of
      Right (Just spec) ->
        runInclusion' spec allSteps >>= \case
          Left err -> return (Left err)
          Right out-> return (modifiedCodeBlock cb out base)
      Right Nothing -> return (Right [cb]) 
      Left err -> return (Left err)
includeCode' _ x = return (Right [x])
```

Changed `includeCode` to modify `Pandoc`  instead of
    `Block`  types: this allows the metadata to be used for
    extending links with a base url the user may specify by adding the
    value to the metadata block of the input file or by adding the
    metavalue flag in command line

``` haskell
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
```
