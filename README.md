# pandoc-include-code

_A Pandoc filter for including code from source files._

You get to:

* Keep your examples and documentation compiled and in sync
* Include small snippets from larger source files without needing to keep
  track of line numbers
* Dedent included snippets

## Usage

The filter recognizes code blocks with the `include` attribute present. It
swaps the content of the code block with contents from a file.

### Including Files

The simplest way to use this filter is to include an entire file:

    ```{include=docs/MyFile.hs}
    ```

You can still use other attributes, and classes, to control the code blocks:

    ```{.purescript include=docs/MyFile.purs}
    ```

### Snippets

There is support for delimited _snippets_. Use a line comment of
whatever kind you want, and enclose the snippet between `start snippet
<name>` and `end snippet <name>`.

    -- start snippet cool-thingy
    main =
      putStrLn "I explain some cool concept in Haskell code."
    -- end snippet cool-thingy

Or why not some C code:

    // start snippet wow
    int main() {
        printf("such performance");
    }
    // end snippet wow

**NOTE:** There can only be whitespace and a newline _after_ the
snippet name. This means that multi-line comments in C, Java, etc,
will not work. Only single-line comments will.

Then, in your code block, specify the snippet name:

    ```{include=docs/MyFile.hs snippet=cool-thingy}
    ```

### Ranges

If you want to include a specific range of lines, use `startLine` and `endLine`:

    ```{include=docs/MyFile.hs startLine=35 endLine=80}
    ```

### Dedent

Using the `dedent` attribute, you can have whitespaces removed on each line,
where possible (non-whitespace character will not be removed even if they occur
in the dedent area).

    ```{include=docs/MyFile.hs dedent=4}
    ```

### Conflicting Modes

"Snippet mode" and "range mode" cannot be used together.

### Line Numbers

If you include the `numberLines` class in your code block, and use `include`,
the `startFrom` attribute will be added with respect to the included code's
location in the source file.

    ```{include=docs/MyFile.hs startLine=35 endLine=80 .numberLines}
    ```

### Adding Direct Links

It is possible to add hyperlinks to the original source code file specified in the `include` attribute by adding the `.includeLink` class in your code block.

    ```{include=docs/MyFile.hs .includeLink}
    ```
## Adding Base url for all CodeBlock links
A base url will be appended to all relative paths specified in the
`include` attribute of each `CodeBlock` . It does not affect paths beginning with `file:` , `C:`, `\`, `/`,.... This can be done
two option:

#### Option 1: YAML

Specify a base key along with the base url as the attribute in
the YAML header:

``` 
    ---
    title:    All About Wonderland
    author:   Alice
    date:     November 2020
    base:     http://localhost:8000/
    ---
```

#### Option 2: Command Line

Add the base as a metavalue ` -M base=<base url>` or 
` --metavalue base=<base url>` in the command line when
calling pandoc.

```
pandoc --filter pandoc-include-code -M base=http://localhost:8000/  in.md -o out.html
```

NOTE: If the base url is specified in the metadata block, then
by specifying a different base in the command line, it will override
the original base.

Both of these options will add a hyperlink to the filepath defined
in the `include`  attribute linking to
`http://localhost:8000/source/sample.hs`:

#### Overriding a specified base url

Adding a base attribute in the metadata block or the command line
will affect all relative links in the `CodeBlocks`. To add an alternative base for a
specific link, add the base as an attribute `base=https....`  to
the`CodeBlock`:

````
```{.haskell .includeLink include=source/sample.hs snippet=animals base=<path>}
```
````

This adds a hyperlink to the filepath specified in the
`include` attribute linking to `../source/sample.hs` :


### More Usage Examples

* The blog post [Automating the Build of Your Technical Presentation](https://wickstrom.tech/programming/2017/09/24/automating-the-build-of-your-technical-presentation.html)
  shows practical examples of how to use this filter.

## Install

Executables for Linux and macOS are available in the [Releases
page](https://github.com/owickstrom/pandoc-include-code/releases).

### From Homebrew

You can use [Homebrew](https://brew.sh) to install this filter:

```bash
brew install pandoc-include-code
```

### From Hackage

If you'd rather install using `cabal` or `stack`, you can use the following
command:

```bash
cabal install pandoc-include-code
```

The package is [available at Hackage](https://hackage.haskell.org/package/pandoc-include-code).

## Build

Requirements:

* [Cabal](https://www.haskell.org/cabal/) or
  [Stack](https://docs.haskellstack.org/en/stable/README/), either works.

To install from sources, run:

```bash
git clone git@github.com:owickstrom/pandoc-include-code.git
cd pandoc-include-code
cabal configure
cabal install
```

## Run

If you have installed from sources, and you have `~/.local/bin` on your
`PATH`, you can use the filter with Pandoc like so:

```bash
pandoc --filter pandoc-include-code input.md output.html
```

## Usage with Hakyll

If you are using the [Hakyll](https://jaspervdj.be/hakyll/) static site generator, you can use the filter by importing it as a library and using the snippet below.

Add `pandoc`, `pandoc-types`, and `pandoc-include-code` to your project dependencies, then define a custom Hakyll compiler using a Pandoc transform:

```haskell
import Text.Pandoc (Format (..), Pandoc)
import Text.Pandoc.Walk (walkM)
import Text.Pandoc.Filter.IncludeCode (includeCode)

includeCodeTransform :: Pandoc -> IO Pandoc
includeCodeTransform = walkM (includeCode (Just (Format "html5")))

includeCodePandocCompiler :: Compiler (Item String)
includeCodePandocCompiler =
  pandocCompilerWithTransformM
    defaultHakyllReaderOptions
    defaultHakyllWriterOptions
    (unsafeCompiler . includeCodeTransform)
```

You can now use `includeCodePandocCompiler` instead of the default `pandocCompiler` in your Hakyll rules:

```haskell
match "*.md" $ do
  route $ setExtension "html"
  compile $ includeCodePandocCompiler
    >>= loadAndApplyTemplate "templates/default.html" defaultContext
    >>= relativizeUrls
```

## Changelog

[CHANGELOG.md](CHANGELOG.md)

## License

[Mozilla Public License Version 2.0](LICENSE)
