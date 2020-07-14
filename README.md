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
