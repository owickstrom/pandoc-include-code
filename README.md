# pandoc-include-code

_A Pandoc filter for including code from source files._

You get to:

* Keep your examples and documentation compiled and in sync
* Include small snippets from larger source files
* Use Markdown or LaTeX output in Pandoc

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

There is support for delimited _snippets_. Use line comment of whatever kind
you want, and enclose the snippet between `start snippet <name>` and
`end snippet <name>`.

    -- start snippet cool-thingy
    main =
      putStrLn "I explain some cool concept in Haskell code."
    -- end snippet cool-thingy

Then, in your code block, specify the snippet name:

    ```{include=docs/MyFile.hs snippet=cool-thingy}
    ```

### Ranges

If you want to include a specific range of lines, use `startLine` and `endLine`:

    ```{include=docs/MyFile.hs startLine=35 endLine=80}
    ```

## Install

```bash
stack install pandoc-include-code
```

## Build

Requirements:

* [Stack](https://docs.haskellstack.org/en/stable/README/)

To install from sources, run:

```bash
git clone git@github.com:owickstrom/pandoc-include-code.git
cd pandoc-include-code
stack setup
stack install
```

## Run

If you have installed from sources, and you have `~/.local/bin` on your
`PATH`, you can use the filter with Pandoc like so:

```bash
pandoc --filter pandoc-include-code input.md output.html
```

## License

[Mozilla Public License Version 2.0](LICENSE)
