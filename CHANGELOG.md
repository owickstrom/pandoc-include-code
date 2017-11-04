# Changelog

* **1.2.0.1**
  - Fix compile error
* **1.2.0.0**
  - Move `Range` to its own module (hiding the constructor)
  - Setup automated release builds using Travis. Executables for Linux
    and macOS now get published at
    https://github.com/owickstrom/pandoc-include-code/releases.
* **1.1.1.0**
  - Loosen constraint on `pandoc-types` to include `1.19`.
* **1.1.0.0**
  - Stricter checking of snippet name, fixes issue #3.
* **1.0.1.0**
  - Loosen version constraints on dependencies. Most importantly, allow all 4.x
    versions of `base`.
* **1.0.0.0**:
  - Report errors
  - Add support for dedenting
  - Remove undocumented "formatted" feature
  - Separate library and executable
* Before 1.0.0.0:
  - **2017-04-19:** Bump version
  - **2017-04-18:** Only render as RawBlock when preformatted
  - **2017-02-21:** Use new Pandoc 1.19
