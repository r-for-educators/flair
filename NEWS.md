# flair (development version)

## New features

- `txt_style()` now accepts a `class` argument specifying a vector of classes to
  be applied to the `<span>` of the decorated text (@gadenbuie, #18).

- `chunk_addin()` RStudio addin have been added. add `decorate()` chunk after
selected chunk when used.

## Bugs and fixes

* NULL document types are now treated as default html


# flair 0.0.2

* Released on CRAN (commit 6f6727855a)

* Updated `mask` function to replace text with an empty block of spaces, rather
than simply turning the text transparent.

* Renamed "highlight" to "decorate"

* Added a `NEWS.md` file to track changes to the package.

* Renamed demoR to flair.

* First release.



