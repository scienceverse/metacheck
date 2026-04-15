# metacheck

<!-- badges: start -->
![Made in Europe](https://img.shields.io/badge/Made_in_Europe-003399?logo=european-union&logoColor=FFCC00)

[![Lifecycle: experimental](https://img.shields.io/badge/lifecycle-experimental-orange.svg)](https://lifecycle.r-lib.org/articles/stages.html#experimental)

[![Codecov test coverage](https://codecov.io/gh/scienceverse/metacheck/graph/badge.svg)](https://app.codecov.io/gh/scienceverse/metacheck)
<!-- badges: end -->

The goal of metacheck is to automatically check research outputs for best practices. You can find out more at <https://scienceverse.github.io/metacheck/>.

## Installation

You can install the development version of metacheck from [GitHub](https://github.com/) with:

``` r
# install.packages("devtools")
devtools::install_github("scienceverse/metacheck")
```

## API (optional)

To run metacheck as a REST API either using plumber or Docker, see [`inst/plumber/README.md`](inst/plumber/README.md) for instructions and documentation.

## Notes for Developers

### bibr format 

Metacheck uses bibr format for the paper objects. The schema for this can be found at <https://www.scienceverse.org/schema/paper.json>. If this format changes, or the file returned from bibr changes, the following functions will need checking:

- `paper()` (R/paper.R)
- `validate_paper()` (R/paper.R)
- `paper_write()` (R/paper.R)
- `platform_bibr()` (R/bibr.R)
- `convert_bibr()` (R/bibr.R)
- `read_bibr()` (R/bibr.R) Remove fixes when bibr output conforms to the schema
- `.grobid_to_bibr()` (R/grobid) Cascades to a bunch of tei_**() functions

### tests

You may not contribute any code unless you also contribute a test of this code.

Check tests/testthat/helper.R for custom test skip functions. By default, all tests requiring a web connection, LLM, or long tests are skipped. You can control this globally with the skip functions in this file (e.g., comment out the first `skip()` function in each custom function to run all tests of this type unless on cran/covr).
