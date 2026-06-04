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
# install.packages("remotes")
remotes::install_github("scienceverse/metacheck")
```



## Notes for Developers

You may not contribute any code unless you also contribute a test of this code.

Check tests/testthat/helper.R for custom test skip functions. All tests requiring a web connection, LLM, or long tests should be skipped or mocked. You can control this globally with the skip functions in this file (e.g., comment out the first `skip()` function in each custom function to run all tests of this type unless on cran/covr).
