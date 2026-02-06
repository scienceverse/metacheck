# Get README from GitHub

Get README from GitHub

## Usage

``` r
github_readme(repo)
```

## Arguments

- repo:

  The URL of the repository (in the format "username/repo" or
  "https://github.com/username/repo")

## Value

a character string of the README contents

## Examples

``` r
# \donttest{
github_readme("scienceverse/metacheck")
#> [1] "# metacheck\n\n<!-- badges: start -->\n![Made in Europe](https://img.shields.io/badge/Made_in_Europe-003399?logo=european-union&logoColor=FFCC00)\n\n[![Lifecycle: experimental](https://img.shields.io/badge/lifecycle-experimental-orange.svg)](https://lifecycle.r-lib.org/articles/stages.html#experimental)\n\n[![Codecov test coverage](https://codecov.io/gh/scienceverse/metacheck/graph/badge.svg)](https://app.codecov.io/gh/scienceverse/metacheck)\n<!-- badges: end -->\n\nThe goal of metacheck is to automatically check research outputs for best practices. You can find out more at <https://scienceverse.github.io/metacheck/>.\n\n## Installation\n\nYou can install the development version of metacheck from [GitHub](https://github.com/) with:\n\n``` r\n# install.packages(\"devtools\")\ndevtools::install_github(\"scienceverse/metacheck\")\n```\n\n## API (optional)\nTo run metacheck as a REST API either using plumber or Docker, see [`inst/plumber/README.md`](inst/plumber/README.md) for instructions and documentation.\n"
# }
```
