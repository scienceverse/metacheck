# Get GitHub Repo Info

Get GitHub Repo Info

## Usage

``` r
github_info(repo, recursive = FALSE)
```

## Arguments

- repo:

  The URL of the repository (in the format "username/repo" or
  "https://github.com/username/repo")

- recursive:

  whether to search the files recursively

## Value

a list of information about the repo

## Examples

``` r
# \donttest{
github_info("scienceverse/metacheck")
#> $repo
#> [1] "scienceverse/metacheck"
#> 
#> $readme
#> [1] "# metacheck\n\n<!-- badges: start -->\n![Made in Europe](https://img.shields.io/badge/Made_in_Europe-003399?logo=european-union&logoColor=FFCC00)\n\n[![Lifecycle: experimental](https://img.shields.io/badge/lifecycle-experimental-orange.svg)](https://lifecycle.r-lib.org/articles/stages.html#experimental)\n\n[![Codecov test coverage](https://codecov.io/gh/scienceverse/metacheck/graph/badge.svg)](https://app.codecov.io/gh/scienceverse/metacheck)\n<!-- badges: end -->\n\nThe goal of metacheck is to automatically check research outputs for best practices. You can find out more at <https://scienceverse.github.io/metacheck/>.\n\n## Installation\n\nYou can install the development version of metacheck from [GitHub](https://github.com/) with:\n\n``` r\n# install.packages(\"devtools\")\ndevtools::install_github(\"scienceverse/metacheck\")\n```\n\n## API (optional)\nTo run metacheck as a REST API either using plumber or Docker, see [`inst/plumber/README.md`](inst/plumber/README.md) for instructions and documentation.\n"
#> 
#> $files
#>                      repo             clean_repo             name
#> 1  scienceverse/metacheck scienceverse/metacheck    .Rbuildignore
#> 2  scienceverse/metacheck scienceverse/metacheck          .github
#> 3  scienceverse/metacheck scienceverse/metacheck       .gitignore
#> 4  scienceverse/metacheck scienceverse/metacheck      DESCRIPTION
#> 5  scienceverse/metacheck scienceverse/metacheck       LICENSE.md
#> 6  scienceverse/metacheck scienceverse/metacheck        NAMESPACE
#> 7  scienceverse/metacheck scienceverse/metacheck          NEWS.md
#> 8  scienceverse/metacheck scienceverse/metacheck                R
#> 9  scienceverse/metacheck scienceverse/metacheck        README.md
#> 10 scienceverse/metacheck scienceverse/metacheck _metacheck.Rproj
#> 11 scienceverse/metacheck scienceverse/metacheck           _stuff
#> 12 scienceverse/metacheck scienceverse/metacheck      codecov.yml
#> 13 scienceverse/metacheck scienceverse/metacheck             data
#> 14 scienceverse/metacheck scienceverse/metacheck         data-raw
#> 15 scienceverse/metacheck scienceverse/metacheck             docs
#> 16 scienceverse/metacheck scienceverse/metacheck             inst
#> 17 scienceverse/metacheck scienceverse/metacheck              man
#> 18 scienceverse/metacheck scienceverse/metacheck          pkgdown
#> 19 scienceverse/metacheck scienceverse/metacheck            tests
#> 20 scienceverse/metacheck scienceverse/metacheck        vignettes
#>                path
#> 1     .Rbuildignore
#> 2           .github
#> 3        .gitignore
#> 4       DESCRIPTION
#> 5        LICENSE.md
#> 6         NAMESPACE
#> 7           NEWS.md
#> 8                 R
#> 9         README.md
#> 10 _metacheck.Rproj
#> 11           _stuff
#> 12      codecov.yml
#> 13             data
#> 14         data-raw
#> 15             docs
#> 16             inst
#> 17              man
#> 18          pkgdown
#> 19            tests
#> 20        vignettes
#>                                                                      download_url
#> 1     https://raw.githubusercontent.com/scienceverse/metacheck/main/.Rbuildignore
#> 2                                                                            <NA>
#> 3        https://raw.githubusercontent.com/scienceverse/metacheck/main/.gitignore
#> 4       https://raw.githubusercontent.com/scienceverse/metacheck/main/DESCRIPTION
#> 5        https://raw.githubusercontent.com/scienceverse/metacheck/main/LICENSE.md
#> 6         https://raw.githubusercontent.com/scienceverse/metacheck/main/NAMESPACE
#> 7           https://raw.githubusercontent.com/scienceverse/metacheck/main/NEWS.md
#> 8                                                                            <NA>
#> 9         https://raw.githubusercontent.com/scienceverse/metacheck/main/README.md
#> 10 https://raw.githubusercontent.com/scienceverse/metacheck/main/_metacheck.Rproj
#> 11                                                                           <NA>
#> 12      https://raw.githubusercontent.com/scienceverse/metacheck/main/codecov.yml
#> 13                                                                           <NA>
#> 14                                                                           <NA>
#> 15                                                                           <NA>
#> 16                                                                           <NA>
#> 17                                                                           <NA>
#> 18                                                                           <NA>
#> 19                                                                           <NA>
#> 20                                                                           <NA>
#>     size          ext   type
#> 1    246 rbuildignore   file
#> 2      0       github    dir
#> 3    380    gitignore config
#> 4   2203                file
#> 5  34303           md   text
#> 6   2826                file
#> 7  18053           md   text
#> 8      0                 dir
#> 9    995           md   text
#> 10   462        rproj config
#> 11     0                 dir
#> 12   134          yml config
#> 13     0                 dir
#> 14     0                 dir
#> 15     0                 dir
#> 16     0                 dir
#> 17     0                 dir
#> 18     0                 dir
#> 19     0                 dir
#> 20     0                 dir
#> 
#> $languages
#>                      repo   language    bytes
#> 1  scienceverse/metacheck       HTML 18689007
#> 2  scienceverse/metacheck          R  6428992
#> 3  scienceverse/metacheck        TeX    47751
#> 4  scienceverse/metacheck       AMPL     7571
#> 5  scienceverse/metacheck     Python     6986
#> 6  scienceverse/metacheck        CSS     3358
#> 7  scienceverse/metacheck JavaScript     1018
#> 8  scienceverse/metacheck Dockerfile      929
#> 9  scienceverse/metacheck       SCSS      104
#> 10 scienceverse/metacheck      Shell       17
#> 
# }
```
