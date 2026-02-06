# Get Languages from GitHub Repo

Get Languages from GitHub Repo

## Usage

``` r
github_languages(repo)
```

## Arguments

- repo:

  The URL of the repository (in the format "username/repo" or
  "https://github.com/username/repo")

## Value

vector of languages

## Examples

``` r
# \donttest{
github_languages("scienceverse/metacheck")
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
# }
```
