# Exploring GitHub Repositories

``` r
library(metacheck)
#> 
#> 
#> *******************************************
#> ✅ Welcome to metacheck
#> For support and examples visit:
#> https://scienceverse.github.io/metacheck/
#> 
#> ⚠️ Set an email to use APIs like OpenAlex
#> metacheck::email('your@address.org')
#> 
#> ‼️ This is alpha software; please check any
#> results. False positives and negatives will
#> occur at unknown rates.
#> *******************************************
```

There are some built-in functions in metacheck for exploring GitHub
repositories. You can use these in custom modules.

## github_repo

The github functions all work with the following formats for referring
to repositories:

- `"{username}/{repo}"`  
- `"{username}/{repo}.git"`  
- `"https://github.com/{username}/{repo}.git"`  
- `"https://github.com/{username}/{repo}/{...}"`

The
[`github_repo()`](https://scienceverse.github.io/metacheck/reference/github_repo.md)
function returns the simplified format of. repo name, and NULL if the
repository in inaccessible.

``` r
github_repo("https://github.com/scienceverse/metacheck.git")
#> [1] "scienceverse/metacheck"
```

``` r
github_repo("scienceverse/checkpaper")
#> NULL
```

## github_readme

Get the text of the readme file, regardless of the exact file name
(e.g., README vs README.md).

``` r
readme <- github_readme("scienceverse/metacheck")

cat(readme)
```

    #> # metacheck
    #> 
    #> <!-- badges: start -->
    #> ![Made in Europe](https://img.shields.io/badge/Made_in_Europe-003399?logo=european-union&logoColor=FFCC00)
    #> 
    #> [![Lifecycle: experimental](https://img.shields.io/badge/lifecycle-experimental-orange.svg)](https://lifecycle.r-lib.org/articles/stages.html#experimental)
    #> 
    #> [![Codecov test coverage](https://codecov.io/gh/scienceverse/metacheck/graph/badge.svg)](https://app.codecov.io/gh/scienceverse/metacheck)
    #> <!-- badges: end -->
    #> 
    #> The goal of metacheck is to automatically check research outputs for best practices. You can find out more at <https://scienceverse.github.io/metacheck/>.
    #> 
    #> ## Installation
    #> 
    #> You can install the development version of metacheck from [GitHub](https://github.com/) with:
    #> 
    #> ``` r
    #> # install.packages("devtools")
    #> devtools::install_github("scienceverse/metacheck")
    #> ```
    #> 
    #> ## API (optional)
    #> To run metacheck as a REST API either using plumber or Docker, see [`inst/plumber/README.md`](inst/plumber/README.md) for instructions and documentation.

## github_languages

You can retrieve the number of bytes dedicated to various coding
languages, as detected and classified by GitHub.

``` r
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
```

## github_files

You can get a list of file names, their path, size, file extension, and
a guess at their type.

By default, you just retrieve the files and directories in the base
directory, non-recursively.

``` r
github_files("scienceverse/metacheck")
#>                      repo             clean_repo             name
#> 1  scienceverse/metacheck scienceverse/metacheck _metacheck.Rproj
#> 2  scienceverse/metacheck scienceverse/metacheck           _stuff
#> 3  scienceverse/metacheck scienceverse/metacheck          .github
#> 4  scienceverse/metacheck scienceverse/metacheck       .gitignore
#> 5  scienceverse/metacheck scienceverse/metacheck    .Rbuildignore
#> 6  scienceverse/metacheck scienceverse/metacheck      codecov.yml
#> 7  scienceverse/metacheck scienceverse/metacheck             data
#> 8  scienceverse/metacheck scienceverse/metacheck         data-raw
#> 9  scienceverse/metacheck scienceverse/metacheck      DESCRIPTION
#> 10 scienceverse/metacheck scienceverse/metacheck             docs
#> 11 scienceverse/metacheck scienceverse/metacheck             inst
#> 12 scienceverse/metacheck scienceverse/metacheck       LICENSE.md
#> 13 scienceverse/metacheck scienceverse/metacheck              man
#> 14 scienceverse/metacheck scienceverse/metacheck        NAMESPACE
#> 15 scienceverse/metacheck scienceverse/metacheck          NEWS.md
#> 16 scienceverse/metacheck scienceverse/metacheck          pkgdown
#> 17 scienceverse/metacheck scienceverse/metacheck                R
#> 18 scienceverse/metacheck scienceverse/metacheck        README.md
#> 19 scienceverse/metacheck scienceverse/metacheck            tests
#> 20 scienceverse/metacheck scienceverse/metacheck        vignettes
#>                path
#> 1  _metacheck.Rproj
#> 2            _stuff
#> 3           .github
#> 4        .gitignore
#> 5     .Rbuildignore
#> 6       codecov.yml
#> 7              data
#> 8          data-raw
#> 9       DESCRIPTION
#> 10             docs
#> 11             inst
#> 12       LICENSE.md
#> 13              man
#> 14        NAMESPACE
#> 15          NEWS.md
#> 16          pkgdown
#> 17                R
#> 18        README.md
#> 19            tests
#> 20        vignettes
#>                                                                      download_url
#> 1  https://raw.githubusercontent.com/scienceverse/metacheck/main/_metacheck.Rproj
#> 2                                                                            <NA>
#> 3                                                                            <NA>
#> 4        https://raw.githubusercontent.com/scienceverse/metacheck/main/.gitignore
#> 5     https://raw.githubusercontent.com/scienceverse/metacheck/main/.Rbuildignore
#> 6       https://raw.githubusercontent.com/scienceverse/metacheck/main/codecov.yml
#> 7                                                                            <NA>
#> 8                                                                            <NA>
#> 9       https://raw.githubusercontent.com/scienceverse/metacheck/main/DESCRIPTION
#> 10                                                                           <NA>
#> 11                                                                           <NA>
#> 12       https://raw.githubusercontent.com/scienceverse/metacheck/main/LICENSE.md
#> 13                                                                           <NA>
#> 14        https://raw.githubusercontent.com/scienceverse/metacheck/main/NAMESPACE
#> 15          https://raw.githubusercontent.com/scienceverse/metacheck/main/NEWS.md
#> 16                                                                           <NA>
#> 17                                                                           <NA>
#> 18        https://raw.githubusercontent.com/scienceverse/metacheck/main/README.md
#> 19                                                                           <NA>
#> 20                                                                           <NA>
#>     size          ext   type
#> 1    462        rproj config
#> 2      0                 dir
#> 3      0       github    dir
#> 4    380    gitignore config
#> 5    246 rbuildignore   file
#> 6    134          yml config
#> 7      0                 dir
#> 8      0                 dir
#> 9   2203                file
#> 10     0                 dir
#> 11     0                 dir
#> 12 34303           md   text
#> 13     0                 dir
#> 14  2826                file
#> 15 18053           md   text
#> 16     0                 dir
#> 17     0                 dir
#> 18   995           md   text
#> 19     0                 dir
#> 20     0                 dir
```

``` r
github_files("scienceverse/metacheck", dir = ".github")
#>                     repo             clean_repo       name               path
#> 1 scienceverse/metacheck scienceverse/metacheck .gitignore .github/.gitignore
#> 2 scienceverse/metacheck scienceverse/metacheck CODEOWNERS .github/CODEOWNERS
#> 3 scienceverse/metacheck scienceverse/metacheck  workflows  .github/workflows
#>                                                                       download_url
#> 1 https://raw.githubusercontent.com/scienceverse/metacheck/main/.github/.gitignore
#> 2 https://raw.githubusercontent.com/scienceverse/metacheck/main/.github/CODEOWNERS
#> 3                                                                             <NA>
#>   size       ext   type
#> 1    7 gitignore config
#> 2   41             file
#> 3    0              dir
```

You can also retrieve files recursively. Searching a large repository
recursively can take a while.

``` r
github_files("scienceverse/metacheck",
             dir = ".github",
             recursive = TRUE)
#>                     repo             clean_repo                name
#> 1 scienceverse/metacheck scienceverse/metacheck          .gitignore
#> 2 scienceverse/metacheck scienceverse/metacheck          CODEOWNERS
#> 3 scienceverse/metacheck scienceverse/metacheck           workflows
#> 4 scienceverse/metacheck scienceverse/metacheck        pkgdown.yaml
#> 5 scienceverse/metacheck scienceverse/metacheck    teams-notify.yml
#> 6 scienceverse/metacheck scienceverse/metacheck  test-coverage.yaml
#> 7 scienceverse/metacheck scienceverse/metacheck upload_packages.yml
#>                                    path
#> 1                    .github/.gitignore
#> 2                    .github/CODEOWNERS
#> 3                     .github/workflows
#> 4        .github/workflows/pkgdown.yaml
#> 5    .github/workflows/teams-notify.yml
#> 6  .github/workflows/test-coverage.yaml
#> 7 .github/workflows/upload_packages.yml
#>                                                                                          download_url
#> 1                    https://raw.githubusercontent.com/scienceverse/metacheck/main/.github/.gitignore
#> 2                    https://raw.githubusercontent.com/scienceverse/metacheck/main/.github/CODEOWNERS
#> 3                                                                                                <NA>
#> 4        https://raw.githubusercontent.com/scienceverse/metacheck/main/.github/workflows/pkgdown.yaml
#> 5    https://raw.githubusercontent.com/scienceverse/metacheck/main/.github/workflows/teams-notify.yml
#> 6  https://raw.githubusercontent.com/scienceverse/metacheck/main/.github/workflows/test-coverage.yaml
#> 7 https://raw.githubusercontent.com/scienceverse/metacheck/main/.github/workflows/upload_packages.yml
#>   size       ext   type
#> 1    7 gitignore config
#> 2   41             file
#> 3    0              dir
#> 4 1380      yaml config
#> 5  521       yml config
#> 6 1877      yaml config
#> 7 3231       yml config
```

## github_info

Get all of the information about a repository in one list object, with
items named “repo”, “readme”, “languages”, and “files”.

``` r
github_info("scienceverse/demo")
#> $repo
#> [1] "scienceverse/demo"
#> 
#> $readme
#> [1] "# demo\nFor use in testing functions\n"
#> 
#> $files
#>                repo        clean_repo           name           path
#> 1 scienceverse/demo scienceverse/demo         folder         folder
#> 2 scienceverse/demo scienceverse/demo good-example.R good-example.R
#> 3 scienceverse/demo scienceverse/demo      README.md      README.md
#>                                                              download_url size
#> 1                                                                    <NA>    0
#> 2 https://raw.githubusercontent.com/scienceverse/demo/main/good-example.R  227
#> 3      https://raw.githubusercontent.com/scienceverse/demo/main/README.md   36
#>   ext type
#> 1      dir
#> 2   r code
#> 3  md text
#> 
#> $languages
#>                repo language bytes
#> 1 scienceverse/demo        R   227
```
