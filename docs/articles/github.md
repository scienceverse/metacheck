# Exploring GitHub Repositories

``` r

library(metacheck)
#> 
#> 
#> ***********************************************
#> ✅ Welcome to metacheck beta version 0.1.0
#> ✨ Your version newer than the current release
#> 
#> ℹ For support and examples visit:
#> https://scienceverse.github.io/metacheck/
#> 
#> ⚠️ Set an email to use APIs like OpenAlex
#> metacheck::email('your@address.org')
#> 
#> 🧪 This is beta software; please check any
#> results. Check module validation info for
#> false positive and negative rates.
#> ***********************************************
#> 
#> Attaching package: 'metacheck'
#> The following object is masked from 'package:base':
#> 
#>     message
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

## github_languages

You can retrieve the number of bytes dedicated to various coding
languages, as detected and classified by GitHub.

``` r

github_languages("scienceverse/metacheck")
#>                     repo          language
#> 1 scienceverse/metacheck           message
#> 2 scienceverse/metacheck documentation_url
#> 3 scienceverse/metacheck            status
#>                                                                                                                                                                                                                                                              bytes
#> 1 The 'scienceverse' organization forbids access via a fine-grained personal access tokens if the token's lifetime is greater than 366 days. Please adjust your token's lifetime at the following URL: https://github.com/settings/personal-access-tokens/12490665
#> 2                                                                                                                                                                                               https://docs.github.com/rest/repos/repos#list-repository-languages
#> 3                                                                                                                                                                                                                                                              403
```

## github_files

You can get a list of file names, their path, size, file extension, and
a guess at their type.

By default, you just retrieve the files and directories in the base
directory, non-recursively.

``` r

github_files("scienceverse/metacheck")
#> : The 'scienceverse' organization forbids access via a fine-grained personal access tokens if the token's lifetime is greater than 366 days. Please adjust your token's lifetime at the following URL: https://github.com/settings/personal-access-tokens/12490665
#> NULL
```

``` r

github_files("scienceverse/metacheck", dir = ".github")
#> .github: The 'scienceverse' organization forbids access via a fine-grained personal access tokens if the token's lifetime is greater than 366 days. Please adjust your token's lifetime at the following URL: https://github.com/settings/personal-access-tokens/12490665
#> NULL
```

You can also retrieve files recursively. Searching a large repository
recursively can take a while.

``` r

github_files("scienceverse/metacheck",
             dir = ".github",
             recursive = TRUE)
#> .github: The 'scienceverse' organization forbids access via a fine-grained personal access tokens if the token's lifetime is greater than 366 days. Please adjust your token's lifetime at the following URL: https://github.com/settings/personal-access-tokens/12490665
#> NULL
```

## github_info

Get all of the information about a repository in one list object, with
items named “repo”, “readme”, “languages”, and “files”.

``` r

github_info("scienceverse/demo")
#> : The 'scienceverse' organization forbids access via a fine-grained personal access tokens if the token's lifetime is greater than 366 days. Please adjust your token's lifetime at the following URL: https://github.com/settings/personal-access-tokens/12490665
#> $repo
#> [1] "scienceverse/demo"
#> 
#> $readme
#> [1] ""
#> 
#> $files
#> NULL
#> 
#> $languages
#>                repo          language
#> 1 scienceverse/demo           message
#> 2 scienceverse/demo documentation_url
#> 3 scienceverse/demo            status
#>                                                                                                                                                                                                                                                              bytes
#> 1 The 'scienceverse' organization forbids access via a fine-grained personal access tokens if the token's lifetime is greater than 366 days. Please adjust your token's lifetime at the following URL: https://github.com/settings/personal-access-tokens/12490665
#> 2                                                                                                                                                                                               https://docs.github.com/rest/repos/repos#list-repository-languages
#> 3                                                                                                                                                                                                                                                              403
```
