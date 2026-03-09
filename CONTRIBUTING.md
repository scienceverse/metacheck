# Contributing to MetaCheck

We welcome human contributions to metacheck (we do not accept pull requests with a bot or agent as a named contributor). Small bug fixes can be made as a simple pull request, but contributing a new built-in module or function to the package needs to follow the guidance below.

- [Intro to R Packages](https://psyteachr.github.io/intro-r-pkgs/) for a quick focused intro 
- [R Packages](https://r-pkgs.org/) for all the details

## Adding a new built-in module

- [ ] See the [Creating Modules vignette](https://www.scienceverse.org/metacheck/articles/creating_modules.html)
- [ ] Fork or branch the dev branch of metacheck (or main if there is no current dev branch) with a name like `dev-module-modname`
- [ ] Use the template helper! `module_template()`
- [ ] Give it a name that is consistent in style with existing modules (e.g., 1-2 words separated by underscores, noun_verb or category_noun)
- [ ] Save the file in `inst/modules/`
- [ ] Make sure it has a correct @keywords section
- [ ] [Format any references](https://www.scienceverse.org/metacheck/articles/creating_modules.html#format-references) as a bibentry and use format_ref() to display them (will have added value soon)
- [ ] Write unit tests for your module. Add them to an existing test page if they are part of a linked group of modules, or make a new one. Here is a bare minimum:

    ```
    test_that("module_name", {
      module <- "module_name"
      mods <- module_list()
      expect_true(module %in% mods$name)
      
      paper <- read(demoxml())
      expect_no_error( mo <- module_run(paper, module) )
    })
    ```
    
- [ ] Modify the following code to test your module in report and metascience workflows. If the module is time or resource intensive, this can go in the unit tests and be skipped in the regular test workflow using `skip_if_quick()` (custom skip function). However, please run the code on a paperlist in both report and metascience workflows to make sure it works.

    ```
    # sample 10 random papers
    papers <- sample(psychsci, 10)

    # generate a report for each paper
    reports <- report(papers, "module_name") 
    browseURL(reports)

    # metascience workflow
    mod_output <- module_run(paper, "module_name")
    ```    

- [ ] Do not add the module to report defaults yourself -- ask @debruine or @lakens first
- [ ] Validate your module on a set of papers
- [ ] Submit a pull request with changes to only your module file and its associated test file, link to or expain the validation results in the request, and tag @debruine for review

## Adding a new function

- [ ] Add the new function to an appropriate file under R/ -- make a new file if appropriate ([naming advice](https://style.tidyverse.org/package-files.html#names))
- [ ] Make sure it has complete roxygen documentation (Code > Insert Roxygen Skeleton)
- [ ] Run `devtools::document(roclets = c('rd', 'collate', 'namespace'))` (ctrl-cmd-D) to add the documentation
- [ ] Add unit tests (to the existing test file if adding the function to an existing R file, or make a new one)
- [ ] Run all tests with `devtools::test()` to make sure you didn't mess up anything
- [ ] If internal (only used in other functions, never by a user), make sure you include `@keywords internal` in the roxygen documentation
- [ ] Run CMD check with `devtools::check()` and fix any warnings related to your function (there are always a few notes, ask Lisa if you aren't sure about a warning)
- [ ] Submit a pull request with changes to only your function file and its associated documentation and test files, tagging @debruine for review
- [ ] If the module is acceptable, we may ask you to do the following:
    - [ ] If not internal, add it to the appropriate category in `pkgdown/_pkgdown.yml`
    - [ ] Rerender the website sections with `pkgdown::build_reference_index()` and `pkgdown::build_reference()`



## Unit Tests

I use test-driven development, so I write the unit test below for any new function before I even start to write the function. Then I never forget to document it.

```
test_that("newfunc", {
  expect_true(is.function(metacheck::newfunc))
  expect_no_error(helplist <- help(newfunc, metacheck))
  
  expect_error(newfunc(bad_arg))
  
})
```

A test should give at least one basic example showing the most typical use of the function. Every time you realise the function doesn't behave exactly as you expect, write a failing test for that example and work on the code until it's not failing anymore.

### Helper files

If you need to define functions for your tests, put the functions in `tests/testthat/helper.R`.

If you need to provide specific files (e.g., XMLs for paper that show a specific thing), put them under `tests/testthat/fixtures/`

