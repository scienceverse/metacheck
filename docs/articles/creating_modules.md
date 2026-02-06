# Creating Modules

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

Modules are user-created patterns for checking a paper or set of papers.
Module specifications are written in the same format as functions in R
packages, using roxygen2 for documentation.

    #' Module Title
    #'
    #' @description
    #' A short description of the module.
    #'
    #' @details
    #' This text will show when you use module_info() and in the "How It Works" collapse box after the module report. It can be multiple paragraphs and is styled with markdown.
    #'
    #' @keywords general|intro|method|results|discussion|reference
    #'
    #' @author Author Name (\email{name@email.com})
    #'
    #' @references
    #' # Optional reference to include in reports
    #'
    #' @import dplyr
    #'
    #' @param paper a paper object or paperlist object
    #' @param ... further arguments (not used)
    #'
    #' @returns report list
    module_name <- function(paper, ...) {
      # see https://www.scienceverse.org/metacheck/articles/creating_modules.html

      # module code ----
      pattern <- "significant"

      # create return items ----

      ## table ----
      # detail your results in a format like the result of search_text()
      # this is stored to use in later modules in a report or pipeline
      table <- search_text(paper, pattern)

      ## summary_table ----
      # must have id column as the id of each paper, one row per paper
      # and further columns to be added to a master summary table
      summary_table <- dplyr::count(table, id, name = "n_significant")

      ## traffic light ----
      # displayed in reports, possible values:
      #   green: no problems detected
      #   yellow: something to check
      #   red: possible problems detected
      #   info: informational only
      #   na: not applicable
      #   fail: check failed
      tl <- if (nrow(table)) "info" else "na"

      ## summary_text ----
      # short text to be displayed at the top of reports
      # may be unique for each possible traffic light
      summary_text_options <- c(
        na = "Not applicable",
        info = "This table is provided for your information",
        red = "This is a potential problem",
        yellow = "There may be a problem",
        green = "No problems found",
        fail = "The check failed, sorry"
      )
      summary_text <- summary_text_options[[tl]]

      ## report ----
      # longer text to be displayed in the module section
      # use quarto / markdown for styling
      # https://quarto.org/docs/authoring/markdown-basics.html
      report_text <- "This table shows all of the sentences where the paper used the word *significant*. "
      report_table <- table[, c("section", "text")]
      further_info <- c(
        "For more opinions on the use of the word *significant*:",
        "* Motulsky, H. (2014). Opinion: Never use the word ‘significant’ in a scientific paper. Advances in regenerative biology, 1(1), 25155. doi: [10.3402/arb.v1.25155](https://doi.org/10.3402/arb.v1.25155)"
        )

      report <- c(
        report_text,
        scroll_table(report_table, colwidths = c(.2, .8)),
        collapse_section(further_info, title = "Further Info")
      )

      # return a list ----
      list(
        table = table,
        summary_table = summary_table,
        na_replace = 0,
        traffic_light = tl,
        summary_text = summary_text,
        report = report
      )
    }

## Roxygen Documentation

The module file starts with standard function documentation using
roxygen2. Roxygen documentation always starts with `#'`.

### Title

On the first line, give your module a short title, which will be used as
a section header in reports.

``` r
#' Module Name
```

### Description

You can skip a line and write a 1-sentence description, which will be
shown in
[`module_list()`](https://scienceverse.github.io/metacheck/reference/module_list.md),
or optionally start this with `@description`.

``` r
#' @description
#' A short description of the module
```

### Details

You can write more detailed help under the tag `@details`, which will be
shown when calling
[`module_help()`](https://scienceverse.github.io/metacheck/reference/module_help.md).
This is optional.

``` r
#' @details
#' Here is more information about the module to help you use or understand it.
#' 
#' You can skip more lines to break up paragraphs.
#' 
#' * make a list
#' * check it twice
```

If you have experience writing R functions with roxygen, you can also
omit the `@description` and `@details` tags and rely on paragraph
spacing to distinguish description from details.

### Keywords

Choose one section category for your module to be displayed under in
reports.

``` r
#' @keywords general|intro|method|results|discussion|reference
```

### Author

Include the module authors so they can get credit! Add a new `@author`
tag for each author, and optionally add their email address.

``` r
#' @author Lisa DeBruine (\email{debruine@gmail.com})
#' @author Daniel Lakens (\email{lakens@gmail.com})
```

### References

Optionally include references that you would want available to users. If
you are building a module that uses citable resources, please list them
here.

``` r
#' @references
#' The Retraction Watch Database [Internet].
#' New York: The Center for Scientific Integrity. 2018.
#' ISSN: 2692-4579. [Cited 2025-05-20].
#' Available from: http://retractiondatabase.org/.
```

### Import

If you are using packages other than `metacheck`, add each with an
`@import` statement.

``` r
#' @import dplyr
#' @import tidyr
```

Technically, you can then use functions from these packages in your
function code without the package name prefix, but it is still best
practice to use the package name prefix for all functions, like
[`dplyr::case_when()`](https://dplyr.tidyverse.org/reference/case_when.html).

### Parameters

Each argument should be defined for a function. All metacheck modules
require the first argument to be `paper`. The last argument can
optionally be `...`. This allows the
[`module_run()`](https://scienceverse.github.io/metacheck/reference/module_run.md)
function to pass any arguments, and your code can use them by name
(e.g., `extra_args <- list(...)`).

``` r
#' @param paper a paper object or paperlist object
#' @param ... further arguments (not used)
```

### Returns

It is good practice to explain what your function returns. This is
usually the default list with table, summary, traffic light, and report
text, but you can edit this. It’s just a human-readable string.

``` r
#' @returns a list
```

### Examples

You can add an example of how to use this module with the
[`module_run()`](https://scienceverse.github.io/metacheck/reference/module_run.md)
function. Give a paper or list of papers in the example so you can
demonstrate the purpose of this module and it doesn’t take too much time
to run the example.

``` r
#' @examples
#' module_run(psychsci, "module_name")
```

## Function Code

The module function is written like any R package function, with the
requirement that the first argument be `paper`. Set `module_name` to
your module name, which must be a valid R variable name. Your module
script should also have the same name, with a .R suffix (e.g.,
`module_name.R`).

``` r
module_name <- function(paper, ...) {
  # module code ----
  # create return items ----
  ## table ----
  ## summary_table ----
  ## traffic light ----
  ## summary_text ----
  ## report ----
  # return a list ----
}
```

You can define helper functions below your main module functions, but
the first function defined in the script is what will be run on the
paper object.

A module can technically do anything you want with the paper input, but
you will need to follow the template below for your module to work
automatically with reports and the metascience workflow.

If you are using your modules to build a report, you need to specify
what type of output corresponds to good practice or practice that may
need improvement. We do this through “traffic_light” and “report”.

### Code Helpers

#### Progress Bars

If you want to display progress for a long module, you can use the
[`pb()`](https://scienceverse.github.io/metacheck/reference/pb.md)
function. It’s a modified version of
[`progress::progress_bar()`](http://r-lib.github.io/progress/reference/progress_bar.md)
that only displays a progress bar if
[`verbose()`](https://scienceverse.github.io/metacheck/reference/verbose.md)
is true.

``` r
steps <- c("beginning", "middle", "end")
pb <- pb(length(steps),
           ":what [:bar] :current/:total :elapsedfull")

for (step in steps) {
  pb$tick(tokens = list(what = step))
  Sys.sleep(2)
}
```

#### Get Previous Outputs

If you run module in a chain or via the
[`report()`](https://scienceverse.github.io/metacheck/reference/report.md)
function, the output of the previously run modules is available to later
modules. You will need to handle what happens if the part of the output
you require is missing, but always access these with the
[`get_prev_outputs()`](https://scienceverse.github.io/metacheck/reference/get_prev_outputs.md)
function.

``` r
# get p_table from prev_outputs if available
p_table <- get_prev_outputs(module = "all_p_values", 
                            item = "table")

# run that module if not
if (is.null(p_table)) {
  p <- module_run(paper, "all_p_values")
  p_table <- p$table
}

# ... further code using p_table
```

### Table

Most modules will need to structure their output in a table that can be
shown in a report. The
[`search_text()`](https://scienceverse.github.io/metacheck/reference/search_text.md)
function below creates a table with a row for each sentence that
contains to word “significant”.

``` r
  ## table ----
  # detail your results in a format like the result of search_text()
  # this is stored to use in later modules in a report or pipeline
  table <- search_text(paper, pattern)
```

You will need to make sure that your module works with both single paper
object sand lists of paper objects. The metacheck functions
[`search_text()`](https://scienceverse.github.io/metacheck/reference/search_text.md)
and [`llm()`](https://scienceverse.github.io/metacheck/reference/llm.md)
are already vectorised for paper lists.

### Summary Table

For the metascience workflow, it is useful to create a table with a row
for each paper in a list, and some columns that summarise the results.
You can use nested tables if you want some of your cells to contain
multiple values.

``` r
  ## summary_table ----
  # must have id column as the id of each paper, one row per paper
  # and further columns to be added to a master summary table
  summary_table <- dplyr::count(table, id, name = "n_significant")
```

Your summary table might omit some papers from the whole list because no
relevant text was found. You don’t have to add them into your table, as
the
[`module_run()`](https://scienceverse.github.io/metacheck/reference/module_run.md)
function will do that automatically for you. However, you may want the
values of your summary variables to be something other than `NA` for
these missing papers. You can set the value of `na_replace` in the
return list (below) to this default value. For example, if you are
returning a summary of the count of sentences with the word
“significant”, you can replace `NA`s with 0.

If you are returning more than one summary column and have different
replacement values, use a named list.

``` r
na_replace <- list(
  n_significant = 0,
  paper_type = "unknown"
)
```

### Traffic Light

The traffic lights are used in single-paper reports to give a quick
visual overview of the module results. There are 5 kinds of traffic
lights:

🟢 no problems detected;  
🟡 something to check;  
🔴 possible problems detected;  
🔵 informational only;  
⚪️ not applicable;  
⚫️ check failed

You will need to write some code to determine which traffic lights apply
to your case. If you don’t include a traffic light, but do include a
`table` in the returned list, the following rule will be applied for the
traffic light.

``` r
  ## traffic light ----
  # displayed in reports, possible values: 
  #   green: no problems detected
  #   yellow: something to check
  #   red: possible problems detected
  #   info: informational only
  #   na: not applicable
  #   fail: check failed
  tl <- if (nrow(table)) "info" else "na"
```

### Summary Text

The reports begin with a summary of each module, showing the traffic
light, the module name, and a short summary of the results. The returned
item `summary_text` provides this text. You will usually want to
customise the summary text for the traffic light or other aspects of the
results, such as the number of instances of a practice found and the
number that might be problematic.

``` r
  ## summary_text ----
  # short text to be displayed at the top of reports
  # may be unique for each possible traffic light
  summary_text_options <- c(
    na = "Not applicable",
    info = "This table is provided for your information",
    red = "This is a potential problem",
    yellow = "There may be a problem",
    green = "No problems found",
    fail = "The check failed, sorry"
  )
  summary_text <- summary_text_options[[tl]]
```

### Report

Reports need to explain concepts or give resources for further learning.
This is often specific to the outcome of a check.

The `report` should be a character vector, with one or more item. Use
[quarto /
markdown](https://quarto.org/docs/authoring/markdown-basics.html) for
styling text, such as adding links or lists.

``` r
  ## report ----
  # longer text to be displayed in the module section
  # use quarto / markdown for styling
  # https://quarto.org/docs/authoring/markdown-basics.html
  report_text <- "This table shows all of the sentences where the paper used the word *significant*. "
  report_table <- table[, c("section", "text")]
  further_info <- c(
    "For more opinions on the use of the word *significant*:",
    "* Motulsky, H. (2014). Opinion: Never use the word ‘significant’ in a scientific paper. Advances in regenerative biology, 1(1), 25155. doi: [10.3402/arb.v1.25155](https://doi.org/10.3402/arb.v1.25155)"
    )

  report <- c(
    report_text,
    scroll_table(report_table, colwidths = c(.2, .8)),
    collapse_section(further_info, title = "Further Info")
  )
```

You can use the helper functions
[`scroll_table()`](https://scienceverse.github.io/metacheck/reference/scroll_table.md)
to display tables and
[`collapse_section()`](https://scienceverse.github.io/metacheck/reference/collapse_section.md)
to hide longer sections of text or supplemental tables.

#### Scroll Tables

The function
[`scroll_table()`](https://scienceverse.github.io/metacheck/reference/scroll_table.md)
generates the R code chunk needed to display a scrollable table in a
quarto document, which is how the reports are created. It will include
the contents of the table.

``` r
table <- data.frame(id = 1:10, letter = LETTERS[1:10])

scroll_table(
  table, 
  colwidths = c(.1, .9), # use NA for auto, numbers <=1 are %, > 1 are px, 
                         # or use characters, e.g., c("3em", NA)
  maxrows = 2,           # if table length <= this, show all rows, no paginations
  column = "body"        # page: spans full page, margin: just right margin
) |> cat()
```

    #> 
    #> ```{r}
    #> #| echo: false
    #> 
    #> 
    #> # table data --------------------------------------
    #> table <- structure(list(id = 1:10, letter = c("A", "B", "C", "D", "E", 
    #> "F", "G", "H", "I", "J")), class = "data.frame", row.names = c(NA, 
    #> -10L))
    #> 
    #> # display table -----------------------------------
    #> metacheck::report_table(table, c(0.1, 0.9), 2, FALSE)
    #> ```

#### Collapsible Sections

The function
[`collapse_section()`](https://scienceverse.github.io/metacheck/reference/collapse_section.md)
generates the R code chunk needed to hide a section in a collapsible
box.

``` r
text <- c("This is my first *paragraph*:",
          "* list item 1",
          "* list item 2")

collapse_section(
  text, # a vector of markdown text
  title = "See the full list", # defaults to "Learn More"
  callout = "note", # "tip", " note", "warning", "important", "caution"
  collapse = TRUE # defaults TRUE to start collapsed
) |> cat()
#> ::: {.callout-note title="See the full list" collapse="true"}
#> 
#> This is my first *paragraph*:
#> 
#> * list item 1
#> 
#> * list item 2
#> 
#> :::
```

#### Plurals

Feedback and summary text often needs to refer to the number of
instances something happened. We found ourselves having to awkwardly
write text templates like “We found %d inexact p-value(s).” The
[`plural()`](https://scienceverse.github.io/metacheck/reference/plural.md)
function is a quick helper so you can add in an “s” if the number is
not 1. We like the [`sprintf()`](https://rdrr.io/r/base/sprintf.html)
function for setting up your template sentence and replacing in numbers
or strings.

``` r
n <- 0:2

sprintf("We found %d problem%s that %s serious.",
        n, plural(n), plural(n, "is", "are"))
#> [1] "We found 0 problems that are serious."
#> [2] "We found 1 problem that is serious."  
#> [3] "We found 2 problems that are serious."
```

#### HTML Links

Usually, you will use markdown to create linked text, like
`[text](url)`. However, this doesn’t work inside a scroll table, so we
made a helper for creating html links. If you just give it a URL, it
will remove the “http(s)://” and use the rest of the URL as the linked
text.

``` r
link("https://scienceverse.org/metacheck")
#> [1] "<a href='https://scienceverse.org/metacheck' target='_blank'>scienceverse.org/metacheck</a>"
```

More commonly, you will want to specify the linked text. Links created
by markdown in the main text of a report are automatically opened in a
new window, but you need to specify this for HTML links. Set
`new_window = FALSE` if you don’t want this.

``` r
link(url = "https://scienceverse.org/metacheck", 
     text = "MetaCheck", 
     new_window = FALSE)
#> [1] "<a href='https://scienceverse.org/metacheck'>MetaCheck</a>"
```

#### Format References

In order to keep references displayed consistently, use the
[`format_ref()`](https://scienceverse.github.io/metacheck/reference/format_ref.md)
function. This function can take a bibentry object (like the values in
the `ref` column of a paper’s bib table), bibtex text, or plain text.

``` r
# get refs from a paper
paper <- read(demoxml())
format_ref(paper$bib$ref[2])
```

\[1\] “Gino F, Wiltermuth SS (2014). “Evil Genius? How Dishonesty Can
Lead to Greater Creativity.” *Psychological Science*, **25**(4),
973-981. [doi:10.1177/0956797614520714](NA).”

``` r
bibentry <- bibentry(
    bibtype = "Article",
    title = "Improving transparency, falsifiability, and rigor by making hypothesis tests machine-readable",
    author = c(
      person("D.", "Lakens"),
      person(c("L.", "M."), "DeBruine")
    ),
    journal = "Advances in Methods and Practices in Psychological Science",
    year = 2021,
    volume = 4,
    number = 2,
    pages = "2515245920970949",
    doi = "10.1177/2515245920970949"
  )
format_ref(bibentry)
```

\[1\] “Lakens D, DeBruine LM (2021). “Improving transparency,
falsifiability, and rigor by making hypothesis tests machine-readable.”
*Advances in Methods and Practices in Psychological Science*, **4**(2),
2515245920970949. [doi:10.1177/2515245920970949](NA).”

You can get a bibentry citation for any package with code.

``` r
bib <- citation("metacheck")
format_ref(bib)
```

\[1\] “DeBruine L, Lakens D (2025). *metacheck: Check Research Outputs
for Best Practices*. R package version 0.0.0.9068,
[https://github.com/scienceverse/metacheck](NA).”

The function can also handle references in bibtex format.

``` r
bibtex <- "@Article{,
  title = {Improving transparency, falsifiability, and rigor by making hypothesis tests machine-readable},
  author = {D. Lakens and L. M. DeBruine},
  journal = {Advances in Methods and Practices in Psychological Science},
  year = {2021},
  volume = {4},
  number = {2},
  pages = {2515245920970949},
  doi = {10.1177/2515245920970949},
}"
format_ref(bibtex)
```

\[1\] “Lakens D, DeBruine LM (2021). “Improving transparency,
falsifiability, and rigor by making hypothesis tests machine-readable.”
*Advances in Methods and Practices in Psychological Science*, **4**(2),
2515245920970949. [doi:10.1177/2515245920970949](NA).”

If you just add plain text that isn’t a bibentry or in bibtex format,
you will usually just get the text back.

``` r
format_ref("My wierd citation (2025)")
```

\[1\] “My wierd citation (2025)”

We will work towards future versions being able to collate all
references in a report displayed using this method to automatically
create a linked reference section,

### Return

Structure the returned values in a list. The names below are reserved
for specific uses in the report and piped workflow, but you can also
return other objects for your own purposes.

``` r
  # return a list ----
  list(
    table = table,
    summary_table = summary_table,
    na_replace = 0,
    traffic_light = tl,
    summary_text = summary_text,
    report = report
  )
```
