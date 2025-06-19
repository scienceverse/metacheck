
<!-- README.md is generated from README.Rmd. Please edit that file -->

# papercheck <img src="man/figures/logo.png" align="right" height="120" alt="" />

<!-- badges: start -->

[![Lifecycle:
experimental](https://img.shields.io/badge/lifecycle-experimental-orange.svg)](https://lifecycle.r-lib.org/articles/stages.html#experimental)

[![Codecov test
coverage](https://codecov.io/gh/scienceverse/papercheck/graph/badge.svg)](https://app.codecov.io/gh/scienceverse/papercheck)
<!-- badges: end -->

PaperCheck provides extendable and integrated tools for automatically
checking scientific papers for best practices.

Papercheck is developed by a collaborative team of researchers,
consisting of (from left to right in the picture below) [Lisa
DeBruine](https://debruine.github.io) (developer and maintainer) and
[Daniël Lakens](https://sites.google.com/site/lakens2/Home) (developer),
[René Bekkers](https://research.vu.nl/en/persons/rene-bekkers)
(collaborator and PI of [Transparency
Check](https://tdcc.nl/tdcc-ssh-challenge-projects/research-transparency-check)),
[Cristian
Mesquida](https://ssreplicationcentre.com/author/cristian-mesquida/)
(postdoctoral researcher), and Max Littel and Jakub Werner (research
assistants). Papercheck was initially developed by Lisa and Daniël in
2024 during Lisa’s visiting professorship at the Eindhoven Artificial
Intelligence Systems Institute
([EAISI](https://www.tue.nl/en/research/institutes/eindhoven-artificial-intelligence-systems-institute)).

<img
src="https://scienceverse.github.io/papercheck/articles/papercheck_team.png"
data-fig-alt="Faces of the team" />

Check out our series of [blog posts introducing
Papercheck](https://scienceverse.github.io/papercheck/articles/index.html)!

## Installation

You can install the development version of papercheck from
[GitHub](https://github.com/) with:

``` r
# install.packages("devtools")
devtools::install_github("scienceverse/papercheck")
```

You can launch an interactive shiny app version of the code below with:

``` r
papercheck_app()
```

## Example

``` r
library(papercheck)
#> 
#> 
#> *******************************************
#> ✅ Welcome to PaperCheck
#> For support and examples visit:
#> https://scienceverse.github.io/papercheck/
#> 
#> ⚠️ This is alpha software; please check any
#> results. False positives and negatives will
#> occur at unknown rates.
#> *******************************************
```

### Importing Papers

Convert a PDF to grobid XML format, then read it in as a paper object.

``` r
pdf <- demopdf()       # use the path of your own PDF
xml <- pdf2grobid(pdf) # requires a web connection & resource-intensive, 
                       # so save XMLs for use with scripts
paper <- read_grobid(xml)
```

### Batch Processing

The functions `pdf2grobid()` and `read_grobid()` also work on a folder
of files, returning a list of XML file paths or paper objects,
respectively. Most functions also work on a list of paper objects.

``` r
# read in all the XML files in the demo directory
grobid_dir <- demodir()
papers <- read_grobid(grobid_dir)
```

### Search Text

Search the returned text. The regex pattern below searches for text that
looks like statistical values (e.g., `N=313` or `p = 0.17`).

``` r
pattern <- "[a-zA-Z]+\\S*\\s*(=|<)\\s*[0-9\\.-]*\\d"
text <- search_text(paper, pattern, 
                    return = "match", 
                    perl = TRUE)
```

| text            | section | header  | div |   p |   s | id                  |
|:----------------|:--------|:--------|----:|----:|----:|:--------------------|
| M = 9.12        | results | Results |   3 |   1 |   2 | to_err_is_human.xml |
| M = 10.9        | results | Results |   3 |   1 |   2 | to_err_is_human.xml |
| t(97.7) = 2.9   | results | Results |   3 |   1 |   2 | to_err_is_human.xml |
| p = 0.005       | results | Results |   3 |   1 |   2 | to_err_is_human.xml |
| M = 5.06        | results | Results |   3 |   2 |   1 | to_err_is_human.xml |
| M = 4.5         | results | Results |   3 |   2 |   1 | to_err_is_human.xml |
| t(97.2) = -1.96 | results | Results |   3 |   2 |   1 | to_err_is_human.xml |
| p = 0.152       | results | Results |   3 |   2 |   1 | to_err_is_human.xml |

See [Getting
Started](https://scienceverse.github.io/papercheck/articles/papercheck.html#search-text)
for even more text search capabilities.

### Large Language Models

You can query the extracted text of papers with LLMs using
[groq](https://console.groq.com/docs/). See `?llm` for details of how to
get and set up your API key, choose an LLM, and adjust settings.

Use `search_text()` first to narrow down the text into what you want to
query. Below, we limited search to a paper’s method section, and
returned sentences that contains the word “power” and at least one
number. Then we asked an LLM to determine if this is an a priori power
analysis.

``` r
power <- psychsci[9] |>
  # sentences containing the word power
  search_text("power", section = "method")

# ask a specific question with specific response format
query <- 'Does this sentence report an a priori power analysis? Answer only the words "TRUE" or "FALSE".'

llm_power <- llm(power, query, seed = 8675309)
```

| text | answer | id |
|:---|:---|:---|
| For the first part of the task, 11 static visual images, one from each of the scenes in the film were presented once each on a black background for 2 s using Power-Point. | FALSE | 0956797615583071 |
| A sample size of 26 per group was required to ensure 80% power to detect this difference at the 5% significance level. | TRUE | 0956797615583071 |
| A sample size of 18 per condition was required in order to ensure an 80% power to detect this difference at the 5% significance level. | TRUE | 0956797615583071 |

See [Getting Started](articles/papercheck.html#large-language-models)
for an example with more detailed responses.

### Modules

Papercheck is designed modularly, so you can add modules to check for
anything. It comes with a set of pre-defined modules, and we hope people
will share more modules.

You can see the list of built-in modules with the function below.

``` r
module_list()
```

- all_p_values: List all p-values in the text, returning the matched
  text (e.g., ‘p = 0.04’) and document location in a table.
- all_urls: List all the URLs in the main text.
- effect_size: Detect t-tests and F-tests with missing effect sizes
- exact_p: List any p-values reported with insufficient precision (e.g.,
  p \< .05 or p = n.s.)
- marginal: List all sentences that describe an effect as ‘marginally
  significant’.
- osf_check: List all OSF links and whether they are open, closed, or do
  not exist.
- ref_consistency: Check if all references are cited and all citations
  are referenced
- retractionwatch: Flag any cited papers in the RetractionWatch database
- statcheck: Check consistency of p-values and test statistics

To run a built-in module on a paper, you can reference it by name.

``` r
p <- module_run(paper, "all_p_values")
```

| text      | section | header  | div |   p |   s | id                  |
|:----------|:--------|:--------|----:|----:|----:|:--------------------|
| p = 0.005 | results | Results |   3 |   1 |   2 | to_err_is_human.xml |
| p = 0.152 | results | Results |   3 |   2 |   1 | to_err_is_human.xml |
| p \> .05  | results | Results |   3 |   2 |   2 | to_err_is_human.xml |

See the [Modules
Vignette](https://scienceverse.github.io/papercheck/articles/modules.html)
for more detail on the built-in modules, and [Creating
Modules](https://scienceverse.github.io/papercheck/articles/creating_modules.html)
for examples of how to make your own modules.

### Reports

You can generate a report from any set of modules. The default set is
`c("exact_p", "marginal", "effect_size", osf_check", "retractionwatch", "ref_consistency")`

``` r
paper_path <- report(paper, 
                     output_format = "html", 
                     output_file = "example_report")
```

[Example Report](articles/report-example.html)
