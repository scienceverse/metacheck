
<!-- README.md is generated from README.Rmd. Please edit that file -->

# papercheck <img src="man/figures/logo.png" align="right" height="120" alt="" />

<!-- badges: start -->

[![Lifecycle:
experimental](https://img.shields.io/badge/lifecycle-experimental-orange.svg)](https://lifecycle.r-lib.org/articles/stages.html#experimental)
<!-- badges: end -->

The goal of papercheck is to automatically check scientific papers for
best practices.

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
```

Convert a PDF to grobid XML format, then read it in as a paper object.

``` r
pdf <- demopdf() # use the path of your own PDF
grobid <- pdf2grobid(pdf)
paper <- read_grobid(grobid)
```

### Search Text

Search the returned text. The regex pattern below searches for text that
looks like statistical values (e.g., `N=313` or `p = 0.17`).

``` r
pattern <- "[a-zA-Z]\\S*\\s*(=|<)\\s*[0-9\\.-]*\\d"
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

## Batch Processing

The functions `pdf2grobid()` and `read_grobid()` also work on a folder
of files, returning a list of XML file paths or paper objects,
respectively. The functions `search_text()`, `expand_text()` and `llm()`
also work on a list of paper objects.

``` r
# read in all the XML files in the demo directory
grobid_dir <- demodir()
papers <- read_grobid(grobid_dir)

# select sentences in the intros containing the text "previous"
previous <- search_text(papers, "previous", 
                        section = "intro", 
                        return = "sentence")
```

| text | section | header | div | p | s | id |
|:---|:---|:---|---:|---:|---:|:---|
| Royzman et al’s non-replication potentially calls into question the reliability of previously reported links between having an other-sex sibling and moral opposition to third-party sibling incest. | intro | Introduction | 1 | 3 | 3 | incest |
| Previous research has shown that making cost-benefit analyses of using statistical approaches explicit can influence researchers’ attitudes. | intro | \[div-01\] | 1 | 8 | 5 | prereg |
| When exploring difference in responses between previous experience with pre-registration, we see a clear trend where reasearchers who have pre-registered studies in their own research indicate pre-registration is more beneficial, and indicate higher a higher likelihood of pre-registering studies in the future, and higher percentage of studies for which they would consider pre-registering (see Table 2). | intro | Attitude | 3 | 7 | 1 | prereg |

### Large Language Models

You can query the extracted text of papers with LLMs using
[groq](https://console.groq.com/docs/).

Use `search_text()` first to narrow down the text into what you want to
query. Below, we limited search to the first two papers’ introduction
sections, and returned the full section. Then we asked an LLM “What is
the hypothesis of this study?”.

``` r
hypotheses <- search_text(papers[1:2], 
                          section = "intro", 
                          return = "section")
query <- "What is the hypothesis of this study? Answer as briefly as possible."
llm_hypo <- llm(hypotheses, query)
```

| id | answer |
|:---|:---|
| eyecolor | The hypothesis of this study is that people tend to choose romantic partners with eye colors similar to those of their opposite-sex parents, a phenomenon known as positive sexual imprinting. |
| incest | The hypothesis is that moral opposition to third-party sibling incest is greater among individuals with other-sex siblings than among individuals with same-sex siblings. |

### Modules

Papercheck is designed modularly, so you can add modules to check for
anything. It comes with a set of pre-defined modules, and we hope people
will share more modules.

You can see the list of built-in modules with the function below.

``` r
module_list()
```

- all-p-values: List all p-values in the text, returning the matched
  text (e.g., ‘p = 0.04’) and document location in a table.
- all-urls: List all the URLs in the main text
- imprecise-p: List any p-values reported with insufficient precision
  (e.g., p \< .05 or p = n.s.)
- llm-summarise: Generate a 1-sentence summary for each section
- marginal: List all sentences that describe an effect as ‘marginally
  significant’.
- osf-check: List all OSF links and whether they are open, closed, or do
  not exist.
- ref-consistency: Check if all references are cited and all citations
  are referenced
- retractionwatch: Flag any cited papers in the RetractionWatch database
- sample-size-ml: \[DEMO\] Classify each sentence for whether it
  contains sample-size information, returning only sentences with
  probable sample-size info.
- statcheck: Check consistency of p-values and test statistics

To run a built-in module on a paper, you can reference it by name.

``` r
p <- module_run(paper, "all-p-values")
```

| text      | section | header  | div |   p |   s | id                  |
|:----------|:--------|:--------|----:|----:|----:|:--------------------|
| p = 0.005 | results | Results |   3 |   1 |   2 | to_err_is_human.xml |
| p = 0.152 | results | Results |   3 |   2 |   1 | to_err_is_human.xml |
| p \> .05  | results | Results |   3 |   2 |   2 | to_err_is_human.xml |

### Reports

You can generate a report from any set of modules. The default set is
`c("imprecise-p", "marginal", "osf-check", "retractionwatch", "ref-consistency")`

``` r
paper_path <- report(paper, output_format = "html")
```
