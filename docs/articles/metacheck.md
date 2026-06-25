# metacheck

## Installation

You can install the development version of metacheck from
[GitHub](https://github.com/scienceverse/metacheck) with:

``` r

pak::pkg_install("scienceverse/metacheck")
```

``` r

library(metacheck)
```

You can launch a simple shiny app that creates a report from a PDF, with
options to control what information is sent to or retrieved from
external servers.

``` r

metacheck::report_app()
```

### Load from PDF

The function
[`convert()`](https://scienceverse.github.io/metacheck/reference/convert.md)
can read PDF files and save them in [JSON
format](https://www.scienceverse.org/schema/paper.json). This requires
an internet connection and takes a few seconds per paper, so should only
be done once and the results saved for later use. If you don’t supply an
`api_url`, we check a [list of active
servers](https://www.scienceverse.org/metacheck/convert.json) and choose
the first available.

You can also check the reference section against crossref, which allows
you to find potential errors or omissions. This can take some time, and
you can always add it later with the
[`add_bib_match()`](https://scienceverse.github.io/metacheck/reference/add_bib_match.md)
function.

``` r

pdf_file <- demofile("pdf")
json_file <- convert(file_path = pdf_file, 
                     save_path = "converted",
                     crossref_lookup = TRUE)
```

You can set up your own local grobid server following instructions from
<https://grobid.readthedocs.io/>. The easiest way is to use Docker.

``` bash
docker run --rm --init --ulimit core=0 -p 8070:8070 lfoppiano/grobid:0.9.0
```

Then you can set your api_url to the local path <http://localhost:8070>.
If the method is the default “auto” and you don’t explicitly set the
api_url,
[`convert()`](https://scienceverse.github.io/metacheck/reference/convert.md)
will default to a local server if one is detected.

``` r

json_file <- convert(file_path = pdf_file, 
                     save_path = "converted",
                     method = "grobid",
                     api_url = "http://localhost:8070")
```

### Load from JSON

The function
[`read()`](https://scienceverse.github.io/metacheck/reference/read.md)
can read converted JSON files.

``` r

paper <- read(json_file)
```

### Load from non-PDF document

To take advantage of grobid’s ability to parse references and other
aspects of papers, for now the best way is to convert your papers to
PDF. We will introduce our custom backend, bibr, soon and this will be
able to convert DOC and DOCX files directly.

### Batch Processing

The functions
[`convert()`](https://scienceverse.github.io/metacheck/reference/convert.md)
and
[`read()`](https://scienceverse.github.io/metacheck/reference/read.md)
also work on a folder of files, or a vector of paths, returning a list
of JSON file paths or paper objects, respectively.

## Paper Components

Paper objects contain a lot of structured information, including info,
references, and citations.

### Info

``` r

paper$info
```

    #>                                         title keywords
    #> 1 To Err is Human: An Empirical Investigation       NA
    #>                               doi        file_hash input_format
    #> 1 10.32614/10.5281/zenodo.2669586 62ede2964b174f6d grobid 0.9.0
    #>                                                                     file_name
    #> 1 /Users/debruine/rproj/scienceverse/metacheck/inst/demos/to_err_is_human.xml
    #>   bibr_version paper_type paper_type_confidence oecd_l1 oecd_l2 oecd_confidence
    #> 1         10.0    unknown                     0    <NA>    <NA>              NA

### Bibliography

The bibliography is provided in a tabular format.

``` r

paper$bib
```

| bib_type | doi | title | authors | editors | publisher | year | volume | issue | first_page | last_page | container | bib_id | year_suffix | text_id |
|:---|:---|:---|:---|:---|:---|---:|:---|:---|:---|:---|:---|---:|:---|---:|
| book | 10.32614/10.5281/zenodo.2669586 | Faux: Simulation for Factorial Designs | Debruine, Lisa |  |  | 2025 |  |  | NA | NA | NA | 0 |  | 33 |
| article | 10.1037/0003-066x.54.6.408 | The Origins of Sex Differences in Human Behavior: Evolved Dispositions Versus Social Roles | Eagly, Alice H; Wood, Wendy |  |  | 1999 | 54 | 6 | 408 | 423 | American Psychologist | 1 |  | 34 |
| article | 10.1177/0956797614520714 | Evil Genius? How Dishonesty Can Lead to Greater Creativity | Gino, Francesca; Wiltermuth, Scott S |  |  | 2014 | 25 | 4 | 973 | 981 | Psychological Science | 2 |  | 35 |
| article |  | Equivalence Testing for Psychological Research | Lakens, Daniël |  |  | 2018 | 1 |  | 259 | 270 | Psychological Science | 3 |  | 36 |
| article | 10.0000/0123456789 | Human Error Is a Symptom of a Poor Design | Smith, F |  |  | 2021 |  |  | NA | NA | Journal of Journals | 4 |  | 37 |

### Cross-References

Cross-references are also provided in a tabular format, with `xref_id`
to match the bibliography table.

``` r

paper$xref
```

| xref_id | xref_type | contents                   | text_id |
|--------:|:----------|:---------------------------|--------:|
|       2 | bibr      | (Gino and Wiltermuth 2014) |       4 |
|       0 | foot      | foot_0                     |       8 |
|       0 | figure    | 2                          |      17 |
|      NA | table     | 1                          |      25 |
|       1 | figure    | 1                          |      25 |

### Batch

The `psychsci` built-in dataset contains all 250 open-access articles
published in *Psychological Science* from 2004 to 2014. *Psychological
Science* is the flagship journal of the Association for Psychological
Science and publishes empirical research from all subfields of
psychology. The PDFs were converted to metacheck paper objects using
[`convert()`](https://scienceverse.github.io/metacheck/reference/convert.md)
and stored in the package. This dataset is used throughout the
documentation to demonstrate how to work with a list of papers.

There are functions to combine the information from a list of papers,
like `psychsci`.

``` r

paper_table(psychsci[1:5], "info", c("title", "doi"))
```

    #> # A tibble: 5 × 3
    #>   title                                                           doi   paper_id
    #>   <chr>                                                           <chr> <chr>   
    #> 1 Mirror neurons, originally discovered in macaque monkeys using… 10.1… 0956797…
    #> 2 Beyond Gist: Strategic and Incremental Information Accumulatio… 10.1… 0956797…
    #> 3 Serotonin and Social Norms: Tryptophan Depletion Impairs Socia… 10.1… 0956797…
    #> 4 Action-Specific Disruption of Perceptual Confidence             10.1… 0956797…
    #> 5 Emotional Vocalizations Are Recognized Across Cultures Regardl… 10.1… 0956797…

``` r

paper_table(psychsci[1:5], "bib") |>
  dplyr::filter(!is.na(doi), doi != "")
```

    #> # A tibble: 6 × 16
    #>   bib_type doi     title authors editors publisher  year volume issue first_page
    #>   <chr>    <chr>   <chr> <chr>   <chr>   <chr>     <int> <chr>  <chr> <chr>     
    #> 1 article  10.338… The … Zylber… ""      ""         2012 6      ""    NA        
    #> 2 article  10.103… Stro… Ekman,… ""      ""         1994 115    ""    268       
    #> 3 article  10.117… Cult… Gendro… ""      ""         2014 25     ""    911       
    #> 4 article  10.103… Is t… Russel… ""      ""         1994 115    ""    102       
    #> 5 article  10.108… Perc… Sauter… ""      ""         2010 63     ""    2251      
    #> 6 article  10.107… Cros… Sauter… ""      ""         2010 107    ""    2408      
    #> # ℹ 6 more variables: last_page <chr>, container <chr>, bib_id <int>,
    #> #   year_suffix <chr>, text_id <int>, paper_id <chr>

## Search Text

You can access a parsed table of the full text of the paper via
`paper$text`, but you may find it more convenient to use the function
[`text_search()`](https://scienceverse.github.io/metacheck/reference/text_search.md).
The defaults return a data table of each sentence, with the section
type, header, div, paragraph and sentence numbers, and file name. (The
section type is a best guess from the headers, so may not always be
accurate.)

``` r

text <- text_search(paper)
```

| text | text_id | paragraph_id | section_id | page_number | formatted | paper_id | header | section_type |
|:---|---:|---:|---:|---:|:---|:---|:---|:---|
| This paper demonstrates some good and poor practices for use with the {metacheck} R package. | 1 | 1 | 0 | NA | NA | to_err_is_human | Abstract | abstract |
| All data are simulated. | 2 | 1 | 0 | NA | NA | to_err_is_human | Abstract | abstract |
| The paper shows examples of (1) open and closed OSF links; (2a) citation of retracted papers, (2b) citations without a doi, (2c) citations with Pubpeer comments, (2d) citations in the FLoRA replication database, and (2e) missing/mismatched/incorrect citations and references; (3a) R files with code on GitHub that do not load libraries in one location, (3b) load files that are not shared in the repository, (3c) lack comments, and (3d) have absolute file paths; (4) imprecise reporting of non-significant p-values; (5) tests with and without effect sizes; (6) use of “marginally significant” to describe non-significant findings; (7) a power analysis reporting some of the essential attributes; and (8) retrieving information from preregistrations. | 3 | 1 | 0 | NA | NA | to_err_is_human | Abstract | abstract |
| Although intentional dishonesty might be a successful way to boost creativity (Gino and Wiltermuth 2014), it is safe to say most mistakes researchers make are unintentional. | 4 | 2 | 1 | NA | Although intentional dishonesty might be a successful way to boost creativity (Gino and Wiltermuth 2014), it is safe to say most mistakes researchers make are unintentional. | to_err_is_human | \[div-01\] | intro |
| From a human factors perspective, human error is a symptom of a poor design (Smithy, 2020). | 5 | 2 | 1 | NA | NA | to_err_is_human | \[div-01\] | intro |
| Automation can be used to check for errors in scientific manuscripts, and inform authors about possible corrections. | 6 | 2 | 1 | NA | NA | to_err_is_human | \[div-01\] | intro |

### Pattern

You can search for a specific word or phrase by setting the `pattern`
argument. The pattern is a regex string by default; set `fixed = TRUE`
if you want to find exact text matches.

``` r

text <- text_search(paper, pattern = "metacheck")
```

| text | text_id | paragraph_id | section_id | page_number | formatted | paper_id | header | section_type |
|:---|---:|---:|---:|---:|:---|:---|:---|:---|
| This paper demonstrates some good and poor practices for use with the {metacheck} R package. | 1 | 1 | 0 | NA | NA | to_err_is_human | Abstract | abstract |
| In this study we examine the usefulness of metacheck to improve best practices. | 7 | 2 | 1 | NA | NA | to_err_is_human | \[div-01\] | intro |

### Return

Set `return` to one of “sentence”, “paragraph”, “section”, or “match” to
control what gets returned.

``` r

text <- text_search(paper, "GitHub", 
                    return = "paragraph")
```

| text | text_id | paragraph_id | section_id | page_number | formatted | paper_id | header | section_type |
|:---|:---|---:|---:|:---|:---|:---|:---|:---|
| This paper demonstrates some good and poor practices for use with the {metacheck} R package. All data are simulated. The paper shows examples of (1) open and closed OSF links; (2a) citation of retracted papers, (2b) citations without a doi, (2c) citations with Pubpeer comments, (2d) citations in the FLoRA replication database, and (2e) missing/mismatched/incorrect citations and references; (3a) R files with code on GitHub that do not load libraries in one location, (3b) load files that are not shared in the repository, (3c) lack comments, and (3d) have absolute file paths; (4) imprecise reporting of non-significant p-values; (5) tests with and without effect sizes; (6) use of “marginally significant” to describe non-significant findings; (7) a power analysis reporting some of the essential attributes; and (8) retrieving information from preregistrations. | NA | 1 | 0 | NA | NA | to_err_is_human | Abstract | abstract |
| Data and analysis code is available on GitHub from <https://github.com/Lak-ens/to_err_is_human> and from <https://researchbox.org/4377>. | NA | 8 | 5 | NA | NA | to_err_is_human | Data Availability | availability |

### Regex matches

You can also return just the matched text from a regex search by setting
`return = "match"`. The extra `...` arguments in
[`text_search()`](https://scienceverse.github.io/metacheck/reference/text_search.md)
are passed to [`grep()`](https://rdrr.io/r/base/grep.html), so
`perl = TRUE` allows you to use more complex regex, like below.

``` r

pattern <- "[a-zA-Z]\\S*\\s*(=|<)\\s*[0-9\\.,-]*\\d"
text <- text_search(paper, pattern, return = "match", perl = TRUE)
```

| text | text_id | paragraph_id | section_id | page_number | formatted | paper_id | header | section_type |
|:---|---:|---:|---:|---:|:---|:---|:---|:---|
| M = 9.12 | 15 | 4 | 3 | NA | NA | to_err_is_human | Procedure | method |
| M = 10.9 | 15 | 4 | 3 | NA | NA | to_err_is_human | Procedure | method |
| t(97.7) = 2.9 | 15 | 4 | 3 | NA | NA | to_err_is_human | Procedure | method |
| p = 0.005 | 15 | 4 | 3 | NA | NA | to_err_is_human | Procedure | method |
| d =0.59 | 15 | 4 | 3 | NA | NA | to_err_is_human | Procedure | method |
| M = 5.06 | 16 | 5 | 3 | NA | NA | to_err_is_human | Procedure | method |
| M = 4.5 | 16 | 5 | 3 | NA | NA | to_err_is_human | Procedure | method |
| t(97.2) = -1.96 | 16 | 5 | 3 | NA | NA | to_err_is_human | Procedure | method |
| p =0.152 | 16 | 5 | 3 | NA | NA | to_err_is_human | Procedure | method |
| N = 50 | 24 | 11 | 7 | NA | NA | to_err_is_human | Power Analysis | annex |
| pwr::pwr.t.test(n = 50 | 27 | 14 | 8 | NA | NA | to_err_is_human | Results | annex |
| power = 0.8 | 27 | 14 | 8 | NA | NA | to_err_is_human | Results | annex |

### Expand Text

You can expand the text returned by
[`text_search()`](https://scienceverse.github.io/metacheck/reference/text_search.md)
or a module with
[`text_expand()`](https://scienceverse.github.io/metacheck/reference/text_expand.md).

``` r

marginal <- text_search(paper, "marginal") |>
  text_expand(paper, plus = 1, minus = 1)

marginal[, c("text", "expanded")]
```

    #> # A tibble: 2 × 2
    #>   text                                                                  expanded
    #>   <chr>                                                                 <chr>   
    #> 1 "The paper shows examples of (1) open and closed OSF links; (2a) cit… "All da…
    #> 2 "On average researchers in the experimental condition found the app … "On ave…

## Large Language Models

**LLM use is entirely optional.** Metacheck follows the principle that
AI should be opt-in, restricted to classification of existing text, and
used only where it provides clear benefits that cannot be achieved with
other methods such as regular expressions. The vast majority of modules
work without any LLM at all.

Currently, the only module that will unlock substantial extra
functionality when an lmm is used is the **`power` module**, which can
read sentences about power analyses and extract structured information
(test type, sample size, effect size, etc.) that would be difficult to
capture reliably with regular expressions alone.

### Option 1: Run a model locally with Ollama (recommended)

The recommended approach is to run an AI model on your own computer
using [Ollama](https://ollama.com), a free open-source tool. This means:

- No API key or account required
- No data leaves your computer
- No usage costs or rate limits

The trade-off is speed — your computer does the processing, so it is
slower than a cloud API, especially the first time a model loads. See
the dedicated [Local AI with
Ollama](https://scienceverse.github.io/metacheck/articles/ollama.md)
article for full setup instructions. In brief:

1.  Download and install Ollama from <https://ollama.com/download>
2.  Pull a model, e.g. `ollama pull llama3.2` in a terminal
3.  In R, set metacheck to use it:

``` r

llm_model("ollama/llama3.2")
llm_use(TRUE)
```

### Option 2: Use a cloud API

You can also use any model supported by
[ellmer](https://ellmer.tidyverse.org/) via a cloud API. This is faster
but requires an account and API key with a provider, and sends text to
an external service.

Get an API key from your preferred provider
(e.g. <https://console.groq.com/keys>) and add it to your `.Renviron`
file (use
[`usethis::edit_r_environ()`](https://usethis.r-lib.org/reference/edit.html)
to open it):

``` bash
GROQ_API_KEY="sk-proj-abcdefghijklmnopqrs0123456789ABCDEFGHIJKLMNOPQRS"
```

When metacheck starts it checks for API keys in `.Renviron` and sets the
model automatically. You can also set it manually:

``` r

llm_model()                              # check which model is currently set
llm_model("groq")                        # use ellmer's default Groq model
llm_model("groq/llama-3.3-70b-versatile") # use a specific model
```

A list of available models for a provider:

### LLM Queries

You can query the extracted text of papers with LLMs. See
[`?llm`](https://scienceverse.github.io/metacheck/reference/llm.md) for
details of how to get and set up your API key, choose an LLM, and adjust
settings.

Use
[`text_search()`](https://scienceverse.github.io/metacheck/reference/text_search.md)
first to narrow down the text into what you want to query. Below, we
limited search to the first ten papers, and returned sentences that
contains the word “power” and at least one number. Then we asked an LLM
to determine if this is an a priori power analysis, and if so, to return
some relevant values in a JSON-structured format.

``` r

power <- psychsci[1:10] |>
  # sentences containing the word power
  text_search("power") |>
  # and containing at least one number
  text_search("[0-9]") 

# ask a specific question with specific response format
system_prompt <- 'Does this sentence report an a priori power analysis? If so, return the test, sample size, critical alpha criterion, power level, effect size and effect size metric plus any other relevant parameters, in JSON format like:

{
  "apriori": true, 
  "test": "paired samples t-test", 
  "sample": 20, 
  "alpha": 0.05, 
  "power": 0.8, 
  "es": 0.4, 
  "es_metric": "cohen\'s D"
}

If not, return {"apriori": false}

Answer only in valid JSON format, starting with { and ending with }.'

llm_power <- llm(power, system_prompt)
```

### Expand JSON

It is useful to ask an LLM to return data in JSON structured format, but
can be frustrating to extract the data, especially where the LLM makes
syntax mistakes. The function
[`json_expand()`](https://scienceverse.github.io/metacheck/reference/json_expand.md)
tries to expand a column with a JSON-formatted response into columns and
deals with it gracefully (sets an ‘error’ column to “parsing error”) if
there are errors. It also fixes column data types, if possible.

``` r

llm_response <- json_expand(llm_power, "answer") |>
  dplyr::select(text, apriori:es_metric)
```

| text | apriori | test | sample | alpha | power | es | es_metric |
|:---|:---|:---|---:|---:|---:|---:|:---|
| It is possible that less-consistent effects were observed on trials with errors because of reduced power to detect an effect on these trials, which by design were less numerous (~25%). | FALSE | NA | NA | NA | NA | NA | NA |
| Figure 1 shows that CY had very little predictive power for CLIM, but the fit in the transposed plot has an obvious bell-shaped curve. | FALSE | NA | NA | NA | NA | NA | NA |
| Sample size was calculated with an a priori power analysis, using the effect sizes reported by Küpper et al. (2014), who used identical procedures, materials, and dependent measures. | TRUE | NA | NA | NA | NA | NA | NA |
| We determined that a minimum sample size of 7 per group would be necessary for 95% power to detect an effect. | TRUE | t-test | 7 | 0.050 | 0.95 | NA | NA |
| For the first part of the task, 11 static visual images, one from each of the scenes in the film were presented once each on a black background for 2 s using Power-Point. | FALSE | NA | NA | NA | NA | NA | NA |
| A sample size of 26 per group was required to ensure 80% power to detect this difference at the 5% significance level. | TRUE | two-sample t-test | 26 | 0.050 | 0.80 | NA | NA |
| A sample size of 18 per condition was required in order to ensure an 80% power to detect this difference at the 5% significance level. | TRUE | t-test | 18 | 0.050 | 0.80 | NA | NA |
| The 13,500 selected loan requests conservatively achieved a power of .98 for an effect size of .07 at an alpha level of .05. | TRUE |  | 13500 | 0.050 | 0.98 | 0.07 | NA |
| On the basis of simulations over a range of expected effect sizes for contrasts of fMRI activity, we estimated that a sample size of 24 would provide .80 power at a conservative brainwide alpha threshold of .002 (although such thresholds ideally should be relaxed for detecting activity in regions where an effect is predicted). | TRUE | fMRI activity contrast | 24 | 0.002 | 0.80 | NA | NA |
| Stimulus sample size was determined via power analysis of the sole existing similar study, which used neural activity to predict Internet downloads of music (Berns & Moore, 2012). | TRUE | NA | NA | NA | NA | NA | NA |
| The effect size from that study implied that a sample size of 72 loan requests would be required to achieve .80 power at an alpha level of .05. | TRUE |  | 72 | 0.050 | 0.80 | NA | NA |
| Categorical ratings of the emotional expressions in the loan photographs had a similarly powerful impact on loan-request success; requests with “happy” photographs received \$5.15 more per hour than requests with “sad” photographs, on average; they achieved full funding in 7.6% less time. | FALSE | NA | NA | NA | NA | NA | NA |
| Although previous research has provided mixed evidence about the impact of positive versus negative affect on charitable giving (Andreoni, 1990;Small & Verrochi, 2009), by simultaneously assessing affect at both Internet-aggregate and laboratory-sample levels of analysis, our studies provide consistent evidence that photograph-elicited positive arousal most powerfully promoted lending rates and outcomes (Tables 1 and 2, Fig. 2a, and Fig. | FALSE | NA | NA | NA | NA | NA | NA |

### Rate Limiting

The [`llm()`](https://scienceverse.github.io/metacheck/reference/llm.md)
function makes a separate query for each row in a data frame from
[`text_search()`](https://scienceverse.github.io/metacheck/reference/text_search.md).
(Using parallel functions in ellmer can be more efficient but currently
does not associate structured output correctly when inputs may have 0 or
more outputs.) To prevent accidentally making too many calls because of
errors in your code, a default limit of 30 queries is set, which you can
change:

``` r

llm_max_calls(30)
```

## Repository Functions

Metacheck can find links to research repositories in a paper, retrieve
the list of files they contain, and download those files for further
checking. Four online services are supported, as well as local folders
on your own computer.

| Repository | Link function | Info / file list | Download |
|----|----|----|----|
| OSF | [`osf_links()`](https://scienceverse.github.io/metacheck/reference/osf_links.md) | [`osf_info()`](https://scienceverse.github.io/metacheck/reference/osf_info.md) | [`osf_file_download()`](https://scienceverse.github.io/metacheck/reference/osf_file_download.md) |
| GitHub | [`github_links()`](https://scienceverse.github.io/metacheck/reference/github_links.md) | [`github_files()`](https://scienceverse.github.io/metacheck/reference/github_files.md), [`github_info()`](https://scienceverse.github.io/metacheck/reference/github_info.md) | — |
| ResearchBox | [`rbox_links()`](https://scienceverse.github.io/metacheck/reference/rbox_links.md) | [`rbox_info()`](https://scienceverse.github.io/metacheck/reference/rbox_info.md) | [`rbox_file_download()`](https://scienceverse.github.io/metacheck/reference/rbox_file_download.md) |
| Zenodo | [`zenodo_links()`](https://scienceverse.github.io/metacheck/reference/zenodo_links.md) | [`zenodo_info()`](https://scienceverse.github.io/metacheck/reference/zenodo_info.md) | [`zenodo_file_download()`](https://scienceverse.github.io/metacheck/reference/zenodo_file_download.md) |
| Local folder | — | [`local_files()`](https://scienceverse.github.io/metacheck/reference/local_files.md) | — |

The `repo_check` and `code_check` modules use all of these
automatically: `repo_check` finds all repository links in a paper and
builds a unified file list, and `code_check` then analyses any code
files in that list. See the
[GitHub](https://scienceverse.github.io/metacheck/articles/github.md)
and [Local
Files](https://scienceverse.github.io/metacheck/articles/local-files.md)
articles for more detail on those two sources.

### OSF Links and IDs

Get any OSF links from a paper or list of papers.

``` r

links <- osf_links(psychsci)

links$href |> unique() |> head()
```

    #> [1] "https://osf.io/e2aks/"                                           
    #> [2] "https://osf.io/tvyxz/wiki/view/"                                 
    #> [3] "https://osf.io/t9j8e/?view_only=f171281f212f4435917b16a9e581a73b"
    #> [4] "https://osf.io/tvyxz/wiki/1.%20View%20the%20Badges/"             
    #> [5] "https://osf.io/eky4s/"                                           
    #> [6] "https://osf.io/xgwhk"

You can see that some of them have rogue spaces or view-only links. The
function
[`osf_check_id()`](https://scienceverse.github.io/metacheck/reference/osf_check_id.md)
takes most formats of OSF links (with or without <https://> and osf.io/,
as well as the 25-character waterbutler IDs) and converts them to short
IDs.

``` r

osf_ids <- osf_check_id(links$href) |> unique()

head(osf_ids)
```

    #> [1] "e2aks"                                           
    #> [2] "tvyxz"                                           
    #> [3] "t9j8e?view_only=f171281f212f4435917b16a9e581a73b"
    #> [4] "eky4s"                                           
    #> [5] "xgwhk"                                           
    #> [6] "5t0b7"

However, all of the `osf_***()` functions fix IDs for you and handle
duplicate IDs without making extra API calls, so you don’t need to add
this step to most workflows.

### OSF Info

Get basic information about OSF links, such as the name, description,
osf_type (nodes, files, preprints, registrations, users, set to
“private” if you don’t have authorisation to view it, and “invalid” if
the ), whether it is public

``` r

info <- osf_info(links[1:6, "href"])

info[, c("href","osf_id", "osf_type", "public", "category")]
```

| href | osf_id | osf_type | public | category |
|:---|:---|:---|:---|:---|
| <https://osf.io/e2aks/> | e2aks | nodes | TRUE | project |
| <https://osf.io/tvyxz/wiki/view/> | tvyxz | nodes | TRUE | project |
| <https://osf.io/tvyxz/wiki/view/> | tvyxz | nodes | TRUE | project |
| <https://osf.io/t9j8e/?view_only=f171281f212f4435917b16a9e581a73b> | t9j8e?view_only=f171281f212f4435917b16a9e581a73b | nodes | FALSE | project |
| <https://osf.io/tvyxz/wiki/1.%20View%20the%20Badges/> | tvyxz | nodes | TRUE | project |
| <https://osf.io/eky4s/> | eky4s | nodes | TRUE | project |

View-only links may be listed in the table as public = FALSE if they
haven’t been made publicly available. This means they are not searchable
and only accessible with the view_only link.

You can set the argument `recursive = TRUE` to also retrieve information
about all nodes and files that are contained by the OSF link.

``` r

all_contents <- osf_info(links$href[1], recursive = TRUE)

all_contents[, c("osf_id", "name")]
```

| osf_id | name |
|:---|:---|
| e2aks | Action-specific disruption of perceptual confidence |
| 7jh5v | Data |
| pj4e8 | Analysis scripts |
| 553e66b48c5e4a219919e0e7 | osfstorage |
| 553e58658c5e4a219919a627 | osfstorage |
| 553e7e168c5e4a21991a4dab | osfstorage |
| 553e58658c5e4a219919a628 | Mratio_all.txt |
| 553e58658c5e4a219919a629 | allData_orientation.txt |
| 553e58658c5e4a219919a62a | allData_contrast_M1.txt |
| 553e58658c5e4a219919a62b | allData_contrast_PMC.txt |
| 553e58658c5e4a219919a62c | Mratio_contrast_M1.txt |
| 553e7e168c5e4a21991a4dac | tms_analysis.R |

### Download OSF Files

OSF projects let you organise information into nested components, and
files within those components. Therefore, to retrieve all of the files
associate with a project, you may need to navigate to several components
and download zip files for the files from each components, then
reorganise and rename the downloaded folders.

The function
[`osf_file_download()`](https://scienceverse.github.io/metacheck/reference/osf_file_download.md)
does all of this for you, recreating a folder structure based on the
component names and downloading all files smaller than `max_file_size`
(defaults to 10 MB) up to a total size of `max_download_size` (defaults
to 100 MB).

``` r

osf_file_download(osf_id = "pngda",
                  download_to = ".", 
                  max_file_size = 1, 
                  max_download_size = 10)
```

    Starting retrieval for pngda
    - omitting metacheck.png (1.5MB)
    Downloading files [=====================] 24/24 00:00:35

``` r

list.files("pngda", recursive = TRUE)
```

    #>  [1] "Data/Individual/data-01.csv"                         
    #>  [2] "Data/Individual/data-02.csv"                         
    #>  [3] "Data/Individual/data-03.csv"                         
    #>  [4] "Data/Individual/data-04.csv"                         
    #>  [5] "Data/Individual/data-05.csv"                         
    #>  [6] "Data/Individual/data-06.csv"                         
    #>  [7] "Data/Individual/data-07.csv"                         
    #>  [8] "Data/Individual/data-08.csv"                         
    #>  [9] "Data/Individual/data-09.csv"                         
    #> [10] "Data/Individual/data-10.csv"                         
    #> [11] "Data/Individual/data-11.csv"                         
    #> [12] "Data/Individual/data-12.csv"                         
    #> [13] "Data/Individual/data-13.csv"                         
    #> [14] "Data/Individual/data-14.csv"                         
    #> [15] "Data/Processed Data/processed-data.csv"              
    #> [16] "Data/Raw Data/data.xlsx"                             
    #> [17] "Data/Raw Data/nest-1/nest-2/nest-3/nest-4/test-4.txt"
    #> [18] "Data/Raw Data/nest-1/nest-2/nest-3/test-3.txt"       
    #> [19] "Data/Raw Data/nest-1/nest-2/test-2.txt"              
    #> [20] "Data/Raw Data/nest-1/README"                         
    #> [21] "Data/Raw Data/nest-1/test-1.txt"                     
    #> [22] "Data/Raw Data/README"                                
    #> [23] "README"

### GitHub, ResearchBox, and Zenodo

The same pattern — find links, retrieve file lists, optionally download
— applies to the other three services.

``` r

# GitHub
gh_links  <- github_links(paper)
gh_files  <- github_files(gh_links$href, recursive = TRUE)

# ResearchBox
rb_links  <- rbox_links(paper)
rb_info   <- rbox_info(rb_links)

# Zenodo
z_links   <- zenodo_links(paper)
z_info    <- zenodo_info(z_links)
```

See the
[GitHub](https://scienceverse.github.io/metacheck/articles/github.md)
article for a detailed walkthrough of the GitHub functions.

### Local files

If files are not in an online repository — for example because you
downloaded a zip archive from a reviewer submission, or because the
authors used a service not yet supported — you can point metacheck at a
local folder instead.

``` r

result <- module_run(test_paper(), "code_check",
                     local_path = "path/to/downloaded/files")
```

See the [Local
Files](https://scienceverse.github.io/metacheck/articles/local-files.md)
article for full details, including how to handle cloud-synced folders
and multiple paths at once.

## Modules

metacheck is designed modularly, so you can add modules to check for
anything. It comes with a set of pre-defined modules, and we hope people
will share more modules.

### Module List

You can see the list of built-in modules with the function below.

``` r

module_list()
```

    #> 
    #> *** GENERAL ***
    #> 
    #> * all_urls: List all the URLs in the main text.
    #> * coi_check: Identify and extract Conflicts of Interest (COI) statements.
    #> * coi_check_oi: Identify and extract Conflicts of Interest (COI) statements.
    #> * funding_check: Identify and extract funding statements.
    #> * funding_check_oi: Identify and extract funding statements.
    #> * open_practices: This module searches for open data, code, materials, and registration statements.
    #> 
    #> *** METHOD ***
    #> 
    #> * causal_claims: Aims to identify the presence of random assignment, and lists sentences that make causal claims in title or abstract.
    #> * power: This module uses uses regular expressions to identify sentences that contain a statistical power analysis. If specified by the user, it also uses a large language module (LLM) to extract information reported in power analyses, including the statistical test, sample size, alpha level, desired level of power, and magnitude and type of effect size.
    #> * prereg_check: Retrieve information from preregistrations in a standardised way,
    #> and make them easier to check.
    #> 
    #> *** RESULTS ***
    #> 
    #> * all_p_values: List all p-values in the text, returning the matched text (e.g., 'p = 0.04') and document location in a table.
    #> * code_check: This module retrieves information from repositories checked by repo_check about code files (R, SAS, SPSS, Stata).
    #> * marginal: List all sentences that describe an effect as 'marginally significant'.
    #> * repo_check: This module retrieves information from repositories.
    #> * stat_check: Check consistency of p-values and test statistics
    #> * stat_effect_size: The Effect Size module checks if effect sizes are correctly reported in t-tests and F-tests.
    #> * stat_p_exact: List any p-values reported with insufficient precision (e.g., p < .05 or p = n.s.) or reported as exactly zero (e.g., p = .000).
    #> * stat_p_nonsig: This module checks for imprecisely reported p values. If p > .05 is detected, it warns for misinterpretations.
    #> 
    #> *** REFERENCE ***
    #> 
    #> * ref_accuracy: This module checks references for mismatches with CrossRef.
    #> * ref_consistency: Check if all references are cited and all citations are referenced
    #> * ref_miscitation: Check for frequently miscited papers. This module is just a proof of concept -- the miscite database is not yet populated with real examples.
    #> * ref_pubpeer: This module checks references and warns for citations that have comments on pubpeer (excluding Statcheck comments).
    #> * ref_replication: This module checks references and warns for citations of original studies for which replication or reproduction studies exist in the FLoRA database.
    #> * ref_retraction: This module checks references and warns for citations in the RetractionWatch Database.
    #> * ref_summary: Summarise information about each reference in a paper.
    #> 
    #> Use `module_help("module_name")` for help with a specific module

### Running modules

To run a built-in module on a paper, you can reference it by name.

``` r

p <- module_run(paper, "all_p_values")
```

| text | text_id | paragraph_id | section_id | page_number | formatted | paper_id | header | section_type | p_comp | p_value |
|:---|---:|---:|---:|---:|:---|:---|:---|:---|:---|---:|
| p = 0.005 | 15 | 4 | 3 | NA | NA | to_err_is_human | Procedure | method | = | 0.005 |
| p =0.152 | 16 | 5 | 3 | NA | NA | to_err_is_human | Procedure | method | = | 0.152 |
| p \> .05 | 17 | 6 | 3 | NA | There was no effect of experience on the reduction in errors when using the tool (p \> .05), as the correlation was non-significant (Figure 2). | to_err_is_human | Procedure | method | \> | 0.050 |

### Creating modules

You can create your own modules using R code. Modules can also contain
instructions for reporting, to give “traffic lights” for whether a check
passed or failed, and to include appropriate text feedback in a report.
See the [modules
vignette](https://scienceverse.github.io/metacheck/articles/modules.md)
for more details.

## Reports

You can generate a report from any set of modules. Check the function
help for the default set.

``` r

report(paper, output_format = "qmd")
```

See the [example
report](https://scienceverse.github.io/metacheck/report-example.md).
