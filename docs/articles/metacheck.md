# metacheck

## Installation

You can install the development version of metacheck from
[GitHub](https://github.com/scienceverse/metacheck) with:

``` r
# install.packages("devtools")
devtools::install_github("scienceverse/metacheck")
```

``` r
library(metacheck)
```

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

You can launch an interactive shiny app version of the code below with:

``` r
metacheck_app()
```

### Load from PDF

The function
[`pdf2grobid()`](https://scienceverse.github.io/metacheck/reference/pdf2grobid.md)
can read PDF files and save them in the [TEI](https://tei-c.org/) format
created by [grobid](https://grobid.readthedocs.io/). This requires an
internet connection and takes a few seconds per paper, so should only be
done once and the results saved for later use.

If the server is unavailable, you can [use a grobid web
interface](https://huggingface.co/spaces/kermitt2/grobid).

``` r
pdf_file <- demopdf()
xml_file <- pdf2grobid(pdf_file)
```

You can set up your own local grobid server following instructions from
<https://grobid.readthedocs.io/>. The easiest way is to use Docker.

``` bash
docker run --rm --init --ulimit core=0 -p 8070:8070 lfoppiano/grobid:0.8.2
```

Then you can set your grobid_url to the local path
<http://localhost:8070>.

``` r
xml_file <- pdf2grobid(pdf_file, grobid_url = "http://localhost:8070")
```

### Load from XML

The function
[`read()`](https://scienceverse.github.io/metacheck/reference/read.md)
can read XML files parsed by grobid or cermine, plus any XML files in
JATS-DTD APA or NLM formats.

``` r
paper <- read(xml_file)
```

XML files parsed by [cermine](http://cermine.ceon.pl) are not as good as
grobid at parsing papers, and omits figure and table captions.

``` r
cermine_xml_file <- system.file("psychsci/0956797620955209.cermine.xml",
                                package = "metacheck")
paper <- read(cermine_xml_file)
```

### Load from non-PDF document

To take advantage of grobid’s ability to parse references and other
aspects of papers, for now the best way is to convert your papers to
PDF. However, metacheck can read in plain text from a text/docx file
with
[`read()`](https://scienceverse.github.io/metacheck/reference/read.md).

``` r
filename <- system.file("extdata/to_err_is_human.docx", 
                        package = "metacheck")
paper_from_doc <- read_text(filename)
```

### Batch Processing

The functions
[`pdf2grobid()`](https://scienceverse.github.io/metacheck/reference/pdf2grobid.md)
and
[`read()`](https://scienceverse.github.io/metacheck/reference/read.md)
also work on a folder of files, returning a list of XML file paths or
paper objects, respectively. The functions
[`search_text()`](https://scienceverse.github.io/metacheck/reference/search_text.md),
[`expand_text()`](https://scienceverse.github.io/metacheck/reference/expand_text.md)
and [`llm()`](https://scienceverse.github.io/metacheck/reference/llm.md)
also work on a list of paper objects.

``` r
grobid_dir <- demodir()

papers <- read(grobid_dir)

hypotheses <- search_text(papers, "hypothesi", 
                          section = "intro", 
                          return = "paragraph")
```

## Paper Components

Paper objects contain a lot of structured information, including info,
references, and citations.

### Info

``` r
paper$info
```

    #> $title
    #> [1] "To Err is Human: An Empirical Investigation"
    #> 
    #> $description
    #> [1] "This paper demonstrates some good and poor practices for use with the {metacheck} R package and Shiny app. All data are simulated. The paper shows examples of (1) open and closed OSF links; (2a) citation of retracted papers, (2b) citations without a doi, (2c) citations with Pubpeer comments, (2d) citations in the FORTT replication database, and (2e) missing/mismatched/incorrect citations and references; (3a) R files with code on GitHub that do not load libraries in one location, (3b) load files that are not shared in the repository, (3c) lack comments, and (3d) have hard-coded files, (4) imprecise reporting of non-significant pvalues; (5) tests with and without effect sizes, (6) use of \"marginally significant\" to describe non-significant findings, and (7) retrieving information from preregistrations."
    #> 
    #> $keywords
    #> [1] ""
    #> 
    #> $doi
    #> [1] ""
    #> 
    #> $submission
    #> [1] ""
    #> 
    #> $filename
    #> [1] "/private/var/folders/t6/7x6md_5s2j5bfb324s784yzw0000gn/T/RtmpiPaWWJ/temp_libpath1edb7d40e629/metacheck/extdata/to_err_is_human.xml"

### Bibliography

The bibliography is provided in a tabular format.

``` r
paper$bib
```

| xref_id | ref | doi | bibtype | title | journal | year | authors | id |
|:---|:---|:---|:---|:---|:---|:---|:---|:---|
| b0 | 10.1098/rspb.1998.0380 , Menstrual cycle variation in women’s preferences for the scent of symmetrical men, S , W , Gangestad , R , Thornhill , Proceedings Biological Sciences , 1998 , 22 , 927-933 | 10.1098/rspb.1998.0380 | Article | Menstrual cycle variation in women’s preferences for the scent of symmetrical men | Proceedings Biological Sciences | 1998 | S W Gangestad, R Thornhill | to_err_is_human |
| b1 | 10.1177/0956797614520714 , Evil Genius? How Dishonesty Can Lead to Greater Creativity, F , Gino , S , S , Wiltermuth , Psychological Science , 2014 , 25 , 4 , 973-981 | 10.1177/0956797614520714 | Article | Evil Genius? How Dishonesty Can Lead to Greater Creativity | Psychological Science | 2014 | F Gino, S S Wiltermuth | to_err_is_human |
| b2 | 10.0000/0123456789 , Human error is a symptom of a poor design, F , Smith , Journal of Journals , 2021 , 0 , 0 , 0 | 10.0000/0123456789 | Article | Human error is a symptom of a poor design | Journal of Journals | 2021 | F Smith | to_err_is_human |
| b3 | Equivalence testing for psychological research , D , Lakens , Advances in Methods and Practices in Psychological Science, 2018 , 1 , 259-270 | NA | Article | Equivalence testing for psychological research | Advances in Methods and Practices in Psychological Science | 2018 | D Lakens | to_err_is_human |

### Cross-References

Cross-references are also provided in a tabular format, with `xref_id`
to match the bibliography table.

``` r
paper$xrefs
```

| xref_id | type | contents | text | id | section | div | p | s |
|:---|:---|:---|:---|:---|:---|---:|---:|---:|
| b1 | bibr | (Gino & Wiltermuth, 2014) | Although intentional dishonestly might be a successful way to boost creativity (Gino & Wiltermuth, 2014), it is safe to say most mistakes researchers make are unintentional. | to_err_is_human | intro | 1 | 1 | 1 |
| NA | bibr | (Smithy, 2020) | From a human factors perspective, human error is a symptom of a poor design (Smithy, 2020). | to_err_is_human | intro | 1 | 1 | 2 |

### Batch

There are functions to combine the infomation from a list of papers,
like the `psychsci` built-in dataset of 250 open access papers from
Psychological Science.

``` r
info_table(psychsci[1:5], c("title", "doi"))
```

    #> # A tibble: 5 × 3
    #>   id               title                                                   doi  
    #>   <chr>            <chr>                                                   <chr>
    #> 1 0956797613520608 Continuous Theta-Burst Stimulation Demonstrates a Caus… 10.1…
    #> 2 0956797614522816 Beyond Gist: Strategic and Incremental Information Acc… 10.1…
    #> 3 0956797614527830 Serotonin and Social Norms: Tryptophan Depletion Impai… 10.1…
    #> 4 0956797614557697 Action-Specific Disruption of Perceptual Confidence     10.1…
    #> 5 0956797614560771 Emotional Vocalizations Are Recognized Across Cultures… 10.1…

``` r
concat_tables(psychsci[1:5], "bib") |>
  dplyr::filter(!is.na(doi))
```

    #>   xref_id
    #> 1     b40
    #> 2      b0
    #> 3      b1
    #> 4      b2
    #> 5      b3
    #> 6      b4
    #>                                                                                                                                                                                                                                     ref
    #> 1                                                        10.3389/fnint.2012.00079/full, The construction of confidence in a perceptual decision, A, Zylberberg, P, Barttfeld, M, Sigman, Frontiers in Integrative Neuroscience, 2012, 6
    #> 2                                                             10.1037/0033-2909.115, Strong evidence for universals in facial expressions: A reply to Russell's mistaken critique, P, Ekman, Psychological Bulletin, 1994, 115, 268-287
    #> 3                                         10.1177/0956797613517239, Cultural relativity in perceiving emotion from vocalizations, M, Gendron, D, Roberson, J, M, Van Der Vyver, L, F, Barrett, Psychological Science, 2014, 25, 911-920
    #> 4                                      10.1037/0033-2909.115.1.102, Is there universal recognition of emotion from facial expression? A review of the cross-cultural studies, J, A, Russell, Psychological Bulletin, 1994, 115, 102-141
    #> 5                      10.1080/17470211003721642, Perceptual cues in non-verbal vocal expressions of emotion, D, A, Sauter, F, Eisner, A, J, Calder, S, K, Scott, The Quarterly Journal of Experimental Psychology, 2010, 63, 2251-2272
    #> 6 10.1073/pnas.0908239106, Crosscultural recognition of basic emotions through nonverbal emotional vocalizations, D, A, Sauter, F, Eisner, P, Ekman, S, K, Scott, Proceedings of the National Academy of Sciences, 2010, 107, 2408-2412
    #>                             doi bibtype
    #> 1 10.3389/fnint.2012.00079/full Article
    #> 2         10.1037/0033-2909.115 Article
    #> 3      10.1177/0956797613517239 Article
    #> 4   10.1037/0033-2909.115.1.102 Article
    #> 5     10.1080/17470211003721642 Article
    #> 6       10.1073/pnas.0908239106 Article
    #>                                                                                                      title
    #> 1                                                  The construction of confidence in a perceptual decision
    #> 2             Strong evidence for universals in facial expressions: A reply to Russell's mistaken critique
    #> 3                                             Cultural relativity in perceiving emotion from vocalizations
    #> 4 Is there universal recognition of emotion from facial expression? A review of the cross-cultural studies
    #> 5                                               Perceptual cues in non-verbal vocal expressions of emotion
    #> 6                    Crosscultural recognition of basic emotions through nonverbal emotional vocalizations
    #>                                            journal year
    #> 1            Frontiers in Integrative Neuroscience 2012
    #> 2                           Psychological Bulletin 1994
    #> 3                            Psychological Science 2014
    #> 4                           Psychological Bulletin 1994
    #> 5 The Quarterly Journal of Experimental Psychology 2010
    #> 6  Proceedings of the National Academy of Sciences 2010
    #>                                                 authors               id
    #> 1                   A Zylberberg, P Barttfeld, M Sigman 0956797614557697
    #> 2                                               P Ekman 0956797614560771
    #> 3 M Gendron, D Roberson, J M Van Der Vyver, L F Barrett 0956797614560771
    #> 4                                           J A Russell 0956797614560771
    #> 5           D A Sauter, F Eisner, A J Calder, S K Scott 0956797614560771
    #> 6              D A Sauter, F Eisner, P Ekman, S K Scott 0956797614560771

``` r
concat_tables(psychsci[1:40], "xrefs") |>
  dplyr::filter(grepl("replicat", text)) |>
  dplyr::count(id, text)
```

    #> # A tibble: 11 × 3
    #>    id               text                                                       n
    #>    <chr>            <chr>                                                  <int>
    #>  1 0956797614560771 1 We reanalyzed the data from the 29 Himba participan…     1
    #>  2 0956797615617779 Experiment 1b replicated the results of Experiment 1a…     1
    #>  3 0956797616647519 Note that the average expected ES found in Study 1 is…     1
    #>  4 0956797616647519 This is worrisome, as the results of our first study …     2
    #>  5 0956797616665351 At a group level, therefore, the perceptual data repl…     5
    #>  6 0956797617693326 Experiment 2 replicated Experiment 1 under conditions…     1
    #>  7 0956797617693326 Experiment 2 replicated these findings on a separate …     4
    #>  8 0956797617702699 A study by Papesh (2015) is particularly relevant: Sh…     2
    #>  9 0956797617705667 Experiment 3 therefore replicated the effects in a mi…     1
    #> 10 0956797617705667 This replicates the main finding of Beck et al. (2012…     1
    #> 11 0956797617716922 Though we await replication of our findings, we see t…     1

## Search Text

You can access a parsed table of the full text of the paper via
`paper$full_text`, but you may find it more convenient to use the
function
[`search_text()`](https://scienceverse.github.io/metacheck/reference/search_text.md).
The defaults return a data table of each sentence, with the section
type, header, div, paragraph and sentence numbers, and file name. (The
section type is a best guess from the headers, so may not always be
accurate.)

``` r
text <- search_text(paper)
```

| text | section | header | div | p | s | id |
|:---|:---|:---|---:|---:|---:|:---|
| This paper demonstrates some good and poor practices for use with the {metacheck} R package and Shiny app. | abstract | Abstract | 0 | 1 | 1 | to_err_is_human |
| Although intentional dishonestly might be a successful way to boost creativity (Gino & Wiltermuth, 2014), it is safe to say most mistakes researchers make are unintentional. | intro | Introduction | 1 | 1 | 1 | to_err_is_human |
| In this study we examine whether automated checks reduce the amount of errors that researchers make in scientific manuscripts. | method | Method and Participants | 2 | 1 | 1 | to_err_is_human |
| On average researchers in the experimental condition found the app marginally significantly more useful (M = 5.06) than researchers in the control condition found the checklist (M = 4.5), t(97.2) = -1.96, p = 0.152. | results | Results | 3 | 1 | 1 | to_err_is_human |
| It seems automated tools can help prevent errors by providing researchers with feedback about potential mistakes, and researchers feel the app is useful. | discussion | Discussion | 4 | 1 | 1 | to_err_is_human |

### Pattern

You can search for a specific word or phrase by setting the `pattern`
argument. The pattern is a regex string by default; set `fixed = TRUE`
if you want to find exact text matches.

``` r
text <- search_text(paper, pattern = "metacheck")
```

| text | section | header | div | p | s | id |
|:---|:---|:---|---:|---:|---:|:---|
| This paper demonstrates some good and poor practices for use with the {metacheck} R package and Shiny app. | abstract | Abstract | 0 | 1 | 1 | to_err_is_human |
| In this study we examine the usefulness of metacheck to improve best practices. | intro | Introduction | 1 | 1 | 4 | to_err_is_human |

### Section

Set `section` to a vector of the sections to search in.

``` r
text <- search_text(paper, "metacheck", 
                    section = "abstract")
```

| text | section | header | div | p | s | id |
|:---|:---|:---|---:|---:|---:|:---|
| This paper demonstrates some good and poor practices for use with the {metacheck} R package and Shiny app. | abstract | Abstract | 0 | 1 | 1 | to_err_is_human |

### Return

Set `return` to one of “sentence”, “paragraph”, “section”, or “match” to
control what gets returned.

``` r
text <- search_text(paper, "metacheck", 
                    section = "intro", 
                    return = "paragraph")
```

| text | section | header | div | p | s | id |
|:---|:---|:---|---:|---:|:---|:---|
| Although intentional dishonestly might be a successful way to boost creativity (Gino & Wiltermuth, 2014), it is safe to say most mistakes researchers make are unintentional. From a human factors perspective, human error is a symptom of a poor design (Smithy, 2020). Automation can be used to check for errors in scientific manuscripts, and inform authors about possible corrections. In this study we examine the usefulness of metacheck to improve best practices. | intro | Introduction | 1 | 1 | NA | to_err_is_human |

### Regex matches

You can also return just the matched text from a regex search by setting
`return = "match"`. The extra `...` arguments in
[`search_text()`](https://scienceverse.github.io/metacheck/reference/search_text.md)
are passed to [`grep()`](https://rdrr.io/r/base/grep.html), so
`perl = TRUE` allows you to use more complex regex, like below.

``` r
pattern <- "[a-zA-Z]\\S*\\s*(=|<)\\s*[0-9\\.,-]*\\d"
text <- search_text(paper, pattern, return = "match", perl = TRUE)
```

| text            | section | header                  | div |   p |   s | id              |
|:----------------|:--------|:------------------------|----:|----:|----:|:----------------|
| M = 9.12        | method  | Method and Participants |   2 |   1 |  11 | to_err_is_human |
| M = 10.9        | method  | Method and Participants |   2 |   1 |  11 | to_err_is_human |
| t(97.7) = 2.9   | method  | Method and Participants |   2 |   1 |  11 | to_err_is_human |
| p = 0.005       | method  | Method and Participants |   2 |   1 |  11 | to_err_is_human |
| d = 0.59        | method  | Method and Participants |   2 |   1 |  11 | to_err_is_human |
| M = 5.06        | results | Results                 |   3 |   1 |   1 | to_err_is_human |
| M = 4.5         | results | Results                 |   3 |   1 |   1 | to_err_is_human |
| t(97.2) = -1.96 | results | Results                 |   3 |   1 |   1 | to_err_is_human |
| p = 0.152       | results | Results                 |   3 |   1 |   1 | to_err_is_human |

### Expand Text

You can expand the text returned by
[`search_text()`](https://scienceverse.github.io/metacheck/reference/search_text.md)
or a module with
[`expand_text()`](https://scienceverse.github.io/metacheck/reference/expand_text.md).

``` r
marginal <- search_text(paper, "marginal") |>
  expand_text(paper, plus = 1, minus = 1)

marginal[, c("text", "expanded")]
```

    #> # A tibble: 2 × 2
    #>   text                                                                  expanded
    #>   <chr>                                                                 <chr>   
    #> 1 "The paper shows examples of (1) open and closed OSF links; (2a) cit… "All da…
    #> 2 "On average researchers in the experimental condition found the app … "On ave…

## Large Language Models

You can query the extracted text of papers with LLMs using any models
supported by [ellmer](https://ellmer.tidyverse.org/).

### Setup

You will need to get your own API key from your preferred provider
(e.g. <https://console.groq.com/keys>). To avoid having to type it out,
add it to the .Renviron file in the following format (you can use
[`usethis::edit_r_environ()`](https://usethis.r-lib.org/reference/edit.html)
to access the .Renviron file).

``` bash
GROQ_GPT_KEY="sk-proj-abcdefghijklmnopqrs0123456789ABCDEFGHIJKLMNOPQRS"
```

``` r
# useful if you aren't sure where this file is
usethis::edit_r_environ()
```

You can get or set the default LLM model with
[`llm_model()`](https://scienceverse.github.io/metacheck/reference/llm_model.md)
and access a list of the current available models using
[`llm_model_list()`](https://scienceverse.github.io/metacheck/reference/llm_model_list.md).

| platform | id | object | owned_by | context_window | max_completion_tokens | created_at |
|:---|:---|:---|:---|---:|---:|:---|
| groq | llama-3.1-8b-instant | model | Meta | 131072 | 131072 | 2023-09-03 |
| groq | openai/gpt-oss-20b | model | OpenAI | 131072 | 65536 | 2025-08-05 |
| groq | moonshotai/kimi-k2-instruct-0905 | model | Moonshot AI | 262144 | 16384 | 2025-09-05 |
| groq | meta-llama/llama-prompt-guard-2-22m | model | Meta | 512 | 512 | 2025-05-30 |
| groq | groq/compound-mini | model | Groq | 131072 | 8192 | 2025-09-04 |

When you start metacheck for the first time, it will check for relevant
API keys in your Renviron and automatically set the model to use. You
can get or set this with
[`llm_model()`](https://scienceverse.github.io/metacheck/reference/llm_model.md).

``` r
llm_model() # get current model
llm_model("openai") # set to ellmer's default openai model
llm_model("openai/gpt-4.1") # set to specific openai model
```

### LLM Queries

You can query the extracted text of papers with LLMs. See
[`?llm`](https://scienceverse.github.io/metacheck/reference/llm.md) for
details of how to get and set up your API key, choose an LLM, and adjust
settings.

Use
[`search_text()`](https://scienceverse.github.io/metacheck/reference/search_text.md)
first to narrow down the text into what you want to query. Below, we
limited search to the first ten papers’ method sections, and returned
sentences that contains the word “power” and at least one number. Then
we asked an LLM to determine if this is an a priori power analysis, and
if so, to return some relevant values in a JSON-structured format.

``` r
power <- psychsci[1:10] |>
  # sentences containing the word power
  search_text("power", section = "method") |>
  # and containing at least one number
  search_text("[0-9]") 

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
|:---|:---|:---|---:|---:|---:|:---|:---|
| Sample size was calculated with an a priori power analysis, using the effect sizes reported by Küpper et al. (2014), who used identical procedures, materials, and dependent measures. | TRUE |  | NA | NA | NA | NA | NA |
| We determined that a minimum sample size of 7 per group would be necessary for 95% power to detect an effect. | TRUE |  | 7 | NA | 0.95 | NA | NA |
| For the first part of the task, 11 static visual images, one from each of the scenes in the film were presented once each on a black background for 2 s using Power-Point. | FALSE | NA | NA | NA | NA | NA | NA |
| A sample size of 26 per group was required to ensure 80% power to detect this difference at the 5% significance level. | TRUE |  | 26 | 0.050 | 0.80 | NA | NA |
| A sample size of 18 per condition was required in order to ensure an 80% power to detect this difference at the 5% significance level. | TRUE |  | 18 | 0.050 | 0.80 | NA | NA |
| The 13,500 selected loan requests conservatively achieved a power of .98 for an effect size of .07 at an alpha level of .05. | FALSE | NA | NA | NA | NA | NA | NA |
| On the basis of simulations over a range of expected effect sizes for contrasts of fMRI activity, we estimated that a sample size of 24 would provide .80 power at a conservative brainwide alpha threshold of .002 (although such thresholds ideally should be relaxed for detecting activity in regions where an effect is predicted). | TRUE | contrasts of fMRI activity | 24 | 0.002 | 0.80 | NA | NA |
| Stimulus sample size was determined via power analysis of the sole existing similar study, which used neural activity to predict Internet downloads of music (Berns & Moore, 2012). | TRUE |  | NA | NA | NA | NA | NA |
| The effect size from that study implied that a sample size of 72 loan requests would be required to achieve .80 power at an alpha level of .05. | TRUE |  | 72 | 0.050 | 0.80 | NA | NA |

### Rate Limiting

The [`llm()`](https://scienceverse.github.io/metacheck/reference/llm.md)
function makes a separate query [^1] for each row in a data frame from
[`search_text()`](https://scienceverse.github.io/metacheck/reference/search_text.md).
To prevent accidentally making way too many calls because of errors in
your code, we set the default limits to 30 queries at a time, but you
can change this:

``` r
llm_max_calls(30)
```

## OSF Functions

Metacheck provides several function to help you assess resources
archived on the Open Science Framework.

### OSF Links and IDs

Get any OSF links from a paper or list of papers.

``` r
links <- osf_links(psychsci)

links$text |> unique() |> head()
```

    #> [1] "osf.io/e2aks"                                             
    #> [2] "osf.io/tvyxz/"                                            
    #> [3] "osf.io/t9j8e/? view_only=f171281f212f4435917b16a9e581a73b"
    #> [4] "osf .io/ideta"                                            
    #> [5] "osf.io/eky4s"                                             
    #> [6] "osf.io/xgwhk"

You can see that some of them have rogue spaces or view-only links. The
function
[`osf_check_id()`](https://scienceverse.github.io/metacheck/reference/osf_check_id.md)
takes most formats of OSF links (with or without <https://> and osf.io/,
as well as the 25-character waterbutler IDs) and converts them to short
IDs.

``` r
osf_ids <- osf_check_id(links$text) |> unique()

head(osf_ids)
```

    #> [1] "e2aks" "tvyxz" "t9j8e" "ideta" "eky4s" "xgwhk"

However, all of the `osf_***()` functions fix IDs for you and handle
duplicate IDs without making extra API calls, so you don’t need to add
this step to most workflows.

### OSF Info

Get basic information about OSF links, such as the name, description,
osf_type (nodes, files, preprints, registrations, users, set to
“private” if you don’t have authorisation to view it, and “invalid” if
the ), whether it is public

``` r
info <- osf_retrieve(links[1:6, ])
```

    #> Starting OSF retrieval for 4 URLs...

    #> * Retrieving info from e2aks...

    #> * Retrieving info from tvyxz...

    #> * Retrieving info from t9j8e...

    #> * Retrieving info from ideta...

    #> ...OSF retrieval complete!

``` r
info[, c("text","osf_id", "osf_type", "public", "category")]
```

    #> # A tibble: 6 × 5
    #>   text                                           osf_id osf_type public category
    #>   <chr>                                          <chr>  <chr>    <lgl>  <chr>   
    #> 1 osf.io/e2aks                                   e2aks  nodes    TRUE   project 
    #> 2 osf.io/tvyxz/                                  tvyxz  nodes    TRUE   project 
    #> 3 osf.io/tvyxz/                                  tvyxz  nodes    TRUE   project 
    #> 4 osf.io/t9j8e/? view_only=f171281f212f4435917b… t9j8e  private  FALSE  NA      
    #> 5 osf .io/ideta                                  ideta  nodes    TRUE   project 
    #> 6 osf.io/tvyxz/                                  tvyxz  nodes    TRUE   project

For now, the OSF API does not let us retrieve any information about
view-only links. They may be viewable by you in the web browser if the
link is still active, but will be listed in the table as public = FALSE
and osf_type = “private”.

You can set the argument `recursive = TRUE` to also retrieve information
about all nodes and files that are contained by the OSF link.

``` r
osf_api_calls(0)
all_contents <- osf_retrieve(links$text[1], recursive = TRUE)
```

    #> Starting OSF retrieval for 1 URL...

    #> * Retrieving info from e2aks...

    #> ...Main retrieval complete

    #> Starting retrieval of children...

    #> * Retrieving children for e2aks...

    #> * Retrieving children for pj4e8, 7jh5v...

    #> * Retrieving files for e2aks...

    #> * Retrieving files for pj4e8...

    #> * Retrieving files for 7jh5v...

    #> ...OSF retrieval complete!

``` r
n_calls <- osf_api_calls()
```

The function
[`osf_api_calls()`](https://scienceverse.github.io/metacheck/reference/osf_api_calls.md)
lets you reset and retrieve the number of API calls made since the last
reset. You can see that the project osf.io/e2aks had 3 nodes and 6
files, which required 10 API calls.

``` r
sum(all_contents$osf_type == "nodes")
```

    #> [1] 3

The OSF API does not (yet) have a way to find out what type of thing a
URL represents, so we may have to make a few API calls to figure out if
a URL represents a node, a file, a preprint, a preregistration, a user,
or is an invalid URL (e.g., a typo or an object that has been deleted).

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
    #>  * all_urls: List all the URLs in the main text.
    #>  * coi_check: Identify and extract Conflicts of Interest (COI) statements.
    #>  * coi_check_oi: Identify and extract Conflicts of Interest (COI) statements.
    #>  * funding_check: Identify and extract funding statements.
    #>  * funding_check_oi: Identify and extract funding statements.
    #>  * open_practices: This module incorporates ODDPub into metacheck. ODDPub is a text mining algorithm that detects which publications disseminated Open Data or Open Code together with the publication.
    #>  
    #> *** METHOD ***
    #>  * causal_claims: Aims to identify the presence of random assignment, and lists sentences that make causal claims in title or abstract.
    #>  * power: This module uses a large language module (LLM) to extract information reported in power analyses, including the statistical test, sample size, alpha level, desired level of power,and magnitude and type of effect size.
    #> 
    #> If you have not set llm_use(TRUE) and supplied a groq API, the module will return paragraphs that potentially contain power analyses, based on a regular expression search.
    #>  * prereg_check: Retrieve information from preregistrations in a standardised way,
    #> and make them easier to check.
    #>  
    #> *** RESULTS ***
    #>  * all_p_values: List all p-values in the text, returning the matched text (e.g., 'p = 0.04') and document location in a table.
    #>  * code_check: This module retrieves information from repositories checked by repo_check about code files (R, SAS, SPSS, Stata).
    #>  * marginal: List all sentences that describe an effect as 'marginally significant'.
    #>  * repo_check: This module retrieves information from repositories.
    #>  * stat_check: Check consistency of p-values and test statistics
    #>  * stat_effect_size: The Effect Size module checks for effect sizes in t-tests and F-tests.
    #>  * stat_p_exact: List any p-values reported with insufficient precision (e.g., p < .05 or p = n.s.)
    #>  * stat_p_nonsig: This module checks for imprecisely reported p values. If p > .05 is detected, it warns for misinterpretations.
    #>  
    #> *** REFERENCE ***
    #>  * ref_accuracy: This module checks references for mismatches with CrossRef.
    #>  * ref_consistency: Check if all references are cited and all citations are referenced
    #>  * ref_doi_check: This module checks references for missing DOIs or DOIs with an invalid format.
    #>  * ref_miscitation: Check for frequently miscited papers. This module is just a proof of concept -- the miscite database is not yet populated with real examples.
    #>  * ref_pubpeer: This module checks references and warns for citations that have comments on pubpeer (excluding Statcheck comments).
    #>  * ref_replication: This module checks references and warns for citations of original studies for which replication studies exist in the Replication Database.
    #>  * ref_retraction: This module checks references and warns for citations in the RetractionWatch Database.
    #>  * ref_summary: Summarise information about each reference in a paper.
    #>  
    #> Use `module_help("module_name")` for help with a specific module

### Running modules

To run a built-in module on a paper, you can reference it by name.

``` r
p <- module_run(paper, "all_p_values")
```

| text | section | header | div | p | s | id | p_comp | p_value |
|:---|:---|:---|---:|---:|---:|:---|:---|---:|
| p = 0.005 | method | Method and Participants | 2 | 1 | 11 | to_err_is_human | = | 0.005 |
| p = 0.152 | results | Results | 3 | 1 | 1 | to_err_is_human | = | 0.152 |
| p \> .05 | results | Results | 3 | 1 | 2 | to_err_is_human | \> | 0.050 |

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
report](https://scienceverse.github.io/metacheck/articles/report-example.md).

[^1]: Using the parallel functions in ellmer can be more efficient, but
    currently doesn’t do a good job of associating structured output to
    the input text when input may have 0+ outputs.
