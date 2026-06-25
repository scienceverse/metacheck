# Batch Processing

``` r

devtools::load_all(".")
library(dplyr) # for data wrangling
library(readr) # reading and writing CSV files
```

In this vignette, we will process 250 open access papers from
Psychological Science.

## Convert PDFs

To use smart defaults, read in all of the PDF files from a directory
called “pdf”, and save the converted files in JSON format a directory
called “converted”.

This function will use a local version of grobid or bibr if available,
and then check a [list of currently available free
servers](https://www.scienceverse.org/metacheck/convert.json) and check
those in order for accessibility (some require API keys).

``` r

convert(file_path = "pdf", 
        save_path = "converted")
```

The returned JSON files will contain infomations about how they were
converted (with grogib or bibr, which version, and which server), but if
you want more control, you can specific the bibr or grobid server to
use.

### Using Bibr

Bibr is a bibliographic metadata extractor, which has been developed
specifically for metacheck. It uses OCR, regular expressions, machine
learning, and limited LLMs to extract the contents of research papers in
PDFs or Word format into structured metadata.

Currently, you need an API key to use bibr while we work out how to
afford this resource, but we hope this will change soon.

``` r

convert(file_path = "pdf", 
        save_path = "converted", 
        method = "bibr",
        api_url = "https://platform.metacheck.app")
```

### Using Grobid

An alternate way to process PDFs is with the machine-learning library
grobid, and then convert the resulting XML files to bibr format. This
will have most, but not all, of the features of a paper processed by
bibr.

Read in all of the PDF files from a directory called “pdf”, process them
with a local version of grobid, and save the JSON files in a directory
called “converted”.

``` r

convert(file_path = "pdf", 
        save_path = "converted", 
        method = "grobid",
        api_url = "http://localhost:8070")
```

If you have existing grobid XML files, you can convert them to bibr
format by setting the method to “xml” (this is the auto default if the
file_path only contains XML files). Save them in a directory called
“converted”.

``` r

convert(file_path = "xml", 
        save_path = "converted",
        method = "xml")
```

### Read in converted files

After you convert your papers to JSON format, read in the files to
metacheck and save in an object called `papers`.

``` r

papers <- read("converted")
```

These steps can take some time if you are processing a lot of papers,
and only needs to happen once, so it is often useful to save the
`papers` object as an Rds file, comment out the code above, and load
`papers` from this object on future runs of your script.

``` r

# load from RDS for efficiency
# saveRDS(papers, "psysci_oa.Rds")
papers <- readRDS("psysci_oa.Rds")
```

## Paper Objects

Now `papers` is a list of metacheck paper objects, each of which
contains structured information about the paper.

``` r

paper <- papers[[10]]
```

### Paper ID

The `paper_id` is taken from the name of the original file.

``` r

paper$paper_id
```

    #> [1] "0956797615588467"

### Authors

The `author` table contains information for each author.

``` r

paper$author
```

    #>   author_id     given   family              affiliation                 email
    #> 1         1 Alexander Genevsky Department of Psychology genevsky@stanford.edu
    #> 2         2     Brian  Knutson Department of Psychology                      
    #>   corresponding orcid role
    #> 1         FALSE       NULL
    #> 2         FALSE       NULL

You can get the authors as a table for a paper object or list of papers.
Use the
[`paper_table()`](https://scienceverse.github.io/metacheck/reference/paper_table.md)
function to extract and combine tables from a paper list.

``` r

paper_table(papers, "author") |> 
  dplyr::filter(grepl("Glasgow", affiliation)) |>
  count(given, family)
```

    #> # A tibble: 14 × 3
    #>    given     family          n
    #>    <chr>     <chr>       <int>
    #>  1 Anthony   Lee             1
    #>  2 Benedict  Jones           2
    #>  3 Chengyang Han             1
    #>  4 Claire    Fisher          1
    #>  5 Danielle  Morrison        1
    #>  6 Hongyi    Wang            1
    #>  7 Iris      Holzleitner     1
    #>  8 Kieran    O'shea          1
    #>  9 Lisa      Debruine        2
    #> 10 Martin    Lages           1
    #> 11 Michal    Kandrik         1
    #> 12 Philippe  Schyns          1
    #> 13 Stephanie Boyle           1
    #> 14 Vanessa   Fasolt          2

### Info

The `info` table lists the filename, title, keywords, doi, and other
info. The import sometimes makes mistakes with the DOI, so be cautious
about using this.

``` r

paper$info
```

    #>                                                           title     keywords
    #> 1 Neural Affective Mechanisms Predict Market-Level Microlending c("affec....
    #>                        doi        file_hash input_format
    #> 1 10.1177/0956797615588467 c484f85b4211b469 grobid 0.9.0
    #>                                                 file_name bibr_version
    #> 1 data-raw/psychsci/grobid_0.9.0-crf/0956797615588467.xml         10.0
    #>   paper_type paper_type_confidence oecd_l1 oecd_l2 oecd_confidence
    #> 1    unknown                     0    <NA>    <NA>              NA

You can get this as a table for a batch of papers using
[`paper_table()`](https://scienceverse.github.io/metacheck/reference/paper_table.md).

``` r

paper_table(papers, "info") |> 
  select(doi, title) |>
  head()
```

    #> # A tibble: 6 × 2
    #>   doi                      title                                                
    #>   <chr>                    <chr>                                                
    #> 1 10.1177/0956797613520608 Mirror neurons, originally discovered in macaque mon…
    #> 2 10.1177/0956797614522816 Beyond Gist: Strategic and Incremental Information A…
    #> 3 10.1177/0956797614527830 Serotonin and Social Norms: Tryptophan Depletion Imp…
    #> 4 10.1177/0956797614557697 Action-Specific Disruption of Perceptual Confidence  
    #> 5 10.1177/0956797614560771 Emotional Vocalizations Are Recognized Across Cultur…
    #> 6 10.1177/0956797614566469 Conspiracist Ideation as a Predictor of Climate-Scie…

### Bibliography

The `bib` table contains the items in the reference list, including an
id to link them to cross references (bib_id), the text ID for the full
reference text (text_id), and the reference parsed by doi, title,
author, year, etc.

``` r

paper$bib[1, ] |> str()
```

    #> 'data.frame':    1 obs. of  15 variables:
    #>  $ bib_type   : chr "article"
    #>  $ doi        : chr ""
    #>  $ title      : chr "Impure altruism and donations to public goods: A theory of warm-glow giving"
    #>  $ authors    : chr "Andreoni, J"
    #>  $ editors    : chr ""
    #>  $ publisher  : chr ""
    #>  $ year       : int 1990
    #>  $ volume     : chr "100"
    #>  $ issue      : chr ""
    #>  $ first_page : chr "464"
    #>  $ last_page  : chr "477"
    #>  $ container  : chr "The Economic Journal"
    #>  $ bib_id     : int 0
    #>  $ year_suffix: chr ""
    #>  $ text_id    : int 243

The `bib_match` table contains CrossRef or DataCite entries for each
item in the reference list, if a match was found. In this table, the
authors and editors columns are list columns containing tables.

``` r

bib_match_1 <- paper$bib_match[1, ]
str(bib_match_1)
```

    #> 'data.frame':    1 obs. of  20 variables:
    #>  $ bib_id    : int 0
    #>  $ service   : chr "crossref"
    #>  $ service_id: chr NA
    #>  $ score     : num 96.9
    #>  $ bib_type  : chr "article"
    #>  $ doi       : chr "10.2307/2234133"
    #>  $ title     : chr "Impure Altruism and Donations to Public Goods: A Theory of Warm-Glow Giving"
    #>  $ authors   :List of 1
    #>   ..$ :'data.frame': 1 obs. of  2 variables:
    #>   .. ..$ given : chr "James"
    #>   .. ..$ family: chr "Andreoni"
    #>  $ editors   :List of 1
    #>   ..$ :'data.frame': 0 obs. of  2 variables:
    #>   .. ..$ given : chr 
    #>   .. ..$ family: chr 
    #>  $ publisher : chr "Oxford University Press (OUP)"
    #>  $ year      : int 1990
    #>  $ date      : chr NA
    #>  $ container : chr "The Economic Journal"
    #>  $ volume    : chr "100"
    #>  $ issue     : chr "401"
    #>  $ first_page: chr "464"
    #>  $ last_page : chr NA
    #>  $ edition   : chr NA
    #>  $ version   : chr NA
    #>  $ url       : chr "https://doi.org/10.2307/2234133"

The function `ref_table` is a helper function that lets you combine info
from the bib and bib_match tables with the text table and returns the
paper_id, bib_id, DOI, and the text of the reference.

``` r

ref_table(paper) |> head()
```

    #> # A tibble: 6 × 4
    #>   paper_id         bib_id doi                                text               
    #>   <chr>             <int> <chr>                              <chr>              
    #> 1 0956797615588467      0 10.2307/2234133                    Andreoni, J. (1990…
    #> 2 0956797615588467      1 10.2307/2118508                    Andreoni, J. (1995…
    #> 3 0956797615588467      2 10.1037/0022-3514.61.3.413         Batson, C. D., Bat…
    #> 4 0956797615588467      3 10.1037/0022-3514.40.2.290         Batson, C. D., Dun…
    #> 5 0956797615588467      4 10.1016/b978-0-12-374176-9.00009-9 Bernheim, B. D. (2…
    #> 6 0956797615588467      5 10.1016/j.jcps.2011.05.001         Berns, G. S., & Mo…

### Cross References

The `xref` table contains each cross-reference to the bibliography,
tables or figures. It includes an id to link them to a table
(`xref_id`), whether the cross-reference is to a bib, table, or figure
(`xref_type`), the contents of the reference (`contents`), and the ID of
the sentence that it is cited in (`text_id`).

``` r

xref <- paper$xref
filter(xref, xref_id == 5, xref_type == "bib")
```

    #> [1] xref_id   xref_type contents  text_id  
    #> <0 rows> (or 0-length row.names)

### Text

The `text` item is a table containing each sentence from the main text
(`text`). Each sentence has a unique sequential `text_id` number, and
each paragraph and section are also sequentially numbered. The
page_number is the page of the original document, starting with 1, that
this sentence starts on.

``` r

paper$text |> head()
```

    #>                                                                                                                                                                                                                                                                                                   text
    #> 1                                                                                                                                                                              Humans sometimes share with others whom they may never meet or know, in violation of the dictates of pure selfinterest.
    #> 2                                                                                                               Research has not established which neuropsychological mechanisms support lending decisions, nor whether their influence extends to markets involving significant financial incentives.
    #> 3                                                                                                                                                                                          In two studies, we found that neural affective mechanisms influence the success of requests for microloans.
    #> 4                                                                                                                                    In a large Internet database of microloan requests (N = 13,500), we found that positive affective features of photographs promoted the success of those requests.
    #> 5 We then established that neural activity (i.e., in the nucleus accumbens) and self-reported positive arousal in a neuroimaging sample (N = 28) predicted the success of loan requests on the Internet, above and beyond the effects of the neuroimaging sample's own choices (i.e., to lend or not).
    #> 6                                                                                                                                                    These findings suggest that elicitation of positive arousal can promote the success of loan requests, both in the laboratory and on the Internet.
    #>   text_id paragraph_id section_id page_number formatted
    #> 1       1            1          0          NA      <NA>
    #> 2       2            1          0          NA      <NA>
    #> 3       3            1          0          NA      <NA>
    #> 4       4            1          0          NA      <NA>
    #> 5       5            1          0          NA      <NA>
    #> 6       6            1          0          NA      <NA>

### Section

The `section` table supplements the text table to help group and search
text. The `section_id` matches that in the text table, and
`parent_section_id` is the ID of the section this one is nested under in
the case of subsections. The `header` is the section header. The
`section_type` is our best guess based on the header of the section type
and the `classification_score` is a confidence rating of this guess
(this is under development and currently not very accurate). Papers read
in with grobid will not have a parent_section_id or
classification_score.

``` r

paper$section |> head()
```

    #>   section_id                          header parent_section_id section_type
    #> 1          0                        Abstract                NA     abstract
    #> 2          1                Research Article                NA        intro
    #> 3          2                          Method                NA       method
    #> 4          3                  Internet study                NA       method
    #> 5          4              Neuroimaging study                NA       method
    #> 6          5 Power analysis and sample size.                NA       method
    #>   classification_score
    #> 1                   NA
    #> 2                   NA
    #> 3                   NA
    #> 4                   NA
    #> 5                   NA
    #> 6                   NA

## Text Search

The
[`text_search()`](https://scienceverse.github.io/metacheck/reference/text_search.md)
function helps you search the text of a paper or list of papers.

The default arguments give you a data frame containing a row for every
sentence in every paper in the set. The data frame has the same column
structure as the `text` table above, so that you can easily chain text
searches.

``` r

all_sentences <- text_search(papers)
```

You can customise
[`text_search()`](https://scienceverse.github.io/metacheck/reference/text_search.md)
to return paragraphs or sections instead of sentences.

``` r

paragraphs <- text_search(papers, return = "paragraph")
```

A paragraph from the first paper.

    #> [1] "According to the direct-matching model, activation of the PMC during action observation constitutes a covert simulation of the observed action, which enables the observer to match it with an action in his or her own repertoire of intentional actions and thereby to identify the goal of the action (Gallese et al., 2004). The directmatching model therefore holds that somatotopically organized regions of PMC play a causal role in understanding observed actions. The predictive-coding model (Kilner, Friston, & Frith, 2007) is based on the conception of a hierarchy of reciprocally connected models. Each model generates predictions about the representations at the immediately subordinate level. These predictions are compared with the actual state of the subordinate-level model, and a prediction error is returned to the superordinate-level model, which is revised and then generates a new prediction. By this process, the interconnected models are continuously updated and prediction errors minimized. Thus, according to the predictive-coding model, premotor activation and higher-level representations reciprocally modulate each other. Like the directmatching model, then, the predictive-coding model holds that somatotopically organized regions of PMC play a causal role in action understanding, with the mechanisms for action understanding overlapping with those for the production of actions."

### Pattern

You can just code every sentence or paragraph in a set of papers, but
this is usually not very efficient, so we can use a search pattern to
filter the text.

``` r

search <- text_search(papers, pattern = "Scotland")
```

Here we have 9 results. We’ll just show the text columns along with
text_id and paper_id of the returned table, but the table also provides
the papgraph_id, section_id, page_number, header, and section_type.

### Chaining

You can chain together searches to iteratively narrow down results. The
following example first finds all sentences with “DeBruine” and then
searches only that set for “2006”.

``` r

search <- papers |>
  text_search("DeBruine") |>
  text_search("2006")
```

If you want to do a search for any of a set of words, you can set the
pattern to a vector of terms to search.

``` r

pattern <- c("Chicago Face Database", 
             "Face Research Lab London")
search <- papers |>
  text_search(pattern)
```

### Regex

You can also use regular expressions to refine your search. The pattern
below returns every sentence that contains a word that contains text
with p \> \###, regardless of the spaces.

``` r

search <- text_search(papers, pattern = "p\\s*>\\s*0?\\.[0-9]+\\b")
```

### Match

You can return just the matching text for a regular expression by
setting the results to “match”.

``` r

match <- text_search(papers, 
                     pattern = "p\\s*>\\s*0?\\.[0-9]+\\b", 
                     return = "match")
```

You can expand this to the whole sentence, paragraph, or +/- some number
of sentences around the match using
[`text_expand()`](https://scienceverse.github.io/metacheck/reference/text_expand.md).

``` r

expand <- text_expand(results_table = match, 
                      paper = papers,
                      expand_to = "sentence",
                      plus = 0,
                      minus = 0)

expand$expanded[1]
```

    #> [1] "No main effects or interactions with time were found (p > .29), which indicates that the action-specific effects of TMS on confidence are not specific to its delivery before or after a perceptual decision."
