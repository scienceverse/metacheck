# papercheck 0.0.0.9035

* updated the shiny app for recent changes

# papercheck 0.0.0.9034

* `openalex()` takes paper objects, paper lists, and vectors of DOIs as input, not just a single DOI
* fixed paper object naming problem when nested files are not all at the same depth


# papercheck 0.0.0.9033

* added `read_cermine()` as associated internal functions for reading cermine-formatted XMLs

# papercheck 0.0.0.9032

* New functions for exploring github repositories: `github_repo()`, `github_readme()`, `github_languages()`, `github_files()`, `github_info()`
* A new vignette about github functions

# papercheck 0.0.0.9031

* `read_grobid()` now includes figure and table captions, plus footnotes, in the full_text table
* the `psychsci` paper list object is updated to include the above
* The functions that `module_run()` delegates to now check and only pass valid arguments

# papercheck 0.0.0.9030 (2025-03-01)

* modules are now updated for clearer output, and added a new module vignette
* `llm()` no longer returns NA when the rate limit is hit, but slows down queries accordingly
* `read_grobid()` now includes back matter (e.g., acknowledgements, COI statements) in the full_text, so is searchable with `search_text()`
* references are now converted to bibtex format, so are more complete and consistent
* Machine-learning module types are removed (the python/reticulate setup was too complex for many users), and instructions for how to create simple text feature models is included in the metascience vignette

# papercheck 0.0.0.9029 (2025-02-26)

* added `author_table()` to get a dataframe of author info from a list of paper objects
* fixed a bunch of tests now that multiple matches in a sentence are possible
* added back text (acknowledgements, annex, funding notes) to the full_text of a paper
* Fixed a bug in `search_text()` that omitted duplicate matches in the same sentence when using results = "match"
* Upgraded the search string for the "all-p-values" module to not error when a numeric value is followed by "-"
* Error catching for `stats()` related to the above problem (and filed an issue on statcheck)
* URLs in grobid XML are now converted to "<url>" using the source url, not the text url, which is often mangled

# papercheck 0.0.0.9028 (2025-02-18)

* added `psychsci` dataset of 250 open access papers from Psychological Science
* added "all" option the the return argument of `search_text()`
* added `info_table()` to get a dataframe of info from a list of paper objects
* experimental functions for text prediction: `distinctive_words()` and `text_features()`

# papercheck 0.0.0.9027 (2025-02-07)

* Removed ChatGPT and added groq support
* Updated `llm()` and associated functions like `llm_models()`
* Working on div vs section aggregation for `search_text()`

# papercheck 0.0.0.9026 (2025-02-06)

* metascience and batch vignettes
* removed scienceverse as a dependency
* revised validation functions
* added `tl_accuracy()`

# papercheck 0.0.0.9025 (2025-02-04)

* Added `expand_text()`

# papercheck 0.0.0.9024 (2025-01-31)

* Added `validate()` function and vignette
