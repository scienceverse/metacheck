# papercheck 0.0.0.9045

* When reading a paper with `read_grobid()`, the paper$references table now contains new columns for bibtype, title, journal, year, and authors to facilitate reference checks, and more reliably pulls DOIs.
* The `psychsci` set has  been updated for the new reference tables
* fixed bug in `info_table()` where adding "id" to the items argument borked the id column
* Added `json_expand()` function to expand JSON-formatted LLM responses
* Updated the LLM examples in the vignettes
* Added `find_project` argument to `osf_retrieve()` to make searching for the parent project optional (it takes 1+ API calls)
* Added `emojis` for convenience

# papercheck 0.0.0.9044

* Revised the OSF functions again!
* Organised the Reference section of the website
* Added some blog posts to the website
* Upgraded the "osf_check" module to give more info

# papercheck 0.0.0.9043

* Totally re-wrote the OSF functions

# papercheck 0.0.0.9042

* New OSF functions and vignette
* Build pkgdown manually 

# papercheck 0.0.0.9041

* Fixed a bug in `validate()` that returned incorrect summary stats if the data type of an expected column didn't match the data type of an observed column (e.g., double vs integer)
* Combined the two effect size modules into "effect_size"
* Renamed the module "imprecise_p" to "exact_p" (I keep typo-ing "imprecise")
* Added a loading message
* Added code coverage at https://app.codecov.io/gh/scienceverse/papercheck
* updated "all_p_values" to handle unicode operators like <=or >>

# papercheck 0.0.0.9040

* Updated default llm model to llama-3.3-70b-versatile (old one is being deprecated in August)
* Updated reporting function for modules to show the summary table
* Fixes a bug in `validate()` that returned FALSE for matches if the expected and observed results were both `NA`
* Added two preliminary modules: "effect_size_ttest" and "effect_size_ftest"

# papercheck 0.0.0.9039

* removed the llm_summarise module
* updated `papercheck_app()` to show all modules
* removed the LLM tab from the shiny app
* fixed a bug in `pdf2grobid()` where a custom grobid_url was not used in batch processing
* `psychsci` object updated to use XMLs from grobid 0.8.2, which fixes some grobid-related errors in PDF import

# papercheck 0.0.0.9038

* `validate()` function is updated for the new module structure
* the validation, metascience, and text_model vignettes are updated
* modules can now use relative paths (to their own location) to access helper files

# papercheck 0.0.0.9037

* The way modules are created has been majorly changed -- it is now very similar to R package functions, using roxygen for documentation, instead of JSON format. There is no longer a need to distinguish text search, code, and LLM types of modules, they all use code. The vignettes have been updated to reflect this.
* Modules now return a `summary` table that is appended to a master summary table if you chain modules like `psychsci |> module_run("all_p_values") |> module_run("marginal")`
* The `validate()` function is temporarily removed to adapt the workflow to the new summary tables.
* new `module_help()` function and some help/examples in modules
* new `module_info()` helper function
* new `paperlist()` function to create paper list objects
* paper lists now print as a table of IDs, titles, and DOIs
* updated `read_grobid()` to have fewer false positives for citations
* updated `retractionwatch`

# papercheck 0.0.0.9036

* Now reads in grobid XMLs that have badly parsed figures

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
