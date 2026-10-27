# metacheck 0.4.1

* `add_bib_match()` looks up every reference that gives a DOI by that DOI. A DOI written as a URL (`https://doi.org/10...`, `http://journals.plos.org/plosone/article?id=10...`) failed the DOI format check, so the reference was matched by a CrossRef title search instead, which can return a different paper. A DOI that CrossRef does not know is looked up at DataCite, which registers most data and code DOIs (Zenodo, OSF, Dryad); these references had no match, so `ref_accuracy` reported their DOI as not resolving
* `doi_clean()` removes spaces, trailing punctuation and URL encoding, which a DOI cannot contain. A DOI broken by a line break in the PDF (`10.1037/0003-066x.54.6.4 08`), followed by the sentence's full stop, or copied from a link (`10.1002/%28SICI%291097-0258...`) was not valid, so `add_bib_match()` matched its reference by title search, and the retraction and PubPeer checks did not look it up
* `report()` and `report_repository()` list all five report types in their usage line, `report_type = c("full", "brief", "simple", "simple_brief", "checklist")`. Both already accepted all five, but the usage line, which the help page and the manual's function reference show, named only "full" and "simple". The default is still "full"
* `marginal`, `stat_p_nonsig`, `causal_claims` and `ref_miscitation` give "na" (not applicable) instead of green when they find nothing to check: no terms like "marginally significant", no non-significant *p* values, no causal language in the title or abstract, or no commonly miscited paper among the references. Green means that something was found, checked and is fine, as in the other modules: `causal_claims` stays green for causal language with random assignment. In a checklist report these papers are now listed as not applicable rather than fine
* `osf_file_download()` saves a project in a folder named after its OSF ID inside `download_to` also when `download_to` does not exist yet, so running it again resumes (#533). The OSF-ID folder was added only when `download_to` already existed: a first run into a new folder wrote the project straight into it, and a second run, which then found the folder, downloaded the whole project again into `download_to/<osf_id>/` (in both `mode = "select"` and `mode = "all"`). `FLoRA_update()` now looks for the file in that folder, and removes its temporary folder and restores the download time limit when it is done
* `repo_info_cache()` is removed (#531). `repo_info_cache(TRUE)` set an option that nothing read, so it never turned the repository-listing cache on. The cache is turned on with `cache = TRUE` in `repo_check` (or `code_check` and `data_check`, which pass it on) and in the `*_info()` functions; `repo_info_cache_clear()` still clears it, and its help page now describes the cache
* The module that `module_template()` writes runs (#532). It counted by a column `id` and showed a column `section`, which `text_search()` does not return, so it stopped with an error; it now uses `paper_id` and `header`, and links to the manual's chapter on creating modules
* The OSF download app's links to the manual point to its chapter on archiving OSF projects (#534); they pointed to a page that does not exist
* The help of `data_check` and `code_check` describes the download limits as they work: a file over `max_file_size` is skipped on its own, and `max_download_size` is filled with the smallest files first (#535). It said a repository with one file over the limit was refused as a whole. `code_check`'s help also lists the languages it checks (R, Python, SAS, SPSS, Stata, Mplus and MATLAB) and its checks of `setwd()`, `install.packages()` and whether R code parses

* The checklist report (`report_type = "checklist"`) shows `reproducibility_check` in a section of its own when it ran the code, after the results and before "More detail". It says how the scripts were run, how many ran to the end, and, in a table in run order, what happened to each script: ran to the end, stopped with an error (with the error itself, taken from R's or Python's error output), ran out of time, could not run because a package is missing, or was not run (with the reason, such as the input files that are not in the repository, or a syntax error). Then come other problems in the files (`setwd()`, `os.chdir()`) and how many of the statistical tests reported in the paper were found in the output of the code. Before, the check was one line, and when it was fine or for information its whole report was folded under "More detail" at the end. Any module can give such a section, as `checklist_section` in its output (see `report_type()`); its check then links to the section instead of folding its report
* `reproducibility_check` with `install_missing = FALSE` counts a script that stops because one of its packages is not installed as "could not run here" (`dependency_unavailable`), not as an error in the script (`errored`). Only a package an install was tried for and failed counted so, so without installs a missing package read as the authors' error (OSF pqkj7's `02_Analysis.R`, which needs `rcrossref`)
* `reproducibility_check` no longer treats a Python module of the repository itself (`from utils import helpers`, with `utils/helpers.py` in the repository) as a package: it is not installed from PyPI, and a script whose import of it fails (as when the Psych-DS layout moves the module) has stopped with an error (`errored`), not "could not run here" (`dependency_unavailable`), which the change above made it with `install_missing = FALSE`. With `install_missing = TRUE`, pip tried to install the PyPI package of the same name, which for a name such as `utils` exists and is unrelated
* `convert(method = "grobid", keep_xml = FALSE)` names the paper after the PDF (`to_err_is_human.pdf` gives the paper ID `to_err_is_human` and `to_err_is_human.json`). The temporary XML file had a random name, which became the paper ID and the JSON file name

* `ref_accuracy` is no longer run by default, for now: it is left out of `report()`'s default modules and is no longer offered in the metacheck app or the report app. A reference that printed no DOI, but for which Crossref found one, was counted neither as checked nor as without a DOI, so the summary could say "We checked the 0 references that supplied a DOI … 5 references had no DOI" for a paper with 28 references, and suggested DOIs were not shown; and on references with their own DOI it flagged up to 15% of them in nine PsyArXiv preprints, which its validation finds are mostly errors in reading the PDF. The module itself is unchanged and can still be run with `module_run(paper, "ref_accuracy")`. The Crossref lookup is still used: `ref_table()` gives the DOIs Crossref found to `ref_pubpeer`, `ref_retraction`, `ref_replication`, `ref_consistency` and `ref_summary`

* `reproducibility_check`'s `total_timeout` (the time for all scripts together) can also be a clock time (`POSIXct`) by which the scripts must be done, such as a server's job limit minus the time its report needs (#523; Lakens/tue_datamanager_check#4, item 6). In seconds it counts from when the first script starts, so time spent fetching files and on other modules came on top, and a job limit of the same length stopped the whole job before its report was written. As before, a running script is stopped and the rest are not started when the time has run out
* `reproducibility_check` searches only the first 20,000 lines of each script's printed output for statistical results (option `metacheck.repro_output_max_lines`), and the report says when it did (#523, item 2). The search reads about 700 lines a second. A script that prints a growing table in a loop (OSF sq8y6, whose loops print about 3.5 million lines) was stopped on time after 300 seconds, but the job then ran on for 54 minutes, until its 60-minute limit, so no report came back; searching that much output at this speed takes about that long
* In the Docker sandbox an R script can now use its cache, configuration and data folders, and `rmarkdown::render()` finds the Pandoc that Quarto bundles in the image (#523, items 4 and 5). The container's home folder is read-only, so `renv::restore()` stopped with "failed to create directory at path '/home/rcheck/.cache/R/renv'", and `rmarkdown::render()` with "pandoc version 2.8 or higher is required and was not found". These folders are now in `/tmp`, and `RSTUDIO_PANDOC` points to Quarto's Pandoc
* In the package layout (`local_path`), a path that counts from the script's own folder, or from its RStudio project's (`.Rproj`) folder, now works for files the code makes (#523, item 3). A script in `Study 2/` writing `"Data/processed.csv"` into `Study 2/Data/` stopped with "Cannot open file for writing", since scripts run from the top of the package, and a later script reading that file stopped too. A quoted relative path whose folder exists from the script's (or `.Rproj`) folder but not from the top is now rewritten to its path from the top; reads of the package's own files were already rewritten
* `codebook_check` no longer stops with "row names contain missing values" on a codebook variable whose normalised name is `NA` (#523, item 1)
* An SPSS file (`.sav`) whose text is in the Windows character set (CP1252) is now read; haven stopped with "Unable to convert string to the requested encoding (invalid byte sequence)", so `data_check` and `codebook_check` did not check the file (#523, item 6). Such a file is read again as CP1252
* `dryad_info()` with `cache = TRUE` no longer reuses a listing stored before the paging fix of 0.3.3.1 (#506). Such a listing holds only Dryad's first page of 20 files and has no `files_total`, so a deposit with more files kept missing the rest (deposit 10.5061/dryad.m8j17 lacked its README). Such a listing, or one marked incomplete, is now fetched again
* The file-naming check's length messages (`check_file_naming()`, used by `repo_check` and so shown in its reports and the checklist reports) now say what is too long in words: "The file name is 58 characters, over the recommended 50" instead of "filename-length-50 is 58 characters, over the 50-character budget"; likewise for the folder path (recommended 100), the path (recommended 228, which leaves room for a download folder) and the 255-character limit. The rule's id stays in `rule`
* `ref_accuracy` on a paper whose references were not looked up at Crossref (no `bib_match` table: converted without `crossref`, and no `add_bib_match()`) now reports "na" with "The references were not looked up at Crossref, so their accuracy was not checked." It reported "fail" with "We found no bib_match entries. You may need to add them with `add_bib_match()`", which a checklist report lists under "To fix", although a skipped lookup is not a fault of the paper
* `report(report_type = "checklist")` (and `"simple"`, `"brief"`, `"simple_brief"`) now finds the Pandoc that Quarto bundles on Linux. Quarto keeps it in `bin/tools/<arch>/` there (`x86_64` or `aarch64`), not in `bin/tools/` as on Windows and macOS, so on a Linux server without a separate Pandoc these reports stopped with "pandoc version 1.12.3 or higher is required and was not found"

* `report()` on several papers no longer stops in metacheck's apps. It named each paper by `x$id`, but a paper keeps its id in `paper_id`, so the name was always empty; the apps' progress bar (`use_shiny_progress()`) cannot show an empty value and stopped with "invalid 'replacement' argument". The console progress bar ignored it. Papers are now named by `paper_id()`, and the apps' progress bar shows an empty value as nothing
* An empty `SURF_API_URL`, `BIBR_API_URL` or `OLLAMA_BASE_URL` now counts as not set. On Linux and macOS a variable set to "" stays set (Windows removes it), so `Sys.getenv(name, default)` gave "" and `convert_bibr()` and the Ollama functions had no address to send to; the default address is now used
* A ResearchBox page that is not served is reported with ResearchBox's HTTP status ("ResearchBox did not serve <address> (HTTP <status>)") instead of "could not be found". A box that does not exist still answers 200, so any other status is a refusal or a server error
* `data_check` and `codebook_check` no longer stop on three kinds of column (#507: U55, U60). `data_check_outliers()` stopped when both quartiles were infinite (`c(1, Inf, Inf, Inf)`); there is no spread, so no outliers. `data_check_scale_values()` stopped on a codebook range with fractional bounds (1.5 to 4.5); such a range is now an interval, so 2.2 is valid, and the message prints its bounds as decimals ("outside the 1.5-4.5 scale"); ranges with whole-number bounds are unchanged. A labelled SPSS or Stata column with a missing label name stopped the label reader; that label is now left out
* A zip made by an old Windows zip tool no longer stops `data_check` (#507: U76). Such tools store file names in the CP437 character set without marking them, so a name with an accent ("café.csv") was not valid UTF-8, and `data_classify_files()` and `check_file_naming()` stopped with "invalid multibyte string", while `file_category()` and `filetype()` found no type. `zip_peek()` now reads unmarked names as CP437 and returns them in UTF-8 (with the stored length in a new column `name_bytes`), and the four functions classify a name that is still not valid UTF-8 from a readable copy, returning it as given
* `report_qmd()` no longer stops on three inputs (#507: U7, U128). A module whose `summary_text` has several parts stopped the whole report in R 4.3 and later ("'length = 2' in coercion to 'logical(1)'"); the parts are now joined with a space. `report_qmd()` without a paper (its default `paper = list()`) stopped with "argument is of length zero"; the report is now written without the title and DOI. A traffic light metacheck does not know (such as "blue") left an empty summary row and made `module_report()` drop the module's heading; both are now kept, without a symbol
* `extract_p_values()` and `extract_eq()` accept a character vector, as `extract_urls()` does (#507: U150); each string is searched as one row of text, numbered by `text_id`. They stopped with "$ operator is invalid for atomic vectors" and a `bind_rows()` error. `extract_eq()` also no longer stops on a text table without `paper_id` or `text_id`
* A licence or DOI that a repository sends as plain text instead of an object no longer stops the lookup (#507: U34). An older Dataverse installation's licence ended `dataverse_info()` for every dataset in the call ("$ operator is invalid for atomic vectors"), and `dataverse_file_download()` for that dataset; the same applied to Zenodo's and Figshare's licence and Mendeley Data's DOI. The text is now read as the value (also for GitHub's and GitLab's licence), and a Dataverse answer whose `data` is text is reported as a `parse_error`
* A Dataverse link whose `persistentId` decodes to invalid characters ("%FF") no longer stops `dataverse_links()` and `dataverse_info()` for every link (#507: U39). That link gets no DOI; before, a fallback could also give it a cut-off DOI ("10.7910/DVN/AB" for ".../DVN/AB%FFCD")
* `download_repo_files()` no longer stops when a file list mixes a Dryad dataset of 13 to 50 wanted files with a row that has no `repo_url` (#507: U74): the count of the dataset's files became NA ("missing value where TRUE/FALSE needed"). A wanted file that arrived with an out-of-date `file_location` also counted as delivered by a Dryad, GitHub or GitLab zip that did not contain it, so it was never downloaded; such locations are now cleared before the zip, and a file the zip did not deliver is downloaded on its own
* Every module now runs on an empty paper (`paper()`) or an empty paper list (`paperlist()` or `list()`) (#507: U79). On an empty list, 28 of the 30 modules stopped, and on an empty paper 10 did. `paper_table()` of an empty list returned a table without columns, so `text_search()` gave logical ones and joins on `paper_id` and string functions on `text` stopped; it now returns the same typed, empty tables as an empty paper. `repo_check` (and so `code_check`, `data_check`, `codebook_check`, `psychds_check` and `reproducibility_check`), `prereg_check`, `reg_check`, `power` and `ethics_check` built their summary from a paper id an empty input does not have, and `module_run()` lost the `paper_id` column for an unnamed `list()`
* An empty paper or paper list now gives "na" in every module. `coi_check`, `funding_check` (and their `_oi` versions) and `open_practices` reported red, as if a statement were missing, and `causal_claims`, `marginal`, `stat_p_nonsig` and `ref_miscitation` reported green; with no text and no references there is nothing to check ("There was no text to check.")
* `codebook_check`'s list of scales described in the manuscript no longer prints "NA" for a missing name, acronym or item count ("**NA** (X; NA items)"), and no longer stops on an acronym with a special character such as "C++" or "(X" (#507: U93). Missing fields are left out, and acronyms are matched literally, as whole words
* `prereg_check` no longer stops on a paper that links AsPredicted but no OSF page ("argument is of length zero"), and each AsPredicted preregistration is now its own row, attached to the paper that links it (#507: U109). All AsPredicted preregistrations of a call were pasted into one row whose link matched no paper, so `paper_id` was NA and every paper counted 0 preregistrations, and a missing field showed as the text "NA". AsPredicted is no longer contacted for a paper without AsPredicted links, so a paper that links only OSF also works while aspredicted.org cannot be reached
* `paper = NULL`, documented for checking only a local folder (`local_path`), works again in `repo_check`, `code_check`, `data_check`, `psychds_check`, `codebook_check` and `reproducibility_check` (#507: U78). `paper_id(NULL)` stopped `repo_check`, and so every module that runs it, with "paper must be a paper or paperlist object". `module_run()` now treats `NULL` as an empty paper of unknown id, so the folder counts as one paper with `paper_id` NA; that id also no longer becomes "0" when a module fills missing summary values (`na_replace`)
* The repository `*_info()` functions (`dryad_info()`, `figshare_info()`, `zenodo_info()`, `researchdata4tu_info()`, `dataone_info()`, `reshare_info()`, `mendeley_info()`) no longer stop on a table of links with an NA in it ("row names contain missing values") (#507: U33). `researchdata4tu_info()` also keeps the columns of a table and drops duplicates and NAs from a vector, as the others do. A failed connection (time-out, no network) no longer stops `zenodo_info()` or the DataCite, CrossRef, OpenAlex and DOI lookups, which tested for an error object that `.batch_query()` never returns
* `dryad_file_download()`, `figshare_file_download()` (and so `researchdata4tu_file_download()`), `dataverse_file_download()`, `zenodo_file_download()` and `reshare_file_download()` keep files left out by `max_file_size` or `max_download_size` in the returned table with `downloaded = FALSE`, as documented; they were dropped from it, and when every file was left out the result was `NULL` instead of the list of files (now with no folder made) (#507: U36). A zero-byte file no longer stops the whole download ("Can't retrieve empty body"). Zenodo's one-archive download is still used only when no file is left out
* `osf_file_download()` in `mode = "select"` or `"zip"` no longer stops on a project that holds only folders ("replacement has 1 row, data has 0"); it returns a table without rows (#507: U53). `osf_api_check()` now applies `on_error` when there is no internet connection; it returned "no internet" without stopping or warning, whatever `on_error` said
* `rbox_file_download()`, `dspace7_file_download()` and `psycharchives_file_download()` return `NULL` when several links all fail, as for one; they stopped with "Join columns in `y` must be present" (#507: U43). `repo_check` then reports such ResearchBox boxes as "ResearchBox did not serve the box" instead of an internal error message. An empty ResearchBox page (a refusal can have none) is reported as not found or as the refusal it is, instead of stopping (#507: U45)
* `code_check` no longer stops on a Quarto file whose front matter is a single value or a list ("just a title"), or on a Jupyter notebook that is not a JSON object or has malformed metadata ("$ operator is invalid for atomic vectors") (#507: U68); the language falls back to the first chunk's engine, or is left unknown for such a notebook
* Papers in a list that share a `paper_id` are renamed "id~2", "id~3", ... with a warning, where a list comes in (`paperlist()`, `module_run()`, `report()`) (#507: U208, U101). Tables are joined and grouped on `paper_id`, so a shared id mixed the papers: sentences were joined to the other paper's sections (two papers with 2 p-values each gave 8 rows instead of 4), summaries had more rows than papers, references were pooled, `ethics_check` and `open_practices` stopped ("factor level [2] is duplicated"), and `report()` wrote both reports to one file
* An AsPredicted page that answers with an error status (removed, private) or cannot be reached no longer stops `prereg_check`; the error is recorded for that page, with a warning (#507: U54)
* `read_stat_tables()` of a path that is not a zip archive (another file, an empty zip, a folder) returns an empty list instead of stopping (#507: U137)
* `check_orcid()` returns FALSE for NA or an ORCiD with an X before the check digit, instead of stopping; `pubpeer_comments()` sends a single DOI as a list, ignores feedback without an id (which stopped it) and keeps one row per DOI; the abstract rebuilt from OpenAlex no longer stops on an empty index; and `regcheck_start_local(method = "docker")` uses `port` (#507: U14)
* `reproducibility_check` reads Python dependencies from a `pyproject.toml` properly (#507: U236): only `[project]` `dependencies` and the Poetry dependency table (without `python`) are read; every quoted string of the file (the project name, its version, the first word of its description) became a requirement, and some files stopped the check. Poetry versions are written for pip (`^1.26` as `>=1.26,<2.0`), and a `requirements.txt` line loses its inline comment and options
* `json_expand()` returns a table without rows unchanged instead of stopping ("Join columns in `y` must be present") (#507: U151)
* `causal_claims` no longer stops on a paper without a title, and checks the title of every paper in a paper list (a list has no `$info`, so its titles were never checked) (#507: U84). `causal_relations()` no longer sends a missing sentence as the text "NA"
* `coi_check` no longer stops when a short heading such as "Conflict of interest" is the last sentence, and no longer pastes "NA" into a statement whose follow-up runs past the end of the text (#507: U97)
* `funding_check` no longer stops (for the whole paper list) when two lines match a funding, grant, support or acknowledgement title ("Funding" and "Funding:"), and no longer pastes "NA" into the statement when the line after a title is past the end (#507: U103)
* `ref_pubpeer` reports "PubPeer could not be reached" (traffic light "fail") when PubPeer answers with an error, instead of stopping ("`x` and `y` must share the same src") (#507: U23)
* `github_files()` no longer stops on an empty folder or when given a single file, passes `dir` on when listing several repositories (each was listed from its root), and `github_languages()` no longer reads an error answer's fields as languages (#507: U48)
* `repo_check` reports an OSF link with an invalid id as "not a valid OSF link" (#507: U122); alone it got dplyr's error text as its reason, and beside valid links it vanished from the report. A platform file without a name no longer stops the file-naming check
* `power`'s fallback for providers without structured output uses the module's own schema instead of downloading it from scienceverse.org, so it works offline, and a field that no answer gives now counts as missing, so an incomplete power analysis is no longer reported as complete (#507: U108)
* `psychds_check` (and `reproducibility_check`'s package layout) take a missing file name from the file's path, exclude a file with neither, and no longer stop on a file list without a `file_name` column; a missing name became the target "unknown/NA" (#507: U113, part)
* The API's `/paper/search` takes its documented `section` parameter: a comma-separated list of section types (`method,results`, any case; `references` is the reference list), and searches only those sections. It was passed on to `text_search()`, which has no such argument, so every request using it failed (#507: U2)
* `llm()` no longer stops when some requests fail and others succeed; the failed rows get no answer and every one of them is named in the warning (it never warned for several failed rows) (#507: U19). Answers are joined back to the texts as given, so a text the sanitiser changed (a control character, invalid UTF-8) keeps its answer and numeric text works; a missing text is not sent (it went out as "NA")
* `match_reported_output()` (used by `reproducibility_check`) no longer stops on an output row without a `test_id` next to a jamovi residuals row, on an empty `test_id`, or on infinite values (an infinite value simply does not match); a paper list gets a clear error asking for one paper at a time (#507: U142)
* A column with value labels but no variable label (in an SPSS, Stata, JASP or jamovi file) no longer gets its first value code as its variable label: `attr(x, "label")` partially matched the value labels (`labels`) (#507: U145). A JASP or jamovi column shorter than the row count is padded with missing values instead of stopping the import, and a JASP column without a column type is read as nominal
* `convert()` of GROBID XML files works offline: it looked up the online conversion servers although XML is converted locally, and stopped with "online versions not available" (#507: U18). `papers_available()` returns an empty table for a repository whose releases hold no `.rds` files instead of stopping, so `papers_load()` reports the corpus as not found
* GROBID XML files with a page range giving only `from` or `to`, a reference without its raw text, or a link without text no longer fail to load; and a paper whose TEI header has no DOI no longer gets its first reference's DOI (#507: U17)
* `export_spv_html()` no longer fails on an SPSS table whose last level label is empty, or on a chart it cannot draw (that chart is left out with a note); images in exported JASP, jamovi and SPSS HTML are embedded at every occurrence (only the first was), and an image path with a malformed `%`-escape no longer stops the export (#507: U148)
* `data_check` reports a collision between blank column names (it was stored where it could not be found), reads the digits of 100000 stored as a number the same as of the integer 100000 (it read "1e+05", with warnings), peeks into 8-bit (Latin-1) text files instead of failing, and no longer re-reads a valid UTF-8 file with an "NA" cell as Latin-1, which turned "é" into "Ã©" (#507: U56, U59, U63, U64)
* `data_check` reads UTF-16 text files (E-Prime and other Windows exports start with a byte order mark) instead of reporting them as empty: read as bytes, the first column was named "ÿþS" and the table had no rows (#507: U217)
* The data manifest keeps small numbers (0.000012345 was written as 0) and keeps JSON nulls as null when it is merged again (they became `{}`) (#507: U62)
* Codebook ranges written with the prefix repeated ("V1-V10") are expanded, as "V1-10" already was, and a data column without a study group no longer gets labels of rows that belong to a group (#507: U58)
* Variable labels in `.rds` and `.RData` data files are kept, whatever the session (`head()` dropped the label of plain and factor columns, and of value-labelled columns when vctrs was not loaded) (#507: U61)
* An RTF codebook reaches the language model with its line breaks and its accented and escaped characters, without the font and colour tables; it was one line that began with the font table, with `\'e9`, `\u233` and escaped braces dropped (#507: U215)
* `reproducibility_check()` reads R's "there is no package called" error with curly quotes, as R writes it in a UTF-8 locale, so a script that needs a package that failed to install is "dependency_unavailable", not "errored" (#507: U132)
* `reproducibility_check()` keeps a named argument's name when it sends a write to the output folder: `save(a, b, file = "ab.RData")` had become `save(a, b, "<out>/ab.RData")`, which saves the path as another object (#507: U134)
* `reproducibility_check()` finds the arguments after a file name in `paste()`, `paste0()`, `sprintf()` or `file.path()` where the file name ends, not where the call starts (#507: U133). `paste(wd, "data.csv", sep = "/")` was not recognised as a built path, so only its file name was replaced, giving "wd/<sandbox path>"; the whole call is now replaced. `paste0(path, ".R")` was taken as a reference to a file ".R"
* `repro_file_io()` counts a Python `open()` call as a write only by its mode argument (positional or `mode =`): `w`, `a`, `x` or `r+`, with `b`, `t` or `+` (#507: U240). Any quoted word with a w, a or x in it counted, so `open(p, encoding = "ascii")` made the file an output that orders the scripts, and `open(p, "r+")` did not
* `reproducibility_check` no longer marks a script as missing a file it writes itself (#507: U235). A Python `open(path, "w")` names its path like a read, so the script that creates the file was "missing_input" and not run, and the script reading it then failed; an R script that reads back a file it saved was likewise not run. A script's own writes now count as available to it, for R and Python
* A zip member's CRC32 must match exactly (#507: U70). `all.equal()` accepted a CRC up to about 60 off for large values, so a corrupt download could pass the check
* `zip_peek(cache = TRUE)` no longer keeps a failed download on disk (#507: U210). A connection failure, a rate limit or a server error passes, but the disk entry outlived the session, so the zip was not listed again until `zip_peek_cache_clear()`; it is now kept for the session only. A cache entry is written to a temporary file and renamed into place, and an entry that cannot be read is no longer returned as a cached failure
* A file that `data_check` takes out of a zip, tar or compressed file has its own `file_type` from its extension, not the archive's (#507: U213). The rows for an archive's files were copies of the archive's row, so a CSV from `data.zip` was typed "archive"
* `data_check` no longer writes into the folder given as `local_path` (#507: U214). A zip, tar or compressed file there was extracted beside itself, to "<archive>.contents"; the next run listed the extracted files as files of their own and extracted the archive again, so every file inside counted twice. An archive outside the repository cache and the temporary folder is now extracted into the temporary folder; a downloaded archive is still extracted beside itself
* `module_report()` keeps the "How It Works" box of a module without `@details`, and the report of a module without `summary_text` (#507: U129). Searching the missing details for a validation note failed, which dropped the whole box with the description and authors; and without `summary_text`, `all(NULL == report)` is TRUE, so the report was dropped
* `extract_urls()` no longer lists the two halves of an e-mail address as URLs (`k.aristovich@ucl.ac.uk` gave `k.aristovich` and `ucl.ac.uk`), nor "et al." glued to the next word ("et al.Inhibitory" gave `al.Inhibitory`) (#507: U158, U206). A URL with an `@` after its host (`twitter.com/@user`) is kept
* `extract_p_values()` finds p written as "ps", "p's", "p’s" or "p-values" ("ps < .05" gave no row), and reads scientific notation written with "×", without "^", or with the Unicode minus: "p = 1.8 × 10 -6", "p = 6.1 × 10−5" and "p=2.14e−213" were read as 1.8, 6.1 and 2.14 (#507: U204). The p-value modules (`all_p_values`, `stat_p_exact`, `stat_p_nonsig`) see these p-values too, so a paper reporting "ps < .05" can now be flagged for imprecise p-values
* `extract_eq()` reads statistics with the Unicode minus (U+2212), which text taken from PDFs often has: "t(28) = −2.15", "d = −0.80" and "r = − 0.12" gave no row; `rhs` is returned with "-" (#507: U205). It also reads the same scientific notation as `extract_p_values()`, and "and z = 3.1" is the statistic z, not "d z" (#507: U218)
* `dataverse_info()` asks the right Dataverse installation for each URL (#507: U31, U155). The hosts found were put back by position, so a URL without a host name (a bare DOI) took the next URL's host, and the next URL a later one's; and "dataverse.no" matched the start of a longer host name such as dataverse.northwestern.edu
* A download, an extraction or a decompression that is stopped half way, or a download that comes back empty, no longer leaves a file in the cache that the next run reuses as complete (#507: U75, U203). Downloads are written to "<file>.part" and renamed when complete, archives are extracted into a ".part" folder beside their destination and moved in when done, and a ResearchBox box is unzipped into "unzipped.part" and renamed
* `stats()` keeps a sentence's statcheck results when statcheck warns about another part of it: one unparseable p-value ("p = .05-.10") dropped every valid result next to it (#507: U4). An argument `statcheck()` does not have is now an error (it made every call fail silently, so `stats()` returned an empty table), and a character vector is accepted
* Grobid import keeps the space between a number and a tag that follows it ("Table 1", "Hamilton 1964", "Fig. 1</ref>" had become "Table1", "Hamilton1964", "Fig.1"), joins omega's partial eta ("ω p 2" gives "ωp²"; the rule was written so that it rewrote "<p2" instead) and Hedges' "g z" (the rule turned "dgz" into "gz") (#507: U16)
* Grobid 12.0 conversion squishes runs of spaces in body text, as it already did for captions, notes and the abstract (#507: U27)
* Grobid import replaces a URL's link text only in the row the link is in, once: a short link text such as "osf" was also replaced in every other row, inside other links ("https://https://osf.io/q2.io/48ncu"), and in the 12.0 conversion a link without a target turned every row printing its text into NA. An author with several ORCIDs keeps the first, not the last (#507: U28)
* `code_check` reports a repository refused by the size caps once, with the number of files in its whole listing: the second download counted the code files only, so the refusal was reported twice with two counts ("holds 6 files", "holds 4 files"). The per-paper check for a pinned environment downloads with the module's size caps and `cache`, not the defaults (#507: U88)
* `code_check` scans every R file for a groundhog or checkpoint date-pin: the texts were kept by file name, so of two files with the same name (in different folders) only the last was scanned, as was only the last file of a paper without a paper id; a located renv.lock or sessionInfo file is copied back to its own row, not only to the first file of that name (#507: U89)
* `codebook_check` on a paper list keeps a definition that is identical to one in another paper's codebook (it was dropped, so that column was reported as undocumented and `codebook_var_n` was too low), and sends each data file's own paper's sentences when it asks the language model to name a scale (it sent none for a paper list) (#507: U91)
* `codebook_check` merges scale labels that differ only in capitals and extra words ("Emotion Recognition", "emotion recognition task"): capitals were removed before the labels were lower-cased. With `codebook_max_calls = 0` (or when the call cap is reached), the scale groups of every file are still listed; the listing stopped after the first file (#507: U92)
* `prereg_check` joins an OSF registration to its paper by the OSF id: a link written with http://, in capitals, with a view_only key or with a path matched no paper (paper_id NA, every paper counting 0), and a link given twice counted its registration twice. A registration that cannot be fetched is named by the paper's own link, not the OSF API address (#507: U110)
* `prereg_check` reads the answers of the "Pre-Registration in Social Psychology" template by their exact key: a missing answer took the answer of a longer key that starts the same ("84-5" read "84-56"). A template label containing U+FFFE or U+FFFF no longer stops the module (#507: U111)
* `repo_check` de-duplicates files one by one: a remote file by its URL and path, a local file by its location. The URL and the path were tested separately, so a file was dropped when its URL appeared on one other row and its path on another (#507: U121)
* `repo_check` leaves out every private OSF file, not only when it is the listing's one row; and a local file that `file_category()` cannot place takes its extension's type, so a local .zip or .tar.gz counts as an archive file, as the same file in a repository does (#507: U123)
* 4TU.ResearchData: a DOI with a uuid that starts with digits (10.4121/7f866e02-...) is looked up by its uuid; the digits were taken as the id ("7"). Short article addresses (data.4tu.nl/articles/_/<id>/<version>) and addresses of other item types (articles/software/<title>/<id>) get their id, and an article known only by its uuid is downloaded (#507: U41, U156)
* `dataone_info()` fetches a dataset once when several URLs name it (it was fetched once per URL, and the join then gave each URL one row per fetch: 2 URLs, 4 rows). A DataONE PID no longer keeps a sentence's final full stop ("doi:10.5063/PG1Q4B."), a PID marked "doi:" may contain "/", and a creator with a missing name part is no longer written "NA Smith" (#507: U35, U40)
* DSpace 7: a resolved DOI counts as a DSpace 7 link only when its host name is a known DSpace 7 host or a subdomain of one; a known host anywhere in the URL, such as in its query string, counted. Messages about an item without a uuid name its handle instead of "(NA)", and a legacy DSpace handle on an unknown host is asked of PsychArchives instead of https://NA/rest (#507: U42, U211)
* `osf_check_id()` takes the project id from a link that ends in a route name (osf.io/j3gcx/files/ gave the id "files"), no longer cuts a word after osf.io/ to five characters (osf.io/prereg gave "prere"), and does not take a registration schema (osf.io/<id>/register/<schema>) as a file id (#507: U51)
* `zenodo_upload(as_zip = TRUE)` builds its zip only from the files it keeps: the zip was made from the whole folder, so the `_osf_metadata` folder (with `upload_osf_metadata = FALSE`) and files over `max_file_size` were uploaded inside it while `files_skipped` reported them as skipped (#507: U47)
* `code_extract_r()` reads an .Rmd or .qmd with a numeric or logical YAML chunk label (`#| label: 3`): knitr used the label as a position and stopped ("subscript out of bounds"), so `code_check` skipped the file (#507: U153)
* `paper_write(schema_version = "12.0")` keeps every decimal of a double exactly: jsonlite wrote 15 significant digits, so rewriting a bibr export changed values such as 0.9411764705882353 into 0.941176470588235. Such values are written with the 16 or 17 digits they need; all others as before (#507: U22)
* Reading R output: an output line that starts with "+" stays output; every line starting with ">" or "+" was counted as part of the statement, so output lines before it were lost (#507: U140). An untitled one-line result takes the source label as its analysis, a notebook line without a `*Result` class has `call_fn` "", and an SPSS sub-heading, table or chart without `commandName` keeps the enclosing command: these fallbacks never applied, because `%||%` replaces NULL but not NA (#507: U139)
* `link(type = "doi")` gives no link for a missing DOI; it linked to https://doi.org/NA (#507: U8)
* `stat_output_validate()` reads a JSON document given as a string; every string was reported as "Input is not valid JSON." (#507: U136)
* `github_links()` and `gitlab_links()` find hyperlinks in papers read from a bibr 12.x export or converted with `grobid_to_bibr(schema_version = "12.0")`: they searched the url table's first column, which in those papers is the number `url_id`, so no link was found and `repo_check` missed every GitHub and GitLab repository linked there (#507: U196)
* `text_search(return = "section")` keeps each section's header; it was NA, so `coi_check_oi` tables named no section (#507: U80)
* An SPSS box plot drawn from case data is decoded; the decoder always stopped (it took the absolute value of the text category column), so such box plots were dropped with a warning (#507: U144)
* `match_reported_output()` counts the exponent in a reported number's decimals: "1.5e-05" had one decimal, so its tolerance (0.05) matched almost any small output value. A table's own caption is used whenever it has one, also for an older-format paper (#507: U143)
* `json_expand()` keeps a `_row` key as a column (jsonlite read it as row names, so it was lost) and gives nested objects columns of their own (`v.k`); each row of a nested object read "c(1, NA)" (#507: U9)
* Reading statistics tables from HTML output: a short row without content is dropped like an empty row (it became a row of NAs), and table fields are read by their exact name, so a table with only `line_seq` no longer gets a locator from it as if it were `line` (#507: U141)
* `causal_claims` finds randomisation written with an s ("randomised"), leaves out negated statements ("not randomized", "non-randomized"), and counts a sentence that describes random assignment next to random effects ("randomly assigned ... random intercepts"), which was dropped whole (#507: U85)
* `coi_check` keeps a statement that ends with "Nil.", "No.", "None mentioned." or "Nothing to declare.": these were emptied and dropped, so the paper got a red light (#507: U95)
* `paper_id()` takes each paper's own id, so a paper whose info table has no rows keeps its id in a paper list (it was read from the info tables, so the id was lost and its rows got paper_id NA in `ethics_check` and elsewhere). `ethics_check` quotes each ethics statement once; one found in several places was quoted each time (#507: U102)
* `funding_check`: the synonyms "Foundation", "papers", "Literature" and "Bibliography" were misspelled in their patterns and never matched, and an unclosed caseless part made later patterns ignore case; the reference list is found at a sentence that begins with its title, not at any sentence that ends with "References" and a capitalised word (#507: U104)
* `ref_accuracy` (a paper without `bib_match` rows) and `reg_check` (every comparison failed) give the traffic light "fail"; they gave "error", which is not one of metacheck's lights, so the report showed no symbol (#507: U30)
* `open_practices` on a paper list gives every paper a row, so the light and summary follow the number of papers: a list where only one paper had a flagged sentence was reported as a single paper (with a green, yellow or red light), and papers without a flagged sentence did not count as "neither" (#507: U106)
* `open_practices` flags "on request" whatever its case ("ON REQUEST" passed the repository search but was not flagged), quotes each on-request sentence once in one paragraph, and counts only a capital "R" as a word for code, so a statistic such as "r(68) = .58" is not a code statement (#507: U107)
* `ref_consistency` reports a citation whose reference number is not in the bibliography (it was never reported), counts the citations of older bibr exports, which have the type "bib" (every reference was reported as uncited and the light was red), counts a reference with several `bib_match` rows once, and always has the summary columns `n_missing` and `n_extra` (#507: U115, U207)
* `ref_accuracy` checks and counts a reference with several `bib_match` rows once (three copies of one row gave 31 rows for 5 references), removes footnote superscripts from titles before comparing them, and turns dashes into spaces instead of removing them (#507: U114)
* `ref_pubpeer` counts each commented DOI once per paper: a DOI cited three times was joined to three lookups each ("You cited 9 references", with the comments summed nine times). It lists every commented reference, with a link where PubPeer gives one, and no longer stops when no feedback has a URL (#507: U118)
* `local_files()` given several paths lists their subfolders when `recursive = TRUE`; the setting was not passed on (#507: U49)
* Reading a bibr export matches its keys exactly: R's `$` matches the start of a name, so a file without `bib` read its `bib_match` rows as the references (and `info`, `metadata`, `source`, `extraction`, `producer` and `schema_version` were read the same way) (#507: U25)
* `doi_lookup()`, `datacite_doi()` and `openalex_doi()` keep every other DOI when one lookup fails: a `return()` inside `tryCatch()` returned from the whole function, so one unknown DOI made `doi_lookup()` and `datacite_doi()` return NULL and `openalex_doi()` return only that DOI's error. A failed DOI gets an empty row (in `openalex_doi()` an entry with error "not found", as a list of entries like any other call), later rows no longer shift, and an organisation author is named from the record's name (#507: U12)
* `extract_tests()` on a paper list gives each paper its own tests once; every test was repeated for every paper (3 papers: 3 × 116 rows). `extract_eq()` numbers the sentences (`grp_id`) within each paper in text order; counted in search order, the numbering restarted when the search moved to another paper, so sentences of a paper shared numbers (#507: U10)
* `ref_retraction`, `ref_replication` and `ref_miscitation` compare DOIs without regard to case, and keep the reference's own DOI: a fifth of the RetractionWatch DOIs have capitals (such as the retracted Wakefield paper, 10.1016/S0140-6736(97)11096-0) while reference DOIs are lower case, so these were never flagged. A DOI that RetractionWatch lists under two spellings gets both notices (#507: U157)
* `ref_replication` leaves out a citation of an original only when the same paper also cites the replication; in a paper list, one paper citing a replication hid every other paper's citation of the original (#507: U116)
* `ref_miscitation` counts only citation xrefs (a footnote, figure or table xref with the same number counted), a missing DOI in the database matches nothing (it matched every reference without a DOI), and two references with the same DOI give one entry instead of list columns with a warning (#507: U117)
* `funding_check_oi` favours acknowledgment sections; it looked for the type "acknowledgement", but metacheck spells the section type "acknowledgment", so only funding sections were favoured (#507: U105)
* `stat_p_exact` reports "p < .000" and "p == .000" as zero, treats "==" as "=" (it counted as imprecise; in `stat_p_nonsig` as non-significant), lets a starred footnote ("*p < .05") excuse only that p-value instead of every p-value in its sentence (and no longer "*p < .005"), and lists the same p-value once (#507: U127)
* `module_run()` fills missing summary values (`na_replace`) only in the columns the module adds: in a chain, an unnamed `na_replace = 0` or `FALSE` (as in `stat_check`, `marginal`, `coi_check`) also filled the earlier modules' NA cells, turning list columns into numbers (#507: U77)
* `reg_check` leaves a missing judgement out of its counts (the summary read "flagged NA potential deviationNA", and `na_replace` turned the counts into 0), and gives each paper and preregistration its own report section, naming the paper when several papers link the same preregistration; they shared one section (#507: U120)
* Reading Mplus output: a section header that is the last line, or is followed directly by another header, has no lines; its lines were NA or the next header (#507: U147)
* Reading Stata (SMCL) output: `{c 0xNN}` is read as hexadecimal (`{c 0x41}` gave ")"), `{c -(}` and `{c )-}` give literal braces (they were dropped), and a `{dup}` count outside the integer range is left out instead of making the line NA (#507: U146)
* `stat_effect_size` counts only eta labels as eta-squared: d, r, beta or R² next to an F-test were reported as "Eta-squared reported; cannot be reconstructed" and are now "not verifiable from F and degrees of freedom". An F-test with zero degrees of freedom, such as F(0, 0), no longer stops the module (#507: U126)
* `ref_summary` labels a reference without a match "no match" even when none of its fields mismatches (it got NA), keeps one row per reference when there are several replications or PubPeer rows (the summary counted 7 references for a paper with 6; their values are combined), and works with an accuracy table without `no_match` (#507: U119)
* `data_check` counts the files it actually extracted columns from ("extracted 2 columns from 3 of them" also counted .RData and unreadable files), keeps the manifest's zip-peek reasons with their own files after archives are expanded, and reports the language model used for the study groups; it read a column that does not exist, so the model was never reported (#507: U99)
* OSF helpers: a versioned id (`abcde_v1`) is looked up as a GUID (it was sent to /files/), only unnamed folders take their storage provider's name (unnamed rows of any kind were renamed when several rows were read at once), and a record without `root` no longer reads `root_folder` in its place (#507: U52)
* A CrossRef record with a null field is read (it stopped with "differing number of rows: 1, 0", so the record was lost), a list-valued field becomes one text value, and `openalex_query()` sorts its results by relevance score as numbers; they were sorted as text (#507: U13, partly: `crossref_query()` on data frames and `openalex_doi()`'s `select` are unchanged)
* `psychds_check` judges whether a file is misplaced against its final target (raw data already at `data/eeg.npy` was told "Move data/eeg.npy -> data/eeg.npy") and keeps a data file name that is already a valid Psych-DS name (`study-a_data.csv` became `data/study-studyadata_data.csv`, so data files were never in place) (#507: U112, partly: where `dataset_description.json`, LICENSE and CHANGES go is unchanged)
* Reports credit every author of a module (a single author line naming two authors was cut to the first), `report()` on a paper list with one `output_file` names the files "<id>_<name>" (it was "<id>report.html"), `scroll_table()` and `report_table()` turn line breaks into `<br>` in every text column, also a blank-named one, and a shortened Zenodo progress line fits its width (#507: U6, U130, U131, U50)
* Wording fixes: plurals in `code_check` ("In 1 code files"), `data_check` ("1 file contain", "1 distinct respondent were") and the no-hit reports of `ref_retraction`, `ref_replication` and `ref_pubpeer` ("We checked 1 references"); typos and copied text in the descriptions of `funding_check`/`funding_check_oi` (which spoke of a conflict of interest statement), `power` ("uses uses", "LMM"), `ref_accuracy`, `ref_consistency` and `open_practices`. The archive-download timeout message shows whole seconds (a fractional timeout made the message itself fail), and without a `file_path` column the files of a downloaded archive are matched by name (#507: U82, U83, U73)
* `retractionwatch()` (and `rw_update()`) leaves out rows with a blank DOI and trims DOIs; a blank DOI matched every reference without a DOI in a join on `doi`. `stat_check` without statistics gives every paper of a list a summary row (#507: U15, U124)
* A second download of the same Figshare, ReShare, Zenodo, Dataverse or Dryad deposit goes to "<folder>_1", "<folder>_2", ...; the deposit id in a folder name such as "figshare_<id>" was stripped, so it went to "figshare_1" (#507: U37)
* The Grobid 12.0 conversion writes dates before the year 1000 with four digits ("0026-05-27T08:58:00Z"; the zero padding was lost, which is not a valid 12.0 date-time), gives NA for a time outside the years 0000-9999, and converts a vector of times (#507: U26)
* `stat_effect_size` keeps the note that both eta-squared and partial eta-squared were reported (the partial-eta result overwrote it), and its summary ends with a full stop (#507: U125)
* `marginal` cites its reference as "Olsson-Collentine A, van Assen MALM, Hartgerink CHJ (2019)"; the authors, written as one string, were printed as "van Assen, M. MAL, Hartgerink &amp;, J. CH" (#507: U3)
* Internal ids of reported statistics are tidy: a missing source file gives the prefix "result" (it gave "na_na_result"), the fallback id no longer repeats the source prefix ("odd_name_spv_odd_name_spv_t3_a"), and both a leading and a trailing underscore are trimmed (#507: U138)
* Reproducibility helpers: `repro_missing_inputs()` reports a withheld file without its size when there is no `file_size` column (it stopped), the "already installed" row of `repro_install_deps()` has the same columns as the others, byte-identical mirrors tied between study groups keep the first group listed (not the alphabetically first), and a CRAN Archive line without a date sorts last instead of stopping the install (#507: U135)
* The LLM cache key no longer includes the R version: it hashed R's serialisation, whose header records the version of R that wrote it, so upgrading R emptied the cache. Because the key changes once, existing cached answers are not found after this update. A `GITHUB_PAT` no longer makes the defunct "github" provider the default language model (#507: U20, partly: `llm_timeout()` still bounds only Ollama requests)
* `psychds_check` checks each paper of a list against Psych-DS from its own files, codebook columns and labels: each paper gets its own summary row and its own report section ("Paper <id>"), and the summary counts how many papers are compliant, convertible, not yet convertible, or without files. The files of all papers were checked as one dataset, credited to the first paper, and the other papers got 0. A single paper is checked as before (#507: U113)
* The reproducibility sandbox also removes `base::setwd(...)` and a Python `chdir(...)` under any module name (`from os import chdir` followed by `chdir(...)`, or `o.chdir(...)`); only `setwd(` and `os.chdir(` were caught, so such a call ran and failed on the author's own path (Lakens/tue_datamanager_check#4, item 5)
* An old Excel `.xls` file is first opened in a separate R process before metacheck reads it. Some files (an old Mac Excel file) make readxl crash the whole R process, which no error handler can catch, so the job ended without a report; such a file is now reported as unreadable and the rest of the check goes on. Applies to `data_read_head()`, the codebook reader and data_check's sheet loop (Lakens/tue_datamanager_check#4, item 4)
* In a package layout, a path-building call that the reproducibility check keeps as written is no longer put in quotes: `sprintf("%s/x.xlsx", wd)` became the unparseable `"sprintf("%s/x.xlsx", wd)"`, so the script stopped with a parse error before running (Lakens/tue_datamanager_check#4, item 9)
* Every language-model request `llm()` makes now waits at most `llm_timeout()` (3 minutes by default), and a failed structured answer is not tried again once that time has passed. ellmer's own limit was 5 minutes with 3 tries, and with metacheck's 5 attempts one answer could take over an hour, so a job hung at "Querying LLM" until the server stopped it (Lakens/tue_datamanager_check#4, item 7; also U20)
* The repository check app says "The .zip holds no files." for an uploaded .zip without files (no entries, or only empty folders). R's `unzip()` cannot open a .zip without entries, so the app said the file could not be opened as a .zip archive and asked for it to be zipped again (Lakens/tue_datamanager_check#4, item 8)
* reproducibility_check has a new argument, `total_timeout`: the time for running all scripts together, R and Python, counted from when the first script starts (default 3600 seconds, one hour; `NULL` or `Inf` for no limit). Once it has run out, no further script is started (new outcome `skipped_time_limit`, named in the report, and the light is at least yellow) and a running script is stopped. Before, only each script had a limit (`timeout`), so a package with many slow scripts could run past the server's limit and return no report. A script the corrective second run has no time for keeps its first result. The four script runners (`repro_run_scripts()`, `repro_run_scripts_docker()`, `repro_run_scripts_py()`, `repro_run_scripts_py_docker()`) take the limit as `deadline` (Lakens/tue_datamanager_check#4, item 6)
* data_check reads an `.rds` or `.rda` file whose columns hold values that need an R package, such as lubridate's periods and durations. R loads such a file without the package, but every later step stopped with "unable to load required package", so on a computer without that package the file was reported as unreadable and not checked. Each such column is now read as its plain stored numbers, recorded in the new `needs_package` column of data_check's table, and reported with a new warning, "Needs an R package": anyone without that package cannot open the file (Lakens/tue_datamanager_check#4, item 8a)
* Reports no longer show the folder on the computer that ran the check. The data tree names a local folder by its own name ("Repository: my_study"), not its full path, which on the server was its job folder (`/data/metacheck_storage/metacheck-api-jobs/<job id>/repo/...`). reproducibility_check's note on duplicate scripts names each copy by its link or its path in the repository (Lakens/tue_datamanager_check#5, item 2)
* An `.rds` file made with `save()` instead of `saveRDS()` (an R workspace, which `readRDS()` refuses with "unknown input format") is opened with the `.RData` reader, and data_check warns about it ("R workspace named .rds"; the light is at least yellow). Before, it was reported as unreadable, with the advice to replace it as if damaged (Lakens/tue_datamanager_check#5, item 3)
* The output of a script run by reproducibility_check no longer ends with "function 'library' appears not to be S3 generic". The code that captures each statement's result asked `methods()` for the methods of every function called at top level, including `library()`; it now asks only for a generic. The helpers sent to each script's R process now call each other, not the copies in whatever metacheck is installed there (Lakens/tue_datamanager_check#5, item 4)
* In the Docker sandbox, results of `aggregate()`, `tapply()` and similar calls are captured again: the code sent into the container lacked the vocabulary of statistic names, so the step that recognises `mean` or `sd` failed every time (found with Lakens/tue_datamanager_check#5, item 4)
* codebook_check no longer stops with "replacement has length zero" when a column's only codebook entries without a label are haven ones (a `.jasp` or `.omv` file whose columns have no variable labels): `match_column_labels()` chose the longest of the haven labels, and when all were empty there was none. A codebook entry without a label now never labels a column (it stays unlabelled for the next source), while its value labels, missing-value codes and question still count (#520, item 1)
* reproducibility_check, package layout: a file the package has only inside an archive (`data.zip/data/x.csv`), read by the code at the path unpacking it would give (`data/x.csv`), is now a missing input, with the status `in_archive` and the reason "only inside data.zip; unpack it so the code finds it", and the script is not run. The path was left as written (it names no file of the package as it is), which also counted the file as present, so the script ran and failed: in OSF t69jd `fread()` then took the path for a shell command and reported a full disk (#520, item 2)

# metacheck 0.4.0

* `convert_bibr()`'s SURF backend uses metacheck's API at its new address, `https://metacheckserve.metacheck.src.surf-hosted.nl/api`: the SURF Research Cloud servers moved from the training collaboration `src-training-01` to the collaboration Metacheck, and a workspace's address contains its collaboration's name. `SURF_API_URL` still sets another address
* `reproducibility_check()` runs the code of a local folder in a copy of the package as it is (new argument `layout`). It ran code against a Psych-DS release of the data files it recognised: only those files, renamed and moved into Psych-DS folders, with the code's paths rewritten to match. That suits a study whose scripts read a few tables, but code that reads folders of images or scan files, or imports its own helper modules from a subfolder, failed for files that are in the package: in a physics data package, 12 of 21 Python scripts stopped with "No such file or directory" for files the package holds, or could not import their own modules. With `layout = "package"` every file the run has is copied to its own path, so a path that works in the package works in the sandbox. Only a path that cannot work as written is rewritten: an absolute path on the author's computer or one out of the package, to the file with that name, and a path relative to the script's own folder (`source("helpers.R")`, `"../data/x.csv"`), to the file's path from the top of the package. Any other relative path is left as written, also when other files share its name, since code commonly joins it with a folder of its own (`os.path.join(base, "fig7/x.csv")`; rewriting it gave `processed_data/fig7/processed_data/fig7/x.csv`). A script's writes stay where the code puts them, so a later script reads the file where it expects it (only a write to an absolute path or out of the package goes to `output/`), and the copied files and folders are writable, so a script can save over a file the package holds (in the Docker sandbox the code runs as another user, and this failed with "Permission denied"). Each script is written to its own path, so scripts with the same name no longer overwrite each other, and each file is copied from its own source, so 20 files called `log.csv` no longer all get the first one's content. `layout = "auto"` (the default) uses the package layout for a local folder (`local_path`, as `report_repository()` and the repository app give it) and the Psych-DS layout otherwise, as before (internal `.repro_package_plan()`, `.repro_keep_working_refs()`)
* `reproducibility_check()` no longer runs a Python file that other files import as a script of its own: a module such as `utils/data_handling.py` defines functions for the scripts that import it and fails or does nothing on its own. Like an R script that another script `source()`s, it is reported as `run_by_import`, and not counted among the scripts that ran; a file with an `if __name__ == "__main__":` block still runs (internal `.repro_py_imported()`)

* OSF files download again for a user with an OSF token, and the token no longer leaves the OSF (#508). The OSF moved its file storage from Google to Oracle in October 2026. With a token set, every request carried it to every host a redirect led to (`unrestricted_auth`), so the user's OSF token was sent to the cloud storage with every file download; Oracle refuses a pre-signed address that also carries a token (HTTP 400), so every file stored on the OSF failed: `osf_file_download(mode = "select")` got 29 of the 57 files of project `pngda`, only those of its linked GitHub repository, and `repo_check`, `data_check` and `code_check` missed the same files. The token is now sent only to the OSF: the redirect from `osf.io/download/<id>/` to the OSF's own file server is asked for with the token and without following it (new internal `.osf_file_server_url()`), the file is requested there with the token, and curl drops the token at the storage host. The hop to the file server was made before too, so the number of requests is the same. Zenodo, which serves its files itself, no longer sets `unrestricted_auth` either. Checked live: 57 of 57 files of `pngda` with a token
* ResearchBox files download again. ResearchBox now gives its files text tokens instead of numbers as ids, and `rbox_file_download()` turned them into numbers, so every id became NA, nothing was downloaded, and `repo_check` reported a ResearchBox repository as one it could not check. The ids are sent as text, as ResearchBox's own download button does; numeric ids are still sent as numbers. A box with a single file, which ResearchBox may send as the file itself instead of a zip, is saved as that file
* `code_check`'s advice on `install.packages()` names a list of packages for Python (`requirements.txt`) next to R's (`renv.lock`, `DESCRIPTION`)

* A fifth report type, `report_type = "checklist"`, lays a report out by check instead of by module. Every check of every module is sorted into to fix, to improve, decide by hand, for information, fine and not applicable, with the number in each at the top. Each check that needs work is a block that says what was found and what to do on separate lines, with its tables folded under it; the checks that are fine or not applicable are one line each, and how each module works is folded at the bottom. It is rendered like the simple report (static, no JavaScript) and takes the colours and the font (Cabin, embedded) of metacheck's website on SURF Research Cloud; it stays light when the reader's computer is set to dark. A module gives its checks in a `checks` data frame in its output (`status`, `title`, `found`, `todo`, and optionally `why`, `note`, `section`, `details` and `details_title`; see `report_type()`), and can add an `intro` paragraph and a `logo`. A module without `checks` is shown as one check, from its traffic light, summary and report. New internal `R/report-checklist.R` and template `inst/templates/_report_checklist.Rmd`; `collapse_section()` folds in this report type, and the "How It Works" text of a module is built by the new internal `.report_how_it_works()`, which `module_report()` now uses too
* A table in a simple report no longer swallows the rest of the report when a cell starts like a list item. Pandoc reads the cells of these tables as markdown, so a cell such as "1. GENERAL INFORMATION" became a numbered list left open inside the table, and every section after it was nested in that table cell. The list marker is now escaped (internal `.report_table_static()`), for the simple, simple brief and checklist reports
* `code_check` gives Python code Python advice on package versions. It looked only for R's ways of recording them (`renv.lock`, `sessionInfo()`, groundhog, checkpoint), and told a repository with only Python code to add an `renv.lock`. It now also finds a list of Python packages (`requirements*.txt`, `environment.yml`, `Pipfile(.lock)`, `poetry.lock`, `uv.lock`, `pyproject.toml`), returned as `python_files` and `python_pinned` in `version_pin`, whose `pinned` stays about R. Python code without such a list is told to add a `requirements.txt`, R code keeps the R advice, and code in both languages needs both; code in neither language is judged as before. The traffic light and `code_version_pinned` follow this (new internal `.code_env_pinned()`)

* An installed module can no longer be replaced by a file in the working directory (#512). `module_find()` looked for a module in the working directory and in a `modules/` folder in it before the modules installed with `module_install()`, so a file `modules/<name>.R` was run instead of the installed module `<name>`. A server that runs a check from inside an uploaded folder therefore ran code from the upload: on metacheck's API on SURF Research Cloud, an upload named `modules.zip` holding a `tue_datamanager_check.R` was run as that module, outside the code sandbox. The working directory is now searched after the installed modules; a module there is still found when no installed or built-in module has its name, as `module_template()` and local development use it
* `code_read()` no longer crashes R on a byte its file's encoding does not define (#511). It read files with `readr::read_lines()` in the encoding `readr::guess_encoding()` guessed; on a byte that encoding leaves undefined (0x81 in windows-1252, common in old DOS text), vroom raised "Invalid multibyte sequence" and then crashed R while unwinding from that error, also inside `tryCatch()`, so `code_check` and every module or app running it stopped without a message. On metacheck's API on SURF Research Cloud a README of the freetype sources in a Zenodo repository ended the whole job this way. `code_read()` now reads the file's bytes and converts them to UTF-8 with base R's `iconv()`, which writes such a byte as `<81>`, in the same guessed encoding; UTF-16 files, a byte order mark and files without any guess (which gave "subscript out of bounds") are handled too. Lines end at LF, CRLF or CR, as in `readLines()`; files with mixed line endings (`\r\r\n`) no longer get a line break inside a line. Compared with the old version on 1,435 code and text files from 64 repositories: the same lines for all but those 2 files with mixed line endings. A web address that answers with an HTTP error (such as 404) stops with that error, as before, so `code_check` reports the file as not read instead of reading its error page as code
* `report()` saves the report when the output file is on another disk than R's temporary folder. It renders the report in the temporary folder and moved it with `file.rename()`, which cannot move a file to another disk: it returned FALSE without an error, and the output file kept the word "test" that `report()` writes to check the path. This happened on metacheck's API on SURF Research Cloud, whose job folders are on a storage volume: the report of an installed module came back as "test". The report is now copied when renaming fails, and an error says so when that fails too (internal `.report_move()`)
* `code_check`'s report names the code files of a local folder. Each file name was made a link to the file's web address, and a local file has none, so the File Name column of its table was empty (NA) for every file. A file without a web address is now shown by its path in the folder (`PPC_examples/sine_frequency/tools.py`), which also tells apart files with the same name
* `data_check` reads every table when several files share a name. It kept each table it read under its file name alone, so files with the same name in different folders replaced each other, and only one of them was checked: in a measurement repository with a `log.csv` and a `logResults.csv` in each of 22 scan folders, 4 of 46 tables were checked for personal information, missing values and the other column checks. Each file now has a `source_file` in `data_check`'s file table (`structure`), unique within the run: the file name when no other file has it, so nothing changes for most repositories; else its path in the repository (`scan1/log.csv`); and where even that repeats, the repository's name in front (internal `.data_source_keys()`). The tables (`previews`), the column table, the findings and `codebook_check`'s labels use this name
* `convert_bibr()` can convert PDFs with metacheck's API on SURF Research Cloud (`backend = "surf"`), which needs no API key, and uses it by default: `backend = "auto"` now picks SURF unless a Scienceverse key is given (`api_key`, `SCIVRS_API_KEY`), which still picks the Scienceverse platform, or a bibr address (`api_url`, `BIBR_API_URL`), which still picks a self-hosted bibr. Before, it picked a self-hosted bibr at `http://localhost:8000` when no key was set. The PDF is sent in one call to the API's `/convert` endpoint, whose address can be changed with the `SURF_API_URL` environment variable; the server's reason is passed on when a conversion is refused or fails. The SURF API converts only whole PDFs without figures, so a DOC or DOCX file is refused before anything is sent, and `include_figures`, `start_page` and `end_page` give a warning that they are not used. The server saves every upload as `input.pdf`, so that name is replaced by the file's own name in the JSON. `convert()` asks the SURF API first too: with `method = "auto"` or `"bibr"` a PDF is converted there when its bibr is ready (checked with the new internal `.surf_isalive()`), unless `api_url`, `api_key` or `backend` is given or there are DOC or DOCX files. When SURF cannot convert, the order is as before: a local grobid, a local bibr, then the online server list
* `repo_check` follows the junk-file rules of TU/e's data managers (the RAPS data manager check). Besides the operating-system files, RStudio session files, Jupyter checkpoints, compiled Python files and Office lock files it already left out, it now also leaves out editor backups (`file~`, `.bak`, `.swp`, `.swo`), temporary files (`.tmp`, `.temp`), `.Rhistory` (which can hold file paths and passwords), macOS `.TemporaryItems`, every `~$` Office lock file, and, as suggestions, version-control folders (`.git`, `.svn`, `.hg`) and editor settings (`.idea`, `.vscode`). These files are now something to fix: the light is red when the repository has any of the first kind, and yellow when it only has version-control folders or editor settings. They are returned in a new `useless_files` element (`paper_id`, `repo_url`, `file_path`, `kind`, `severity`), so other modules can use them. `local_files()` now lists hidden files of these kinds (`.DS_Store`, `.Rhistory`, `.git/`, ...), which it never listed, so `repo_check` finds them in a local folder too; other hidden files stay out
* The repository file tree (`data_check`'s "Data Tree") is drawn by internal functions in `R/file-tree.R` (`.file_tree_block()`, `.file_tree()`), which `data_check`, the repository apps' brief report and external modules share; the apps no longer keep their own copy
* The file tree shows everything in a repository: a local folder is listed itself, with hidden files, junk files and empty folders, and a repository link shows `repo_check`'s junk files too. Junk files are marked "junk: remove" or "junk: keep only on purpose", and a junk folder (`.git/`, `__pycache__/`, `__MACOSX/`) is one line with the number of files inside; other hidden files are marked "hidden file" and empty folders "empty folder". Folders, empty ones included, come before files. The tree sits in a box with its own scroll bar, so a large repository no longer makes the report endless, and the repository apps show it open instead of folded
* The file classifier recognises a licence file without an extension (`LICENSE`, `LICENCE`, `COPYING`) as documentation, Simulink (`.slx`) and COMSOL (`.mph`) models as code, Value Change Dump recordings (`.vcd`) as data, LaTeX figures (`.pgf`) as output, and lists of the packages a program needs (`requirements.txt`, `Pipfile`) as code; all were "unknown" or documentation
* The internal `.mc_request()` no longer takes over the help page of `.batch_query()` (it stood between that function and its documentation)

* Modules can be installed from GitHub (`module_install()`), so a module can be developed and shared in its own public repository instead of being built into metacheck. `module_install("owner/repo")` downloads the repository's default branch (or `ref`, a branch, tag or commit) into the metacheck user data folder, where downloaded paper corpora are also kept. Every .R file with a roxygen header in the repository's top folder or in a `modules/` folder becomes a module that `module_run()`, `report()`, `module_info()` and `module_help()` find by name, like a built-in module; other files (helper code, data) are kept with it. Nothing is saved when a module's name is already used by a built-in module, by a module of another installed repository, or twice in the same repository. A module can state the lowest metacheck version it needs with `#' @metacheck 0.3.4` in its header, which `module_install()` and `module_run()` check. `module_update()` downloads the newest commit of the branch a repository was installed from, `module_remove()` deletes it, and `module_installed()` lists the installed repositories with their commits. The environment variable `METACHECK_MODULES_DIR` names another folder for installed modules, so several users can share them (e.g. on a Shiny Server, where apps run as the `shiny` user)
* `module_list()` lists installed modules too, with the repository they came from in a new `source` column (NA for built-in modules); `module_list(external = FALSE)` lists only the built-in ones. In a report, an installed module's "How It Works" box names the repository and commit it came from, instead of linking to a chapter of the metacheck manual

# metacheck 0.3.3

* The brief report shows a reproducibility check whose code was run, also when every script ran without error (#504). The brief report keeps only modules with a red, yellow or failed light, and `reproducibility_check` is green when the code ran, so a successful run disappeared, and the report read as if the code had not been checked. A module can now ask to be kept in the brief report whatever its light, with `brief = TRUE` in its output; `reproducibility_check` does so when it ran at least one script (`execute = TRUE`). Checked with the repository from the issue, run in Docker: the brief report now lists the Reproducibility Check ("1 of 1 script ran without error")

* `reproducibility_check(execute = TRUE, sandbox = "docker")` no longer fails every script that writes a file (#502). Writes are redirected into the sandbox's `output/` folder, which was given as an absolute path on the host; the Docker sandbox mounts that folder at `/sandbox`, so the host path did not exist inside the container, and a script ending in `write.csv(...)` was reported as `errored` ("cannot open the connection") and turned the light red although its code was fine. The output folder is now given relative to the sandbox root (`output/`), which works in both sandboxes, since every script runs from the root, as the rewritten read paths already assume. Checked in a Docker container: the script runs and its file arrives in the host's `output/` folder

* `repo_check()` no longer drops OSF links when the OSF cannot be reached (#501). During an OSF outage (every request answered HTTP 503) each OSF link was removed together with the registrations, so the paper read as linking no repository at all, with nothing in `gated_repos`: in a corpus rerun, 21 OSF papers came back with `repo_n = 0`. Such a link now stays, as a repository that could not be checked, with its reason in `gated_repos` ("the OSF API did not answer (HTTP 503)", "not found on the OSF (HTTP 404)", "deleted or withdrawn from the OSF (HTTP 410)", "the OSF rate limit was reached (HTTP 429)"). An outage is no longer reported as "could not be found". A failed OSF lookup is no longer cached, on disk or for the session, so a later run asks again instead of replaying the outage
* `repo_check()`'s summary and report name repositories that could not be checked, with their reasons, instead of counting them as empty repositories or as repositories without a README, and the traffic light is no longer green when one could not be checked; this applies to every platform. The summary table gains `repo_unchecked`, so "no repository linked" (`repo_n = 0`) and "a repository linked but not retrieved" are told apart
* The repository check app no longer shows the server's temporary folder as the name of an uploaded .zip (#500). The zip is unpacked into a folder named after it, and the check runs from the folder above, so the report names the repository by its own folder name ("Repository: my_study")

* The plumber API accepts uploads from PowerShell and C# (#498). It read uploads with `mime::parse_multipart()`, which only finds a part whose `Content-Disposition` header comes first; .NET's `MultipartFormDataContent` writes `Content-Type` first, so every upload from those clients was rejected with "No file uploaded", and their form fields (`modules`, `fields`, ...) were lost too. The new `parse_upload()` reads the body with `webutils::parse_http()`, the parser plumber itself uses, which accepts the headers in either order; the endpoints are unchanged. Checked against a running API: curl, a raw body in either order, and a PowerShell 5.1 `MultipartFormDataContent` upload all deliver the file and the fields
* `read()` refuses a JSON file that is not bibr output (#499). A file such as `{}` became a paper with no `paper_id`, and every module then failed with "replacement has 0 rows, data has 1". Such a file now gives "… is not bibr output (it has no paper_id)": `read()` skips it with a warning, and the plumber API returns the message as a 400 error. `module_run()` also keeps a failing module's own error when the paper's id cannot be read, instead of replacing it with that second error

* A stalled web request no longer makes a check wait forever (#473). Most requests had no time limit, so a connection that stopped without closing (a Zenodo range request during `repo_check()`) kept the process waiting, with no output, for 40+ minutes. The 38 requests to repositories and databases that had no limit (zip and tar listing, GitHub, GitLab, OSF, ResearchBox, PsychArchives, DSpace, AsPredicted, FSD, DataONE, Dryad, CrossRef, PubPeer, Retraction Watch, RegCheck status, the paper corpora and `prereg_check`'s schema) now give up when the connection moves less than 1 KB/s for 60 s (300 s for a ResearchBox download, which the server builds first), or cannot connect within 30 s. This is not a limit on total time, so a large download that keeps moving is never cut off; a stall is an ordinary failed request, so it is retried or recorded like one. LLM calls, GROBID and bibr conversions, and uploads are not given this limit, because their servers send nothing while they work, and they have timeouts of their own
* `zip_peek()` asks for exactly the bytes of a large central directory (#473). When the first 128 KB tail is too short, the archive's end record says how many bytes the directory needs, and the second request asks for those instead of a fixed 1 MB: 293 KB for the Zenodo archive in the issue, and a directory over 1 MB, which a 1 MB retry could never list, is listed too (up to 64 MB)

* `data_check()` no longer fills the cache with decompressed copies (#472). A downloaded `.gz`, `.bz2` or `.xz`, zip or tar was unpacked in full, and its files were classified afterwards; the download limits saw only the compressed size. One repository (picrust2) left 6.4 GB of decompressed reference tables in the cache, all of which were then dropped as plain documentation (a 74 MB `go.txt.gz` became 1.2 GB). Archives now unpack only the files that are kept (data, code, READMEs and codebooks), and none larger than `max_file_size`: a zip's or plain tar's listing gives the sizes in advance, a `.gz` stops decompressing at the limit, and a compressed tar's kept files over the limit are removed straight after unpacking. A file over the limit is listed without a local copy and reported with the files too large to download, and a copy over the limit left by an earlier run is removed. Results for files within the limits are unchanged
* A test of `.rate_limit_wait()` failed when the clock passed a whole second while it ran; it now allows a few seconds

* `power` no longer loses power analyses without saying so (#475):
  * The LLM answers the type of power analysis, the statistical test, the effect size metric and the software in free text, and metacheck maps each answer onto the known values afterwards. Before, each was a fixed list, and Groq rejects an answer outside the list, so one unexpected value failed the whole call: two power analyses done with POWSIM were lost this way, every time. Known values keep their spelling; an answer that matches none becomes "other" (type: "unknown") and is kept in `statistical_test_other`, `effect_size_metric_other` or the new `software_other`
  * The software list now holds the power-analysis and sample-size packages on CRAN (116, from CRAN's own package list), widely used packages whose title does not say so (MBESS, faux, simglm, ...), packages no longer on CRAN or from Bioconductor (PowerUpR, powerlmm, mixedpower, powsimR, ...), and other software (POWSIM, nQuery, Optimal Design, GLIMMPSE, MLPowSim, PINT, jpower, jamovi, JASP, SAS, Stata, SPSS, and R's own `power.t.test()`)
  * Paragraphs the LLM could not check are reported instead of being counted as having no power analysis: the summary and report say how many, the traffic light is yellow instead of green or "not applicable", and the summary table has a new `power_unchecked` column per paper, so a corpus run can find and re-run them
  * metacheck needs ellmer 0.4.2 or later: with an older ellmer, Groq rejects every structured call (structured output for `chat_groq()` came in 0.4.1), and its default Groq model has been retired
  * The test for structured `power` calls to Ollama replaces `ellmer::chat_vllm()` as well, which those calls use since #494; it failed since that change

* `report()` keeps its header for a paper without a title (#497). An `NA` title (a converter that found none) filled the header template with `NA`, and `sub()` then returned `NA` for the whole header: the report lost its styling, logo, table of contents and info box, started with "NA", and the full report was no longer self-contained (25 KB with 21 links to a `_files/libs/` folder, against 3.7 MB). A missing value now fills its place with nothing. A paper with no `doi` field at all, which stopped `report()` with "invalid 'replacement' argument", is handled the same way. In the simple report (`report_type = "simple"`), `report_table()` now writes its column widths as `<col .../>`: Pandoc read the unclosed `<col>` as an element left open, warned that the page's `<div>` was unclosed, and the page ended with a stray `</div>`

* `repo_check()` no longer hides why a repository link failed (#476). When every link of a platform on a paper failed, the platform's lookup returned no `doi`/`license` columns, and building the metadata table stopped with "arguments imply differing number of rows: n, 0", which was then reported for every link in place of the real reason (220 rows of `gated_repos` in a corpus of 1,861 papers). All platform blocks (Zenodo, Dataverse, Figshare, Dryad, ReShare, 4TU, Mendeley, DataONE, LDbase) now build that table the same safe way, and each link's own reason ("unfound", "parse_error", ...) is reported in `gated_repos`; before, a link that could not be looked up disappeared from the results without a word
* DataONE: a DOI written in lower or mixed case is found (#476). DataONE matches identifiers as exact strings and KNB stores DOIs in upper case, so `10.5063/f11z42jx` was "unfound". After a 404, a `doi:` identifier is asked for once more in upper case; an identifier found as written is never changed, and identifiers that are not DOIs are never changed
* Dataverse: a dataset cited by its Handle (`persistentId=hdl:10411/B2VWDH`) is found (#476); only `persistentId=doi:` was read, so the lookup returned an empty record. A `persistentId` whose colon is percent-encoded (`doi%3A...`) is read too

* `convert(method = "bibr")` reaches bibr again (#489). It passed only the names of its arguments to `convert_bibr()`, so it always stopped with "second argument must be a list"; the change for #299 had replaced `args` with `valid_args` instead of `args[valid_args]`
* `convert_bibr(backend = "selfhosted")` works with a bibr server that requires a bearer token (#490), which `bibr serve` does in production mode and on any non-loopback address. The token comes from `api_key` or the new `BIBR_API_KEY` environment variable, the default address from `BIBR_API_URL` (else `http://localhost:8000`), and `timeout` now applies to this backend too instead of a fixed 300 seconds. Without a token nothing changes
* `.bibr_isalive()` no longer stops with "missing value where TRUE/FALSE needed" on a running self-hosted bibr (#491): `bibr serve`'s `/ready` answer has no `checks$bibr` entry (only the Platform's has), so `convert(method = "auto")` crashed exactly when a local bibr was running. The entry is now only checked when present
* In `report_app()`, choosing the local GROBID server no longer fails with "could not find function" (#493): the app calls the internal `.grobid_isalive()` as `metacheck:::.grobid_isalive()`
* Structured LLM calls to Qwen 3 models on Ollama work (#494). Ollama's OpenAI-compatible `/v1` endpoint ignores `think = FALSE`, so the model kept reasoning until `max_tokens` cut the answer off, and every extraction failed (all 3 of the demo paper's power analyses, after 126 s with qwen3.5:4b). Such calls now also send `reasoning_effort = "none"`, which that endpoint honours, and go through ellmer's OpenAI-compatible provider: ellmer's Ollama provider marked optional schema fields as not required, and the small model then left out sample size and power and invented an effect size. The demo paper's power check now takes about 6 s and finds N = 50 and power = 0.8. Free-text calls keep using Ollama's native `/api/chat`
* In the Shiny Server report app (`inst/app/shiny_app`), unticking "Query Data Repositories" also skips `prereg_check` (#495), which contacts the OSF API and AsPredicted; the checkbox names the OSF and the privacy panel said no repository would be contacted
* `repo_check()` follows Dryad sharing links (`datadryad.org/stash/share/...`) to the dataset they lead to (#484). Authors sometimes cite the link made for reviewers instead of the DOI; it still works after publication, but carries no DOI, so the paper ended up with no Dryad repository. The link is now resolved with one request (Dryad redirects it to the dataset's page, whose address carries the DOI), and `dryad_links()` also finds it in the text. A link that leads nowhere is reported as "private or expired Dryad sharing link". Dryad's metadata is now built as Zenodo's is, so when every Dryad link of a paper fails the report gives each link's own reason ("unfound", or the sharing-link message) instead of "arguments imply differing number of rows" (the Dryad part of #476)
* `repo_check()` reports a link to a GitHub account or organisation (`https://github.com/OneZoom/`) as "links to a GitHub account, not to a specific repository", not as an "invalid or inaccessible GitHub repository" (#485). `github_links()` now keeps such links instead of dropping them, and a repository name of only dots (what a sentence's full stop after an account address produced) is no longer looked up. A GitLab address with one path segment is reported the same way
* The OSF licence is found for registrations (#486). The OSF API answers 404 for a registration at `/nodes/<id>/`; `.osf_license()` and `.osf_node_metadata()` now ask `/registrations/<id>/` after a 404. With `osf_license = TRUE`, `repo_check()` now looks up the licence of the project a cited registration was made from, which is where its files are listed from, and attaches that row to the paper (the row for the registration had no `paper_id`); a project that is closed or has no licence takes its registration's licence
* README detection (#487): `read.me.txt` is recognised as a README (the dotted form only as a whole word, so `spread.means.csv` is not), by one rule now shared by `file_category()`, `repo_check()` and `code_check()`. A Word, PDF, ODT or RTF document with "data" in its name or folder is documentation, not data (`Data description.docx` was classed as data), and a document named "Data description" or "Dataset description" gets the README role
* `repo_check()` lists the files inside a plain (uncompressed) `.tar` (#487). A tar has no index at its end, so it cannot be listed with a partial download as a `.zip` is; listing it header by header took one request per file (Zenodo refused after 134). Instead, a `.tar` whose size the repository gives, up to 50 MB, is downloaded once, and its file names and sizes are read from that copy, including long names in pax and GNU form; a larger one, or one of unknown size, is not downloaded and stays one row, as before. The copy is kept for the session, so `data_check()` takes the data and documentation files from it without another request (in a new session the archive is downloaded once more). This also means the default `download = "data"` now fetches data from such a tar, which it never did (the plain-tar part of #444). The READMEs of the two deposits in #487 (Zenodo 10.5281/zenodo.4037178 and Dryad 10.5061/dryad.3r1v5) are now found. A listed `.tar` is still named in the report's advice to use `.zip`. Compressed tars (`.tar.gz`, ...) are unchanged
* GeoTIFF rasters are data (#488). A `.tif`/`.tiff` in a folder that holds GIS files (shapefile parts, world files, GDAL sidecars, other raster formats) is classed as data instead of an image; elsewhere it stays an image. World files (`.tfw`, `.tifw`), raster overviews (`.ovr`), GDAL sidecars (`.aux.xml`), NetCDF (`.nc`), raster grids (`.grd`/`.gri`), band files (`.bil`), virtual rasters (`.vrt`) and KML/KMZ are data by extension
* `.csv_is_space_separated()` no longer prints a warning for a file that does not exist

* `reproducibility_check()` no longer spends hours parsing a script's long console output (#483). In `.r_output_tables()`, a printed data frame with no numeric column made every row start a new scan of the rest of the print, so the time grew with the square of the number of rows: 384 s for 1,000 rows, and hours for the 10,000-row prints of long-running scripts (one paper took 6.7 hours, another was stopped after 7). A rejected block is now remembered, and a later start inside it is only split when its pattern of non-blank character positions differs, which is the only way its result can differ. The output is identical on 113 cases, including real `summary(lm)`, `anova`, `t.test`, `aov`, `glm`, tibble and statsmodels output and a table printed directly after a text-only print; 1,000 rows take about 3 s. A progress line is printed before each script's output is parsed
* Eleven internal functions and one constant were defined twice in `R/` (#482): nine statistics helpers and `.STAT_PLACEHOLDERS` also in `R/stat_helpers.R`, which was created as their single home, `.html_inline_images()` in both `jasp.R` and `omv.R`, and `.repro_split_args()` in both `r-output.R` and `reproducibility_check.R`. R loads `R/` alphabetically, so one copy of each silently replaced the other, and an edit to the unused copy had no effect. One copy of each is kept (the copies were identical, so nothing changes), and a new test fails when any name is defined twice
* `reproducibility_check()` no longer hangs for hours on a large script (#481). Finding the end of each write call walked the text one character at a time with `substr()`, which for text with a non-ASCII character counts from the start on every call, so the walk was quadratic in the text's length: 201 seconds for a 200,000-character script, and hours for the 1-MB notebooks (read as raw JSON before #479) that hung 3 of 486 papers. The text is now split into characters once; results are identical on 413 real R and Python scripts, and a 1-MB script takes 8 seconds. The same change is made in the six other places that walked text this way (`.repro_format_call_refs()`, `.repro_split_args()` (defined twice), `.repro_content_sniff()`, `.r_call_object_ref()`, `.omv_extract_syntax()` and `.smcl_render_line()`), and `repro_write_scripts()` prints a line per script, so a slow rewrite can be told apart from a hang
* Follow-up to #479 (#480):
  * `reproducibility_check()` replaces a `setwd()` or `os.chdir()` call with a statement that does nothing (`invisible(NULL)` in R, `pass` in Python), keeping the call visible in a comment. It used to comment the call out, which left a block with no statement when the call was the only one in it: Python stopped with "IndentationError: expected an indented block", and in R `if (x)` followed by `setwd()` on the next line took the following statement as its body. The Python comment now says `os.chdir` instead of `setwd`
  * When Python cannot be used because the `processx` package is not installed, `reproducibility_check()` now says so, instead of reporting that no Python interpreter was found
  * A notebook is labelled Python only when its kernel is Python: other kernels get their own language ("Julia", "Scala", ...), and a notebook that declares none gets no language. A Quarto document whose engine is Julia is no longer labelled R. `code_check` now reads the language from the downloaded file; before, it only ever saw the file name, so every notebook was labelled Python and every Quarto document R, and an R notebook or a Julia notebook could be run as Python
  * The report names `metacheck_r_large`, the current default Docker image, and says the scripts ran in a Docker container when `sandbox = "docker"`
  * `data_check()`'s report no longer opens by naming checks it does not run (outliers, miscoded missing values, constant columns), and code comments no longer describe them as active
  * `data_check()` reports careless responding per file: the same respondent id in two files (e.g. respondent 58 of Study 2 and of Study 4) was reported as one person
  * `data_check()`'s light is yellow, not green, when tabular data files were found but none could be read, since not one column was checked
  * Only scale blocks of at least 5 items count for the careless-responding screen, as documented; a file whose blocks are all shorter was counted as screened although it could never flag anyone
  * Spreadsheet findings use the right verb for the count ("1 cell uses fill colour", "1 merged range breaks the grid", "1 column is empty or has a blank header")
  * `codebook_check` checks values only against a range the codebook documents, as its documentation says: when the documented codes explain less than half of a column's values (probably codes attached to the wrong column), the column is skipped instead of being checked against a range guessed from the data. `data_check_scale_values()` gains `infer` (default `TRUE`, unchanged) to allow this
  * Tests for the space-separated `.csv` warning
* Not previously listed: when `data_check` took over the former `data_validate` module's checks (0.2.0), the rating-scale check (out-of-range values and miscoded missing values) moved to `codebook_check`; constant columns are no longer flagged; in the careless-responding screen only straightlining flags a respondent, not response variability (IRV); the distribution figure no longer draws outlier lines; and the `outlier_k` argument was removed (passing it stops with "unused argument")

* `repo_check()` and `data_check()` leave out files that do not belong in a repository because software creates them by itself and they hold nothing a reader can use: files the operating system adds (macOS's `__MACOSX/` folder and `._` copies added to every file a Mac puts in a zip, `.DS_Store`, Windows's `Thumbs.db` and `desktop.ini`, and similar), RStudio's `.Rproj.user/` session files, Jupyter's `.ipynb_checkpoints/`, compiled Python files (`__pycache__/`, `.pyc`), and the lock files Office and LibreOffice leave while a document is open (`~$*.docx`, `.~lock.*#`). The patterns come from GitHub's ignore-file templates, and each was found in real repositories; files that may matter to someone (`.RData`, `.Rhistory`, backups, `*.tmp`, knitr caches) are kept. These files were counted, flagged for naming, and the `._` copies of `.csv` files were read as data, which failed with a warning each: in one LDbase zip they were 993 of 1,826 listed files. `repo_check()` lists them, by kind, in a new report section, "Files that do not belong in the repository", and asks the authors to remove them
* `repo_check()` supports LDbase (https://ldbase.org), the data repository for educational and developmental sciences at Florida State University. New `ldbase_links()` finds links to LDbase projects, datasets, code and documents and LDbase DOIs (prefix 10.33009; LDbase's own DOI `10.33009/ldbase` is ignored), and `ldbase_info()` lists their files: for a project, every dataset, code and document in its hierarchy (child projects included), each in a folder named after the item; for a single item, its current file. LDbase has no API, so the public pages are read (one request per project or item) and files download without an account. A DOI is resolved through doi.org's handle API. The licence comes from each item's page (a project's distinct licences are joined), and a link that cannot be listed keeps its reason in `repo_error`. LDbase's own "download all project files" zip is not used: it failed (HTTP 500/503) for four of five projects tried. LDbase pages are requested at most once per second, because LDbase's firewall answers with a browser check after a burst of requests. Checked on 10 LDbase projects: every item in each project's hierarchy was listed, and data_check downloaded and read every data file within its size limit. DOIs under 10.33009 are no longer also looked up by `dspace7_links()`, and `open_practices` counts "LDbase" as a repository word
* When a host's Amazon web firewall answers a request with a browser check (HTTP 202 and `x-amzn-waf-action: challenge`) instead of the file, metacheck now warns once per host that its files cannot be sized or downloaded until the check lifts. Such files used to be left out of `download_repo_files()`'s downloads without a word, since their size could not be read
* `reproducibility_check()` assesses and runs Python code as well as R code (#479). Python files, and notebooks and Quarto documents whose code is Python, go through the same static phase (dependencies, path rewrites, run order, missing inputs) and, with `execute = TRUE`, are run with `python`, in their own "Python code" section of the report
* `reproducibility_check()` compares the reported statistics with the analysis output once, after the R and the Python code, so it includes what the Python scripts print. It used to run inside the R part only, before any Python output existed
* In `reproducibility_check()`, a paper with Python but no R code no longer loses the SPSS/Stata, self-reproducible-output and other-language sections, the comparison of self-reproducible output with the paper, and the red light for SPSS or Stata data without syntax
* `reproducibility_check()` extracts the code in Python notebooks (`.ipynb`) and Python Quarto documents (`.qmd`) before running it; the notebook's JSON or the document's Markdown was run as Python before. R-kernel notebooks are extracted the same way on the R side
* `reproducibility_check()` checks Python files for syntax errors with Python's own parser when a Python interpreter is available, so `not_parsed` now occurs for Python too (code written for Python 2 fails this check)
* `reproducibility_check()` uses `python3` when `python` is not on the path, and when neither is found the Python files are reported as not run while the R results are still returned; the whole module used to stop
* `reproducibility_check()` installs Python packages whose import name differs from their pip name (`sklearn`, `cv2`, `PIL`, `yaml`, `bs4` and others) under the pip name (`scikit-learn`, `opencv-python`, ...) and matched to their version in `requirements.txt`
* `reproducibility_check()` only requires `callr` when the paper has R code
* `reproducibility_check()` no longer re-runs a Python script that fails on an undefined name in a corrected order: each Python script runs in its own process, so a name another script defines is never available to it. The report names the script that defines the name instead
* `reproducibility_check()` returns its summary as one text for a paper with both R and Python code. It returned two, which made the report stop with an error under R 4.3 and later
* `.repro_py_defined_vars()` is no longer exported
* `reproducibility_check()` no longer fails R scripts on `%>%` (or another function) because the packages are loaded in a different script of the same project. Every script runs in its own R process, and 15 scripts in 11 papers failed this way. A script that another script runs with `source()` is now run only inside that script (outcome `run_by_source`), instead of also on its own, where it lacks the packages and objects the sourcing script sets up first. For another script that fails with `could not find function`, the re-run adds the `library()` calls the project's other scripts make (all of them, since such a script usually needs more than the one function that failed first), or else the one package that exports the function, or else magrittr for `%>%`. As for a removed `setwd()`, each added call is recorded in `modifications`, and a script that then runs is reported as reproducible only after the repair. `repro_run_order()` returns every `source()` edge in a `"source_edges"` attribute, and `repro_write_scripts()`'s `inject_libs` accepts several packages per script
* `data_check()` warns when a `.csv` file is separated by spaces instead of commas ("Space-separated .csv"), recommending to save it as a comma-separated `.csv` or a tab-separated `.tsv`. Such a file reads as one column of text, so none of its columns were checked, and the light could stay green
* `data_check()` keeps its summary, report and traffic light for a repository with no readable tabular data (only code, documents or images, or data files that are not usable tables). These were lost: the module returned only the spreadsheet-formatting findings, and returned them as `table`, where `codebook_check` and `psychds_check` expect the column table. `table` is now an empty data frame, as data_check returns whenever no column could be read, and the findings are in `findings`, as for every other repository
* The comparison of reported statistics with analysis output (`match_reported_output()`) also uses the type the output side resolved from the producing call when a value's own name is not recognised. A printed scipy result names its values `statistic` and `pvalue`, so a reported t-test could never match a `TtestResult` before. `F_onewayResult` (F), `PearsonRResult` (r) and `Chi2ContingencyResult` (chi-squared, with its `dof`) are now typed as well; `spearmanr()` and `kendalltau()` share one result class, so their coefficient stays untyped
* `reproducibility_check(workers > 1)` copies every `metacheck.*` option into each paper's background R process (#474). Only the Docker resource limits were passed before, so `metacheck.cache.dir` was lost and every cache was written into the working directory (4.5 GB in one run), downloading repositories again that were already in the configured cache
* `data_check()` no longer takes hours on wide tables with long column names (#477). The check for trial-level files (Behaverse, Inquisit, jsPsych) reads only the header line instead of calling `read.csv(nrows = 1)`, whose time grew with the square of the number of columns: for a 1,600-column table with 250-character names, 0.06 s instead of 20 s, and about 1.5 hours per file on a real 12,110-column table
* `repo_check()` (and so every module that uses its file list) keeps the files of a root-level R package that is the paper's deposit when the package has configuration files for build, test or website services (`.travis.yml`, `codecov.yml`, `appveyor.yml`, `.gitlab-ci.yml`, `_pkgdown.yml`, `.github/`, ...) (#469). These classify as code by their `.yml` extension, so the package looked like a copy of someone else's package and all its real files were discarded (229 of 238 files for one paper). They are now left out of that decision only, by exact name, so a paper's own `config.yml` still counts
* `check_file_naming()`'s help no longer lists letter-case rules (lowercase only, no CamelCase), which it does not check
* `data_group_llm()` (the study-group step of `data_check()`, `repo_check()` and `psychds_check()`) sends nothing to a model when `llm_use()` is off and groups the files by its rules alone. `llm()`'s refusal used to be counted as a failed request, so with LLM use off (as on the hosted apps) every report said "The study-group step did not get an answer for N files (the model request failed)"
* `data_check()`'s summary in a brief report (`report_type()` "brief" or "simple_brief") no longer says how the files were grouped into studies, whether the study-group step got an answer, or which demographic columns were found: a brief report is about what to improve. The full report is unchanged
* Listing an OSF repository takes far fewer requests. `osf_info(recursive = TRUE)` (used by `repo_check()` and `osf_file_download()`) lists a project's whole component tree, with every component's storage providers, in one request per 100 components (`/nodes/?filter[root]=<project>&embed=files`) instead of two requests per component; `osf_file_download(mode = "all")` finds its components the same way. A GitHub repository linked to an OSF project is listed through GitHub itself (2 requests for the whole repository, with file sizes) instead of one slow OSF-proxied request per folder, and its files are downloaded from GitHub. On a paper linking three OSF projects, a ResearchBox and a Zenodo record, `repo_check()` went from 61 requests and 246 seconds to 38 requests and 81 seconds, finding the same 2,809 files. Each shortcut falls back to the previous one-request-per-item listing whenever it cannot be used (e.g. a private GitHub repository the OSF shows through its owner's account). One difference: a private component that the OSF token's owner can see without being a contributor to it is no longer listed, since it is not part of what a paper shares with its readers
* `zip_peek()` gains a `size` argument: a size the caller already has (from a repository listing) replaces the HEAD request otherwise made to learn it. `repo_check()` and `code_check()` pass the listed size, which saves one request per archive -- on the OSF's file server each request took 2 to 20 seconds, however small the file
* `dryad_info()` now lists every file of a Dryad dataset. Only the first 20 were read before (Dryad's default page size), so larger datasets were silently cut short; on one corpus, 18 datasets lost 645 of their 1,005 files. The listing asks for 100 files per page and follows every page, warns when fewer files arrive than Dryad reports, and is then not cached. It also returns `storage_size` (Dryad's `storageSize`, which `download_repo_files()` now uses to size a whole-dataset download), `files_total` and `files_incomplete`
* `dryad_file_download()` now verifies SHA-256 digests as well as MD5. Dryad publishes SHA-256 for current datasets, and these were never checked
* `download_repo_files()` uses Dryad's bulk download for every Dryad DOI form, including other prefixes (such as 10.25338) and bare `10.5061/dryad.x` citations
* `dspace7_links()` no longer looks up the paper's own DOI, or DOIs under prefixes that other detectors handle (OSF, Zenodo, Dryad, Figshare, 4TU.ResearchData, Mendeley Data, DataONE, ReShare, PsychArchives, Dataverse). The remaining DOIs are read from doi.org's handle API instead of loading the landing page, once per session, which cut the average time per DOI from 5.3 s to 0.3 s on 366 DOIs. DOIs registered at a repository's former address (e.g. smartech.gatech.edu) are now recognised too
* DSpace 7 items are listed in 1 request by uuid (was 3) or 2 by handle (was 4), and items with more than 20 files are no longer cut short (DSpace's page size). dspace.ut.ee is now looked up only through the DSpace 7 API; it was listed for both API families, so every citation was looked up twice
* PsychArchives and other legacy DSpace items are listed in one request instead of three, and only the files the author deposited (the ORIGINAL bundle) are listed, as for DSpace 7: the deposit agreement (license.txt), text DSpace extracts from PDFs and preview images are no longer counted as shared files. The licence is now read from `dc.rights.license` / `dc.rights.uri`; `dc.rights`, which holds the access level on PsychArchives ("restrictedAccess"), was reported as the licence before and now only drives the restricted-access flag. DSpace 5 installations (e.g. datastorre.stir.ac.uk) are now supported; their items were always reported as not found
* 4TU.ResearchData: only legacy `10.4121/uuid:` DOIs are resolved to a numeric id, now through doi.org's handle API. Bare-uuid DOIs go straight to the API instead of first trying a DOI that does not exist, and `researchdata4tu_file_download()` now resolves `uuid:` DOIs too
* `zip_peek()` now reads Zip64 archives (over 4 GB or over 65,535 entries), including sizes and offsets stored only in the Zip64 extra field, so their members can be fetched one by one (verified on a 9.1 GB Figshare zip). It also accepts the path of a zip already on disk: a zip inside a ResearchBox is now listed and extracted from the local copy instead of peeked at the box's web page, which failed with a 406
* Fetching a member of a remote zip takes one range request instead of two, and members that lie close together share one request (7 code files from a Harvard Dataverse zip: 14 requests became 1). Every host, not only Dryad, now downloads a zip whole when that is cheaper than fetching its members by range; Dryad keeps taking the archive whenever it fits the download budget
* A host that ignores Range requests (such as PsychArchives) no longer makes `zip_peek()` transfer the whole file into memory: large answers are closed unread, and later peeks of the same kind of URL give up at once for the rest of the session
* `zip_decision()` and the zip-member helpers for Zenodo, Figshare, Dataverse, Dryad and ReShare pass a known archive size on to `zip_peek()`, and repo_check's archive-member rows carry `archive_size`, which saves a HEAD request per archive (Figshare's file server refuses HEAD with 403)
* `dataverse_info()` queries each dataset once, even when `dataverse_links()` returns both its page link and its bare DOI (it was fetched twice). A DOI whose prefix several Dataverse installations share (DANS, RODBUK) is resolved through doi.org's handle record (about 0.35 s instead of 3.8 s) and remembered for the session; its hostname is now matched exactly, so a DOI registered at `uj.rodbuk.pl` is no longer sent to `rodbuk.pl`
* `github_tree_files()` and `gitlab_tree_files()`, and `download_repo_files()` for GitHub and GitLab repositories, no longer send a HEAD request to the repository's web page before the API; a 404 from the API reports the repository as invalid or inaccessible
* DataONE file sizes missing from the EML now come from one Solr query on the member node instead of one HEAD request per file; the HEAD request remains as a fallback
* `zenodo_file_download()` stops trying Zenodo's whole-record `files-archive` endpoint after its first 404 in a session (it returned 404 for every record tested), and no longer reports "Downloaded as one archive" when that download failed
* Figshare: files in folders keep their folder (`folder_structure`) in `figshare_info()`, in `repo_check()`'s file paths, and when saved by `figshare_file_download()`. When an article's listed files add up to less than its size, the full list is read from `/v2/articles/{id}/files`
* `.batch_query()` no longer waits after the last batch, which removes 0.5 s from every single-record lookup
* `file_types` has one type per extension (377 rows; it had 404, with 27 extensions listed twice). The duplicates doubled the rows of those files in GitHub, GitLab, DSpace, PsychArchives and ResearchBox listings, and gave `file_category()` types like "code;data" that none of its rules match. `.json`, `.rds`, `.por` and `.jasp` are now data; shell scripts and program source (`.sh`, `.bat`, `.js`, `.ts`, `.php`, ...) are code; `.html`/`.htm` are web pages (documentation)
* `.osf_pat_validate()` allows 15 seconds and one retry instead of a single 5-second try, so a slow OSF no longer makes a valid token look unverifiable
* `module_report()` is exported again. A helper placed between its documentation and its definition made the exported name `.report_title_slug` instead, and its help page was renamed to match, since commit d3b8594
* `mendeley_info()` reports a nonexistent dataset as "unfound" whether Mendeley answers with an error status (HTTP 400, as now) or with HTTP 200 and only an error in the body (as before); the latter used to come back as a dataset with every field empty
* The bundled paper schema (`inst/schema/paper.json`, bibr 10.5) now names itself `https://scienceverse.org/schema/paper-v10.5.json`. Since scienceverse/schema#2 (2026-10-13), `paper.json` at that site is bibr's 12.2 export schema and the 10.5 schema is kept at `paper-v10.5.json`; `data-raw/schema.R` and the schema test now use that address
* `.bibr_isalive()` reports a rejected API key (HTTP 401/403, as platform.metacheck.app now answers) as "the API key is not valid" instead of "the server does not appear up and running"

# metacheck 0.3.2

* Members of a Dryad zip archive are now fetched by downloading the archive once and extracting them locally, when at least 3 members are wanted and the archive fits within `max_download_size`. Fetching member by member cost two requests per member against Dryad's daily request limit, so a corpus run either took days or left most archive contents undownloaded. The downloaded archive is kept, so a restarted run does not request it again. Other hosts still fetch members by byte range. Applies to `download_repo_files()` (used by `data_check()` and `code_check()`) and to `code_check()`'s own zip expansion (scienceverse/metacheck#471)

* `read()` reads bibr export schema 12.x files (a JSON file with a root `schema_version`; other versions stop with an error). Captions and footnotes are text rows with no section, which `figure`, `table` and the new `footnote` table point at with `text_id`; in the `xref` table `xref_id` is the row's own key and `target_id` the row it cites. Files without a root `schema_version` read as before
* `read()` reads bibr export schema 12.1, which bibr 0.6.0 and the ScienceVerse Platform write: the `extraction.fields` block it adds, the state of each tracked field, is kept in `paper$extraction$fields`. An export in another schema stops with an error that says what to do: extract the paper again with a bibr version that writes 12.x, or, for a newer schema, update metacheck
* `read()` warns with the reason when it cannot read a file and returns the papers it could read. It used to stop with "The arguments must be paper objects or lists of paper objects", which hid the reason
* `paper_write()` gains a `schema_version` argument: `"12.0"` saves a paper read from a bibr 12.x export as a bibr export schema 12.0 file, keeping its producer and naming metacheck as the converter. The default output is unchanged
* `grobid_to_bibr()` gains a `schema_version` argument: `"12.0"` converts Grobid TEI to bibr export schema 12.0, with 1-based ids, cross-reference targets resolved from the TEI's xml:ids, one text row per caption and footnote, and the PDF next to the TEI as the source. The default conversion is unchanged
* Fixed `data_check()` crashing on any report that builds a per-file tabset (every data/codebook preview with more than one file): `inst/modules/data_check.R`'s `file_tabset()` called `report_type()`, a function that was never defined anywhere in the package -- its call site had been committed without the function's own definition landing alongside it. Reverted `file_tabset()` to its previous, unconditional-tabset behaviour.
* `.process_full_text()`'s GROBID line-wrap whitespace fix (#411) extended to more safe anchors (a DOI's "10.NNNN/" prefix, and the single-fixed-host literals `zenodo.org/`, `osf.io/`, `researchbox.org/`) and more platforms, resolving 10 more corpus papers whose repository DOI/URL was split by the same line-wrap artifact at a different position (scienceverse/metacheck#458)

# metacheck 0.3.1

* DSpace 7+: `dspace7_file_download()` now carries an item's doi/license as attributes (mirroring legacy DSpace's `psycharchives_file_download()`), so `repo_check()`'s `repo_metadata` table gets a row for a DSpace 7 repository instead of none at all, even when its file listing succeeded (scienceverse/metacheck#435)
* DataONE: a record whose EML lists a file as a bare `<otherEntity>` with no `<physical>` child (seen on `knb.ecoinformatics.org`) no longer silently returns zero files -- its own `urn:uuid:...`-shaped id is now used as the file's PID when resolvable, with size recovered via a HEAD request (scienceverse/metacheck#435)
* Fixed `dataverse.no`'s DOI-prefix list wrongly including `10.6084` (Figshare's own globally-registered prefix), which misrouted a Figshare collection DOI through Dataverse's dispatch and failed retrieval instead of ever reaching `figshare_links()`'s own correct match (scienceverse/metacheck#434)
* Figshare: `.figshare_collection_id()` now recognises a collection DOI (`10.6084/m9.figshare.c.<id>`), not just a `figshare.com/collections/...` URL -- previously invisible to id extraction even though `figshare_links()` already detected the citation (scienceverse/metacheck#434)
* `zip_peek()` gained an on-disk cache (`cache = TRUE`, mirroring `repo_info_cache()`) so a restarted R process reuses a zip's already-peeked listing instead of re-issuing the range request; `repo_check()`/`data_check()`/`code_check()` forward their own `cache` argument to it. Also fixed `skip_on_api_limit` not reaching `zip_peek()`'s own HTTP requests: `repo_check()` gained a `skip_on_api_limit` parameter, and `data_check()`/`code_check()` now forward theirs to both `repo_check()` and the zip-peeking step, so a caller who asked not to wait for a confirmed rate limit no longer still waits during a zip peek (scienceverse/metacheck#427)
* `reproducibility_check`: a paperlist with `workers > 1` (and `execute = TRUE, sandbox = "docker"`) now runs that many papers' Docker checks concurrently, each in its own background R process, starting the next queued paper as soon as a slot frees up; each container is capped at `1/workers` of the host's CPU and memory, and a `results_dir` writes each paper's result as soon as it finishes
* Fixed `repro_install_deps_docker()` silently corrupting the generated install script for a paper with enough dependencies (18+ in practice) that `deparse()` wrapped to multiple lines, zeroing out every install in the batch (scienceverse/metacheck#418)
* Fixed a Docker container being left running (and consuming CPU/memory indefinitely) when a script's execution or dependency-install timeout fired — the container is now explicitly stopped, not just the `docker run` CLI process that started it (scienceverse/metacheck#417)
* `extract_p_values()` now also detects upper-case "P = .04" (affects "all_p_values", "stat_p_exact" and "stat_p_nonsig")
* "stat_p_nonsig" treats "≤" like "<", so "p ≤ .01" is no longer listed as non-significant
* "code_check" can now return a green traffic light, and the parse-error table is shown when more than one file was checked
* "causal_claims" no longer excludes every sentence containing the word "random" (e.g., "random assignment to conditions"); only "pseudorandom" is excluded
* "open_practices" no longer counts "available on request" as open sharing in the summary table
* "ref_miscitation" reports all matched papers, not just the last one
* "ref_retraction" report text no longer refers to replication studies

# metacheck 0.3.0

* DataONE: added `data.piscoweb.org` (PISCO, UC Santa Barbara) as a known member node, and fixed `.dataone_pid()` not recognising a bare DOI whose suffix itself contains a further slash (PISCO's own `10.6085/AA/<id>` shape) -- both found via a real paper citing this DOI with no host domain in the URL at all, which `dataone_links()` could not resolve at all before this fix
* Figshare: added 28 more institutional instances (found via DataCite's client registry) and Monash University's `bridges.monash.edu` (found separately, via a real paper citation), and taught `.figshare_id()`/`figshare_links()` to recognise each instance's own DOI prefix (e.g. `10.26180` for Monash) when cited as a bare DOI with no host domain in the URL -- previously only the generic `10.6084/m9.figshare.*` DOI shape was recognised this way, silently missing every institutional instance's own prefix regardless of how many hosts were in `.figshare_vanity_hosts()`
* Fixed `.figshare_id()` not recognising a Figshare article URL with no resource-type segment (`figshare.com/articles/<slug>/<id>`, as opposed to `figshare.com/articles/dataset/<slug>/<id>`) -- an older/plainer, still commonly-cited Figshare URL shape that `figshare_links()` already detected but could not resolve to an id, silently leaving `repo_check` unable to fetch a real Figshare article cited by its own link. See scienceverse/metacheck#411
* New `reproducibility_check` module: assesses whether a paper's code can run on its data. Static analysis (dependencies, run order, missing inputs, undefined symbols with a same-file source suggestion) always runs; `execute = TRUE` additionally runs the code in an isolated `callr` subprocess or a locked-down Docker container, and matches its console output against the paper's own reported statistics
* New `psychds_check` module
* New repository platforms: GitLab (with licence/DOI metadata), Mendeley Data, DataONE, the Finnish Social Science Data Archive (FSD), and many more Dataverse/Figshare/DSpace installations; `repo_check` also now lists what a Zenodo/Dryad zip archive contains without downloading it (`zip_peek()`), and excludes R package source trees (DESCRIPTION/NAMESPACE/R/man/tests) from a repository's data/code listing
* Dryad: OAuth2 client-credentials authentication, a real byte cap during zip streaming, quota-aware zip-vs-file-by-file downloading, and a raised zip-vs-file threshold
* Rate-limit handling rewritten: a 429 with a confirmed `RateLimit-Reset`/`X-RateLimit-Reset` header (Dryad, Zenodo, GitHub, GitLab) now waits for the host's own stated reset instead of blind exponential backoff, and repeated requests to an already-rate-limited host reuse that reset instead of each independently rediscovering it. New `skip_on_api_limit` parameter on `data_check()`/`code_check()` to give up immediately instead of waiting. Rate-limit messages now name the host
* `download_repo_files()` and friends: a per-repository file-count cap and request timeout, per-paper download manifests, an optional on-disk cache for `repo_check`'s own listing/API calls, and `max_file_size`/`max_download_size` caps now also apply to archive-member downloads
* `code_check`: flags `install.packages()` calls, classifies genomic sequence files (FASTA/FASTQ) as data, reads code out of more output formats, and several crash/false-positive fixes found during corpus validation (including a crash when every file in a batch fails to download, and empty-file/`setwd()`-string-literal blindness in `code_read()`)
* `data_check`: fixed a crash and wasted work in date/datetime column classification, and a crash on a paper with no readable tabular data
* LLM: new `llm_timeout()` bounds a stuck/overloaded local model call instead of hanging indefinitely; `llm_model_list()` switched from a blocklist to an allowlist of providers; new opt-in `capture_reasoning` argument to `llm()`
* Fixed a `repo_check`/`data_check`/`code_check` per-paper `summary_table` aggregation bug that could silently reuse a corpus-wide count across every paper's row, or collapse it to a single paper, when a module ran on a paperlist
* Fixed `.researchdata4tu_id()` not recognizing a 4TU.ResearchData DOI whose uuid has no `uuid:` label (`10.4121/<uuid>`, as opposed to `10.4121/uuid:<uuid>`), which silently left `repo_check` unable to fetch a real, working 4TU dataset cited by its own DOI
* Fixed `.batch_query()` (the shared query helper behind every `*_info()`/`*_links()` call across Dryad, Zenodo, OSF, Figshare, 4TU, Dataverse, Mendeley Data, DataONE, GitLab, and ReShare) crashing with "resp must be an HTTP response object" on a connection-level failure (timeout, DNS, dropped connection) instead of returning `NULL` like every caller already expects
* Dryad: `dryad_links()`/`.dryad_doi()` now recognise 13 additional DOI prefixes Dryad has minted datasets under over the years (10.15146, 10.25338, 10.25349, 10.5068, 10.6071, 10.6075, 10.6076, 10.6078, 10.6086, 10.7272, 10.7280, 10.7291, 10.7941), not just the usual 10.5061 -- the complete set found by exhaustively sampling DataCite's DOI index until no new prefix appeared, each individually confirmed live to resolve to a real datadryad.org dataset page
* Dataverse: `dataverse_links()` now also recognises a bare DOI citation with no host domain in the URL at all (e.g. a Data Availability Statement citing only `https://doi.org/10.18167/DVN1/T0DMFJ`, never naming `dataverse.cirad.fr`), via a new per-host, individually-verified DOI-prefix allowlist covering 87 of the package's 121 known Dataverse installations
* Fixed `dryad_info()`, `dataverse_info()`, `dataone_info()`, `figshare_info()`, `mendeley_info()`, `reshare_info()`, and `zenodo_info()` crashing with "Join columns in `x` must be present in the data" when called with their own `*_links()` output (the normal, documented usage) for a repository that isn't found -- the derived id/host/DOI columns the caller's table already carried collided with the ones freshly recomputed internally
* Fixed the per-host DOI-prefix allowlist added just above under-counting badly: it was originally built by sampling only the first 25 DOIs DataCite's search returned per host, which missed every additional prefix on any host with more real DOIs than that (63 of the 121 known hosts, some by orders of magnitude -- dataverse.harvard.edu alone has 817,836 real DOIs and was missing its 10.26300 prefix entirely). Found when a paper citing `10.7939/DVN/M6Q2PE`, the University of Alberta's legacy Dataverse prefix (now migrated onto the shared Borealis platform at borealisdata.ca), failed to resolve even though borealisdata.ca was already a known host, mapped to only its dominant prefix. Every host was re-queried using DataCite's facet aggregation, which returns every distinct prefix actually in use for a host in one request instead of a sample; the allowlist now carries 100 distinct prefixes across the same 87 hosts

# metacheck 0.2.1

* New `report_repository()` creates a report for a folder of files on your own computer, with no manuscript needed: give it a path and it runs `repo_check`, `code_check`, `data_check` and `codebook_check` and writes a report named after the folder
* Fixed `data_check` failing on a data file whose columns are themselves data frames or matrices, as `jsonlite` produces from a nested API response (common in an `.RData` holding a saved OSF result). Such columns are now flattened to text, so they are described rather than crashing the module with "length of 'dimnames' [2] not equal to array extent"
* Fixed `test_paper()` dropping every column of the `info` table except three, which made building a report from it warn about the missing `doi` column
* Removed a spurious "one argument not used by format" warning from `data_check`'s personal-information note

# metacheck 0.2.0

* Four new modules: `data_check`, `codebook_check`, `reg_check` and `ethics_check`
* Large updates to existing modules:
  * `repo_check` now lists what is inside zip archives instead of just counting them, and checks file naming
  * `code_check` reads code out of output files, supports Python and notebooks, and detects environment pins
  * `ref_accuracy` was rewritten to check DOI-backed reference coherence
  * `stat_effect_size` finds tests via `extract_eq()` and reports implied sample sizes
  * `power` uses structured LLM extraction, with a prompt-based fallback
* Data files in repositories are now downloaded and checked, not just listed, with a shared on-disk cache (`download_repo_files()`, `repo_cache_dir()`)
* New archive sources: PsychArchives, Zenodo deposit and upload, and OSF project listing; GitHub repositories are now listed via the Git Trees API
* Statistical output files can be read and compared against the manuscript: JASP, jamovi, SPSS, Stata, Mplus and R (`read_stat_tables()`, `match_reported_output()`)
* LLM responses are cached on disk (`llm_cache()`), with new `llm_reasoning()` and `llm_max_tokens()` settings
* Removed `metacheck_app()`; use `report_app()` instead

# metacheck 0.1.0

* Actual beta release with proper number and Zenodo citation!
* PDF conversion with `convert()` or `convert_grobid()` now defaults to the new GDPR-compliant server at TUE
* Updates to the effect_size module
* New `report_app()` to make a report with all default modules in a GUI by just uploading a PDF
* Improvements to unit tests
* Removed {fs} dependency and added custom `path_sanitize()` 
* Added internal functions to the website for developer reference
* Updated the vignette on creating modules to explain validation text.

# metacheck 0.0.1.9001

* `extract_eq` now catches "Hedges's g" (formerly just "g") and returns values ordered by paper_id, text_id and group_id
* Updated `xml_read_grobid()` (an internal helper function for reading Grobid XMLs) to handle some stats better (e.g., "... g z =" is now read as "... gz = ")
* Updated grobid XML read-in to better handle URLs with ? in the middle (less likely to cause an incorrect sentence split), and to remove no-content headers from the text table
* Fixed some bibliography parsing problems with non-articles.
* Updated `psychsci` for the read-in improvements.
* `retractionwatch` database updated


# metacheck 0.0.1.0

Our beta release! We've made so many changes, and we're sure there are still many bugs to catch and things to improve, but we need other people to start using metacheck to help us.

# metacheck 0.0.0.9107

* code_check now checks if code is parseable (thanks @Raphael-Merz!)
* many new `code_*()` functions abstracted out from the code_check module. These may eventually move to a new package specifically for codecheck

# metacheck 0.0.0.9106

* Added functions from svutils back in. 
* Reorganised some ML read-in functions (internal).
* Ollama further support in `llm()` and vignette.
* The code_check module handles local file with the argument `local_path`
* New `local_files()` function (thanks @lakens!)
* Updated vignettes

# metacheck 0.0.0.9105

* Much less buggy `.grobid_to_bibr()` conversion, handling URLs in text, xrefs, url, and eq tables better.
* `extract_equations()` renamed to `extract_eq()` and now extracts degrees of freedom (df column)
* Improvements to `.tei_text()` to fix common problems with grobid handling of equations (e.g., "")
* Corresponding paper schema changes
* Updated `psychsci` and `demopaper()` and `demofile()` for new schema and read

# metacheck 0.0.0.9104

* Updated `file_types` to fix a bug that prepended X to all extensions starting with a number.
* `paper_id()` now returns a vector, not a table, fixing modules that used it that way
* `read()` no longer errors when reading an empty directory, just messages and returns an empty paperlist
* `read()` only reads in the .json version if a .json and .xml file with the same name exist
* `read()` has a new argument `recursive` (default FALSE) to recursively read a directory. This does not handle it well if individual files have the same paper_id, so don't do that.


# metacheck 0.0.0.9103

* converting grobid xml to bibr json now saves the file after each conversion, instead of at the end, making it better for large batches (although slightly less efficient by potentially duplicating crossref lookups shared between papers)
* `convert()` has new arguments `crossref_lookup` (default FALSE) and `keep_xml` (default TRUE). It also saves XML and/or JSON files as they are converted, rather than at the end, in case of breaking failure. 
* Updated the "open_practices" module, which is much faster than the ODDPub version of this module (about 40x faster), also returns open materials and registrations, and has a lower false negative rate, but also a higher false positive rate. This removes the oddpub dependency.
* Restructured file names (not function names) for functions so all archive helper (e.g., osf, github, zenodo) start with "archive-" and database helpers (e.g., pubpeer, retractionwatch) start with "db-".
* Restructured text functions to start with text_, so `search_text()` is now `text_search()` and `expand_text()` is now `text_expand()`. The old names will exist as aliases. 
* Internal functions now prefaced with . to make it clearer for developers.
* All `{archive}_retrieve()` functions now renamed to `{archive}_info()` and the old `{archive}_info()` internal functions are now `.{archive}_info()`

# metacheck 0.0.0.9102

* Shiny app improvements: you can now view HTML reports in the browser
* Fixes the "prereg_check" module to address an error when there are more than 10 OSF registrations in a batch that caused unmergable data frames.
* Fixes the "code_check" module to address an error when checking multiple files that have no repositories with code.
* The module "code_check" now has an argument "file_limit" to control how many code files per repo are downloaded and processed. The default is 20.
* Fixed a problem where invisible figures in grobid would mess up the text section ids

# metacheck 0.0.0.9101

* `metacheck_app()` the shiny app is back!
* `grobid_convert()` now reads in the url table more accurately
* `extract_urls()` uses a simplified regex that seems better at catching full URLs
* updated FLoRA and rw databases
* `osf_links()`, `rb_links()`, `github_links()` and `aspredicted_links()` simplified to use the more accurate url table instead of a full text search.

# metacheck 0.0.0.9100

* So many updates to fix things that broke with the new structure
* Using httptest2 to mock tests that access external APIs

# metacheck 0.0.0.9070

* Major updates to replace grobid functions with bibr
* Remove `author_table()`, as this is just `concat_tables()` now


# metacheck 0.0.0.9069

* Updated osf_* and rb_* functions to use progress bars instead of messages
* New logging functions: `logger()` and `lastlog()` inspired by @levibaruch 
* New `test_paper()` for creating paper objects with specific test text
* `summarize_contents()` changed to `file_category()` and now works with a vector of file names, as well as a data frame
* `compare_tables()`, `text_features()` and `distinctive_words()` now deprecated
* `validate()` function simplified

# metacheck 0.0.0.9068

* FReD replication database and associated functions now renamed to `FLoRA()`
* Various bug fixes discovered when running modules on large numbers of papers (e.g., handling when zero references have DOIs)
* Modules "function_check" and "coi_check" reverted to the rtransparent versions (the re-written version were overinclusive and need more development).

# metacheck 0.0.0.9067

* `reports()` now takes a paperlist and makes a report from each
* New `report_module_run()` and `report_qmd()` break down the `report()` function to allow separation of module output lists and creation of QMD report from them (might be changed to internal functions).
* Ability to select returned columns in `crossref_query()`
* Module "ref_accuracy" now returns info for references with missing DOIs that were found by ref_doi_check
* Module "code_check" split into "repo_check" and "code_check"

# metacheck 0.0.0.9066

* `lmm()` allows you to set the model to any provider or provider/model supported by ellmer (must have appropriate *****_API_KEY set in your Renviron)
* `lmm()` arguments have changed to align with `ellmer::chat()` arguments
* `lmm_models()` now returns models from all platforms for which you have a valid API key set
* The power module uses a new prompt that utilises a JSON schema for power
* Updated report styles

# metacheck 0.0.0.9065

* New `github_links()` function to find github references in a paper.
* `code_check` module very much improved - checks SAS and STATA code in OSF, researchbox, and github repos.
* `power` module much improved
* New modules: `coi_check`, `funding_check`
* New functions `extract_p_values()` and `extract_urls()`, so now no need to use `all_p_values` and `all_urls` modules to get their tables. These modules remain because they are used in demos, but may be deprecated soon. 

# metacheck 0.0.0.9064

* Enhanced module help
* "ref_replication" module no longer warns about replications if you have cited them.
* Extensive chenges to clen up tests.

# metacheck 0.0.0.9063

* `get_doi()` has been removed in favour of `crossref_query()`, to look up crossref info by bibliographic query, and `crossref_doi()`, to look up crossref info by DOI. 
* `scroll_table()` changed arguments. `height` is removed and `scroll_above` changed to `maxrows`. It not paginates above maxrows (default = 2), rather than scrolling within a fixed height. This is a more accessible solution, since scrolling is hard with touchscreens and it's often hard to copy text in a scroll window. We will continually improve this with further user feedback.
* Fixed a bunch of small problems with modules and let the report render even with errors 
* Updated the report template with light and dark themes (set to user preference)
* The module `reference_check` is split into `ref_doi_check` and `ref_accuracy`.
* Lots of modules got renamed so they have a consistent format.

# metacheck 0.0.0.9062

* `json_expand()` updated to handle LLM JSON errors more gracefully.
* You can pass arguments to modules via `report()` now with the new `args` argument. 
* New `get_prev_outputs()` module helper function
* Updated the vignettes.
* Modules `aspredicted` and `retractionwatch` are removed, as they are superseded by `prereg_check` and `reference_check`.
* The module `nonsignificant_pvalue` has changed to `nonsig_p`
* The default modules in a report have changed. 
* A new module report helper, `format_ref()` for displaying references in bibentry or bibtex formats
* The ref column of the bib table in paper objects is now the bibentry for a reference, not just the formatted text. This will allow for more formatting options. 

# metacheck 0.0.0.9061

* Efficiency improvements to the OSF functions
* Fixed some confusing parts of the articles that changed when the module output report structure changed.
* Modules are now categorised by section: general, intro, method, results, discussion, reference
* Reports are organised by section
* Display improvement in reports
* Module report improvement (e.g., fixing broken links)
* New example report on the pkgdown website

# metacheck 0.0.0.9060

* Lots of changes for how reports are formatted
* In module output, `summary` is now `summary_table`
* Fixed a bug where some .docx file wouldn't read in (support for Word files is still patchy -- ideally render to PDF)
* New `pubpeer_comments()` function (now vectorised)
* Module helpers: `scroll_table()`, `collapse_section()`, `link()`, `plural()`, `pb()`

# metacheck 0.0.0.9059

* Package name changed to metacheck!
* Fixed a bug in `osf_file_download()` when multiple files have the same name and `ignore_folder_structure = TRUE`.
* `osf_file_download()` should handle errors more gracefully (with warnings, but not fail)


# papercheck 0.0.0.9058

* `openalex()` results now include `abstract`, which parses the abstract_inverted_index for you

# papercheck 0.0.0.9057

## New functions/modules

* New module: `miscitation` to detect commonly mis-cited papers (a proof-of-concept)
* New module: `power` to detect and classify power analyses (currently being validated)
* New module: `aspredicted` to get structured data from AsPredicted preregistrations (mainly for info)
* `module_template()` creates a module file from a template
* `orcid_person()` gets details from an ORCiD, such as name, emails, country
* `osf_preprint_list()` returns a table of preprints from the OSF optionally filtered by archive and dates created or modified
* Added an API wrapper - it is now possible to run papercheck functions and modules via a REST API. See `inst/plumber/README.md` for details.
* Added documentation and plumber/Docker quickstart for the API

## Changes

* Changes to `module_find()` to find potential modules in the working directory and ./modules/
* Changes to `effectsize` module so text of the potential effect size is given in `mod_output$table$es` (`mod_output$summary$ttests_n` and `mod_output$summary$Ftests_n` columns removed, as they are just the sum of `*tests_with_es` and `*tests_without_es`)
* `pdf2grobid()` now gives more useful information in the warning if some files do not convert when converting more than one PDF
* Changed parameter names in pdf2grobid to be consistently snake_case (consolidate_headers etc.) whilst keeping backward compatibility for the old camelCase (consolidateHeaders etc.)

## Bug Fixes

* Fixed warning messages in `osf_check` module when there are no OSF links
* Fixed a problem in module_report() that happens when the table returned from module_run() has no rows
* Fixed a bug that crashed `stat_table()` function by generating a summary table in case of empty stat table


# papercheck 0.0.0.9056

* If `expand_text()` doesn't find a text match because sentence location info is missing, it now returns the original text instead of NA
* Fixed a bug that prevented matching xrefs sentences under some circumstances (when there was an initial with a full stop in the citation) -- re-run `read()` on XMLs to update any saved paper objects
* `psychsci` updated for these fixes
* Changed `retractionwatch` internal data to `retractionwatch()` function (alias `rw()`) to support user updating.
* Added new function `rw_date()` so you can find out when retractionwatch was last updated
* New function `rw_update()` lets you update retractionwatch yourself

# papercheck 0.0.0.9055

* `pdf2grobid()` handles `save_path` batter if any path components don't exist yet. The argument `save_path` also now can take a vector of the same length as the number of PDFs to convert, so you can specify the name of each output XML.
* `read()` now skips any imports with errors and warns you about them after importing all files
* Fixed a bug that errored on read() when bibentry files don't format correctly
* Function `osf_get_all_pages()` now has a new argument `page_end` to limit the number of pages retrieved (mainly for testing purposes), and is external (previously internal)
* Fixed a bug in `osf_files()` that failed on paths with spaces
* Fixed a bug in `read()` that duplicated entries in xrefs

# papercheck 0.0.0.9054

* `osf_file_download()` now also retrieves files from linked storage
* Removed the last dependency to {osfr} and updated `osf_check_id()` to return expected IDs from various URLs
* OSF functions added to getting started vignette
* Functions that require and API are now tested using httptest
* module_list() doesn't fail if there are any errors in the modules

# papercheck 0.0.0.9053

* Updated `read()` to parse more stupid date formats that turn up in the submission string (and added the unparsed submission string back just in case)
* Completely overhauled how paper objects handle references.
  * the `paper$reference` table is now `paper$bib`
  * the `paper$citations` table is now `paper$xrefs` and also contains  information for internal cross-references to figures, tables, footnotes, and formulae
  * the `ref_id` and `bib_id` in both tables is now `xref_id`
  * the `xrefs` table also contains location information (section, div, p, s) for the sentence containing the cross-ref, so you can use `expand_text()`
  * The `read()` function now returns paper objects with these new tables, so you will need to re-read any XML files (if you have stored the papercheck list as Rdata)
  * The `psychsci` object has been updated for this new format
  * Modules and vignettes have been updated as well

# papercheck 0.0.0.9052

* Fixed a bug in `expand_text()` where expanded sentences were duplicated if there are multiple matches from the same sentence in the data frame.
* Updated the `retractionwatch` table
* Fixed a bug in `read()` that omitted paper DOIs from paper$info
* Updated `read()` to add correctly parsed "accepted" and "received" dates to paper\$info (replaces paper\$submission string) (ISO 8601 is the only correct date format!)
* Updated `psychsci` for new info structure

# papercheck 0.0.0.9051

* Small bug fixes to `osf_file_download()`
* `osf_file_download()` now returns a table of file info, including info for files not downloaded because of file size limits

# papercheck 0.0.0.9050

* Added `read()` function, which superceeds `read_grobid()`, `read_cermine()` and `read_text()` (they are still available, but are now just aliases to `read()`). This should work with XML files in TEI (grobid), JATS APA-DTD, NLM-DTD and cermine formats, plus full text-only parsing of .docx and plain text files.
* Added `osf_file_download()` function, which downloads all files under a project or node and structures them the same as the project.

# papercheck 0.0.0.9049

* Updated `read_grobid()` to classify headers as intro, method, results, discussion with better accuracy (to handle garbled headers)
* Updated `pdf2grobid()` to allow some grobid parameters
* Updated the module "all_p_values" to handle more scientific notation formats

# papercheck 0.0.0.9048

* Functions to check ResearchBox.org (`rbox_links()` and `rbox_retrieve()`) -- very preliminary
* The module "all_p_values" now returns the p-value as a numeric column `p_value` and the comparator as `p_comp`, like "exact_p"

# papercheck 0.0.0.9047

* fixed some bugs in osf and aspredicted functions (mainly around dealing with private or empty projects)
* added rvest dependency for better webpage parsing
* changed name of resulting column from `summarize_contents()` from `best_guess` to `file_category`

# papercheck 0.0.0.9046

* New `aspredicted_links()` and `aspredicted_retrieve()` functions
* New related blog post
* General bug fixes in newer stuff
* Updated license to AGPL (GNU Affero General Public License)

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
* Added code coverage at <https://app.codecov.io/gh/scienceverse/papercheck>
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

* `read_grobid()` now includes figure and table captions, plus footnotes, in the text table
* the `psychsci` paper list object is updated to include the above
* The functions that `module_run()` delegates to now check and only pass valid arguments

# papercheck 0.0.0.9030 (2025-03-01)

* modules are now updated for clearer output, and added a new module vignette
* `llm()` no longer returns NA when the rate limit is hit, but slows down queries accordingly
* `read_grobid()` now includes back matter (e.g., acknowledgements, COI statements) in the text, so is searchable with `search_text()`
* references are now converted to bibtex format, so are more complete and consistent
* Machine-learning module types are removed (the python/reticulate setup was too complex for many users), and instructions for how to create simple text feature models is included in the metascience vignette

# papercheck 0.0.0.9029 (2025-02-26)

* added `author_table()` to get a dataframe of author info from a list of paper objects
* fixed a bunch of tests now that multiple matches in a sentence are possible
* added back text (acknowledgements, annex, funding notes) to the text of a paper
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
