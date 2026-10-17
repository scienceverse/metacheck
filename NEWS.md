# metacheck 0.3.3

* `reproducibility_check(execute = TRUE, sandbox = "docker")` no longer fails every script that writes a file (#502). Writes are redirected into the sandbox's `output/` folder, but the redirect named that folder by its path on the host, which does not exist inside the container (the sandbox is mounted at `/sandbox`), so the script stopped with "cannot open the connection" after its analysis had run and was reported as `errored`. The redirect now uses the relative path `output/<name>`, which is right in both sandboxes, because every script runs with the sandbox root as its working directory

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
