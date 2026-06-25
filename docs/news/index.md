# Changelog

## metacheck 0.1.0

- Actual beta release with proper number and Zenodo citation!
- PDF conversion with
  [`convert()`](https://scienceverse.github.io/metacheck/reference/convert.md)
  or
  [`convert_grobid()`](https://scienceverse.github.io/metacheck/reference/convert_grobid.md)
  now defaults to the new GDPR-compliant server at TUE
- Updates to the effect_size module
- New
  [`report_app()`](https://scienceverse.github.io/metacheck/reference/report_app.md)
  to make a report with all default modules in a GUI by just uploading a
  PDF
- Improvements to unit tests
- Removed {fs} dependency and added custom
  [`path_sanitize()`](https://scienceverse.github.io/metacheck/reference/path_sanitize.md)
- Added internal functions to the website for developer reference
- Updated the vignette on creating modules to explain validation text.

## metacheck 0.0.1.9001

- `extract_eq` now catches “Hedges’s g” (formerly just “g”) and returns
  values ordered by paper_id, text_id and group_id
- Updated `xml_read_grobid()` (an internal helper function for reading
  Grobid XMLs) to handle some stats better (e.g., “… g z =” is now read
  as “… gz =”)
- Updated grobid XML read-in to better handle URLs with ? in the middle
  (less likely to cause an incorrect sentence split), and to remove
  no-content headers from the text table
- Fixed some bibliography parsing problems with non-articles.
- Updated `psychsci` for the read-in improvements.
- `retractionwatch` database updated

## metacheck 0.0.1.0

Our beta release! We’ve made so many changes, and we’re sure there are
still many bugs to catch and things to improve, but we need other people
to start using metacheck to help us.

## metacheck 0.0.0.9107

- code_check now checks if code is parseable (thanks
  [@Raphael-Merz](https://github.com/Raphael-Merz)!)
- many new `code_*()` functions abstracted out from the code_check
  module. These may eventually move to a new package specifically for
  codecheck

## metacheck 0.0.0.9106

- Added functions from svutils back in.
- Reorganised some ML read-in functions (internal).
- Ollama further support in
  [`llm()`](https://scienceverse.github.io/metacheck/reference/llm.md)
  and vignette.
- The code_check module handles local file with the argument
  `local_path`
- New
  [`local_files()`](https://scienceverse.github.io/metacheck/reference/local_files.md)
  function (thanks [@lakens](https://github.com/lakens)!)
- Updated vignettes

## metacheck 0.0.0.9105

- Much less buggy
  [`.grobid_to_bibr()`](https://scienceverse.github.io/metacheck/reference/dot-grobid_to_bibr.md)
  conversion, handling URLs in text, xrefs, url, and eq tables better.
- `extract_equations()` renamed to
  [`extract_eq()`](https://scienceverse.github.io/metacheck/reference/extract_eq.md)
  and now extracts degrees of freedom (df column)
- Improvements to
  [`.tei_text()`](https://scienceverse.github.io/metacheck/reference/dot-tei_text.md)
  to fix common problems with grobid handling of equations (e.g., ““)
- Corresponding paper schema changes
- Updated `psychsci` and
  [`demopaper()`](https://scienceverse.github.io/metacheck/reference/demopaper.md)
  and
  [`demofile()`](https://scienceverse.github.io/metacheck/reference/demofile.md)
  for new schema and read

## metacheck 0.0.0.9104

- Updated `file_types` to fix a bug that prepended X to all extensions
  starting with a number.
- [`paper_id()`](https://scienceverse.github.io/metacheck/reference/paper_id.md)
  now returns a vector, not a table, fixing modules that used it that
  way
- [`read()`](https://scienceverse.github.io/metacheck/reference/read.md)
  no longer errors when reading an empty directory, just messages and
  returns an empty paperlist
- [`read()`](https://scienceverse.github.io/metacheck/reference/read.md)
  only reads in the .json version if a .json and .xml file with the same
  name exist
- [`read()`](https://scienceverse.github.io/metacheck/reference/read.md)
  has a new argument `recursive` (default FALSE) to recursively read a
  directory. This does not handle it well if individual files have the
  same paper_id, so don’t do that.

## metacheck 0.0.0.9103

- converting grobid xml to bibr json now saves the file after each
  conversion, instead of at the end, making it better for large batches
  (although slightly less efficient by potentially duplicating crossref
  lookups shared between papers)
- [`convert()`](https://scienceverse.github.io/metacheck/reference/convert.md)
  has new arguments `crossref_lookup` (default FALSE) and `keep_xml`
  (default TRUE). It also saves XML and/or JSON files as they are
  converted, rather than at the end, in case of breaking failure.
- Updated the “open_practices” module, which is much faster than the
  ODDPub version of this module (about 40x faster), also returns open
  materials and registrations, and has a lower false negative rate, but
  also a higher false positive rate. This removes the oddpub dependency.
- Restructured file names (not function names) for functions so all
  archive helper (e.g., osf, github, zenodo) start with “archive-” and
  database helpers (e.g., pubpeer, retractionwatch) start with “db-”.
- Restructured text functions to start with text\_, so
  [`search_text()`](https://scienceverse.github.io/metacheck/reference/text_search.md)
  is now
  [`text_search()`](https://scienceverse.github.io/metacheck/reference/text_search.md)
  and
  [`expand_text()`](https://scienceverse.github.io/metacheck/reference/text_expand.md)
  is now
  [`text_expand()`](https://scienceverse.github.io/metacheck/reference/text_expand.md).
  The old names will exist as aliases.
- Internal functions now prefaced with . to make it clearer for
  developers.
- All `{archive}_retrieve()` functions now renamed to `{archive}_info()`
  and the old `{archive}_info()` internal functions are now
  `.{archive}_info()`

## metacheck 0.0.0.9102

- Shiny app improvements: you can now view HTML reports in the browser
- Fixes the “prereg_check” module to address an error when there are
  more than 10 OSF registrations in a batch that caused unmergable data
  frames.
- Fixes the “code_check” module to address an error when checking
  multiple files that have no repositories with code.
- The module “code_check” now has an argument “file_limit” to control
  how many code files per repo are downloaded and processed. The default
  is 20.
- Fixed a problem where invisible figures in grobid would mess up the
  text section ids

## metacheck 0.0.0.9101

- `metacheck_app()` the shiny app is back!
- `grobid_convert()` now reads in the url table more accurately
- [`extract_urls()`](https://scienceverse.github.io/metacheck/reference/extract_urls.md)
  uses a simplified regex that seems better at catching full URLs
- updated FLoRA and rw databases
- [`osf_links()`](https://scienceverse.github.io/metacheck/reference/osf_links.md),
  `rb_links()`,
  [`github_links()`](https://scienceverse.github.io/metacheck/reference/github_links.md)
  and
  [`aspredicted_links()`](https://scienceverse.github.io/metacheck/reference/aspredicted_links.md)
  simplified to use the more accurate url table instead of a full text
  search.

## metacheck 0.0.0.9100

- So many updates to fix things that broke with the new structure
- Using httptest2 to mock tests that access external APIs

## metacheck 0.0.0.9070

- Major updates to replace grobid functions with bibr
- Remove `author_table()`, as this is just `concat_tables()` now

## metacheck 0.0.0.9069

- Updated osf\_\* and rb\_\* functions to use progress bars instead of
  messages
- New logging functions:
  [`logger()`](https://scienceverse.github.io/metacheck/reference/logger.md)
  and
  [`lastlog()`](https://scienceverse.github.io/metacheck/reference/lastlog.md)
  inspired by [@levibaruch](https://github.com/levibaruch)
- New
  [`test_paper()`](https://scienceverse.github.io/metacheck/reference/test_paper.md)
  for creating paper objects with specific test text
- `summarize_contents()` changed to
  [`file_category()`](https://scienceverse.github.io/metacheck/reference/file_category.md)
  and now works with a vector of file names, as well as a data frame
- `compare_tables()`, `text_features()` and `distinctive_words()` now
  deprecated
- [`validate()`](https://scienceverse.github.io/metacheck/reference/validate.md)
  function simplified

## metacheck 0.0.0.9068

- FReD replication database and associated functions now renamed to
  [`FLoRA()`](https://scienceverse.github.io/metacheck/reference/FLoRA.md)
- Various bug fixes discovered when running modules on large numbers of
  papers (e.g., handling when zero references have DOIs)
- Modules “function_check” and “coi_check” reverted to the rtransparent
  versions (the re-written version were overinclusive and need more
  development).

## metacheck 0.0.0.9067

- `reports()` now takes a paperlist and makes a report from each
- New
  [`report_module_run()`](https://scienceverse.github.io/metacheck/reference/report_module_run.md)
  and
  [`report_qmd()`](https://scienceverse.github.io/metacheck/reference/report_qmd.md)
  break down the
  [`report()`](https://scienceverse.github.io/metacheck/reference/report.md)
  function to allow separation of module output lists and creation of
  QMD report from them (might be changed to internal functions).
- Ability to select returned columns in
  [`crossref_query()`](https://scienceverse.github.io/metacheck/reference/crossref_query.md)
- Module “ref_accuracy” now returns info for references with missing
  DOIs that were found by ref_doi_check
- Module “code_check” split into “repo_check” and “code_check”

## metacheck 0.0.0.9066

- `lmm()` allows you to set the model to any provider or provider/model
  supported by ellmer (must have appropriate \*\*\*\*\*\_API_KEY set in
  your Renviron)
- `lmm()` arguments have changed to align with
  [`ellmer::chat()`](https://ellmer.tidyverse.org/reference/chat-any.html)
  arguments
- `lmm_models()` now returns models from all platforms for which you
  have a valid API key set
- The power module uses a new prompt that utilises a JSON schema for
  power
- Updated report styles

## metacheck 0.0.0.9065

- New
  [`github_links()`](https://scienceverse.github.io/metacheck/reference/github_links.md)
  function to find github references in a paper.
- `code_check` module very much improved - checks SAS and STATA code in
  OSF, researchbox, and github repos.
- `power` module much improved
- New modules: `coi_check`, `funding_check`
- New functions
  [`extract_p_values()`](https://scienceverse.github.io/metacheck/reference/extract_p_values.md)
  and
  [`extract_urls()`](https://scienceverse.github.io/metacheck/reference/extract_urls.md),
  so now no need to use `all_p_values` and `all_urls` modules to get
  their tables. These modules remain because they are used in demos, but
  may be deprecated soon.

## metacheck 0.0.0.9064

- Enhanced module help
- “ref_replication” module no longer warns about replications if you
  have cited them.
- Extensive chenges to clen up tests.

## metacheck 0.0.0.9063

- `get_doi()` has been removed in favour of
  [`crossref_query()`](https://scienceverse.github.io/metacheck/reference/crossref_query.md),
  to look up crossref info by bibliographic query, and
  [`crossref_doi()`](https://scienceverse.github.io/metacheck/reference/crossref_doi.md),
  to look up crossref info by DOI.
- [`scroll_table()`](https://scienceverse.github.io/metacheck/reference/scroll_table.md)
  changed arguments. `height` is removed and `scroll_above` changed to
  `maxrows`. It not paginates above maxrows (default = 2), rather than
  scrolling within a fixed height. This is a more accessible solution,
  since scrolling is hard with touchscreens and it’s often hard to copy
  text in a scroll window. We will continually improve this with further
  user feedback.
- Fixed a bunch of small problems with modules and let the report render
  even with errors
- Updated the report template with light and dark themes (set to user
  preference)
- The module `reference_check` is split into `ref_doi_check` and
  `ref_accuracy`.
- Lots of modules got renamed so they have a consistent format.

## metacheck 0.0.0.9062

- [`json_expand()`](https://scienceverse.github.io/metacheck/reference/json_expand.md)
  updated to handle LLM JSON errors more gracefully.
- You can pass arguments to modules via
  [`report()`](https://scienceverse.github.io/metacheck/reference/report.md)
  now with the new `args` argument.
- New
  [`get_prev_outputs()`](https://scienceverse.github.io/metacheck/reference/get_prev_outputs.md)
  module helper function
- Updated the vignettes.
- Modules `aspredicted` and `retractionwatch` are removed, as they are
  superseded by `prereg_check` and `reference_check`.
- The module `nonsignificant_pvalue` has changed to `nonsig_p`
- The default modules in a report have changed.
- A new module report helper,
  [`format_ref()`](https://scienceverse.github.io/metacheck/reference/format_ref.md)
  for displaying references in bibentry or bibtex formats
- The ref column of the bib table in paper objects is now the bibentry
  for a reference, not just the formatted text. This will allow for more
  formatting options.

## metacheck 0.0.0.9061

- Efficiency improvements to the OSF functions
- Fixed some confusing parts of the articles that changed when the
  module output report structure changed.
- Modules are now categorised by section: general, intro, method,
  results, discussion, reference
- Reports are organised by section
- Display improvement in reports
- Module report improvement (e.g., fixing broken links)
- New example report on the pkgdown website

## metacheck 0.0.0.9060

- Lots of changes for how reports are formatted
- In module output, `summary` is now `summary_table`
- Fixed a bug where some .docx file wouldn’t read in (support for Word
  files is still patchy – ideally render to PDF)
- New
  [`pubpeer_comments()`](https://scienceverse.github.io/metacheck/reference/pubpeer_comments.md)
  function (now vectorised)
- Module helpers:
  [`scroll_table()`](https://scienceverse.github.io/metacheck/reference/scroll_table.md),
  [`collapse_section()`](https://scienceverse.github.io/metacheck/reference/collapse_section.md),
  [`link()`](https://scienceverse.github.io/metacheck/reference/link.md),
  [`plural()`](https://scienceverse.github.io/metacheck/reference/plural.md),
  [`pb()`](https://scienceverse.github.io/metacheck/reference/pb.md)

## metacheck 0.0.0.9059

- Package name changed to metacheck!
- Fixed a bug in
  [`osf_file_download()`](https://scienceverse.github.io/metacheck/reference/osf_file_download.md)
  when multiple files have the same name and
  `ignore_folder_structure = TRUE`.
- [`osf_file_download()`](https://scienceverse.github.io/metacheck/reference/osf_file_download.md)
  should handle errors more gracefully (with warnings, but not fail)
