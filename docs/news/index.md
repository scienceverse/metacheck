# Changelog

## metacheck 0.0.0.9068

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
