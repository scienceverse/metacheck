# Helpers for the data_check module.
#
# Ported from the `datacheck` pipeline (0_index.R / helper.R), rewritten to
# build on metacheck's existing file-handling (`file_category()`,
# `file_types`) rather than datacheck's standalone rule tables. The LLM path
# from datacheck is deliberately NOT ported here: these helpers are rules-only
# and run with `llm_use(FALSE)`. The module upgrades to an LLM classifier only
# when `llm_use(TRUE)` (see data_check.R).

# -- File-type crosswalk ------------------------------------------------------

# data_check's semantic file types. A file's finer documentation ROLE (is this
# row the root readme? a codebook? plain supplemental text?) is NOT part of
# this vocabulary -- that is `.data_doc_role()`, a separate, orthogonal column.
# `data_type` answers "what kind of repository content is this" (drives
# Psych-DS folder placement and the report grouping); `doc_role` answers "which
# specific documentation artifact is this" (drives root-vs-per-study placement
# and codebook-parser selection). Splitting these two axes avoids needing a
# top-level slot for every documentation sub-kind, which is what the old 9-way
# scheme did (readme/codebook/supplemental as three separate top-level types).
.data_check_types <- c(
  "data", "code", "documentation", "materials", "output", "unknown"
)

# Map metacheck's coarse `file_types$type` values onto data_check's semantic
# types. Used as the fallback layer after the name-based rules in
# `file_category()` (readme / codebook) have had first refusal.
#
# `materials` covers everything a participant/experimenter interacts with
# directly rather than an analytic artifact: stimuli/media (audio/video/image/
# 3D/font) AND runnable software (installers, experiment-runner scripts,
# compiled binaries) -- both are needed to REPLICATE the study procedure, as
# opposed to `data`/`code`, which are needed to RE-USE or REPRODUCE results.
# `documentation` covers everything that explains the data/study rather than
# being data/code/materials itself: readme, codebook, and supplemental text
# (preprints, slide decks, Word docs).
#
# Archives (.zip/.tar/...) are NOT a content type: a zip is a container, never
# itself research content, so it crosswalks to `unknown` only for the case it
# could not be opened/peeked (an unreadable format, or peeking disabled) -- once
# opened, its contents are classified normally and the container's own row is
# dropped, not relabelled (see inst/modules/data_check.R's archive-expansion
# step). A `.csv.gz` is, for every purpose, a `.csv` -- the archive layer is
# purely a delivery mechanism.
.file_type_crosswalk <- c(
  data    = "data",
  code    = "code",
  stats   = "code",       # SPSS/SAS/Stata syntax -> code
  exec    = "materials",  # exe/dll/app/... -> materials (runnable, not analytic)
  config  = "materials",  # yaml/ini/toml/... -> materials
  audio   = "materials",
  video   = "materials",
  image   = "materials",
  `3D`    = "materials",
  font    = "materials",
  book    = "documentation",
  slide   = "documentation",
  text    = "documentation",
  web     = "documentation",
  archive = "unknown"
)

# -- Unified extension registry -----------------------------------------------
#
# ONE table, keyed by extension, replacing what used to be six independent
# hardcoded lists that could (and did) silently disagree: .fixed_ext_type,
# .readable_extensions, code_lang()'s grepl chain (R/code_check.R),
# .code_predownload()'s candidate regex (R/code_check.R), and
# .psychds_encoding_format()'s mimes vector (R/psychds-convert.R). Two real
# disagreements this merge fixes: .psychds_encoding_format() had no entry for
# "omv" even though .jasp and .omv are treated as siblings everywhere else in
# the package (a jamovi file got no schema.org MIME type while its JASP
# counterpart did); and "por" (SPSS portable) had a MIME type asserted with no
# reader anywhere (now real: haven::read_por(), see data_read_head()).
#
# Columns:
#   ext       - lowercase extension, no dot (registry key)
#   data_type - data_check semantic type (see .data_check_types); replaces
#               .fixed_ext_type. NA when format alone does not fix the type
#               (data_classify_files() falls through to keyword/crosswalk).
#   readable  - TRUE when metacheck has an actual tabular reader for this
#               format (data_read_head()'s switch); replaces
#               .readable_extensions. A new reader branch added there must
#               get readable = TRUE here, or the format is downloaded but
#               never parsed.
#   code_lang - the programming language code_check treats this extension as;
#               replaces code_lang()'s grepl chain. NA for non-code formats.
#   mime      - schema.org encodingFormat for Psych-DS DataDownload entries;
#               replaces .psychds_encoding_format()'s mimes vector. NA when
#               no standard MIME type applies/is asserted.
#
# A row's `data_type` is FORMAT-LOCKED: nothing past Tier 1 of
# data_classify_files() (no filename/folder keyword, no coarse crosswalk) can
# override it -- the extension is stronger evidence of what a file actually
# IS than any name is (a real .csv/.R/.sav found under a mislabelled
# "Materials/" folder is still data/code).
.ext_registry <- (function() {
  r <- function(ext, data_type = NA_character_, readable = FALSE,
               code_lang = NA_character_, mime = NA_character_) {
    data.frame(ext = ext, data_type = data_type, readable = readable,
              code_lang = code_lang, mime = mime, stringsAsFactors = FALSE)
  }
  dplyr::bind_rows(
    # -- Code --------------------------------------------------------------
    # .ipynb's LANGUAGE is content-dependent (see code_lang()'s own roxygen:
    # of 144 real corpus notebooks, 126 declared Python and 7 declared R via
    # in-file metadata) -- registered as data_type "code" (format-locked
    # regardless of kernel) but code_lang left NA here; code_lang() keeps its
    # own .ipynb_lang() content-sniff as a post-lookup special case.
    #
    # .qmd is Quarto, and (unlike .Rmd, which is R/knitr by construction)
    # explicitly polyglot -- the same document can run knitr (R) or Jupyter
    # (Python, Julia, ...) depending on its YAML `engine`/`jupyter:` field or
    # chunk fences. Previously format-locked to code_lang = "R" here, which
    # meant every .qmd was purled with the R-only code_extract_r()/
    # knitr::purl() regardless of its actual engine (issue #180). code_lang
    # left NA, same treatment as .ipynb: code_lang()'s own .qmd_lang()
    # content-sniff decides R vs Python when the file is available locally,
    # falling back to "R" (Quarto's and this registry's own prior default)
    # when it is not.
    r("r",    "code", readable = FALSE, code_lang = "R",    mime = "text/x-r-source"),
    r("rmd",  "code", readable = FALSE, code_lang = "R"),
    r("qmd",  "code", readable = FALSE, code_lang = NA),
    r("ipynb","code", readable = FALSE, code_lang = NA),
    r("py",   "code", readable = FALSE, code_lang = "Python", mime = "text/x-python"),
    r("do",   "code", readable = FALSE, code_lang = "Stata"),
    r("ado",  "code", readable = FALSE, code_lang = "Stata"),
    r("sps",  "code", readable = FALSE, code_lang = "SPSS"),
    r("sas",  "code", readable = FALSE, code_lang = "SAS"),
    r("inp",  "code", readable = FALSE, code_lang = "Mplus"),
    r("m",    "code", readable = FALSE, code_lang = "MATLAB"),
    # Probabilistic-programming / cognitive-modelling source, confirmed
    # against real corpus examples: .stan (Stan model code), .wppl (WebPPL,
    # confirmed as source under a node_modules/ package, not data), .mpt
    # (MultiTree cognitive-model files). .sbatch/.sbt are build/job scripts
    # (SLURM batch job; Scala build tool).
    r("stan", "code"), r("wppl", "code"), r("mpt", "code"),
    r("sbatch","code"), r("sbt", "code"),
    # .mjs/.cjs are JavaScript module-system variants, sibling to .js
    # (confirmed against real corpus source files, e.g. WebGazer's
    # src/dom_util.mjs). .typ is Typst source (confirmed under a Quarto
    # apaquarto extension's typst/ folder). .spwb is IBM SPSS Statistics'
    # modern Workbook format (v28+, a ZIP container bundling syntax +
    # descriptive text -- successor to the older plain-text .sps).
    r("mjs", "code"), r("cjs", "code"), r("typ", "code"), r("spwb", "code"),
    # A .jasp/.omv bundles a dataset with its analyses -- a binary (zip)
    # archive, so it is both a code_lang() ("JASP"/"jamovi", listed not
    # analysed) AND readable as data (import_jasp()/import_omv() recover a
    # labelled data frame, treated exactly like .sav). data_type stays "data"
    # (file_category()'s sure_class already keys ft=="data;stats" -> "data";
    # the dataset is the primary artifact, the bundled analyses are recovered
    # separately as the code file).
    r("jasp", "data", readable = TRUE,  code_lang = "JASP",   mime = "application/x-jasp"),
    r("omv",  "data", readable = TRUE,  code_lang = "jamovi", mime = "application/x-jamovi"),
    # Reproducibility/build config for a code project -- goes WITH the .Rmd/
    # .qmd it configures (a _quarto.yml, an .Rproj, an renv.lock) rather than
    # documentation or materials. Previously these fell through to the coarse
    # "config" crosswalk category, which lands on "materials" -- wrong: a
    # .lock/.yaml/.Rproj is project scaffolding for the ANALYSIS, not
    # participant-facing stimuli/software.
    r("yaml", "code"), r("yml", "code"), r("toml", "code"),
    r("ini",  "code"), r("lock","code"), r("rproj","code"),

    # -- Data --------------------------------------------------------------
    # MATLAB source (.m) is code; MATLAB's own binary data container (.mat)
    # is data, never code -- distinct extensions, no ambiguity between them
    # (unlike .out below, whose reclassification-after-download logic does
    # not apply here: nothing about a .mat's CONTENT could make it code, so
    # no downstream content check is needed the way .out gets one).
    r("mat",  "data", readable = FALSE),
    # csv/tsv/dat/xlsx/xls/ods/fods are deliberately NOT format-locked here
    # (data_type = NA): each is readable as a table (readable = TRUE feeds
    # .readable_extensions/data_format()) but the file's data_check TYPE stays
    # decided by file_category()'s coarse crosswalk (csv/dat/xls/xlsx/ods all
    # already resolve to "data" there via metacheck::file_types' own coarse
    # "data" category -- confirmed in data/file_types.rda) and Tier 2 keyword
    # rules, exactly as before this registry existed: a real corpus
    # "README.txt" or "codebook.xlsx" must still reach the keyword layer, the
    # same way a genuine ".csv" named "Materials - Exp 2.csv" should still
    # become "materials", not get force-locked to "data" by extension alone.
    # .txt is a stronger case of the same thing: its OWN coarse type is "text"
    # (documentation-leaning), not "data", so locking it here would have been
    # wrong in the other direction too.
    r("csv",  NA,     readable = TRUE,  mime = "text/csv"),
    r("tsv",  NA,     readable = TRUE,  mime = "text/tab-separated-values"),
    r("txt",  NA,     readable = TRUE,  mime = "text/plain"),
    r("dat",  NA,     readable = TRUE),
    r("json", NA,     readable = FALSE, mime = "application/json"),
    r("xlsx", NA,     readable = TRUE,
      mime = "application/vnd.openxmlformats-officedocument.spreadsheetml.sheet"),
    r("xls",  NA,     readable = TRUE,  mime = "application/vnd.ms-excel"),
    r("ods",  NA,     readable = TRUE,
      mime = "application/vnd.oasis.opendocument.spreadsheet"),
    r("fods", NA,     readable = TRUE,
      mime = "application/vnd.oasis.opendocument.spreadsheet"),
    r("sav",  "data", readable = TRUE,  mime = "application/x-spss-sav"),
    r("dta",  "data", readable = TRUE,  mime = "application/x-stata-dta"),
    r("sas7bdat", "data", readable = TRUE, mime = "application/x-sas-data"),
    # SPSS portable format: real reader via haven::read_por() (added to
    # data_read_head()'s switch alongside sav/dta/sas7bdat) -- previously had
    # a MIME type asserted with no reader anywhere in the package.
    r("por",  "data", readable = TRUE,  mime = "application/x-spss-por"),
    r("rds",  "data", readable = TRUE,  mime = "application/x-r-rds"),
    r("rda",  "data", readable = TRUE,  mime = "application/x-r-data"),
    r("rdata","data", readable = TRUE,  mime = "application/x-r-data"),
    # Binary scientific-data containers that name-based rules miss (they
    # would otherwise fall through to "unknown"). These hold research data,
    # not assets. None have a metacheck reader (readable = FALSE) yet.
    r("npy",  "data", readable = FALSE),
    r("npz",  "data", readable = FALSE),
    r("h5",   "data", readable = FALSE),
    r("hdf5", "data", readable = FALSE),
    r("hdf",  "data", readable = FALSE),
    r("fif",  "data", readable = FALSE),
    r("pkl",  "data", readable = FALSE),
    r("pickle","data",readable = FALSE),
    r("pk",   "data", readable = FALSE),
    r("ft",   "data", readable = FALSE),
    r("feather","data",readable = FALSE),
    r("parquet","data",readable = FALSE),
    r("textgrid","data",readable = FALSE),
    # Trial-level behavioural-task data. Inquisit .iqdat is tab-delimited
    # TEXT, so it is real, readable research data and downloads under the
    # default `download = "data"`; its paradata can be extracted.
    r("iqdat","data", readable = FALSE),
    # Per-participant / per-tool research data confirmed from real corpus
    # examples (.metacheck_repo_cache, 849 cached repositories): .topd (raw
    # per-participant behavioural files, e.g. "plt_10mo/01-1-1009.topd"),
    # .psydat (PsychoPy's own binary per-participant data file), .set
    # (EEGLAB dataset, paired with .fdt), .numbers (Apple Numbers
    # spreadsheet, same family as .xlsx/.ods -- no metacheck reader yet).
    r("topd",  "data", readable = FALSE),
    r("psydat","data", readable = FALSE),
    r("set",   "data", readable = FALSE),
    r("numbers","data",readable = FALSE),
    # E-Prime MERGED data output (sibling of .edat/.edat2, but the merged
    # multi-participant export rather than a single-participant recording).
    r("emrg2", "data", readable = FALSE),
    # NVivo (qualitative analysis software) project file: bundles coded
    # qualitative source content (text/audio/video/image) plus the
    # researcher's analytic annotations on it.
    r("nvp",   "data", readable = FALSE),
    # Other qualitative-analysis-software project files -- same treatment as
    # .nvp (coded source content + analytic annotations). NOT corpus-
    # confirmed (see the EEG/neuroimaging block above for why); .qdpx is the
    # REFI-QDA vendor-neutral exchange format used across ATLAS.ti/MAXQDA/
    # NVivo/Dedoose alike, so it is the single most likely of these to
    # actually appear in an open-science deposit.
    r("qdpx",   "data", readable = FALSE),  # REFI-QDA exchange format
    r("atlproj","data", readable = FALSE),  # ATLAS.ti project
    # Physiological/EEG recording formats. .edf is European Data Format;
    # .vmrk/.vhdr are BrainVision's marker/header files (paired with a
    # binary .eeg data file); .acq is BIOPAC AcqKnowledge data (confirmed
    # against uwmadison-chm/bioread's own test fixtures).
    r("edf",  "data", readable = FALSE),
    r("vmrk", "data", readable = FALSE),
    r("vhdr", "data", readable = FALSE),
    r("acq",  "data", readable = FALSE),
    # NLP parse/annotation formats (confirmed against the naturalstories
    # corpus: languageMIT/naturalstories/parses/{penn,stanford,ud}/...) --
    # derived linguistic data, not source code.
    r("conllx","data",readable = FALSE),
    r("tok",  "data", readable = FALSE),
    r("t2c",  "data", readable = FALSE),
    r("consfeatures","data",readable = FALSE),
    r("depfeatures","data",readable = FALSE),
    # EEG/physiological/neuroimaging RAW DATA formats. UNLIKE every other row
    # in this registry, these are NOT confirmed against a real example in
    # .metacheck_repo_cache (a dedicated search found zero occurrences across
    # all 849 cached repositories -- this corpus skews behavioural/survey,
    # not neuroimaging/EEG-hardware studies). Added anyway because they are
    # standardised, unambiguous formats with essentially no collision risk
    # against anything else in this registry (.fif is the one overlap, and it
    # already resolves to "data" above for the same reason). Revisit/verify
    # against a real example if one turns up.
    r("bdf",  "data", readable = FALSE),  # BioSemi EEG
    r("cnt",  "data", readable = FALSE),  # Neuroscan/ANT Neuro EEG
    r("gdf",  "data", readable = FALSE),  # g.tec EEG
    r("mff",  "data", readable = FALSE),  # EGI Netstation EEG
    r("xdf",  "data", readable = FALSE),  # Lab Streaming Layer
    r("nii",  "data", readable = FALSE),  # NIfTI (fMRI)
    r("dcm",  "data", readable = FALSE),  # DICOM (medical imaging)
    r("mnc",  "data", readable = FALSE),  # MINC (neuroimaging)
    r("mgz",  "data", readable = FALSE),  # FreeSurfer
    r("mgh",  "data", readable = FALSE),  # FreeSurfer
    r("gii",  "data", readable = FALSE),  # GIFTI (surface neuroimaging)
    # Database files holding real research data (psiturk participant
    # records, WebGazer training data).
    r("db",     "data", readable = FALSE),
    r("sqlite", "data", readable = FALSE),
    # .asc is generic ASCII text; in real corpus context it is an SR
    # Research EyeLink eye-tracking data export (confirmed under an
    # "Eye-Tracking/data/" folder).
    r("asc", "data", readable = FALSE),

    # -- Materials ---------------------------------------------------------
    r("exe",  "materials"), r("dmg",  "materials"), r("app",  "materials"),
    r("jar",  "materials"), r("msi",  "materials"), r("deb",  "materials"),
    r("rpm",  "materials"),
    # Compiled binaries / installer packages, not source: elf is a compiled
    # Linux binary; msix/msixbundle are Windows installer packages; jnlp is a
    # Java Web Start launcher (XML pointing at a Java app to fetch and run).
    r("elf",  "materials"), r("msix", "materials"), r("msixbundle", "materials"),
    r("jnlp", "materials"),
    r("sh",   "materials"), r("bash", "materials"), r("zsh",  "materials"),
    r("bat",  "materials"), r("cmd",  "materials"), r("ps1",  "materials"),
    r("dll",  "materials"), r("so",   "materials"), r("dylib","materials"),
    r("lua",  "materials"), r("psyexp","materials"), r("osexp","materials"),
    # E-Prime .edat/.edat2 are proprietary BINARY (OLE compound documents)
    # that metacheck cannot read at all -- .eprime_is_export() rejects them,
    # so a downloaded .edat always fails to parse and yields nothing. The
    # analysable data lives in E-Prime's plain-.txt export (detected from
    # content via .eprime_is_export()). So .edat/.edat2 are classed
    # "materials": recorded in the manifest as present, but never downloaded.
    r("edat", "materials"), r("edat2","materials"),
    # Experiment-runner project/task files confirmed against real corpus
    # examples: .iqx is Inquisit's own experiment-script format (sibling to
    # .iqdat's data output); .resx/.pdb/.manifest/.cache/.myapp/.settings/
    # .sln/.user/.vbproj/.suo are Visual Studio IDE/build scaffolding bundled
    # alongside one corpus study's custom VB experiment program (not research
    # content, but part of the runnable software); .a7p/.ebs/.ebs2/.es/.es2/
    # .wndpos/.exp are DirectRT/Inquisit-family task-definition files;
    # .pde is a Processing/DMDX-style stimulus-presentation file; .mexw64 is
    # a compiled MATLAB MEX binary (Windows); .binarypb is a compiled
    # MediaPipe model binary (WebGazer eye-tracking). .opf is Datavyu (video
    # coding tool, Databrary spinoff) -- confirmed via databrary/pyvyu, a
    # library built specifically to parse Datavyu .opf files; the sampled
    # "costly_template.opf" name suggests a coding-scheme template
    # (apparatus) rather than coded results.
    r("iqx",      "materials"), r("resources", "materials"),
    r("resx",     "materials"), r("pdb",       "materials"),
    r("manifest", "materials"), r("cache",     "materials"),
    r("myapp",    "materials"), r("settings",  "materials"),
    r("sln",      "materials"), r("user",      "materials"),
    r("vbproj",   "materials"), r("suo",       "materials"),
    r("a7p",      "materials"), r("ebs",       "materials"),
    r("ebs2",     "materials"), r("es",        "materials"),
    r("es2",      "materials"), r("wndpos",    "materials"),
    r("exp",      "materials"), r("pde",       "materials"),
    r("mexw64",   "materials"), r("binarypb",  "materials"),
    r("opf",      "materials"),
    # Build/cache artifacts: automatically generated byproducts of running
    # code, not authored content or research data -- .pyc (Python bytecode,
    # confirmed under __pycache__/), .map (JS source maps for vendor
    # libraries like Bootstrap/jQuery, confirmed under a docs/deps/ folder),
    # .rdb/.rdx (R's own knitr lazy-load cache pair, confirmed via knitr's
    # own cache.R source using tools:::makeLazyLoadDB). Filed as materials
    # (the "bundled with the software/tooling, not itself analytic content"
    # bucket), matching the earlier IDE/build-scaffolding batch above.
    r("pyc", "materials"), r("map", "materials"),
    r("rdb", "materials"), r("rdx", "materials"),

    # -- Output ------------------------------------------------------------
    # .spv/.smcl have full readers (import_stat_output()/import_stata_smcl(),
    # R/spv.R, R/stata.R) that recover their embedded analysis syntax as a
    # sibling code file -- the .spv/.smcl row ITSELF still stays data_type
    # "output" (see .code_expand_spv()/.code_expand_smcl(), R/code_check.R);
    # only the recovered syntax becomes a checked code file.
    r("spv",  "output", readable = FALSE),
    r("smcl", "output", readable = FALSE),
    r("fig",  "output", readable = FALSE),
    # A .out is Mplus's rendered output. Unlike .spv/.smcl (unambiguously
    # SPSS/Stata-specific), ".out" is a generic extension also used for
    # compiled Unix binaries and unrelated tool logs, so a false positive
    # here is genuinely possible from extension alone -- classification
    # still keys on the extension (content can't be checked before a file is
    # downloaded), but data_check() reclassifies any downloaded .out back to
    # "unknown" if .mplus_is_genuine_output() (R/mplus.R) rejects it.
    r("out",  "output", readable = FALSE),
    # .rout is an R console transcript (Rscript's own ".Rout" convention,
    # confirmed against real corpus examples under a "PACE R_Code..." study)
    # -- a rendered run log, not source. .amosoutput/.amp/.amw/.bk1/.bk2/
    # .amosp are AMOS SEM software's own output-file family (confirmed
    # against one corpus study's CFA_AMOS/ folder). .afdesign (Affinity
    # Designer) and .pt (PyTorch torch.save() model checkpoint) are both
    # GENERATED artifacts from running analysis/training code -- an editable
    # source figure and a trained-model checkpoint respectively, not raw
    # collected data.
    r("rout",       "output"), r("amosoutput", "output"),
    r("amp",        "output"), r("amw",        "output"),
    r("bk1",        "output"), r("bk2",        "output"),
    r("amosp",      "output"), r("afdesign",   "output"),
    r("pt",         "output"),

    # -- Documentation -----------------------------------------------------
    # A Qualtrics survey-definition file (.qsf) is the survey's own codebook:
    # it carries every question's wording and its response options with
    # coded values (see parse_qsf()). Classing it as documentation (with
    # doc_role "codebook") makes it download under the default
    # `download = "data"` and routes it into codebook_check's parser.
    r("qsf",  "documentation"),
    # A Stata plain-text log (`log using foo.log`, the un-marked-up sibling
    # of .smcl -- both come from Stata's own `log using` command) is treated
    # as documentation for now: no confirmed real-corpus example exists to
    # validate a "is this genuinely Stata output" sniffer against (unlike
    # .out, which DOES have .mplus_is_genuine_output() built and verified).
    # Revisit if/when a real .log example turns up.
    r("log",  "documentation"),
    # Bibliography / manuscript-typesetting support files (LaTeX/citation
    # tooling), confirmed against real corpus examples (bibliography/
    # library.bib, apa.csl, plos2015.bst, wlscirep.cls, jabbrv.sty) --
    # explanatory/reference material for the manuscript, not analytic
    # content. .cff is the Citation File Format; .gdoc/.url are shortcut
    # files pointing at external documents (Google Docs, OSF links); .aux/
    # .fff/.thmx are LaTeX/Office build byproducts of a rendered document;
    # .dcf is an R-package-style DESCRIPTION-format metadata file; .drawio
    # is diagram source. .key (Apple Keynote) is locked to documentation
    # here rather than left to the materials/ folder-keyword rule: it is
    # fundamentally a slide-deck format like .pptx, even though some corpus
    # examples sit inside a "Materials/" folder.
    r("bib", "documentation"), r("csl", "documentation"),
    r("bst", "documentation"), r("cls", "documentation"),
    r("sty", "documentation"), r("cff", "documentation"),
    r("gdoc","documentation"), r("url", "documentation"),
    r("aux", "documentation"), r("fff", "documentation"),
    r("thmx","documentation"), r("dcf", "documentation"),
    r("drawio","documentation"), r("key", "documentation"),
    # .cgi is a generic Common-Gateway-Interface script marker; in real
    # corpus context it names a saved Ovid literature-search results
    # webpage ("ovidweb.cgi"), not an executable script -- reference
    # material for a literature search, not analytic content.
    r("cgi", "documentation")
  )
})()

# Format-locked lookup used by data_classify_files() Tier 1. Kept as a plain
# named vector (not the full data frame) so existing `.fixed_ext_type[ext]`
# call sites are untouched; built from .ext_registry so it can never drift
# from the readable/code_lang/mime columns derived from the same rows.
.fixed_ext_type <- stats::setNames(
  .ext_registry$data_type[!is.na(.ext_registry$data_type)],
  .ext_registry$ext[!is.na(.ext_registry$data_type)]
)

#' Classify repository files into data_check semantic types
#'
#' Rules-only classifier used by the `data_check` module when the LLM is off.
#' Layers metacheck's `file_category()` (name-based readme/codebook/data/code
#' rules) over an extension crosswalk built on `metacheck::file_types`, then
#' applies format-locked extension overrides.
#'
#' @param file_name a character vector of file names (basenames)
#' @param file_path optional character vector, same length as `file_name`: the
#'   full repo-relative path of each file (e.g. `"ResearchBox 801/Materials/
#'   Informant Survey_Redacted.pdf"`). When supplied, a keyword found ANYWHERE
#'   in the path (a folder segment OR the filename itself -- see the Tier 2
#'   keyword table below) reclassifies a file. A researcher's own naming,
#'   whether the folder or the file, is a stronger, more deliberate signal
#'   than a generic extension like `.pdf`/`.docx`/`.txt`, which could hold
#'   anything (a PDF questionnaire is materials; a PDF preprint is
#'   documentation; the extension alone cannot tell them apart, but the
#'   author's own naming can). Does NOT override a format-locked type
#'   (`.fixed_ext_type`, or `file_category()`'s own hard rules): a genuine
#'   `.csv`/`.R`/`.sav` found under a mislabelled "Materials/" folder still
#'   classifies by its real format, since the extension is stronger evidence
#'   of what the file actually IS than a name is. `NULL` (the default)
#'   disables this layer entirely, unchanged from before.
#'
#' @returns a character vector of data_check types (see `.data_check_types`);
#'   `"unknown"` when no rule fires. See `.data_doc_role()` for the finer
#'   readme/codebook/supplemental distinction within `"documentation"`.
#' @export
#' @keywords internal
#'
#' @examples
#' data_classify_files(c("data.csv", "analysis.R", "README.md", "codebook.xlsx"))
data_classify_files <- function(file_name, file_path = NULL) {
  n <- length(file_name)
  if (n == 0) return(character(0))

  # -- Tier 1: format-locked classification ----------------------------------
  # Nothing below this point can override a Tier 1 result. `file_category()`'s
  # hard rules (sure_class: stats/data/code, and the .jasp/.por data+stats
  # compound) key on the file's actual FORMAT, not its name -- translate its
  # old-style labels ("readme", "codebook") into the 6-way data_check
  # vocabulary here so only this one place needs to know about that mapping.
  cat_raw <- file_category(file_name)$file_category
  cat <- dplyr::case_when(
    cat_raw %in% c("readme", "codebook") ~ "documentation",
    .default = cat_raw
  )
  ext <- tolower(tools::file_ext(file_name))
  fixed <- unname(.fixed_ext_type[ext])
  locked <- dplyr::coalesce(fixed, cat)          # fixed extension wins over cat_raw

  # -- Tier 2: keyword-in-name overrides -------------------------------------
  # An explicit category word in the researcher's own folder or file naming
  # ("Materials/", "Results.docx", "analysis_code.zip") is a more deliberate,
  # direct signal than a generic extension like .pdf/.docx/.txt/.html/.zip,
  # each of which could hold almost anything. Checked as whole NAME TOKENS --
  # bounded by "/", "_", "-", ".", space, or start/end of string -- never as a
  # bare substring: R regex's \b treats "_" as a word character, so \b alone
  # would silently miss "my_output_log.html" (verified directly against that
  # string, not by reasoning about the regex), and a bare substring match
  # would wrongly fire on "metadata.csv" or "encoding.csv" for "data".
  # Ordered most-specific-first so a compound (codebook, prereg) is claimed
  # before a shorter generic word (code, data) could grab part of it.
  path_for_kw <- if (!is.null(file_path)) {
    ifelse(is.na(file_path) | !nzchar(file_path %||% ""), file_name, file_path)
  } else file_name
  path_lc <- tolower(path_for_kw)

  tok <- function(pattern) paste0("(^|[/_. -])(", pattern, ")($|[/_. -])")
  keyword_rules <- list(
    list(type = "documentation", pattern = "readme"),
    list(type = "documentation", pattern = "code[ _.-]?book"),
    list(type = "documentation", pattern = "pre[ _-]?reg(istration)?"),
    list(type = "materials",     pattern = "materials|stimuli|stimulus"),
    list(type = "data",          pattern = "data"),
    list(type = "code",          pattern = "code|script"),
    list(type = "output",        pattern = "output|results")
  )

  # FIRST match wins: once a rule has claimed a row (type != locked), a LATER
  # rule must not overwrite it -- e.g. "code_book.csv" matches BOTH the
  # codebook pattern (rule 2) and the bare code|script pattern (rule 6), and
  # without this guard rule 6 would silently clobber rule 2's correct
  # "documentation" back to the wrong "code" on every subsequent iteration
  # (caught by testing the exact string "code_book.csv", not by reasoning
  # about the loop).
  type <- locked
  claimed <- !is.na(locked)
  for (rule in keyword_rules) {
    hit <- grepl(tok(rule$pattern), path_lc) & is.na(fixed) & !claimed
    type[hit] <- rule$type
    claimed <- claimed | hit
  }

  # -- Tier 3: coarse crosswalk fallback -------------------------------------
  # Whatever Tier 1/2 left unresolved falls back to metacheck::file_types via
  # .file_type_crosswalk (e.g. image -> materials, text -> documentation).
  coarse <- filetype(file_name)                  # named vector, may be "a;b"
  coarse_first <- sub(";.*$", "", unname(coarse))
  crosswalked <- unname(.file_type_crosswalk[coarse_first])
  type <- ifelse(is.na(type), crosswalked, type)

  # ro-crate-metadata.json is collection-level documentation (see
  # .data_doc_role()), never code or data -- without this override its .json
  # extension would crosswalk to "code"/"data" via the coarse file_types table.
  type[grepl("^ro-crate-metadata\\.json$", tolower(basename(file_name)))] <- "documentation"

  type[is.na(type)] <- "unknown"
  type
}

#' Classify a documentation file's fine-grained role
#'
#' Within `data_classify_files()`'s coarse `"documentation"` type, distinguish
#' the specific artifact: the (collection-level) readme, a codebook, or plain
#' supplemental text (preprints, slide decks, Word docs). This is the axis
#' `psychds_check` uses to decide root-vs-per-study placement and that
#' `codebook_check` uses to select which files to parse -- orthogonal to
#' `data_type`, which only says "this is documentation of some kind."
#'
#' `ro-crate-metadata.json` is treated as a `"readme"` role: like a README, it
#' is collection-level (root, singular), never assigned to a single study.
#'
#' @param file_name a character vector of file names (basenames)
#'
#' @returns a character vector: `"readme"`, `"license"`, `"codebook"`,
#'   `"supplemental"`, or `NA` for files that are not
#'   `data_type == "documentation"`.
#' @keywords internal
.data_doc_role <- function(file_name) {
  n <- length(file_name)
  if (n == 0) return(character(0))

  is_doc <- data_classify_files(file_name) == "documentation"
  cat_raw <- file_category(file_name)$file_category
  ext <- tolower(tools::file_ext(file_name))
  base <- basename(file_name)

  role <- dplyr::case_when(
    cat_raw == "readme" ~ "readme",
    grepl("^ro-crate-metadata\\.json$", base, ignore.case = TRUE) ~ "readme",
    grepl("^readme($|\\.)", tolower(base)) ~ "readme",
    # A LICENSE file is collection-level, like the readme (one licence for the
    # whole deposit, not per-study), so it gets the same root placement in
    # convert_psychds()'s target_of() -- see psychds_check.R.
    grepl("^licen[sc]e($|\\.)", tolower(base)) ~ "license",
    cat_raw == "codebook" ~ "codebook",
    ext == "qsf" ~ "codebook",
    is_doc ~ "supplemental",
    .default = NA_character_
  )
  role[!is_doc & is.na(role)] <- NA_character_
  role
}

# -- Data format (tabular vs raw) ---------------------------------------------

# The SINGLE source of truth for "can metacheck read this as a table": every
# extension with a branch in data_read_head()'s switch, and nothing else. Keep
# the two in lockstep -- adding a reader branch without adding its extension here
# (via .ext_registry's `readable` column) leaves the format downloaded but
# never read; adding it here without a reader branch makes data_read_head()
# return NULL for a file we promised was tabular.
#
# Three separate behaviours are derived from this one vector, which is why it
# must be capability-based rather than a hand-maintained wish list:
#   * data_format()  -> "tabular"/"raw", which gates DOWNLOADING in data_check
#   * psychds_check  -> which data files are converted to a Psych-DS _data.csv
#   * .psychds_write_data_csv() -> the converter that actually reads the bytes
# Previously this was maintained as its own hand-typed vector, itself already
# a fix for three EARLIER divergent lists (.ods was readable but never
# converted; formats with no reader branch were downloaded and silently read
# as NULL) -- now derived from .ext_registry so a new reader branch only needs
# updating in one place (the registry's `readable` column) instead of two.
.readable_extensions <- .ext_registry$ext[.ext_registry$readable]

#' Classify a data file as tabular or raw
#'
#' `"tabular"` means metacheck has a reader for the format (see
#' `.readable_extensions`), so the file can be downloaded, parsed for columns,
#' and converted to a Psych-DS CSV. Everything else is `"raw"`: it is recorded
#' and archived with its true extension, but never parsed as a table.
#'
#' @param ext a character vector of file extensions (no leading dot); case is
#'   ignored.
#'
#' @returns `"tabular"` or `"raw"` for each element (never `NA`; unknown
#'   extensions fall back to `"raw"`, since an unrecognised format has no
#'   reader).
#' @export
#' @keywords internal
#'
#' @examples
#' data_format(c("csv", "edf", "mp4", "sav"))
data_format <- function(ext) {
  ifelse(tolower(ext) %in% .readable_extensions, "tabular", "raw")
}

#' Detect a file manifest / table-of-contents masquerading as tabular data
#'
#' A manifest (e.g. a "table of contents" CSV) is structurally a valid tabular
#' file, so extension-based classification treats it as data. It is
#' distinguished from real research data by content, using the repository's own
#' file list as ground truth: a manifest has a column in which most values name
#' other files in the repository. This is name- and header-agnostic -- it does
#' not rely on the file or its columns being *called* anything in particular.
#'
#' To avoid demoting genuine data that merely references assets (e.g. a
#' `stimulus` column of image filenames), a candidate column must both reach the
#' `threshold` of file references and reference at least `min_exts` distinct file
#' extensions (a manifest points across code/data/docs; an asset column is
#' usually one extension).
#'
#' @param df a data.frame (the read tabular file)
#' @param repo_files a character vector of the other file names/paths in the
#'   same repository (basenames are compared)
#' @param threshold minimum fraction of a column's non-empty values that must
#'   resolve to repository files
#' @param min_exts minimum number of distinct referenced file extensions
#'
#' @returns `TRUE` when `df` looks like a file manifest, else `FALSE`.
#' @export
#' @keywords internal
data_is_manifest <- function(df, repo_files, threshold = 0.8, min_exts = 2L) {
  if (is.null(df) || nrow(df) == 0 || ncol(df) == 0) return(FALSE)
  repo_files <- repo_files[!is.na(repo_files) & nzchar(repo_files)]
  if (length(repo_files) == 0) return(FALSE)
  repo_base <- tolower(basename(gsub("\\\\", "/", repo_files)))

  for (col in df) {
    vals <- tolower(trimws(as.character(col)))
    vals <- vals[!is.na(vals) & nzchar(vals)]
    if (length(vals) == 0) next
    vbase <- basename(gsub("\\\\", "/", vals))
    is_ref <- vbase %in% repo_base
    if (mean(is_ref) < threshold) next
    exts <- tools::file_ext(vals[is_ref])
    exts <- exts[nzchar(exts)]
    if (length(unique(exts)) >= min_exts) return(TRUE)
  }
  FALSE
}

# Default number of items per LLM classification call. Sized for reliable
# structured-array responses on small models (e.g. Groq's gpt-oss-20b): input
# tokens are not the binding constraint here -- the limit is how many array items
# the model returns complete and correctly indexed. ~50 keeps responses reliable
# while cutting call count ~50x versus one-call-per-item. Used by every batched
# classifier in data_check so batch size is tuned in one place.
.data_check_llm_batch <- 50L

# Default sampler seed for the study-group pass. Fixed (not random) so repeated
# runs of the same paper ask the provider for the same sampling path; callers can
# override via params$seed. Best-effort -- see .data_group_llm_impl().
.data_group_seed <- 8675309L

# Write a per-paper file manifest (JSON) recording every repository file and
# whether it was downloaded -- the provenance needed to audit a corpus or rebuild
# a data archive without re-querying every repo. `files` is data_check's finalised
# `all_files`; `want` is the logical vector of files this run tried to download;
# `gated` is the download gate table (repos refused by the size caps);
# `oversize` / `failed` are download_repo_files()'s "oversize_skipped" and
# "failed" attributes; `zip_peek` the per-row zip-peek skip reasons; `model` the
# LLM model string the run used.
#
# Every file not downloaded is classified as **intentional** (a policy decision:
# download mode, skip_types, zip peek, the size caps -- re-running changes
# nothing unless the settings change) or **unintentional** (the run wanted the
# file and could not fetch it: transient download failure, missing URL -- a
# re-run with the same settings retries exactly these, since cached files are
# reused). The top-level `not_downloaded` block separates the two and sets
# `rerun_recommended`, so a corpus audit can find incomplete papers mechanically.
#
# The `provenance` block records what is needed to reproduce the archive: the
# metacheck version, R version and platform, the production timestamp, and the
# LLM model (when LLM assistance was on). Field names map onto DDI-Codebook 2.5
# elements and the mapping ships inside the manifest (provenance$ddi_mapping) so
# the JSON is self-describing.
#
# Sizes are completed here: a downloaded file's real size comes from disk, and a
# wanted file the listing left unsized (OSF returns NA for some files, often the
# large ones) is resolved with a cheap HEAD probe -- so the manifest carries a
# real size for choosing the archive's size ceiling. Only NA-sized wanted files
# are probed, and only when a manifest is requested, so normal runs pay nothing.
.data_check_write_manifest <- function(manifest, files, want, gated,
                                       paper_id, download,
                                       max_file_size, max_download_size,
                                       skip_types = NULL,
                                       oversize = NULL, failed = NULL,
                                       zip_peek = NULL, model = NULL) {
  # Resolve the output path: a directory -> "<paper_id>.manifest.json" inside it;
  # a ".json" path is used verbatim.
  path <- manifest
  if (!grepl("\\.json$", path, ignore.case = TRUE)) {
    dir.create(path, recursive = TRUE, showWarnings = FALSE)
    pid <- if (length(paper_id) && !is.na(paper_id[[1]])) paper_id[[1]] else "manifest"
    path <- file.path(path, paste0(pid, ".manifest.json"))
  } else {
    dir.create(dirname(path), recursive = TRUE, showWarnings = FALSE)
  }

  n <- nrow(files)
  # `files` can grow after zip expansion; normalize `want` so all logical
  # operations below are length-stable and NA-free.
  if (length(want) == 0) {
    want <- rep(FALSE, n)
  } else if (length(want) != n) {
    want <- rep_len(want, n)
  }
  want <- as.logical(want)
  want[is.na(want)] <- FALSE

  loc <- files$file_location %||% rep(NA_character_, n)
  downloaded <- !is.na(loc) & nzchar(loc) & file.exists(loc %||% "")
  gated_urls <- if (!is.null(gated) && nrow(gated) > 0) gated$repo_url else character(0)

  # Complete the sizes. A downloaded file's real size is on disk. For a wanted
  # file the listing left unsized (OSF returns NA for some files -- exactly the
  # large ones), resolve it with a cheap HEAD probe so the manifest carries a
  # real size for ceiling planning. This runs only when a manifest is requested
  # (opt-in) and only for the NA-sized wanted files, so normal runs pay nothing.
  file_size <- as.numeric(files$file_size)
  on_disk_size <- ifelse(downloaded, suppressWarnings(file.size(loc)), NA_real_)
  file_size <- ifelse(!is.na(on_disk_size), on_disk_size, file_size)

  url <- files$file_url %||% rep(NA_character_, n)
  probe <- which(is.na(file_size) & want &
                   !is.na(url) & nzchar(url) & !downloaded)
  if (length(probe) > 0) {
    pb_probe <- pb(length(probe),
                   "Sizing files (HEAD) [:bar] :current/:total")
    on.exit(pb_probe$terminate(), add = TRUE)
    for (i in probe) {
      file_size[i] <- .remote_size(url[i])
      pb_probe$tick()
    }
  }

  # Why was a file not downloaded? Ordered from most to least specific, and
  # classified: intentional = a policy decision (re-running changes nothing
  # unless settings change); unintentional = wanted but not fetched (a re-run
  # with the same settings retries exactly these).
  dtype <- files$data_type %||% rep(NA_character_, n)
  if (is.null(zip_peek) || length(zip_peek) != n)
    zip_peek <- c(zip_peek, rep(NA_character_, n))[seq_len(n)]
  over_key <- if (!is.null(oversize) && nrow(oversize) > 0)
    paste(oversize$repo_url, oversize$file_name) else character(0)
  fail_err <- if (!is.null(failed) && nrow(failed) > 0)
    stats::setNames(sub("\n.*", "", failed$error),
                    paste(failed$repo_url, failed$file_name)) else character(0)

  reason      <- rep(NA_character_, n)
  intentional <- rep(NA, n)
  for (i in which(!downloaded)) {
    key <- paste(files$repo_url[i], files$file_name[i])
    url <- files$file_url[i] %||% NA_character_
    if (identical(download, "none")) {
      reason[i] <- "download = \"none\""; intentional[i] <- TRUE
    } else if (!is.null(skip_types) && dtype[i] %in% skip_types) {
      reason[i] <- sprintf("excluded type '%s' (linked, not mirrored)", dtype[i])
      intentional[i] <- TRUE
    } else if (!is.na(zip_peek[i]) && nzchar(zip_peek[i])) {
      reason[i] <- zip_peek[i]; intentional[i] <- TRUE
    } else if (!isTRUE(want[i])) {
      reason[i] <- "not a data/codebook/README file (use download = \"all\")"
      intentional[i] <- TRUE
    } else if (is.na(url) || !nzchar(url)) {
      reason[i] <- "no download URL in the listing"; intentional[i] <- FALSE
    } else if (key %in% over_key) {
      reason[i] <- sprintf("exceeds max_file_size (%s MB): skipped by the per-file cap",
                           .cap_num(max_file_size))
      intentional[i] <- TRUE
    } else if (files$repo_url[i] %in% gated_urls) {
      reason[i] <- "repository refused by the size caps"; intentional[i] <- TRUE
    } else if (key %in% names(fail_err)) {
      reason[i] <- paste0("download failed after retries: ", fail_err[[key]])
      intentional[i] <- FALSE
    } else {
      reason[i] <- "download failed"; intentional[i] <- FALSE
    }
  }
  status <- ifelse(downloaded, "downloaded",
                   ifelse(intentional %in% TRUE, "skipped", "failed"))

  entries <- lapply(seq_len(n), function(i) {
    Filter(Negate(is.null), list(
      file_name    = files$file_name[i],
      file_path    = files$file_path[i] %||% files$file_name[i],
      repo_url     = files$repo_url[i],
      file_url     = files$file_url[i] %||% NA_character_,
      # Storage provider (osfstorage / dropbox / github / ...), from repo_check.
      # Recorded so a re-run reconstructing rows from the manifest keeps the
      # Waterbutler-zip eligibility that download_repo_files() keys on. NULL for
      # non-OSF hosts (they have their own zip/file-by-file paths).
      provider     = if (!is.null(files$provider) && !is.na(files$provider[i]))
                       files$provider[i] else NULL,
      file_size    = if (!is.na(file_size[i])) file_size[i] else NULL,
      data_type    = files$data_type[i] %||% NA_character_,
      data_format  = files$data_format[i] %||% NA_character_,
      downloaded   = downloaded[i],
      status       = status[i],
      skip_reason  = if (downloaded[i]) NULL else reason[i],
      skip_intentional = if (downloaded[i]) NULL else intentional[i]
    ))
  })

  generated <- format(Sys.time(), "%Y-%m-%dT%H:%M:%S%z")
  unint <- which(!downloaded & intentional %in% FALSE)
  intent <- which(!downloaded & intentional %in% TRUE)

  # Reproducibility metadata. Field names map onto DDI-Codebook 2.5 elements;
  # ddi_mapping documents the correspondence inside the manifest itself.
  provenance <- list(
    software  = list(name = "metacheck", version = tryCatch(
      as.character(utils::packageVersion("metacheck")),
      error = function(e) NA_character_)),
    r_version = R.version.string,
    platform  = R.version$platform,
    prod_date = generated,
    llm       = if (isTRUE(llm_use()))
      list(used = TRUE, model = model %||% llm_model())
    else list(used = FALSE),
    ddi_mapping = list(
      "provenance.software"  = "docDscr/citation/prodStmt/software (@version)",
      "provenance.prod_date" = "docDscr/citation/prodStmt/prodDate",
      "files[].file_name"    = "fileDscr/fileTxt/fileName",
      "files[].file_url"     = "fileDscr/@URI",
      "files[].data_type"    = "fileDscr/fileTxt/fileCont",
      "files[].status"       = "fileDscr/fileTxt/ProcStat",
      "files[].skip_reason"  = "fileDscr/notes"
    )
  )

  doc <- list(
    paper_id  = if (length(paper_id)) paper_id[[1]] else NA_character_,
    generated = generated,
    download  = download,
    skip_types = if (length(skip_types)) as.list(skip_types) else NULL,
    caps      = list(max_file_size_mb = max_file_size,
                     max_download_size_mb = max_download_size),
    provenance   = provenance,
    n_files      = n,
    n_downloaded = sum(downloaded),
    not_downloaded = list(
      intentional_n   = length(intent),
      unintentional_n = length(unint),
      # The unintentional list is the re-run signal: these are the files a
      # re-run with the same settings will retry (cache reuse skips the rest).
      unintentional_files = lapply(unint, function(i) list(
        file_name = files$file_name[i],
        repo_url  = files$repo_url[i],
        reason    = reason[i])),
      rerun_recommended = length(unint) > 0
    ),
    files        = entries
  )
  doc <- Filter(Negate(is.null), doc)

  # Merge (not overwrite): the manifest is a shared metacheck file. data_check
  # owns every key it builds here; code_check owns a separate `code` section
  # (packages). Each writer passes only its own keys, so a re-run of one module
  # never drops the other's section (see manifest_merge()).
  manifest_merge(path, doc)
  invisible(path)
}

#' Merge fields into a metacheck manifest, preserving other sections
#'
#' The per-paper `*.manifest.json` is written by more than one module:
#' `data_check` records the files/provenance, and `code_check` records the
#' packages the code loads (a `code` section). Because each module rebuilds only
#' its own part of the document, a plain overwrite would let whichever module ran
#' last erase the other's section. This helper reads any existing manifest,
#' overlays `patch` at the top level (each key in `patch` replaces that key
#' wholesale; keys not in `patch` are kept untouched), and writes it back.
#'
#' The rule that makes this safe against run order: each writer's `patch` must
#' contain only the keys it owns. `data_check` patches everything except `code`;
#' `code_check` patches only `code`. Overlapping keys would still clobber.
#'
#' @param path the manifest file path (`*.manifest.json`)
#' @param patch a named list of top-level keys to set
#'
#' @returns (invisibly) the manifest path
#' @export
manifest_merge <- function(path, patch) {
  dir.create(dirname(path), recursive = TRUE, showWarnings = FALSE)
  existing <- list()
  if (file.exists(path)) {
    existing <- tryCatch(
      jsonlite::fromJSON(path, simplifyVector = FALSE),
      error = function(e) list())
    if (!is.list(existing)) existing <- list()
  }
  # Overlay each patched key; a NULL value removes that key.
  for (nm in names(patch)) existing[[nm]] <- patch[[nm]]
  json <- jsonlite::toJSON(existing, auto_unbox = TRUE, pretty = TRUE,
                           na = "null")
  writeLines(json, path, useBytes = TRUE)
  invisible(path)
}

# Classify a vector of items with an LLM in index-mapped batches. Each batch
# sends a numbered listing of `item_texts` and expects an object-wrapped array
# of {index, value} objects back; results are mapped to positions by index, so a
# dropped or reordered entry never misaligns the others. Returns a character
# vector the same length as `item_texts` (NA where the LLM gave no valid value).
#
# `system_prompt` should instruct the model to return one {index, value} per
# input line; `value_desc` documents the `value` field; `valid` optionally
# restricts accepted values (others become NA). Runs only when llm_use(TRUE).
.llm_classify_batched <- function(item_texts, system_prompt, value_desc,
                                  valid = NULL, batch_size = .data_check_llm_batch,
                                  model = llm_model(), params = list(),
                                  phase = NULL) {
  n <- length(item_texts)
  out <- rep(NA_character_, n)
  if (n == 0) return(out)

  # Object-wrapped array: some providers (Groq's gpt-oss-20b) 400 on a bare
  # top-level array; nesting under a field is accepted and llm() unwraps it.
  type_spec <- ellmer::type_object(
    results = ellmer::type_array(
      ellmer::type_object(
        index = ellmer::type_integer("The item's number in the list"),
        value = ellmer::type_string(value_desc)
      )
    )
  )

  batches <- split(seq_len(n), ceiling(seq_len(n) / batch_size))
  model_used <- NA_character_
  for (rows in batches) {
    listing <- paste(seq_along(rows), item_texts[rows], sep = ". ",
                     collapse = "\n")
    resp <- tryCatch(
      llm(text = data.frame(text = listing), text_col = "text",
          system_prompt = system_prompt, type = type_spec, model = model,
          params = params, phase = phase),
      error = function(e) NULL
    )
    resp <- .strip_llm_wrapper(resp, "results")
    if (is.null(resp) || nrow(resp) == 0 ||
        !all(c("index", "value") %in% names(resp))) next
    if (is.na(model_used)) model_used <- attr(resp, "llm")$model %||% NA_character_
    idx <- suppressWarnings(as.integer(resp$index))
    val <- tolower(trimws(as.character(resp$value)))
    good <- !is.na(idx) & idx >= 1 & idx <= length(rows) & nzchar(val)
    if (!is.null(valid)) good <- good & val %in% valid
    if (any(good)) out[rows[idx[good]]] <- val[good]
  }
  attr(out, "llm_model") <- model_used
  out
}

#' Assign a study group to each file with an LLM
#'
#' Classifies every file in a repository into a study group from its path
#' (folder + name) context, so a multi-study repository can be split into
#' `study-<group>/` directories (used by `psychds_check`). Group codes follow
#' datacheck's scheme: `ex1`, `ex2a`, `pilot1`, ... . Every file resolves to
#' exactly one study -- there is no `"shared"` group. The only files that stay
#' collection-level (never grouped, `group` left `NA`) are the root README and
#' the root `ro-crate-metadata.json` (see `.data_doc_role()`), which callers
#' must exclude BEFORE calling this function (see `data_check.R`). Only
#' meaningful with an LLM for the residual cases the deterministic passes leave
#' unresolved; callers get a fully deterministic grouping when `llm_use(FALSE)`.
#'
#' Only files that will actually be analysed or placed (data, documentation,
#' code, materials) are sent to the model; every one of them must end up with a
#' real study code -- a materials or documentation file reused across multiple
#' studies is still assigned to exactly ONE owning study (whichever the
#' deterministic passes or the model placed it in first); every other study
#' that reuses it gets a reference recorded separately (see `referenced_by` in
#' `data_check.R`), not a second group membership. Paths that name their study
#' outright ("Experiment 1/", "study2a_data.csv") are grouped by a
#' deterministic regex first and skip the model entirely; LLM-returned codes
#' are normalized and validated against the scheme, so a malformed code can
#' never become a study directory name. The sent files are batched (see
#' `.data_check_llm_batch`) so large repositories do not exceed the model's
#' request/output limits.
#'
#' @param files a data.frame of files (needs `file_path` or `file_name`; an
#'   optional `data_type` column limits which files are sent to the model --
#'   see Details)
#' @param model the LLM model name
#' @param params a named list passed to `llm()`
#' @param batch_size number of files per LLM call
#'
#' @returns a data.frame with `group` and `referenced_by` columns (one row per
#'   input file, same order) and `"model"`/`"roster"`/`"roster_check"`/
#'   `"unresolved"` attributes, or `NULL` only when `files` is empty/NULL.
#'   Every placeable file resolves to a real study group -- there is no
#'   `"shared"` value and no partial-failure NULL return.
#' @export
#' @keywords internal
data_group_llm <- function(files, model = llm_model(), params = list(),
                           batch_size = .data_check_llm_batch, paper = NULL) {
  return(.data_group_llm_impl(files, model, params, batch_size, paper))
}

#' Study roster named in a paper's text
#'
#' Reads the manuscript for the studies it names -- "Experiment 1", "Study 2a",
#' "Pilot 2" -- and returns them as normalised group codes (`ex1`, `ex2a`,
#' `pilot2`). This is the AUTHORITATIVE list of a paper's studies: the authors
#' say how many there are and what they are called, so it both names the groups
#' and gives a count to validate any file grouping against (see
#' `.data_group_check_roster`). Deterministic and free -- a regex over text we
#' already extracted -- so it runs BEFORE any LLM.
#'
#' Only mentions of the form <word><optional space/punct><number><optional
#' single letter> count; a bare "the experiment" names no specific study and is
#' ignored. Sorted by number then letter, so the order is stable.
#'
#' @param paper a paper object
#'
#' @returns a character vector of group codes, or `character(0)` when the text
#'   names no numbered study.
#' @export
#' @keywords internal
data_study_roster <- function(paper) {
  if (!.is_paper(paper)) return(character(0))
  hits <- tryCatch(
    text_search(paper, "\\b(?:study|experiment|pilot)[ ._-]?[0-9]{1,2}[a-z]?\\b",
                return = "match", ignore.case = TRUE, perl = TRUE),
    error = function(e) NULL)
  if (is.null(hits) || !nrow(hits)) return(character(0))
  m <- tolower(gsub("[ ._-]", "", as.character(hits$text)))
  # A trailing letter is a sub-study suffix ("2a"); anything else is dropped by
  # the normalizer, which also maps experiment/study -> ex and keeps pilot.
  code <- .data_group_normalize(m)
  code <- unique(code[!is.na(code)])
  if (!length(code)) return(character(0))
  # Stable order: by number, then by sub-study letter.
  .data_group_sort(code)
}

# Stable sort for study group codes: by leading number, then by trailing
# sub-study letter. Shared by data_study_roster()'s display order and the
# grouping fallback that picks the lexicographically-first study when a
# reused/unplaced file has no other evidence to go on (see
# .data_group_llm_impl()).
.data_group_sort <- function(codes) {
  if (!length(codes)) return(codes)
  num <- suppressWarnings(as.integer(sub("^(ex|pilot)([0-9]{1,2})[a-z]?$", "\\2", codes)))
  suf <- sub("^(ex|pilot)[0-9]{1,2}([a-z]?)$", "\\2", codes)
  codes[order(num, suf)]
}

# Compare a file grouping to the manuscript's study roster and report the
# agreement. The roster is what the AUTHORS say exists; the grouping is what we
# inferred from the files. A mismatch means the structure we are about to write
# contradicts the paper -- worth surfacing rather than silently emitting. Returns
# list(roster, found, missing, extra, agrees).
.data_group_check_roster <- function(groups, roster) {
  found <- unique(groups[!is.na(groups)])
  list(roster  = roster,
       found   = found,
       missing = setdiff(roster, found),   # named in the paper, not in the files
       extra   = setdiff(found, roster),   # in the files, not named in the paper
       agrees  = length(roster) > 0 && setequal(roster, found))
}

# Data files referenced by a code file. A script names the data it reads and
# writes -- read_csv("raw/x.csv"), readRDS("../data/processed/y.rds"),
# write_csv(df, "processed/z.csv") -- which is HARD evidence that the script and
# those files belong to the same study: no guessing, no LLM. Returns the
# referenced paths' basenames (lowercased), or character(0).
#
# Matching on basenames deliberately ignores the relative prefix: a script's
# "../data/processed/trial_level.csv" and the repository's
# "processed/trial_level.csv" are the same file seen from different working
# directories, and reconciling those prefixes reliably is not worth it when the
# basename already identifies the file within its repository.
.CODE_READ_FNS <- paste(
  "read_csv2?", "read\\.csv2?", "read_tsv", "read_delim", "read\\.delim",
  "read_table2?", "read\\.table", "readRDS", "read_rds", "read_excel",
  "read_xlsx", "read_xls", "read_sav", "read_dta", "read_sas", "read_spss",
  "fread", "read_json", "fromJSON", "read_feather", "read_parquet",
  "write_csv2?", "write\\.csv2?", "write_tsv", "write_delim", "saveRDS",
  "write_rds", "write_xlsx", "write_sav", "write_dta", "write_feather",
  "write_parquet", "load", "save",
  sep = "|")

.data_code_refs <- function(path, max_bytes = 2e6) {
  if (is.na(path) || !nzchar(path) || !file.exists(path)) return(character(0))
  # file.size() can still return NA here even after file.exists() passed (a
  # broken symlink, a permissions/race edge case) -- treat that as unreadable
  # rather than letting `if (NA > max_bytes)` error out the whole grouping
  # pass. Newly reachable now that repo_check() also calls into this code path
  # with files that were never actually downloaded (file_location may point at
  # something that doesn't survive a size check even though it "exists").
  sz <- file.size(path)
  if (is.na(sz) || sz > max_bytes) return(character(0))   # not a script
  txt <- tryCatch(paste(readLines(path, warn = FALSE), collapse = "\n"),
                  error = function(e) NULL)
  if (is.null(txt) || !nzchar(txt)) return(character(0))
  # <fn>( ... "<path>"  -- the first quoted string of a read/write call. Allows
  # arguments before the path (write_csv(df, "out.csv")).
  pat <- paste0("(?:", .CODE_READ_FNS, ")\\s*\\([^)\"']*[\"']([^\"']+)[\"']")
  m <- regmatches(txt, gregexpr(pat, txt, perl = TRUE, ignore.case = TRUE))[[1]]
  if (!length(m)) return(character(0))
  refs <- sub(paste0("^.*?[\"']([^\"']+)[\"'].*$"), "\\1", m)
  refs <- refs[grepl("\\.[A-Za-z0-9]{1,6}$", refs)]   # must look like a file
  unique(tolower(basename(gsub("\\\\", "/", refs))))
}

# Deterministically derive a study group from a file path, or NA when the path
# names no study. Filenames and folder names very often carry the study label
# verbatim -- "Experiment 1/", "study2a_data.csv", even smashed together without
# separators ("...dataexperiment1creplication...") -- and a regex reads those
# more reliably than a small LLM, which has misread exactly such names. The
# filename is searched first, then the enclosing folders from innermost to
# outermost. The short prefixes ("ex", "exp") must not be preceded by a letter
# (so "index1"/"flex2" don't match), while the full words match even embedded
# in smashed-together names. A trailing letter counts as a sub-study suffix
# only when it ends its token ("study2a_data" -> ex2a), not when the next word
# merely starts with a letter ("experiment3explicit" -> ex3).
.data_group_from_path <- function(paths) {
  ex_pat <- paste0(
    "(?:experiment|study|(?<![a-z])expt?|(?<![a-z])ex)",
    "[ ._-]?([0-9]{1,2})([a-z](?![a-z]))?"
  )
  pilot_pat <- "(?<![a-z])pilot[ ._-]?([0-9]{1,2})?"
  vapply(paths, function(path) {
    if (is.na(path) || !nzchar(path)) return(NA_character_)
    parts <- rev(strsplit(tolower(path), "/", fixed = TRUE)[[1]])
    for (part in parts) {
      m <- regmatches(part, regexec(ex_pat, part, perl = TRUE))[[1]]
      if (length(m) > 0) return(paste0("ex", m[2], m[3]))
      m <- regmatches(part, regexec(pilot_pat, part, perl = TRUE))[[1]]
      if (length(m) > 0)
        return(paste0("pilot", if (nzchar(m[2])) m[2] else "1"))
    }
    NA_character_
  }, character(1), USE.NAMES = FALSE)
}

# Normalize an LLM-returned study-group code to the documented scheme and
# reject anything outside it (NA). The model occasionally answers in prose
# variants ("Experiment 1", "study 2a") or with a bare "pilot"; anything that
# still doesn't fit ex<N><letter?>/pilot<N> after normalization is a
# hallucination and must not leak into study directory names. "shared" is not
# a legal code: every file belongs to exactly one study (see
# .data_group_llm_impl()).
.data_group_normalize <- function(x) {
  x <- tolower(trimws(as.character(x)))
  x <- gsub("[ ._-]", "", x)
  x <- sub("^(experiment|study|expt|exp)(?=[0-9])", "ex", x, perl = TRUE)
  x[x == "pilot"] <- "pilot1"
  ifelse(grepl("^(ex|pilot)[0-9]{1,2}[a-z]?$", x), x, NA_character_)
}

# When a structured schema wraps its array in a single object field (needed
# because some providers 400 on a bare top-level array), ellmer returns the
# inner fields prefixed with "<wrapper>." (e.g. assignments.index). Strip that
# prefix so consumers can read the un-prefixed column names either way.
.strip_llm_wrapper <- function(df, wrapper) {
  if (is.null(df) || !is.data.frame(df)) return(df)
  pref <- paste0(wrapper, ".")
  hit <- startsWith(names(df), pref)
  if (any(hit)) names(df)[hit] <- sub(pref, "", names(df)[hit], fixed = TRUE)
  df
}

# Separate a paper's files into studies BY SOURCE REPOSITORY. A paper often links
# several independent repositories (multiple OSF components, a Zenodo archive, a
# GitHub repo); when those repos hold DIFFERENT files, each is a distinct study --
# a far more reliable signal than the file paths, which frequently name only a
# processing stage ("raw/", "processed/") and no study at all. Returns a per-file
# base study code (`ex1`, `ex2`, ... by first appearance of each qualifying repo),
# or all-NA when there is only one repository or the repos are mirrors of each
# other (near-identical file sets -- not separate studies).
#
# The "files differ" guard compares each pair of repos' basename sets by Jaccard
# overlap; repos that overlap >= `mirror_overlap` are treated as one study (the
# earlier slot), so a duplicated/mirrored component does not spawn a bogus study.
.data_group_from_repo <- function(repo, paths, mirror_overlap = 0.9) {
  n <- length(paths)
  if (n == 0) return(character(0))
  repo <- as.character(repo)
  repo[is.na(repo) | !nzchar(repo)] <- NA_character_
  distinct <- unique(repo[!is.na(repo)])
  if (length(distinct) < 2) return(rep(NA_character_, n))   # single (or no) repo

  base <- tolower(basename(gsub("\\\\", "/", paths)))
  files_of <- lapply(distinct, function(r) unique(base[!is.na(repo) & repo == r]))
  names(files_of) <- distinct

  # Assign each distinct repo a study slot, merging repos that mirror an earlier
  # one (near-identical file sets) into that earlier slot.
  slot_of <- stats::setNames(rep(NA_integer_, length(distinct)), distinct)
  next_slot <- 0L
  for (r in distinct) {
    merged_into <- NA_integer_
    for (prev in distinct) {
      if (identical(prev, r) || is.na(slot_of[prev])) next
      a <- files_of[[r]]; b <- files_of[[prev]]
      inter <- length(intersect(a, b)); uni <- length(union(a, b))
      if (uni > 0 && inter / uni >= mirror_overlap) { merged_into <- slot_of[prev]; break }
    }
    if (!is.na(merged_into)) slot_of[r] <- merged_into
    else { next_slot <- next_slot + 1L; slot_of[r] <- next_slot }
  }
  if (max(slot_of, na.rm = TRUE) < 2) return(rep(NA_character_, n))  # all one study

  out <- rep(NA_character_, n)
  have <- !is.na(repo)
  out[have] <- paste0("ex", slot_of[repo[have]])
  out
}

.data_group_llm_impl <- function(files, model = llm_model(), params = list(),
                                 batch_size = 30, paper = NULL) {
  if (is.null(files) || nrow(files) == 0) return(NULL)
  # Pin the sampler unless the caller chose otherwise. llm() already defaults to
  # temperature 0, but on a SERVED model that alone does not guarantee a
  # reproducible answer (request batching, KV-cache state and GPU floating-point
  # non-associativity all perturb the logits), and study groups decide the
  # dataset's directory structure -- a run-to-run flip silently reshapes the
  # output. Providers document `seed` as best-effort rather than a promise, so
  # this narrows the variance, it does not eliminate it; the deterministic passes
  # above are what actually make the common cases reproducible.
  if (is.null(params$seed)) params$seed <- .data_group_seed
  paths <- if ("file_path" %in% names(files)) files$file_path else files$file_name
  paths <- ifelse(is.na(paths) | !nzchar(paths), files$file_name, paths)
  paths <- gsub("\\\\", "/", paths)
  repo  <- if ("repo_url" %in% names(files)) files$repo_url else
           if ("repo_name" %in% names(files)) files$repo_name else
           rep(NA_character_, length(paths))

  # Only group files that will actually be analysed or placed into a study
  # directory: data, documentation, materials, code. When no data_type column
  # is present we fall back to grouping everything. Note: the collection-level
  # root readme / root ro-crate-metadata.json must already have been EXCLUDED
  # from `files` by the caller (data_check.R) before this function runs -- they
  # are never assigned a study and never reach this function at all.
  placeable <- c("data", "documentation", "materials", "code")
  dtype <- if ("data_type" %in% names(files))
    tolower(as.character(files$data_type)) else rep(NA_character_, length(paths))
  send <- if (all(is.na(dtype))) rep(TRUE, length(paths)) else dtype %in% placeable

  # Base group by SOURCE REPOSITORY: a paper that links several repos with
  # different files is multi-study, one study per repo (see .data_group_from_repo).
  # This seeds the default so unrecognised files fall to their repo's study.
  # NA (single repo / mirrors) keeps `group` unresolved (NA) for now -- every
  # remaining pass below tries to fill it, and the final fallback (no file is
  # ever left unresolved) guarantees a real study code by the time this
  # function returns.
  repo_grp <- .data_group_from_repo(repo, paths)
  multi_repo <- any(!is.na(repo_grp))
  group <- repo_grp

  # Deterministic pre-pass: a path that names its study outright ("Experiment
  # 1/", "study2a_data.csv", "...experiment1creplication...") overrides the repo
  # base -- an explicit study name in the path is more specific than "which repo".
  # The regex is exact where a small LLM has misread such names. Files still
  # ambiguous AFTER both repo and path passes go to the LLM.
  pre <- .data_group_from_path(paths)
  fixed <- send & !is.na(pre)
  group[fixed] <- pre[fixed]

  # CODE-REFERENCE pass: a script names the data it reads and writes, so every
  # file it references belongs to the script's study. This is hard evidence (no
  # guessing) and rescues data files whose own path names no study -- the common
  # case, where paths describe a processing stage ("raw/", "processed/") rather
  # than a study. Only fills files still unplaced by the repo/path passes, and
  # only from scripts that ARE placed, so it propagates a known group outward
  # rather than inventing one.
  #
  # This is also the ONLY signal metacheck has for cross-study reuse: when a
  # script belonging to a DIFFERENT study than the file's current owner also
  # references it, that other study is recorded in `referenced_by` (a list
  # column, one entry per file) instead of overwriting `group` -- the file keeps
  # its single owning study, and the other study gets a reference written into
  # its own metadata later (see .psychds_dataset_description() /
  # .psychds_rocrate_json() in psychds-convert.R). Reuse that is never named in
  # any script's code (e.g. two studies described in prose as using "the same
  # stimuli") is NOT detected -- this is a known, accepted limitation, not a bug.
  loc <- if ("file_location" %in% names(files)) files$file_location else
    rep(NA_character_, length(paths))
  is_code <- dtype %in% c("code", "materials")
  placed  <- !is.na(group)
  script_i <- which(is_code & placed & !is.na(loc))
  referenced_by <- vector("list", length(paths))
  if (length(script_i)) {
    base_of <- tolower(basename(paths))
    for (si in script_i) {
      refs <- .data_code_refs(loc[si])
      if (!length(refs)) next
      hit <- base_of %in% refs
      if (!any(hit)) next
      # Files this script references that are still UNPLACED -> the script's
      # group (propagating known structure outward, as before).
      newly_placed <- hit & is.na(group)
      if (any(newly_placed)) group[newly_placed] <- group[si]
      # Files this script references that ALREADY belong to a DIFFERENT study
      # -> cross-study reuse. Record the script's group as an additional
      # referencing study, without changing the file's own group.
      reused <- hit & !is.na(group) & group != group[si]
      for (ri in which(reused))
        referenced_by[[ri]] <- union(referenced_by[[ri]], group[si])
    }
  }

  # When the repository already separates studies, trust it: only send files the
  # repo pass could NOT place (single-repo case) to the LLM. This avoids the LLM
  # re-scattering repo-separated files to a generic slot.
  send <- send & is.na(pre) & (!multi_repo | is.na(repo_grp)) & is.na(group)

  # The LLM is the LAST resort: it only sees files the deterministic passes
  # (repository, path regex, code references) could not place. When they placed
  # everything -- the common case for a multi-repo paper -- no call is made at all.
  # NB: this must not return early; the roster relabelling and the "every file
  # gets a real study" guard below still have to run.
  prompt <- paste(
    "You are grouping the files of a psychology research repository by study.",
    "Many repositories contain multiple studies (Experiment 1, Study 2a, a",
    "pilot, ...). Assign each numbered file to a study group using these codes:",
    "'ex1','ex2','ex2a',... for experiments/studies, 'pilot1','pilot2',... for",
    "pilots. Infer groups from folder names and filenames. EVERY file belongs",
    "to exactly one study -- there is no 'shared' option. If the whole",
    "repository is a single study, put every file in 'ex1'. If a file (e.g. a",
    "shared codebook or a materials file) genuinely serves multiple studies,",
    "assign it to whichever single study it is most closely associated with by",
    "folder or filename, or to the first study by number if there is no such",
    "association.",
    "Return one entry per input file, in the same order."
  )
  # Wrap the array in a single-field object. Some providers (notably Groq's
  # gpt-oss-20b) reject a top-level bare JSON array schema with HTTP 400
  # json_validate_failed; nesting it under an object field is accepted, and
  # llm()'s .unnest_result() unwraps the single-field object back into rows.
  type_spec <- ellmer::type_object(
    assignments = ellmer::type_array(
      ellmer::type_object(
        index = ellmer::type_integer("The file's number in the list"),
        group = ellmer::type_string("Study group code: ex1/ex2a/pilot1")
      )
    )
  )

  # Batch the files to keep each request (and its structured array response)
  # within the model's limits. Each batch is numbered 1..n within itself so the
  # model returns small indices; we map them back via the batch's global rows.
  send_rows <- which(send)
  batches <- split(send_rows, ceiling(seq_along(send_rows) / batch_size))

  any_ok <- FALSE
  used_model <- NA_character_
  unresolved <- integer(0)   # rows no batch (or retry) ever answered for

  # Ask the model about one batch of rows; returns the rows it could NOT place.
  # A batch can fail outright (network error, HTTP 400 json_validate_failed --
  # providers reject a structured response they cannot validate, which happens
  # more often on LONG arrays) or come back partial. Either way the rows left
  # over are reported back so the caller can retry them in smaller pieces rather
  # than silently leaving them at their default -- the old behaviour, which made
  # an intermittent provider error look exactly like "the model said 'shared'"
  # and was the main source of run-to-run instability.
  ask_batch <- function(rows) {
    listing <- paste(seq_along(rows), paths[rows], sep = ". ", collapse = "\n")
    resp <- tryCatch(
      llm(text = data.frame(text = listing), text_col = "text",
          system_prompt = prompt, type = type_spec, model = model,
          params = params, phase = "Assigning study groups"),
      error = function(e) NULL
    )
    resp <- .strip_llm_wrapper(resp, "assignments")
    if (is.null(resp) || nrow(resp) == 0 ||
        !all(c("index", "group") %in% names(resp))) return(rows)
    idx <- suppressWarnings(as.integer(resp$index))
    # Normalize the model's codes to the documented scheme and drop anything
    # that still doesn't fit (a hallucinated code like "pilot" for a file whose
    # name says "experiment3" must not become a study directory name).
    grp <- .data_group_normalize(resp$group)
    ok  <- !is.na(idx) & idx >= 1 & idx <= length(rows) & !is.na(grp)
    if (any(ok)) {
      group[rows[idx[ok]]] <<- grp[ok]
      any_ok <<- TRUE
    }
    if (is.na(used_model))
      used_model <<- attr(resp, "llm")$model %||% NA_character_
    rows[setdiff(seq_along(rows), idx[ok])]   # rows still unanswered
  }

  for (rows in batches) {
    left <- ask_batch(rows)
    # ONE retry, at half the batch size. A provider that rejects a batch usually
    # does so because the structured response was too long, and halving fixes
    # that; if the halves still fail the request itself is the problem and
    # splitting further will not help. Deliberately bounded: retrying down to
    # single files would cost ~2n calls per failed batch (~100 for a 50-file
    # batch) to place files the fallback rules below place for free.
    if (length(left) > 1L) {
      chunks <- split(left, ceiling(seq_along(left) / max(1L, length(left) %/% 2L)))
      left <- unlist(lapply(chunks, ask_batch), use.names = FALSE)
      if (is.null(left)) left <- integer(0)
    }
    if (length(left) > 0) unresolved <- c(unresolved, left)
  }
  # Unlike the old scheme, there is no "give up and return NULL" case here: a
  # NULL return would leave the caller's `group` at its NA default for every
  # file, which is no longer a valid outcome -- every placeable file MUST
  # resolve to a real study group (there is no 'shared' escape hatch), even
  # when every deterministic pass and every LLM batch failed. The fallback
  # immediately below guarantees this unconditionally, so this function always
  # returns a real data.frame from this point on, never NULL.

  # EVERY placeable file must resolve to a real study group -- there is no
  # 'shared' fallback. Generalizes the old "data is never shared" guarantee
  # (previously data-only) to documentation, materials, and code as well: a
  # file that reaches this point still unresolved (nothing placed it, and no
  # LLM answer stuck) falls back, in order, to (1) its own repo's study when
  # the repo pass placed it, (2) the sole study when exactly one exists, (3)
  # 'ex1' when no study exists at all (a single-study repo), (4) the
  # lexicographically-first study code when several studies exist and nothing
  # else resolved it -- e.g. a repo-root materials/ folder no script happens to
  # reference. This runs BEFORE the roster relabelling below so it works with
  # the raw slot labels.
  is_placeable <- dtype %in% placeable
  study_codes <- unique(group[grepl("^(ex|pilot)[0-9]", group)])
  # No real evidence anywhere in this file set: no repo split, no path/code
  # reference, no LLM answer ever produced a study code before reaching this
  # fallback. Distinct from the "sole study" / "several studies" branches
  # below, where AT LEAST ONE file had real evidence -- just not this
  # particular one. Exposed as an attribute so callers (psychds_check) can
  # tell "grouped with real evidence" from "grouped by blanket default" rather
  # than treating every non-NA group the same way.
  no_evidence_at_all <- length(study_codes) == 0L
  stray <- is_placeable & is.na(group)
  if (any(stray)) {
    group[stray & !is.na(repo_grp)] <- repo_grp[stray & !is.na(repo_grp)]
    still <- is_placeable & is.na(group)
    if (any(still) && length(study_codes) == 1L) group[still] <- study_codes[[1]]
    still <- is_placeable & is.na(group)
    if (any(still) && length(study_codes) == 0L) group[still] <- "ex1"
    still <- is_placeable & is.na(group)
    if (any(still) && length(study_codes) > 1L)
      group[still] <- .data_group_sort(study_codes)[[1]]
  }

  # RELABEL from the manuscript's study roster. The authors say what their
  # studies are called ("Experiment 1, 2a, 2b, 3"); our partition may be
  # structurally right but named by slot (ex1..ex4 from four repositories). When
  # the partition has exactly as many groups as the paper names studies, adopt
  # the authors' labels -- the paper is authoritative for naming.
  #
  # Groups already carrying a roster label (a path that literally said
  # "Experiment 2a") are left alone and their label is taken out of the pool, so
  # only the slot-named groups are renamed. Mapping is by first appearance, which
  # matches how both lists are ordered (repo order / study order) but is a
  # heuristic: when the counts differ we do NOT rename at all, and the roster
  # check below reports the disagreement instead of guessing.
  roster <- if (!is.null(paper)) data_study_roster(paper) else character(0)
  if (length(roster)) {
    found <- unique(group[!is.na(group)])
    already <- intersect(found, roster)              # correctly named already
    to_name <- setdiff(found, roster)                # slot-named (ex1, ex2, ...)
    avail   <- setdiff(roster, already)
    if (length(to_name) > 0 && length(to_name) == length(avail)) {
      # Order both by first appearance so the mapping is stable.
      ord <- to_name[order(match(to_name, group[!is.na(group)]))]
      map <- stats::setNames(avail, ord)
      hit <- !is.na(group) & group %in% ord
      group[hit] <- unname(map[group[hit]])
      # The roster relabelling can rename the very group a reused file's
      # referencing studies point at, and code-referenced study codes in
      # `referenced_by` (built above, from the pre-relabel slot names) must be
      # relabelled the same way so they still match `group`'s final values.
      referenced_by <- lapply(referenced_by, function(r) {
        if (is.null(r)) return(r)
        hit_r <- r %in% ord
        r[hit_r] <- unname(map[r[hit_r]])
        r
      })
    }
  }
  out <- data.frame(group = group)
  out$referenced_by <- referenced_by
  attr(out, "model") <- used_model
  attr(out, "roster") <- roster
  attr(out, "roster_check") <- .data_group_check_roster(group, roster)
  # TRUE when no file's path, repository split, code reference, or LLM answer
  # ever named a real study anywhere in this file set -- every group came from
  # the blanket "ex1" default, not actual evidence. psychds_check uses this to
  # warn that the grouping is a guess rather than implying real structure was
  # detected.
  attr(out, "no_evidence") <- no_evidence_at_all
  # Files the model never answered for, even after retries. They keep whatever
  # default they had (the fallback above still guarantees a real study group),
  # but the caller is told so an intermittent provider failure is visible
  # instead of masquerading as a confident verdict.
  attr(out, "unresolved") <- if (length(unresolved))
    paths[sort(unique(unresolved))] else character(0)
  out
}

# -- Tabular reading ----------------------------------------------------------

# Sniff the field delimiter of a delimited text file from its first
# non-blank, non-comment line.
# Reinterpret invalid-UTF-8 bytes in freshly read lines as Latin-1 (a
# conversion that cannot fail, since every byte is a valid Latin-1 character).
# The pre-read sniffers below run string ops (trimws, strsplit, gsub) on raw
# readLines() output, and any of those errors with "input string 1 is invalid
# UTF-8" when a Latin-1-encoded file has a non-ASCII byte in its first lines --
# which used to make the whole file unreadable before the readers' own
# encoding tolerance ever got a chance.
.utf8_lines <- function(x) {
  if (length(x) == 0) return(x)
  bad <- !validUTF8(x)
  if (any(bad)) x[bad] <- iconv(x[bad], from = "latin1", to = "UTF-8")
  x
}

#' Peek at the first lines of a text file
#'
#' Reads the first `n` lines of a file as text, tolerating the encodings research
#' data actually ships in: a UTF-8/UTF-16 BOM, UTF-16 (E-Prime exports), and
#' Latin-1 bytes that would otherwise make `readLines()` output error in later
#' string operations. Intended for cheap format sniffing -- deciding *what* a file
#' is before committing to a reader -- not for reading data.
#'
#' Returns `character(0)` for a missing, empty or unreadable file rather than
#' erroring, so a caller can treat "cannot peek" as "not my format".
#'
#' @param path path to a file.
#' @param n maximum number of lines to read.
#'
#' @returns a character vector of at most `n` lines (UTF-8), or `character(0)`.
#' @export
#' @keywords internal
#'
#' @examples
#' f <- tempfile(fileext = ".txt")
#' writeLines(c("*** Header Start ***", "Experiment: naming"), f)
#' text_peek(f, n = 2)
text_peek <- function(path, n = 20L) {
  if (length(path) != 1L || is.na(path) || !file.exists(path)) return(character(0))
  if (isTRUE(file.info(path)$isdir)) return(character(0))
  size <- file.info(path)$size
  if (is.na(size) || size == 0) return(character(0))

  # Read a bounded chunk rather than the whole file when only the first n lines
  # are wanted (the sniffing case). `n = Inf` reads it all -- callers that need
  # every line (e.g. parsing a whole E-Prime export) must not be silently
  # truncated. 64 KB covers any plausible 20-line header, doubled for UTF-16.
  want <- if (is.finite(n)) min(size, 65536) else size
  raw <- tryCatch(readBin(path, "raw", n = want), error = function(e) raw(0))
  if (!length(raw)) return(character(0))

  # UTF-16 (E-Prime writes it, with a BOM): every other byte of ASCII text is a
  # NUL, which is the reliable tell. iconv converts; a failed conversion returns
  # NA and falls through to the 8-bit path.
  has_nul <- any(raw[seq_len(min(length(raw), 1000L))] == as.raw(0))
  txt <- NA_character_
  if (has_nul) {
    for (enc in c("UTF-16", "UTF-16LE", "UTF-16BE")) {
      t <- tryCatch(iconv(list(raw), from = enc, to = "UTF-8"),
                    error = function(e) NA_character_)
      if (length(t) == 1L && !is.na(t) && nzchar(t)) { txt <- t; break }
    }
  }
  if (is.na(txt)) {
    t <- tryCatch(rawToChar(raw), error = function(e) NA_character_)
    if (!is.na(t)) txt <- t
  }
  if (is.na(txt)) return(character(0))

  txt <- sub("^\ufeff", "", txt)                    # strip a BOM
  lines <- strsplit(txt, "\r\n|\n|\r")[[1]]
  utils::head(.utf8_lines(lines), n)
}

#' Classify a downloaded .txt file from its content
#'
#' A `.txt` extension says nothing about what a file holds: research repositories
#' ship experiment data (E-Prime exports, Ibex/task logs), codebooks, and plain
#' prose notes all as `.txt`. The name-based classifier
#' ([data_classify_files()]) runs on remote file names *before* download, so it
#' cannot tell these apart and settles on a conservative guess. Once the file is
#' on disk, its content can. This is the same fetch-then-reclassify pattern the
#' module already uses for zips.
#'
#' Returns the data_check type the content implies:
#' - `"data"` for an E-Prime export (its fixed `*** Header Start ***` /
#'   `LevelName:` header), or for a delimited table with a real header row;
#' - `NA_character_` when the content is not recognised, meaning "keep whatever
#'   the name-based classifier decided" -- never a downgrade on a guess.
#'
#' The file is only read, never modified or removed: the download cache is
#' persistent by design (see [repo_cache_dir()]), so classification decides how a
#' file is *used*, not whether it survives.
#'
#' @param path path to a local `.txt` file.
#'
#' @returns a single data_check type, or `NA_character_` when undecided.
#' @export
#' @keywords internal
txt_classify_content <- function(path) {
  head_lines <- text_peek(path, n = 30L)
  if (!length(head_lines)) return(NA_character_)

  # E-Prime export: a fixed header block that no prose file carries.
  if (any(grepl("^\\*\\*\\*\\s*Header Start", head_lines)) ||
      (any(grepl("^(Experiment|Subject):", head_lines)) &&
         any(grepl("^LevelName:", head_lines))))
    return("data")

  # A delimited table: a consistent delimiter across the first rows AND a header
  # row. Requires >= 2 columns and >= 2 rows, so a prose file with the odd comma
  # (or a one-line note) is not mistaken for data.
  sep <- tryCatch(.sniff_delimiter(path), error = function(e) NA_character_)
  if (!is.na(sep)) {
    rows <- head_lines[nzchar(trimws(head_lines))]
    rows <- rows[!startsWith(trimws(rows), "#")]
    if (length(rows) >= 2) {
      counts <- vapply(utils::head(rows, 5), function(l)
        length(strsplit(l, sep, fixed = TRUE)[[1]]), integer(1))
      consistent <- length(unique(counts)) == 1L && counts[[1]] >= 2L
      if (consistent && isTRUE(tryCatch(.detect_header(path, sep),
                                        error = function(e) FALSE)))
        return("data")
    }
  }
  # Space/whitespace-delimited data is deliberately NOT detected. It is rare in the
  # corpus (essentially one study's bespoke logs) and irregular (a metadata line,
  # then stimulus-filename rows, then numeric rows, with shifting column counts),
  # so a space-delimiter rule adds false-positive risk for little coverage. Such
  # files stay unclassified (NA) rather than being forced to "data".
  NA_character_
}

.sniff_delimiter <- function(path) {
  line <- character(0)
  con  <- file(path, "r")
  on.exit(close(con))
  for (i in seq_len(10)) {
    line <- .utf8_lines(readLines(con, n = 1, warn = FALSE))
    if (length(line) == 0) break
    l <- trimws(line)
    if (nchar(l) > 0 && !startsWith(l, "#")) break
  }
  if (length(line) == 0) return(",")
  candidates <- c(",", ";", "\t", "|")
  counts <- vapply(candidates, function(d)
    nchar(line) - nchar(gsub(d, "", line, fixed = TRUE)), integer(1))
  if (max(counts) == 0) "," else candidates[which.max(counts)]
}

# Decide whether a delimited text file has a header row. A file is treated as
# headerless when its first two non-comment rows both look all-numeric (real
# headers carry at least one textual label). With <2 readable rows we assume a
# header (safer default).
.detect_header <- function(path, sep) {
  con   <- file(path, "r")
  on.exit(close(con))
  lines <- character(0)
  while (length(lines) < 2) {
    l <- .utf8_lines(readLines(con, n = 1, warn = FALSE))
    if (length(l) == 0) break
    if (nzchar(trimws(l)) && !startsWith(trimws(l), "#")) lines <- c(lines, l)
  }
  if (length(lines) < 2) return(TRUE)
  split_row <- function(l)
    trimws(gsub('^"|"$', '', strsplit(l, sep, fixed = TRUE)[[1]]))
  is_num <- function(x) {
    if (!nzchar(x)) return(TRUE)
    if (toupper(x) %in% c("NA", "NAN", "NULL", "INF", "-INF", "+INF")) return(TRUE)
    suppressWarnings(!is.na(as.numeric(x)))
  }
  all_num <- function(toks) length(toks) > 0 && all(vapply(toks, is_num, logical(1)))
  !(all_num(split_row(lines[1])) && all_num(split_row(lines[2])))
}

# Cheaply detect a "single big field" file -- a .csv/.txt/.dat whose content is
# really one large value stuffed into a single column, not a table. This covers
# any such file, whatever the value is (a JSON blob, an XML document, a base64
# string, a serialised log, ...): the giveaway is format-independent -- the data
# is a *single column* whose rows are *huge*. A real one-column dataset has short
# rows (one value each); a blob-in-a-cell has an enormous row. Such files are
# pathologically slow to parse with read.delim and carry no tabular data, so
# data_read_head() skips them. Reads only the first two lines, so the check is
# effectively free versus the multi-second (sometimes minute-long) read.
#
# We count fields quote-aware (a delimiter inside a quoted value does not split a
# field), so a fully-quoted blob containing thousands of commas is still one
# column. The row-size threshold keeps genuinely narrow one-column CSVs safe.
.blob_row_min_bytes <- 4096L

.count_fields <- function(line, sep) {
  # Number of top-level fields: split on `sep` only when outside double quotes.
  chars <- strsplit(line, "", fixed = TRUE)[[1]]
  if (length(chars) == 0) return(0L)
  in_quote <- FALSE
  fields <- 1L
  for (ch in chars) {
    if (ch == "\"") in_quote <- !in_quote
    else if (!in_quote && ch == sep) fields <- fields + 1L
  }
  fields
}

.is_single_field_blob <- function(path, sep) {
  con <- file(path, "r")
  on.exit(close(con))
  first2 <- tryCatch(.utf8_lines(readLines(con, n = 2, warn = FALSE)),
                     error = function(e) character(0))
  if (length(first2) < 2) return(FALSE)
  header <- first2[[1]]
  row1   <- first2[[2]]
  # A single-column header AND an oversized first data row = one big field, not
  # a table. Field counts are quote-aware so a quoted value's inner delimiters
  # don't inflate the count.
  .count_fields(header, sep) <= 1L &&
    .count_fields(row1, sep) <= 1L &&
    nchar(row1, type = "bytes") >= .blob_row_min_bytes
}

# Read a delimited file into a data.frame. Uses data.table::fread when available
# -- orders of magnitude faster than utils::read.delim on files with large or
# awkward quoted fields (e.g. cells holding multi-line numpy-array dumps), which
# make base R's quote-scanning pathologically slow (minutes per file). Falls back
# to read.delim (with a latin1 retry for invalid UTF-8) when data.table is not
# installed. `n_rows = Inf` reads the whole file.
.read_delim_fast <- function(path, sep, header, n_rows = Inf) {
  nmax <- if (is.finite(n_rows)) n_rows else Inf
  if (requireNamespace("data.table", quietly = TRUE)) {
    # fread self-corrects quoting/field-count quirks but warns while doing so
    # (as read.delim does); suppress those, matching the read.delim path.
    df <- tryCatch(
      suppressWarnings(as.data.frame(
        data.table::fread(
          path, sep = sep, header = header,
          nrows = if (is.finite(nmax)) nmax else -1L,
          showProgress = FALSE, data.table = FALSE,
          check.names = FALSE, encoding = "UTF-8"),
        check.names = FALSE)),
      error = function(e) NULL)
    if (!is.null(df)) return(df)
    # fall through to read.delim on any fread failure
  }
  df <- suppressWarnings(
    utils::read.delim(path, sep = sep, header = header, nrows = n_rows,
                      check.names = FALSE))
  has_invalid <- any(vapply(df, function(col) {
    is.character(col) && any(is.na(iconv(col, from = "UTF-8", to = "UTF-8")))
  }, logical(1)))
  if (has_invalid) {
    df <- suppressWarnings(
      utils::read.delim(path, sep = sep, header = header, nrows = n_rows,
                        check.names = FALSE, fileEncoding = "latin1"))
  }
  df
}

# Coerce a just-read data frame to valid UTF-8, names first, then values.
# A stray non-UTF-8 byte in a header (e.g. a Latin-1 or BOM byte the file's
# own read tolerated) otherwise crashes downstream `grepl(..., perl = TRUE)`
# name checks with "invalid multibyte string"; sub out invalid bytes rather
# than dropping the column. For character VALUES: fread reads with
# encoding = "UTF-8", which marks strings as UTF-8 without validating, so a
# Latin-1 byte in a nominally-UTF-8 file (a mis-encoded apostrophe,  degrees, u,
# e ...) yields strings that crash the base regex calls data_check runs on
# every column ("input string N is invalid UTF-8"). Reinterpret only the
# invalid entries as Latin-1 -- a conversion that cannot fail, since every
# byte is a valid Latin-1 character -- and leave valid values untouched.
# The per-column repair counts are recorded in the "utf8_repaired" attribute
# so data_check can carry them into its columns table and data_validate can
# warn the researcher about the file's mixed encoding (the repaired values
# themselves no longer show it). Idempotent: a second pass finds nothing
# invalid and leaves both the data and the attribute untouched.
.utf8_repair_df <- function(df) {
  if (is.null(df)) return(df)

  # Flatten columns that are not vectors, BEFORE anything else looks at them.
  #
  # A data frame's column is normally a vector, but `jsonlite` turns a nested
  # JSON response into a data frame whose columns are THEMSELVES data frames or
  # matrices (an OSF/API result saved to .RData or .rds is the common case).
  # Every per-column operation downstream assumes a vector: is.na() on such a
  # column returns a MATRIX, so subsetting flattens to one element per cell and
  # counts come back one-per-cell instead of one-per-column. That breaks
  # data_col_stats(), data_col_type(), data_col_facets() and the sample-value
  # summary alike -- four separate failures with one cause, which is why this is
  # fixed here at the single point every reader passes through rather than
  # guarded in each consumer.
  #
  # The column is rendered as text, one string per row, so it is still listed,
  # still classified, and still visible in the sample values -- rather than
  # being dropped, which would hide data the researcher did share.
  if (is.data.frame(df) && ncol(df) > 0) {
    for (j in seq_along(df)) {
      x <- df[[j]]
      if (is.null(dim(x)) && (is.atomic(x) || is.null(x))) next
      flat <- tryCatch({
        if (is.data.frame(x)) {
          # Row-wise, NOT apply(): apply() coerces to a matrix first, which
          # fails outright when the sub-columns are themselves data frames
          # (nesting more than one level deep, as an OSF API response is).
          # Each sub-column is flattened to text on its own, then pasted.
          parts <- lapply(names(x), function(k) {
            v <- x[[k]]
            v <- if (is.data.frame(v) || is.list(v))
              vapply(seq_len(NROW(v)), function(i)
                paste(utils::head(unlist(if (is.data.frame(v)) v[i, ] else v[[i]]), 10),
                      collapse = ","), character(1))
            else as.character(v)
            paste0(k, "=", v)
          })
          do.call(paste, c(parts, sep = "; "))
        } else if (is.matrix(x)) {
          apply(x, 1, function(r) paste(as.character(r), collapse = "; "))
        } else if (is.list(x)) {
          vapply(x, function(e)
            paste(utils::head(unlist(e), 20), collapse = "; "), character(1))
        } else {
          as.character(x)
        }
      }, error = function(e) rep(NA_character_, NROW(x)))
      # Only replace when the flattened form still has one value per row; a
      # mismatch would silently misalign the column against the rest of the
      # table, which is worse than leaving it out.
      df[[j]] <- if (length(flat) == NROW(df)) as.character(flat)
                 else rep(NA_character_, NROW(df))
    }
  }

  if (!is.null(names(df))) {
    nm <- names(df)
    bad <- is.na(iconv(nm, from = "UTF-8", to = "UTF-8"))
    if (any(bad)) {
      fixed <- iconv(nm[bad], from = "latin1", to = "UTF-8", sub = "")
      fixed[is.na(fixed) | !nzchar(fixed)] <- paste0("col_", which(bad))[is.na(fixed) | !nzchar(fixed)]
      nm[bad] <- fixed
      names(df) <- nm
    }
  }
  if (ncol(df) > 0) {
    repaired <- integer(0)
    for (j in seq_along(df)) {
      x <- df[[j]]
      if (is.character(x)) {
        bad <- !is.na(x) & !validUTF8(x)
        if (any(bad)) {
          x[bad] <- iconv(x[bad], from = "latin1", to = "UTF-8")
          df[[j]] <- x
          repaired[names(df)[j]] <- sum(bad)
        }
      } else if (is.factor(x)) {
        lv <- levels(x)
        bad <- !is.na(lv) & !validUTF8(lv)
        if (any(bad)) {
          lv[bad] <- iconv(lv[bad], from = "latin1", to = "UTF-8")
          levels(df[[j]]) <- lv
          repaired[names(df)[j]] <- sum(bad)
        }
      }
    }
    if (length(repaired) > 0) attr(df, "utf8_repaired") <- repaired
  }
  df
}

#' Read the head of a data file regardless of format
#'
#' Reads the first `n_rows` of a tabular data file (csv/tsv/txt/dat/xlsx/xls/
#' ods/fods/sav/dta/sas7bdat/rds/rda/rdata). Delimiter and header presence are
#' auto-detected for delimited text; invalid UTF-8 triggers a latin1 retry.
#' Reading `.ods`/`.fods` needs the suggested `readODS` package; without it the
#' function returns `NULL` for those formats.
#'
#' @param path path to a data file
#' @param n_rows number of rows to read (`Inf` for all)
#'
#' @returns a data.frame, or `NULL` on failure / unsupported format.
#' @export
#' @keywords internal
#
# The set of extensions handled by the switch below IS the package's definition
# of "tabular", exported through .readable_extensions / data_format(). Any new
# branch added here must be added there too, or the format will never be
# downloaded and this reader will never be reached.
data_read_head <- function(path, n_rows = 5) {
  ext <- tolower(tools::file_ext(path))
  tryCatch({
    df <- switch(ext,
      csv = , txt = , tsv = , dat = {
        sep <- if (ext == "tsv") "\t" else .sniff_delimiter(path)
        hdr <- .detect_header(path, sep)
        # Cheap bail-out for a non-tabular file disguised as .csv: one big field
        # (e.g. a JSON blob dumped under a single header). It yields a useless
        # 1-column "table" and there is nothing to extract. Detect it from the
        # first two lines only. See .is_single_field_blob().
        if (.is_single_field_blob(path, sep)) return(NULL)
        df <- .read_delim_fast(path, sep = sep, header = hdr, n_rows = n_rows)
        if (!hdr && !is.null(df) && ncol(df) > 0)
          names(df) <- paste0("col_", seq_len(ncol(df)))
        # Repair invalid UTF-8 before the Qualtrics detection below, whose
        # regex calls on names and head values would otherwise error/warn on a
        # Latin-1 byte in the first rows. (The repair after the switch is then
        # a no-op for this branch.)
        df <- .utf8_repair_df(df)
        # Qualtrics "use choice text" exports carry extra header rows (question
        # text, ImportId JSON) as the first data rows, which force every column
        # to character. Strip them and re-type so the rest of data_check works.
        if (!is.null(df) && data_check_is_qualtrics(df))
          df <- data_strip_qualtrics_header(df)
        # Promote a mis-placed header (banner / blank / units / repeated-label row
        # read as the header). The detector needs to SEE the row the reader took
        # as the header, so re-read the first few rows WITHOUT a header as the scan
        # window (raw_rows). Cheap: a handful of rows only.
        if (!is.null(df) && nrow(df) >= 1) {
          raw <- tryCatch(.utf8_repair_df(.read_delim_fast(
                   path, sep = sep, header = FALSE, n_rows = 6L)),
                 error = function(e) NULL)
          if (!is.null(raw) && nrow(raw) >= 2) {
            raw_rows <- lapply(seq_len(nrow(raw)),
                               function(i) as.character(raw[i, , drop = TRUE]))
            df <- data_promote_header_row(df, raw_rows = raw_rows)$df
          }
        }
        df
      },
      xlsx = , xls = {
        if (!requireNamespace("readxl", quietly = TRUE))
          stop("The 'readxl' package is required to read Excel files.")
        nmax <- if (is.finite(n_rows)) n_rows else Inf
        # Suppress readxl's per-cell type-guess warnings ("Expecting numeric ...
        # got a date"): a mixed column can emit one per row (hundreds on a wide
        # sheet). We re-classify column types ourselves via data_col_type(), so
        # readxl's guess is not relied upon.
        # .name_repair = "unique_quiet": readxl still renames blank/duplicate
        # headers (...1, K...3, ...) -- we handle names ourselves -- but without printing
        # the "New names:" message on every such sheet.
        df <- suppressWarnings(as.data.frame(
          readxl::read_excel(path, n_max = nmax, .name_repair = "unique_quiet")))
        if (!is.null(df) && data_check_is_qualtrics(df))
          df <- data_strip_qualtrics_header(df)
        # A mis-placed header (banner / blank / units / repeated-label row above the
        # real header). Read the first few rows WITHOUT a header (col_names = FALSE)
        # as the scan window so the detector sees the row readxl swallowed as the
        # header, then RE-READ the sheet with skip = k so readxl re-infers column
        # types natively rather than coercing character back to numeric. Falls back
        # to in-memory promotion if the re-read fails.
        if (!is.null(df) && ncol(df) > 1) {
          raw <- tryCatch(.utf8_repair_df(as.data.frame(suppressWarnings(
                   readxl::read_excel(path, col_names = FALSE, n_max = 6L,
                     col_types = "text", .name_repair = "minimal")))),
                 error = function(e) NULL)
          if (!is.null(raw) && nrow(raw) >= 2) {
            raw_rows <- lapply(seq_len(nrow(raw)),
                               function(i) as.character(raw[i, , drop = TRUE]))
            prom <- data_promote_header_row(df, raw_rows = raw_rows)
            if (prom$promoted > 0L) {
              k <- prom$promoted
              nmax2 <- if (is.finite(nmax)) nmax + k else Inf
              reread <- tryCatch(suppressWarnings(as.data.frame(
                readxl::read_excel(path, skip = k, n_max = nmax2,
                                   .name_repair = "unique_quiet"))),
                error = function(e) NULL)
              df <- if (!is.null(reread) && ncol(reread) > 0) reread else prom$df
            }
          }
        }
        df
      },
      ods = , fods = {
        # OpenDocument spreadsheet -- LibreOffice/OpenOffice's native format, and
        # the default for anyone not using Excel. Structurally the same as .xlsx
        # (sheets of rows with a header row), so this mirrors the xlsx branch
        # above step for step; only the reader differs.
        #
        # readODS is in Suggests (not Imports): a missing package returns NULL
        # rather than stopping, so the file is reported as unreadable and the
        # rest of the run continues. (The xlsx branch stop()s because readxl is
        # reachable through the same optional-dependency policy but is far more
        # commonly installed; returning NULL here keeps .ods degradation soft.)
        if (!requireNamespace("readODS", quietly = TRUE)) return(NULL)
        nmax <- if (is.finite(n_rows)) n_rows else Inf
        # A fixed `range=` is deliberately NOT used in this branch: it silently
        # truncates wide sheets to the named column bound (a 30-column sheet read
        # with range "A1:Z6" comes back with 26). `n_max` caps rows only, which
        # is what we want.
        df <- suppressWarnings(as.data.frame(
          readODS::read_ods(path, n_max = nmax, .name_repair = "unique_quiet")))
        if (!is.null(df) && data_check_is_qualtrics(df))
          df <- data_strip_qualtrics_header(df)
        # Mis-placed header, exactly as for .xlsx: scan the first rows WITHOUT a
        # header so the detector sees the row the reader swallowed, then re-read
        # with skip = k so column types are re-inferred natively.
        if (!is.null(df) && ncol(df) > 1) {
          raw <- tryCatch(.utf8_repair_df(as.data.frame(suppressWarnings(
                   readODS::read_ods(path, col_names = FALSE, n_max = 6L,
                     col_types = NA, .name_repair = "minimal")))),
                 error = function(e) NULL)
          if (!is.null(raw) && nrow(raw) >= 2) {
            raw_rows <- lapply(seq_len(nrow(raw)),
                               function(i) as.character(raw[i, , drop = TRUE]))
            prom <- data_promote_header_row(df, raw_rows = raw_rows)
            if (prom$promoted > 0L) {
              k <- prom$promoted
              nmax2 <- if (is.finite(nmax)) nmax + k else Inf
              reread <- tryCatch(suppressWarnings(as.data.frame(
                readODS::read_ods(path, skip = k, n_max = nmax2,
                                  .name_repair = "unique_quiet"))),
                error = function(e) NULL)
              df <- if (!is.null(reread) && ncol(reread) > 0) reread else prom$df
            }
          }
        }
        df
      },
      sav = , dta = , sas7bdat = , por = {
        if (!requireNamespace("haven", quietly = TRUE))
          stop("The 'haven' package is required to read SPSS/Stata/SAS files.")
        nmax <- if (is.finite(n_rows)) n_rows else Inf
        as.data.frame(switch(ext,
          sav      = haven::read_sav(path, n_max = nmax),
          dta      = haven::read_dta(path, n_max = nmax),
          sas7bdat = haven::read_sas(path, n_max = nmax),
          # SPSS portable format: an older, ASCII-transport variant of .sav.
          por      = haven::read_por(path, n_max = nmax)))
      },
      jasp = {
        # A .jasp bundles a labelled data frame (like SPSS): import_jasp() returns
        # the columns with haven-style label/labels attributes, so the rest of
        # data_check (and the CSV conversion in psychds-convert) treats it exactly
        # like a .sav. Both the old binary and modern SQLite formats are handled.
        df <- import_jasp(path)$data
        if (is.data.frame(df) && is.finite(n_rows)) utils::head(df, n_rows) else df
      },
      omv = {
        # A .omv (jamovi) is the JASP counterpart -- import_omv() returns the same
        # labelled data frame, so it is treated exactly like a .jasp / .sav.
        df <- import_omv(path)$data
        if (is.data.frame(df) && is.finite(n_rows)) utils::head(df, n_rows) else df
      },
      rds = {
        obj <- readRDS(path)
        if (is.data.frame(obj)) utils::head(obj, n_rows) else NULL
      },
      rda = , rdata = {
        # An .RData/.rda workspace can hold arbitrary objects -- fitted models,
        # session state -- not just data frames. Restoring a model that
        # references an uninstalled package (e.g. robustlmm, effects) makes
        # load() print namespace/restore diagnostics at the C level (not
        # suppressible from R) and can crash. We read it in an isolated
        # subprocess (.read_rdata_isolated), which returns the first data frame
        # or NULL, plus a "reusability" verdict for data_check's reporting.
        .read_rdata_isolated(path, n_rows)
      },
      NULL
    )
    .utf8_repair_df(df)
  }, error = function(e) {
    if (grepl("time limit", conditionMessage(e), ignore.case = TRUE)) stop(e)
    warning("Could not read ", basename(path), ": ", conditionMessage(e))
    NULL
  })
}

# Read an .RData/.rda workspace in an ISOLATED subprocess and return its first
# data frame (head of `n_rows`), or NULL. Isolation is essential: restoring
# model/session objects that reference uninstalled packages prints C-level
# diagnostics and can crash the process -- none of which must reach the caller.
# A NULL return means the workspace holds no reusable tabular data (only models
# / session objects, or it could not be restored at all); data_check turns that
# into a sharing recommendation.
.read_rdata_isolated <- function(path, n_rows = 5) {
  out_rds <- tempfile(fileext = ".rds")
  on.exit(unlink(out_rds), add = TRUE)
  nmax <- if (is.finite(n_rows)) n_rows else Inf

  # The child loads the workspace with its message stream sunk to null, then
  # writes the first data frame (or NULL) to out_rds. It never errors to the
  # parent, so a model-heavy or broken workspace cannot make noise or crash.
  script <- sprintf(paste(
    "con <- file(nullfile(), open='wt'); sink(con, type='message')",
    "e <- new.env()",
    "ok <- tryCatch({ load(%s, envir = e); TRUE }, error = function(x) FALSE)",
    "sink(type='message'); close(con)",
    "df <- NULL",
    "if (ok) { dfs <- Filter(is.data.frame, as.list(e))",
    "  if (length(dfs) > 0) { d <- as.data.frame(dfs[[1]]); n <- %s",
    "    df <- if (is.finite(n)) utils::head(d, n) else d } }",
    "saveRDS(df, %s)",
    sep = "\n"),
    deparse(path), if (is.finite(nmax)) nmax else "Inf", deparse(out_rds))

  tryCatch(
    processx::run(rscript_path(), args = c("-e", script),
                  error_on_status = FALSE, timeout = 60),
    error = function(e) NULL)

  if (!file.exists(out_rds)) return(NULL)
  tryCatch(readRDS(out_rds), error = function(e) NULL)
}

# Path to the Rscript executable of the current R installation.
rscript_path <- function() {
  file.path(R.home("bin"),
            if (.Platform$OS.type == "windows") "Rscript.exe" else "Rscript")
}

# -- Column-type classification (rules only) ----------------------------------

# Detect a Likert / rating scale in a numeric column and infer its valid range.
#
# A scale is a small set of CONSECUTIVE integers spanning a plausible range
# (0-based, 1-based, or symmetric bipolar). The column is expected to be
# CONTAMINATED -- the whole reason to detect the scale is to surface the weird
# values (a stray 99, a mistyped 33) as being outside the valid range. So the
# range must be inferred robustly, from the DENSE core of common consecutive
# levels, not from min()/max() (which one outlier destroys).
#
# Method (hybrid "E"): find the dense consecutive core (mode-anchored, bridging
# small interior gaps, stopping at a rare+gapped level), then anchor the FLOOR
# to the natural scale start (1, or 0 if a 0 is observed) -- reporting that
# inference -- and take the CEILING as the top core level. Everything outside the
# accepted [lo, hi] is returned as `suspects` for the out-of-range / miscoded
# checks to interpret.
#
# Returns NULL when the column is not a scale (too many levels, non-integer,
# core too short, or the core does not explain enough of the data). Otherwise a
# list: lo, hi, levels_present, coverage, suspects, floor_inferred (the
# levels we inferred below the lowest observed), note (human-readable).
#
# `min_core` = minimum consecutive core levels to count as a scale (default 3).
# `min_coverage` = the accepted range must explain at least this fraction of the
# non-missing values (default 0.90); the leftover are the suspects.
# `common_frac` = a level counts as a real (bridgeable) response level at this
# fraction of the data, absolute floor 2 (default 0.01); rarer gapped levels are
# treated as detached contaminants and left as suspects.
.detect_likert_scale <- function(x, max_levels = 23L, min_core = 3L,
                                  min_coverage = 0.90, common_frac = 0.01) {
  x <- x[!is.na(x) & !is.nan(x) & is.finite(x)]
  if (length(x) < 20) return(NULL)          # need enough data to bound a scale
  if (any(x != round(x)))  return(NULL)     # non-integer -> continuous
  x <- as.integer(round(x))
  u <- sort(unique(x))
  # A scale lives within [-11, 11]; a spread of distinct levels beyond ~23 is a
  # count/continuous column, not a rating scale.
  if (length(u) < 2L || length(u) > max_levels) return(NULL)

  tab <- table(x)
  lv  <- as.integer(names(tab))
  cnt <- as.integer(tab)
  n   <- length(x)
  mode_i <- which.max(cnt)

  # A level is "common" if it holds a non-trivial share of the data; "rare"
  # otherwise. Only used to decide whether a GAPPED level is an interior scale
  # level to bridge, or a detached contaminant (99, 33) to leave as a suspect.
  # A level counts as common at `common_frac` of the data, with an absolute
  # floor of 2 so a single stray value can never read as a real response level.
  common_floor <- max(common_frac * n, 2)
  is_common <- cnt >= common_floor

  # Grow a consecutive-integer core outward from the modal level. Bridge a single
  # missing interior level (a 1-7 scale where nobody picked 4 is still 1-7), but
  # stop when the next OCCURRING level is both rare and separated by a gap >= 2
  # (a detached contaminant), or when there is a gap of >= 3 (clearly not part of
  # the run).
  # The scale is the run of CONSECUTIVE occupied integer levels around the mode.
  # An ADJACENT occupied level (step 1) is always part of the scale, however
  # rare: a lone 6 next to a 1-5 core means the scale really goes to 6 and that
  # level was just rarely used -- it is NOT a typo. A value beyond a GAP (a 99, a
  # mistyped 33, an 8 after 1-6) cannot be a quiet extension (there would be a
  # hole), so the core stops and that value becomes a suspect.
  #
  # Small interior gaps are bridged when both sides are common levels (a 1-7
  # scale showing {1,2,5,6,7} has an empty 3-4 but is still 1-7); a gap into a
  # rare far side is treated as the boundary.
  present <- lv                       # occupied levels, sorted
  hi <- lo <- lv[mode_i]
  extend <- function(dir) {
    repeat {
      cand <- if (dir > 0) present[present > hi] else present[present < lo]
      if (length(cand) == 0) break
      nextlv <- if (dir > 0) min(cand) else max(cand)
      gap    <- abs(nextlv - if (dir > 0) hi else lo)
      ni     <- which(lv == nextlv)
      if (gap == 1L) {                       # adjacent -> always extend
        if (dir > 0) hi <<- nextlv else lo <<- nextlv
      } else if (is_common[ni] && nextlv >= -11L && nextlv <= 11L) {
        # an interior gap (empty middle levels) to a COMMON far side that is
        # still inside the scale envelope: bridge it. A 1-7 scale showing
        # {1,2,5,6,7} bridges the empty 3-4. A detached rare contaminant (99,
        # 33) is NOT common, so it is never bridged.
        if (dir > 0) hi <<- nextlv else lo <<- nextlv
      } else break                            # gap into rare/detached -> stop
    }
  }
  extend(+1); extend(-1)

  core_levels <- lo:hi
  if (length(core_levels) < min_core) return(NULL)

  # Floor anchoring: scales start at 0 or 1. Infer as little as possible, and
  # record what we infer. If the observed floor is 2 or 3, snap down to the
  # natural start -- 0 when a 0 is present anywhere, else 1 -- but never below the
  # data's actual minimum-minus-a-little (we only fill the small gap to 0/1).
  floor_inferred <- integer(0)
  natural_floor <- if (0L %in% u) 0L else 1L
  if (lo > natural_floor && lo <= natural_floor + 2L &&
      natural_floor >= -11L) {
    floor_inferred <- setdiff(natural_floor:(lo - 1L), u)
    lo <- natural_floor
  }
  # Bipolar symmetry: if the core is symmetric-ish around 0 (a -k..k scale) keep
  # it as observed; no floor anchoring applies (natural_floor logic above only
  # fires for non-negative cores because natural_floor is 0/1).

  if (lo < -11L || hi > 11L) return(NULL)    # outside the scale envelope

  accepted <- lo:hi
  in_range <- x %in% accepted
  coverage <- mean(in_range)
  if (coverage < min_coverage) return(NULL)  # core doesn't explain the column

  suspects <- sort(unique(x[!in_range]))

  note <- {
    obs_lo <- min(u); obs_hi <- max(u)
    inf <- if (length(floor_inferred))
      sprintf("; inferred the unobserved floor value%s %s to make it a %d-based scale",
              plural(length(floor_inferred)),
              paste(floor_inferred, collapse = ", "), natural_floor) else ""
    sprintf("Detected a %d-%d rating scale (levels observed: %s%s).",
            lo, hi, paste(intersect(accepted, u), collapse = ", "), inf)
  }

  list(lo = lo, hi = hi,
       levels_present = intersect(accepted, u),
       coverage = coverage, suspects = suspects,
       floor_inferred = floor_inferred, note = note)
}

# data_check column types. The LLM-only refinements (ordinal/categorical for
# ambiguous integer columns) are not produced by the rules path; ambiguous
# columns fall back to continuous (numeric) or text (character).
.data_check_col_types <- c(
  "continuous", "binary", "categorical", "ordinal", "likert", "date", "id",
  "text", "continuous_comma_decimal", "continuous_outliers_excluded",
  "empty", "constant", "unknown"
)

#' Classify a single data column by rule
#'
#' Rule order (ported from datacheck `classify_col_type_rules()`): all-NA ->
#' empty; ID name pattern -> id; 1 unique -> constant; 2 unique -> binary;
#' date-parseable -> date; long strings -> text; numeric -> continuous (or
#' ambiguous integer, flagged for LLM); comma-decimal -> continuous variants.
#'
#' @param col_name the column's name (drives the ID-pattern rule)
#' @param values the column's values (a vector)
#'
#' @returns a list with `col_type` (a value from `.data_check_col_types`, or
#'   `NA` when only the LLM could decide), `ambiguous` (whether the LLM should
#'   be consulted), `numeric_values` (numeric vector for stats, or `NULL`),
#'   `n_coerced`, and `is_numeric`.
#' @export
#' @keywords internal
#'
#' @examples
#' data_col_type("age", c(23, 45, 31, 29))
#' data_col_type("subject_id", c("s01", "s02", "s03"))
data_col_type <- function(col_name, values) {
  # Guard against a non-UTF-8 column name reaching the perl grepl below, which
  # errors (not just warns) on some code paths. data_read_head() sanitises names
  # at read time; this is belt-and-braces for direct callers.
  if (length(col_name) && !is.na(col_name) &&
      is.na(iconv(col_name, "UTF-8", "UTF-8")))
    col_name <- iconv(col_name, "latin1", "UTF-8", sub = "")

  x_noNA <- values[!is.na(values)]
  n_noNA <- length(x_noNA)

  if (n_noNA == 0)
    return(list(col_type = "empty", ambiguous = FALSE, numeric_values = NULL,
                n_coerced = NA_integer_, is_numeric = FALSE))

  n_unique <- length(unique(x_noNA))

  id_pat <- paste0(
    "(?i)(",
    "^(participant|subject|subj|respondent|pp|ppt|pid|sub)$",
    "|^id$",
    "|[_\\-\\.](id|number|num|nr|no|code)$",
    "|^(subjectid|subjectnumber|responseid|recordid|participantid|",
    "subjectno|subjectnum|subjectcode|participantno|participantnum)$",
    "|^sub[_\\-]\\d",
    "|^(participant|subject|subj|pp|sub)[_\\-]?\\d+$",
    ")"
  )
  if (grepl(id_pat, col_name, perl = TRUE))
    return(list(col_type = "id", ambiguous = FALSE, numeric_values = NULL,
                n_coerced = NA_integer_, is_numeric = FALSE))

  if (n_unique == 1)
    return(list(col_type = "constant", ambiguous = FALSE, numeric_values = NULL,
                n_coerced = NA_integer_, is_numeric = FALSE))

  if (n_unique == 2)
    return(list(col_type = "binary", ambiguous = FALSE, numeric_values = NULL,
                n_coerced = NA_integer_, is_numeric = FALSE))

  char_sample <- as.character(unique(x_noNA))[seq_len(min(20, n_unique))]
  n_date_ok <- sum(vapply(char_sample, function(v) {
    tryCatch(!is.na(as.Date(v)), warning = function(w) FALSE, error = function(e) FALSE)
  }, logical(1)))
  if (n_date_ok / length(char_sample) >= 0.70)
    return(list(col_type = "date", ambiguous = FALSE, numeric_values = NULL,
                n_coerced = NA_integer_, is_numeric = FALSE))

  if (stats::median(nchar(as.character(x_noNA))) > 40)
    return(list(col_type = "text", ambiguous = FALSE, numeric_values = NULL,
                n_coerced = NA_integer_, is_numeric = FALSE))

  if (is.numeric(values)) {
    if (any(x_noNA != floor(x_noNA)) || n_unique > 20)
      return(list(col_type = "continuous", ambiguous = FALSE,
                  numeric_values = values, n_coerced = NA_integer_,
                  is_numeric = FALSE))
    # ambiguous integer 3-20 unique: rules can't tell ordinal/categorical/
    # continuous apart. LLM-off -> treat as continuous.
    return(list(col_type = NA_character_, ambiguous = TRUE,
                numeric_values = values, n_coerced = NA_integer_,
                is_numeric = TRUE))
  }

  x_sub  <- suppressWarnings(as.numeric(gsub(",", ".", as.character(x_noNA), fixed = TRUE)))
  pct_ok <- sum(!is.na(x_sub)) / n_noNA
  if (pct_ok >= 0.95) {
    num_vec <- suppressWarnings(as.numeric(gsub(",", ".", as.character(values), fixed = TRUE)))
    return(list(col_type = "continuous_comma_decimal", ambiguous = FALSE,
                numeric_values = num_vec, n_coerced = sum(is.na(x_sub)),
                is_numeric = FALSE))
  }
  if (pct_ok >= 0.80) {
    num_vec <- suppressWarnings(as.numeric(gsub(",", ".", as.character(values), fixed = TRUE)))
    return(list(col_type = "continuous_outliers_excluded", ambiguous = FALSE,
                numeric_values = num_vec, n_coerced = sum(is.na(x_sub)),
                is_numeric = FALSE))
  }

  # remaining character columns: LLM would decide categorical/text/... -- off -> text
  list(col_type = NA_character_, ambiguous = TRUE, numeric_values = NULL,
       n_coerced = NA_integer_, is_numeric = FALSE)
}

# -- Column statistics --------------------------------------------------------

#' Summary statistics for a numeric column
#'
#' @param x_for_stats a numeric vector (may contain NA)
#' @param x_raw the raw source column (for n / n_missing / n_unique)
#'
#' @returns a one-row data.frame of statistics.
#' @export
#' @keywords internal
data_col_stats <- function(x_for_stats, x_raw) {
  # A column is not always a vector. `jsonlite` turns a nested API response into
  # a data frame whose columns are THEMSELVES data frames (or matrices, or
  # lists) -- common in .RData/.rds files holding a saved API result. For those,
  # is.na() returns a MATRIX rather than a vector, so `x_raw[!is.na(x_raw)]`
  # flattens to one element per cell and n_unique below becomes one number per
  # cell instead of one number. data.frame() then recycles that into as many
  # rows as there were cells, and data_check's do.call(rbind, ...) over the
  # columns fails with "length of 'dimnames' [2] not equal to array extent".
  #
  # There are no summary statistics for such a column anyway, so report it as
  # unsummarisable: one row, counts of the values it holds, statistics NA. The
  # column is still listed and still classified; only its statistics are blank.
  if (!is.null(dim(x_raw)) || (!is.atomic(x_raw) && !is.null(x_raw))) {
    n_val <- NROW(x_raw)
    return(data.frame(
      n = n_val, n_missing = 0L, n_unique = NA_integer_,
      mean = NA_real_, sd = NA_real_, se = NA_real_, median = NA_real_,
      min = NA_real_, max = NA_real_, range = NA_real_, p25 = NA_real_,
      p75 = NA_real_, iqr = NA_real_, skewness = NA_real_, kurtosis = NA_real_
    ))
  }

  n_unique_val <- length(unique(x_raw[!is.na(x_raw)]))
  empty_stats <- function(n, n_miss) data.frame(
    n = n, n_missing = n_miss, n_unique = n_unique_val,
    mean = NA_real_, sd = NA_real_, se = NA_real_, median = NA_real_,
    min = NA_real_, max = NA_real_, range = NA_real_, p25 = NA_real_,
    p75 = NA_real_, iqr = NA_real_, skewness = NA_real_, kurtosis = NA_real_
  )

  if (is.null(x_for_stats)) {
    n_miss <- sum(is.na(x_raw)); n_val <- length(x_raw) - n_miss
    return(empty_stats(n_val, n_miss))
  }

  # x_for_stats may hold non-numeric text (e.g. a coding sheet's long free-text
  # column pushed through here): as.numeric() would emit "NAs introduced by
  # coercion" -- surfaced as `In FUN(X[[i]], ...)` because this runs inside
  # data_check's per-column lapply. The NAs are expected and discarded on the
  # next line, so the warning is pure noise; suppress it, matching every other
  # coercion of raw column values in this file.
  x <- suppressWarnings(as.numeric(x_for_stats))
  x <- x[!is.na(x) & !is.nan(x)]
  n <- length(x)
  n_miss <- sum(is.na(x_for_stats))
  if (n == 0) return(empty_stats(0L, n_miss))

  mn <- mean(x)
  s  <- if (n > 1) stats::sd(x) else NA_real_
  p25 <- stats::quantile(x, 0.25, names = FALSE)
  p75 <- stats::quantile(x, 0.75, names = FALSE)
  data.frame(
    n = n, n_missing = n_miss, n_unique = n_unique_val,
    mean = mn,
    sd = s,
    se = if (!is.na(s)) s / sqrt(n) else NA_real_,
    median = stats::median(x),
    min = min(x), max = max(x), range = max(x) - min(x),
    p25 = p25, p75 = p75, iqr = p75 - p25,
    skewness = if (n > 2 && !is.na(s) && s > 0) mean((x - mn)^3) / s^3 else NA_real_,
    kurtosis = if (n > 3 && !is.na(s) && s > 0) mean((x - mn)^4) / s^4 - 3 else NA_real_
  )
}

# -- Codebook parsing + column matching (used by codebook_check) ---------------
#
# Ported from datacheck's 2_codebook_label.R / helper.R. The rules-only path
# (structured CSV/Excel, haven embedded labels, rich-text extraction, exact and
# normalised name matching) runs with `llm_use(FALSE)`; the LLM tiers (parsing
# unstructured codebooks, fuzzy column matching, semantic label merging) are
# gated behind `llm_use(TRUE)` in codebook_check.R.

# Normalise a variable/column name for matching: lowercase, underscores -> space,
# collapse whitespace, strip leading/trailing dots.
normalize_varname <- function(x) {
  x <- tolower(x)
  x <- trimws(x)
  x <- gsub("[_]+", " ", x)
  x <- gsub("\\s+", " ", x)
  x <- gsub("^[.]+|[.]+$", "", x)
  trimws(x)
}

# Reduce each word of an already-lowercased, punctuation-stripped label to its
# Porter stem (SnowballC), falling back to a crude trailing-"s" stripper when
# SnowballC is unavailable so matching still runs in minimal environments.
.stem_words <- local({
  have_snowball <- NULL
  function(s) {
    if (is.null(have_snowball))
      have_snowball <<- requireNamespace("SnowballC", quietly = TRUE)
    words <- strsplit(s, " ", fixed = TRUE)[[1]]
    words <- words[nzchar(words)]
    if (length(words) == 0) return("")
    if (isTRUE(have_snowball)) {
      stemmed <- tryCatch(SnowballC::wordStem(words, language = "porter"),
                          error = function(e) NULL)
      if (!is.null(stemmed)) return(paste(stemmed, collapse = " "))
    }
    paste(sub("^([a-z]{7,})s$", "\\1", words, perl = TRUE), collapse = " ")
  }
})

# Normalise a label for semantic-equivalence comparison: strip possessives and
# punctuation, Porter-stem each word, collapse whitespace. So "Participants'
# responses" and "Participant response" normalise to the same string.
normalize_label <- function(x) {
  x <- tolower(x)
  x <- gsub("'s|\u2019s|\u2018s", "", x, perl = TRUE)
  x <- gsub("[^a-z0-9 ]", " ", x)
  x <- gsub("\\s+", " ", trimws(x))
  vapply(x, .stem_words, character(1), USE.NAMES = FALSE)
}

# Scan a data.frame's headers for a "variable name" column and a "label" column.
# Returns list(var_col, lab_col) or NULL.
# Normalise a header cell for matching: lowercase, and collapse EVERY run of
# punctuation/whitespace to one space. This is what makes the matching
# separator-insensitive, so "variable_name", "variable name", "variable-name"
# and "Variable.Name" are one case rather than four alternations. Harvesting the
# real headers from the corpus showed 968 distinct spellings across 2280 header
# cells, so enumerating literal forms does not scale -- normalise, then match a
# compact word set.
.normalize_header <- function(x) {
  x <- tolower(trimws(as.character(x)))
  x <- gsub("[^a-z0-9]+", " ", x)
  trimws(gsub("\\s+", " ", x))
}

# Header words naming the VARIABLE column and the LABEL column. Both are matched
# against .normalize_header() output, and both tolerate an optional plural -- real
# codebooks write "Variable Names" (the whole Project Implicit IAT corpus) as
# often as "Variable Name".
#
# The two sets are deliberately DISJOINT except for "label", which is genuinely
# ambiguous in the wild ("Variable Names | Label" means label; "Variable | Label"
# could mean either). It is assigned to the LABEL role because in every corpus
# file carrying a bare "label" header there is also a separate, more specific
# variable column, so reading it as the label is correct there and the variable
# column is still found. Keep any new word in exactly one of these.
.cb_var_header_re <- paste0(
  "^(variable|variables|var|vars|varname|varnames|item|items|",
  "field|fields|column|columns|name|names|code|codes|id)$",
  "|^(variable|var|item|field|column|col) (name|names)$")
# "question" and "questions" are label words in their own right (a codebook whose
# only text column is the item's question), not just qualifiers -- the original
# list carried bare "question", and dropping it silently pushed such files onto
# the positional fallback.
.cb_lab_header_re <- paste0(
  "^((variable|var|item|question|response|full|general|specific|short|long) )?",
  "(label|labels|description|descriptions|desc|definition|definitions|meaning|",
  "explanation|explanations|text|wording|interpretation|details|content)$",
  "|^(questions?)$",
  # "full name" is a LABEL ("Variable | Full name | Scale" in the corpus), but
  # bare "name" is a VARIABLE word, so it cannot be reached via the qualifier
  # group above without making every "* name" a label. Listed explicitly.
  "|^((full|long|complete) names?)$")

.find_codebook_cols <- function(col_names) {
  nm <- .normalize_header(col_names)
  # The variable column must not also read as a label ("variable label" names a
  # label, not the variable itself), so label-matching names are excluded first.
  is_lab <- grepl(.cb_lab_header_re, nm, perl = TRUE)
  var_col <- col_names[grepl(.cb_var_header_re, nm, perl = TRUE) & !is_lab][1]
  lab_col <- col_names[is_lab][1]
  if (is.na(var_col) || is.na(lab_col)) return(NULL)
  list(var_col = var_col, lab_col = lab_col)
}

# Empty codebook-variable table (the canonical column set). The DDI-derived
# per-variable properties (value_labels, missing_values, question,
# carried as extra columns; they default to NA and only populate when a source
# supplies them.
.empty_codebook_vars <- function() {
  data.frame(
    codebook_variable = character(0), label = character(0),
    codebook_source = character(0), group = character(0),
    value_labels = character(0), missing_values = character(0),
    question = character(0),
    coding_instructions = character(0),
    parse_method = character(0),
    paper_id = character(0)
  )
}

# A short, human-readable slug used as BOTH the OSD `code` and the on-disk file
# name (scales/<slug>.osd). Lowercase words joined by underscores, from the scale
# NAME when it has one (PANAS -> "positive_and_negative_affect_schedule"), else
# from the column prefix/abbreviation (unnamed block -> "response"). Capped at a
# word boundary so the file name stays reasonable; the full name is kept in
# scale_info$name. Provenance is NOT encoded in the slug (it lives in
# metacheck$scale_source) -- the slug is just a stable, readable identifier.
.osd_slug <- function(name = NULL, prefix = NULL, max_chars = 60L) {
  x <- if (!is.null(name) && !is.na(name) && nzchar(name)) name else prefix %||% ""
  x <- tolower(gsub("[^A-Za-z0-9]+", "_", x))
  x <- gsub("^_+|_+$", "", x)
  if (!nzchar(x)) return("scale")
  if (nchar(x) > max_chars) {
    trunc <- substr(x, 1, max_chars)
    at <- regexpr("_[^_]*$", trunc)          # trim back to last full word
    if (at > 1) trunc <- substr(trunc, 1, at - 1L)
    x <- gsub("_+$", "", trunc)
  }
  if (!nzchar(x)) "scale" else x
}

# A code valid under the OpenScales OSD spec: uppercase letters, digits, and
# hyphens only. Capped at 40 characters (at a hyphen boundary where possible):
# an over-long "code" comes from a self-generated LLM label that is really a
# sentence, not an instrument name -- the full text is kept in scale_info$name.
.osd_safe_code <- function(x, max_chars = 40L) {
  x <- toupper(gsub("[^A-Za-z0-9]+", "-", x %||% ""))
  x <- gsub("^-+|-+$", "", x)
  if (!nzchar(x)) return("SCALE")
  if (nchar(x) > max_chars) {
    trunc <- substr(x, 1, max_chars)
    at <- regexpr("-[^-]*$", trunc)          # trim back to last full token
    if (at > 1) trunc <- substr(trunc, 1, at - 1L)
    x <- gsub("-+$", "", trunc)
  }
  if (!nzchar(x)) "SCALE" else x
}

# Mint the OSD `code` and provenance for one identified scale. The `code` is a
# short, readable slug used as BOTH scale_info$code and the on-disk file name
# (scales/<code>.osd): the scale NAME when it has one (PANAS ->
# "positive_and_negative_affect_schedule"), else the column prefix (unnamed block
# -> "response"). Provenance is NOT encoded in the slug -- it is carried in the
# returned `source` (dictionary / manuscript / self_generated / unnamed_block),
# which the .osd's metacheck block and the README record. Three levels of trust:
#   * dictionary     -- matched a known instrument (OpenScales / curated).
#   * manuscript     -- a real instrument named in the paper.
#   * self_generated -- an LLM-inferred construct label, NOT a named instrument.
#   * unnamed_block  -- a coherent same-prefix rating block, unnamed.
# `prefix` is the column abbreviation, used when the scale has no name. Shared by
# codebook_check (writing .osd files) and psychds-convert (cross-referencing
# variables to a scale code), so it lives here rather than in the module. Returns
# list(code, source, provenance).
.osd_code_and_provenance <- function(scale, prefix, scale_source, dict) {
  src <- scale_source %||% ""
  in_dict <- FALSE
  if (!is.na(scale) && nzchar(scale)) {
    i <- which(tolower(dict$name) == tolower(scale))
    if (length(i)) in_dict <- TRUE
  }
  # Slug from the name when present, else the column prefix. Same value regardless
  # of provenance -- the slug is a readable identifier, not a trust marker.
  code <- .osd_slug(name = scale, prefix = prefix)
  # The UPSTREAM OpenScales code, when this scale has a reference definition.
  # Kept separate from `code`: the slug names the file and stays readable and
  # stable for every scale, while ref_code is a join key into scale_meta /
  # scale_items and exists only for dictionary instruments.
  ref_code <- .scale_ref_code(scale, dict)

  if (in_dict) {
    list(code = code, ref_code = ref_code, source = "dictionary",
         provenance = "Matched a known instrument in metacheck's scale dictionary (OpenScales-derived or curated).")
  } else if (identical(src, "self_generated")) {
    list(code = code, ref_code = NA_character_, source = "self_generated",
         provenance = "This label was GENERATED BY metacheck from the item wording. It is NOT a recognised named instrument, only metacheck's inference of what the items measure.")
  } else if (identical(src, "unnamed_block")) {
    list(code = code, ref_code = NA_character_, source = "unnamed_block",
         provenance = "A coherent block of same-prefix rating columns detected in the data, but NOT named: neither a known instrument nor a construct metacheck could infer from the available text. Recorded for its structure (items + response scale) only.")
  } else {
    list(code = code, ref_code = ref_code, source = "manuscript",
         provenance = "A named instrument identified from the manuscript text but not present in the OpenScales registry.")
  }
}

# -- Reference instrument lookup (OpenScales item-level data) ------------------
# `scales` identifies an instrument by NAME; `scale_meta` / `scale_items` /
# `scale_scoring` (see data-raw/scale_items.R) describe what that instrument
# actually contains. These helpers join the two, so a scale detected in shared
# data can be compared against the published original: how many items it should
# have, which of them are reverse-keyed, and its reported reliability.
#
# The join path is scale NAME -> scales$code -> scale_meta$code. Only the ~200
# dictionary rows carrying an OpenScales code can resolve; curated additions
# (code == "") have no upstream definition and return NULL. That is a property
# of the dictionary, not of the item data.
#
# Lazily loaded and cached: the datasets live in the package, but a module may
# run against a partially-installed namespace, so every access is guarded.
.scale_ref_data <- local({
  cached <- NULL
  function() {
    if (!is.null(cached)) return(cached)
    get_ds <- function(nm) {
      d <- tryCatch(get(nm, envir = asNamespace("metacheck")), error = function(e) NULL)
      if (is.null(d)) d <- tryCatch(get(nm), error = function(e) NULL)
      d
    }
    cached <<- list(meta    = get_ds("scale_meta"),
                    items   = get_ds("scale_items"),
                    scoring = get_ds("scale_scoring"))
    cached
  }
})

# The OpenScales code for a scale NAME, or NA when the name is not a dictionary
# instrument or the dictionary row carries no code (a curated addition).
.scale_ref_code <- function(scale, dict) {
  if (is.null(scale) || length(scale) != 1L || is.na(scale) || !nzchar(scale))
    return(NA_character_)
  i <- which(tolower(dict$name) == tolower(scale))
  if (!length(i)) return(NA_character_)
  code <- dict$code[i[1]]
  if (is.na(code) || !nzchar(code)) return(NA_character_)
  ref <- .scale_ref_data()$meta
  if (is.null(ref) || !(code %in% ref$code)) return(NA_character_)
  code
}

# Everything known upstream about instrument `code`: the registry record plus
# its items and subscales. NULL when the code has no reference definition.
.scale_reference <- function(code) {
  if (is.null(code) || length(code) != 1L || is.na(code) || !nzchar(code))
    return(NULL)
  d <- .scale_ref_data()
  if (is.null(d$meta) || is.null(d$items)) return(NULL)
  m <- d$meta[d$meta$code == code, , drop = FALSE]
  if (!nrow(m)) return(NULL)
  items <- d$items[d$items$code == code, , drop = FALSE]
  if (!nrow(items)) return(NULL)
  scoring <- if (!is.null(d$scoring))
    d$scoring[d$scoring$code == code, , drop = FALSE] else NULL
  list(meta = m[1, , drop = FALSE], items = items, scoring = scoring)
}

# Normalised item wording for comparison: lowercase, punctuation and HTML
# stripped, whitespace collapsed. Item text is copied between codebooks with
# cosmetic drift ("I don't like crowds" / "I do not like crowds."), so an exact
# string test under-matches badly.
.item_text_key <- function(x) {
  x <- gsub("<[^>]*>", " ", x %||% "")            # codebook labels carry markup
  x <- tolower(x)
  x <- gsub("n't\\b", " not", x)                  # don't -> do not
  x <- gsub("[^a-z0-9]+", " ", x)
  trimws(gsub("\\s+", " ", x))
}

# Match a detected block's item wording against a reference instrument.
#
# `wording` is a named character vector: column name -> codebook item text.
# Returns one row per detected column with the reference item it matched (by
# normalised wording) and that item's `reverse` flag, or NULL when the block has
# no usable wording at all.
#
# DELIBERATELY exact-on-normalised-text, not fuzzy: a wrong item->item link
# would attach a wrong reverse flag, which is worse than no flag. Columns that
# do not match a reference item come back with reverse = NA (unknown), never
# FALSE -- absence of a match is not evidence the item is forward-keyed.
.scale_match_items <- function(wording, reference) {
  if (is.null(reference) || is.null(wording) || !length(wording)) return(NULL)
  wording <- wording[!is.na(wording) & nzchar(wording)]
  if (!length(wording)) return(NULL)

  ref_key <- .item_text_key(reference$items$text)
  # Ambiguous wording (the same text twice in one instrument) cannot identify an
  # item, so it is dropped rather than guessed at.
  dup <- duplicated(ref_key) | duplicated(ref_key, fromLast = TRUE)
  ref_key[dup] <- NA_character_

  i <- match(.item_text_key(wording), ref_key)
  data.frame(
    column_name  = names(wording),
    ref_item_id  = reference$items$item_id[i],
    ref_text     = reference$items$text[i],
    ref_reverse  = reference$items$reverse[i],
    ref_dimension = reference$items$dimension[i]
  )
}

# -- Value labels / code lists + missing-value scheme (DDI ValueDomain) ---------
# A categorical variable's meaning lives in its code list -- the mapping
# 1="Strongly disagree" ... 5="Strongly agree" -- and in which codes denote
# missingness (-99="refused"). DDI models these as CodeList / ValueDomain and
# MissingValues. We serialise a code list as a compact JSON object keyed by code
# ("{\"1\":\"Male\",\"2\":\"Female\"}") so it survives as a single data.frame
# column and round-trips through the label-matching machinery unchanged.

# Encode a named code->label mapping as a JSON string. `codes` are the values,
# `labels` the human labels (same length). Returns NA when empty.
.encode_value_labels <- function(codes, labels) {
  keep <- !is.na(codes) & !is.na(labels) & nzchar(trimws(as.character(labels)))
  if (!any(keep)) return(NA_character_)
  obj <- as.list(as.character(labels[keep]))
  names(obj) <- as.character(codes[keep])
  tryCatch(as.character(jsonlite::toJSON(obj, auto_unbox = TRUE)),
           error = function(e) NA_character_)
}

# Decode a value-labels JSON string back to a named character vector
# (names = codes, values = labels). Returns NULL on failure / NA.
.decode_value_labels <- function(s) {
  if (is.null(s) || length(s) != 1 || is.na(s) || !nzchar(s)) return(NULL)
  out <- tryCatch(jsonlite::fromJSON(s), error = function(e) NULL)
  if (is.null(out) || length(out) == 0) return(NULL)
  v <- unlist(out); v[!is.na(v)]
}

# Encode a set of missing-value codes (optionally with reasons) as JSON. `codes`
# is a vector of the sentinel codes; `reasons` an optional same-length vector of
# labels ("refused", "not applicable"). Returns NA when empty.
.encode_missing_values <- function(codes, reasons = NULL) {
  codes <- codes[!is.na(codes)]
  if (length(codes) == 0) return(NA_character_)
  if (is.null(reasons)) {
    tryCatch(as.character(jsonlite::toJSON(as.character(codes))),
             error = function(e) NA_character_)
  } else {
    .encode_value_labels(codes, reasons)
  }
}

# Regex identifying a value-label's TEXT as a missing-data sentinel (e.g.
# "Refused", "N/A", "Prefer not to answer") rather than a genuine substantive
# response category. Every alternative is \b-word-boundary-anchored: an
# earlier unanchored version matched "na"/"unknown" as bare substrings of
# ordinary words and text (e.g. "Argenti-NA", "Native Americans", "Other or
# Unknown" as a real ethnicity category), misclassifying whole country lists
# and Likert-scale anchor sets as missing-value schemes. The bare "unknown"
# alternative was dropped entirely -- a value literally labelled "Unknown" is
# usually a genuine "respondent didn't know their own [ethnicity/status/etc.]"
# response option, not evidence the field is unanswered, and no amount of
# anchoring makes that single word unambiguous. Shared by .haven_value_labels()
# (SPSS/Stata haven attributes) and .missing_from_value_labels() (codebook-text
# derived value labels) so both paths classify identically.
#
# "na"/"n/a" gets its OWN, stricter sub-pattern rather than joining the
# word-boundary alternation below: \b alone isn't enough, because "na" is a
# short, ordinary word in other languages (Polish "na" = "on/for"), so
# \bna\b still matches real prose like "na czym polega" ("what it involves")
# or "na co zasluguje" ("what [they] deserve") in a genuine, non-missing
# response option. Requiring the token to be the WHOLE label (bare "N/A",
# "n/a", "N/A.") or the LAST word after - or : ("ID12345 - N/A") keeps genuine
# abbreviation uses while excluding "na" embedded mid-sentence in running
# prose. "N/A (some reason)" is deliberately NOT matched -- a parenthetical
# reason after N/A means the researcher gave that code a substantive meaning
# ("N/A -- I live alone"), not a bare missingness sentinel, so it is not
# accepted as a valid missing-value declaration here.
.missing_na_re <- paste0(
  "(?i)^\\s*n/?a\\.?\\s*$|",
  "(?i)[-:]\\s*n/?a\\s*[.)]?\\s*$"
)

.missing_label_re <- paste0(
  "(?i)\\b(",
  "missing|",
  "refus(ed|al)?|",
  "declin(ed|e)?|",
  "no\\s*(answer|response|data)|",
  "did\\s*not\\s*respond|",
  "not\\s*(applicable|asked|reported|answered)|",
  "prefer\\s*not\\s*to\\s*(answer|say|respond)|",
  "don'?t\\s*know|",
  "skip(ped)?|",
  "unanswered|",
  "(left\\s*)?blank|",
  "withheld|",
  "system\\s*missing",
  ")\\b"
)

# Does a set of value-label TEXTS look like free-text survey responses rather
# than a controlled category vocabulary? A genuine codebook's category names --
# even a long one, like a 239-country pick-list or a detailed occupation
# taxonomy -- stay short, proper-noun-like phrases. JASP/jamovi auto-
# factorizes ANY nominal-text column (assigning one integer level per UNIQUE
# observed value, then storing that level->string map exactly like a haven
# `attr(,"labels")`), so an open-ended comments field arrives looking like a
# codebook whose "categories" are full sentences, participant IDs, and typos --
# label count alone can't distinguish the two cases (a real country list can
# be far larger than a small free-text field's unique-response count), but
# label LENGTH can: category names are short, free-text responses run long.
.looks_like_freetext_labels <- function(labs) {
  txt <- trimws(as.character(labs))
  txt <- txt[nzchar(txt)]
  if (length(txt) < 5) return(FALSE)   # too few to judge reliably
  mean(nchar(txt) > 40) > 0.2          # a meaningful fraction are long prose
}

# Extract value labels + declared missing values from one haven column. Returns
# list(value_labels = <json|NA>, missing_values = <json|NA>). haven puts the
# code list in attr(,"labels") and SPSS-declared missings in attr(,"na_values")
# / attr(,"na_range"); a labelled code whose label names it missing (e.g.
# "Refused", "N/A") is also treated as a missing code.
.haven_value_labels <- function(col) {
  labs <- attr(col, "labels")
  na_values <- attr(col, "na_values")
  na_range  <- attr(col, "na_range")
  vl <- NA_character_
  miss_codes <- numeric(0); miss_reasons <- character(0)

  # A JASP/omv free-text column factorized to one level per unique value: skip
  # entirely rather than encode it as a bogus "codebook" (see
  # .looks_like_freetext_labels() above) -- checked on the LABEL TEXT, not the
  # label COUNT, since a real codebook (a country pick-list) can legitimately
  # have far more entries than a small free-text field has unique responses.
  if (!is.null(labs) && length(labs) > 0 && .looks_like_freetext_labels(names(labs)))
    labs <- NULL

  if (!is.null(labs) && length(labs) > 0) {
    codes  <- unname(labs)
    reasons <- names(labs)
    vl <- .encode_value_labels(codes, reasons)
    # Labels that read as missingness -> sentinel missing codes.
    is_miss <- grepl(.missing_label_re, reasons, perl = TRUE) |
               grepl(.missing_na_re, reasons, perl = TRUE)
    if (any(is_miss)) {
      miss_codes  <- c(miss_codes, codes[is_miss])
      miss_reasons <- c(miss_reasons, reasons[is_miss])
    }
  }
  if (!is.null(na_values)) {
    miss_codes  <- c(miss_codes, na_values)
    miss_reasons <- c(miss_reasons, rep(NA_character_, length(na_values)))
  }
  if (!is.null(na_range) && length(na_range) == 2 && all(is.finite(na_range))) {
    # A declared missing RANGE: record its endpoints as a compact note.
    miss_codes  <- c(miss_codes, na_range)
    miss_reasons <- c(miss_reasons, rep("range", 2))
  }
  mv <- if (length(miss_codes) > 0) {
    keep <- !duplicated(miss_codes)
    r <- miss_reasons[keep]
    if (all(is.na(r))) .encode_missing_values(miss_codes[keep])
    else .encode_value_labels(miss_codes[keep], r)
  } else NA_character_

  list(value_labels = vl %||% NA_character_, missing_values = mv %||% NA_character_)
}

# Is this string a bare number? Used to find the CODE side of a value-label pair.
.vl_is_numeric <- function(x) grepl("^-?\\d+(\\.\\d+)?$", trimws(x))

# Split a codebook "values" cell into raw left/right halves of each entry.
# Direction is NOT decided here -- see .parse_value_label_text().
.vl_split_pairs <- function(s) {
  # Scale ANCHORS written as "1 (very negative) to 7 (very positive)" -- by far
  # the most common way authors label only the two ends of a rating scale, and
  # the joiner is a word ("to") or a dash rather than a list separator, so the
  # generic entry-splitting below cannot reach it. Handled first, and only when
  # EVERY parenthesised group is preceded by a number, so ordinary prose that
  # merely contains brackets is not mistaken for a code list.
  anchors <- regmatches(s, gregexpr(
    "(-?\\d+(?:\\.\\d+)?)\\s*\\(([^)]{1,60})\\)", s, perl = TRUE))[[1]]
  if (length(anchors) >= 2) {
    am <- regmatches(anchors, regexec(
      "^(-?\\d+(?:\\.\\d+)?)\\s*\\(([^)]{1,60})\\)$", anchors, perl = TRUE))
    good <- lengths(am) == 3
    if (sum(good) >= 2) {
      return(list(lhs = vapply(am[good], `[`, character(1), 2L),
                  rhs = trimws(vapply(am[good], `[`, character(1), 3L))))
    }
  }

  # Entries separate on ; | newline. A comma also separates, but only when the
  # next entry starts with a code -- checked for BOTH directions, since a comma
  # inside a label ("Counselors, Social workers=21") must not split.
  parts <- unlist(strsplit(
    s,
    paste0("\\s*[;|\\n]\\s*",
           "|\\s*,\\s*(?=\\s*-?\\d+(\\.\\d+)?\\s*[:=])",
           "|\\s*,\\s*(?=[^,;|=:]{1,40}[:=]\\s*-?\\d+(\\.\\d+)?\\s*(,|;|\\||$))"),
    perl = TRUE))
  parts <- trimws(parts[nzchar(trimws(parts))])
  if (!length(parts)) return(NULL)
  m <- regmatches(parts, regexec("^\\s*(.+?)\\s*[:=]\\s*(.+?)\\s*$", parts, perl = TRUE))
  ok <- lengths(m) == 3
  if (!any(ok)) {
    # No ":"/"=" anywhere. Authors also write "1-Male", "1. Male", "1) Male"
    # and "1 Male" -- the same convention .extract_codebook_positional() already
    # accepts for anchor columns. Only a NUMERIC code is allowed here: with text
    # on both sides ("High vs. Low") there is no separator to mark the split, so
    # such a string is prose, not a code list. A bare "1,2,3" has no labels at
    # all and is correctly left unparsed by the >=2-pairs rule below.
    m <- regmatches(parts, regexec(
      "^\\s*(-?\\d+(?:\\.\\d+)?)\\s*(?:[-\u2013\u2014).]\\s*|\\s+)([A-Za-z].*?)\\s*$",
      parts, perl = TRUE))
    ok <- lengths(m) == 3
    if (!any(ok)) return(NULL)
  }
  list(lhs = vapply(m[ok], `[`, character(1), 2L),
       rhs = vapply(m[ok], `[`, character(1), 3L))
}

# Parse a codebook "values" cell into value labels. Handles the textual encodings
# authors actually use, in EITHER direction:
#   "1 = Male; 2 = Female"      (code = label)
#   "Male = 1; Female = 2"      (label = code)  <- more common in practice
#   "M = Male; F = Female"      (text = text)   <- needs `observed` to resolve
#
# Direction is decided ONCE PER CELL, never per pair: a cell that mixes
# "1=Male" with "Female=2" would otherwise produce a scrambled mapping. The
# numeric side is the code whenever exactly one side is consistently numeric.
#
# When NEITHER side is numeric the string alone is ambiguous -- "M = Male" and
# "Male = M" are structurally identical. `observed` (a sample of the actual data
# column's values, e.g. data_check's `sample_values`) resolves it: whichever side
# matches what is really stored in the column is the code. Without `observed`
# such a cell is left unparsed rather than guessed, since inventing a direction
# would silently invert every label for that variable.
#
# Returns a value-labels JSON string or NA.
.parse_value_label_text <- function(s, observed = NULL) {
  if (is.null(s) || is.na(s) || !nzchar(trimws(s))) return(NA_character_)
  pr <- .vl_split_pairs(as.character(s))
  if (is.null(pr) || length(pr$lhs) < 2) return(NA_character_)  # need a real mapping

  l_num <- mean(.vl_is_numeric(pr$lhs))
  r_num <- mean(.vl_is_numeric(pr$rhs))
  codes <- NULL; labels <- NULL
  if (l_num >= 0.8 && r_num < 0.8) {
    codes <- pr$lhs; labels <- pr$rhs
  } else if (r_num >= 0.8 && l_num < 0.8) {
    codes <- pr$rhs; labels <- pr$lhs
  } else if (!is.null(observed) && length(observed)) {
    ov <- unique(trimws(as.character(observed)))
    ov <- ov[!is.na(ov) & nzchar(ov)]
    hit_l <- mean(trimws(pr$lhs) %in% ov)
    hit_r <- mean(trimws(pr$rhs) %in% ov)
    if (hit_l > hit_r) { codes <- pr$lhs; labels <- pr$rhs }
    else if (hit_r > hit_l) { codes <- pr$rhs; labels <- pr$lhs }
  }
  if (is.null(codes)) return(NA_character_)
  .encode_value_labels(codes, labels)
}

# From a value-labels JSON string, derive the missing-value scheme: the codes
# whose label reads as missingness ("refused", "n/a", "prefer not to answer").
# Returns a missing-values JSON string or NA. Used so a code list from a text
# codebook contributes to the missing scheme, matching the haven path.
.missing_from_value_labels <- function(vl_json) {
  vl <- .decode_value_labels(vl_json)
  if (is.null(vl) || length(vl) == 0) return(NA_character_)
  is_miss <- grepl(.missing_label_re, unname(vl), perl = TRUE) |
             grepl(.missing_na_re, unname(vl), perl = TRUE)
  if (!any(is_miss)) return(NA_character_)
  .encode_value_labels(names(vl)[is_miss], unname(vl)[is_miss])
}

# Find a dedicated "missing values" column. Kept separate from the value-label
# finder because missingness is its OWN field: a codebook may declare sentinel
# codes ("-9 = not answered") without listing any ordinary value labels, and
# folding the two together would record a missing code as a real category.
.find_missing_value_col <- function(col_names) {
  # "assigned missing values" is the wording in Lewis's template and in
  # datamgmtinedresearch.com Table 8.1, both of which list it as a REQUIRED
  # data-dictionary field.
  re <- paste0("^((assigned|declared|defined) )?(missing|missings|",
               "missing values?|missing codes?|missing data|",
               "na values?|na codes?|missing value codes?)$")
  col_names[grepl(re, .normalize_header(col_names), perl = TRUE)][1]
}

# Find a "value labels" / "coding" column in a structured codebook's headers.
.find_value_label_col <- function(col_names) {
  # Separator-insensitive like .find_codebook_cols(). "code"/"codes" are NOT
  # here: they name the VARIABLE column in real codebooks (an item's code), and
  # a word must belong to exactly one role.
  # "variable levels", "allowed values" and "possible values" were each found in
  # the corpus naming a code list that no pattern matched.
  #
  # "missing values" is deliberately NOT here, even though it appears in real
  # codebooks: missingness is a SEPARATE field (`missing_values`), derived from
  # value labels whose text reads as missingness. Accepting it as a value-label
  # header means that in a codebook with no values column it would be chosen as
  # THE code list, so "-99 = Refused" would be recorded as an ordinary value
  # label rather than a missing code -- a silent semantic error.
  #
  # "response options (see second sheet)" is likewise NOT accepted: the codes
  # live elsewhere, so the cell holds a cross-reference rather than a mapping.
  # "allowable values" is the wording used by Crystal Lewis's widely-shared
  # data-dictionary template and by datamgmtinedresearch.com Table 8.1, which
  # both list it as a REQUIRED field ("allowable values/range, including labels
  # associated with categorical codes").
  re <- paste0("^(value labels?|values?|coding|categor(y|ies)|",
               "response options?|response codes?|levels?|value meanings?|",
               "valid values|value codes?|scoring|answers|",
               "variable levels|allowe?d values|allowable values|",
               "possible values|permitted values)$")
  col_names[grepl(re, .normalize_header(col_names), perl = TRUE)][1]
}

# Find a "question text" column in a codebook.
.find_question_col <- function(col_names) {
  # "question in survey" / "question wording" / "question translated" all occur
  # in the corpus, so a trailing qualifier is allowed after "question".
  re <- paste0("^(question|questions|question text|question wording|",
               "question in survey|item text|item wording|prompt|",
               "survey question|item description)$")
  col_names[grepl(re, .normalize_header(col_names), perl = TRUE)][1]
}
# Find a "coding instructions" column: how this variable's values came about.
#
# DDI models this two ways -- `codingInstructions` (with `typeOfCodingInstruction`,
# an OPEN conceptType with no fixed vocabulary) for processing rules, and
# `var/derivation` (`drvdesc` prose + `drvcmd` syntax) for a variable computed
# from others. Crystal Lewis's template calls the column `transformations`
# ("Recodings or calculations"), and datamgmtinedresearch.com Table 8.1 lists
# "Transformations" as a REQUIRED data-dictionary field.
#
# This is the single most useful optional field because it answers "where did
# this number come from?" -- covering scale scores ("mean of items 1-10"),
# reverse scoring ("6 - bds_3"), exclusions, and transformations in one place.
# It is also the only home DDI offers for REVERSE KEYING: no standard checked
# (DDI-Codebook 2.6, DDI Lifecycle, Psych-DS) has a reverse/polarity element,
# and DDI's own schema example uses `<typeOfCodingInstruction>recode</...>`.
#
# Stored as free text and deliberately NOT parsed: `typeOfCodingInstruction` is
# an open vocabulary, so there is no canonical wording to match against.
.find_coding_instruction_col <- function(col_names) {
  re <- paste0("^(coding instructions?|coding instruction|instructions?|",
               "transformations?|recodings?|recoding|calculations?|",
               "calculation recoding|derivations?|derived|derivation description|",
               "computations?|computed|how computed|scoring|scoring instructions?)$")
  col_names[grepl(re, .normalize_header(col_names), perl = TRUE)][1]
}

# Extract variable-label pairs from a structured data.frame (CSV/Excel rows).
# Returns NULL when no matching header columns are found.
# Does a character vector look like variable NAMES (short, no spaces, mostly
# alnum/underscore -- neo1, BFI_3, q07)? Used by the positional layout detector.
.looks_like_varnames <- function(x) {
  x <- trimws(as.character(x)); x <- x[nzchar(x) & !is.na(x)]
  if (length(x) < 3) return(FALSE)
  ok <- grepl("^[A-Za-z][A-Za-z0-9_.]{0,30}$", x) & !grepl("\\s", x)
  mean(ok) >= 0.8 && length(unique(x)) >= 0.8 * length(x)   # mostly ids, mostly unique
}

# Does a character vector look like item WORDING (sentence-like: has spaces, some
# length, not all identical)?
.looks_like_wording <- function(x) {
  x <- trimws(as.character(x)); x <- x[nzchar(x) & !is.na(x)]
  if (length(x) < 3) return(FALSE)
  mean(grepl("\\s", x)) >= 0.6 && stats::median(nchar(x)) >= 8
}

# Positional codebook extractor for sheets/files with NO usable header row (a
# prose title instead of column names, e.g. an IPIP-NEO sheet whose columns are
# item-id | wording | anchor1 | anchor2 | ...). Scans for a column that looks
# like variable names with an adjacent column that looks like item wording; any
# further columns whose values look like "1 - Label" anchors are gathered into a
# value-labels code list. Returns a codebook-vars data.frame or NULL.
.extract_codebook_positional <- function(df, src) {
  if (is.null(df) || nrow(df) < 3 || ncol(df) < 2) return(NULL)
  raw <- as.data.frame(lapply(df, as.character), stringsAsFactors = FALSE)
  p <- ncol(raw)

  var_j <- NA_integer_; lab_j <- NA_integer_
  for (j in seq_len(p - 1L)) {
    if (.looks_like_varnames(raw[[j]]) && .looks_like_wording(raw[[j + 1L]])) {
      var_j <- j; lab_j <- j + 1L; break
    }
  }
  if (is.na(var_j)) return(NULL)

  keep <- nzchar(trimws(raw[[var_j]])) & !is.na(raw[[var_j]])
  rows <- raw[keep, , drop = FALSE]
  if (nrow(rows) < 3) return(NULL)

  # Anchor columns: columns after the label whose cells look like "N - Label".
  anchor_js <- integer(0)
  for (j in seq.int(lab_j + 1L, p)) {
    if (j > p) break
    vals <- trimws(rows[[j]]); vals <- vals[nzchar(vals) & !is.na(vals)]
    if (length(vals) && mean(grepl("^[0-9]+\\s*[-=:.)]", vals)) >= 0.6)
      anchor_js <- c(anchor_js, j)
  }
  value_labels <- rep(NA_character_, nrow(rows))
  if (length(anchor_js)) {
    # A scale block shares one anchor set; build it once from the first data row
    # that has anchors, as JSON {code: label}.
    for (i in seq_len(nrow(rows))) {
      cells <- trimws(as.character(rows[i, anchor_js]))
      cells <- cells[nzchar(cells) & !is.na(cells)]
      m <- regmatches(cells, regexec("^([0-9]+)\\s*[-=:.)]\\s*(.+)$", cells))
      codes <- vapply(m, function(z) if (length(z) == 3) z[2] else NA_character_, character(1))
      labs  <- vapply(m, function(z) if (length(z) == 3) trimws(z[3]) else NA_character_, character(1))
      ok <- !is.na(codes)
      if (any(ok))
        value_labels[i] <- .encode_value_labels(codes[ok], labs[ok])
    }
  }

  data.frame(
    codebook_variable = trimws(rows[[var_j]]),
    label             = trimws(rows[[lab_j]]),
    codebook_source   = src,
    group             = NA_character_,
    value_labels      = value_labels,
    missing_values    = vapply(value_labels, .missing_from_value_labels,
                               character(1), USE.NAMES = FALSE),
    question          = NA_character_,
    coding_instructions = NA_character_
  )
}

# Parse a GitHub-flavoured markdown table into a codebook. A README that carries
# its data dictionary as a pipe table is a real convention -- TidyTuesday ships
# every dataset this way ("|variable |class |description |") -- and the table is
# fully structured, so it should never reach the LLM. Scans for a separator rule
# ("|---|---|") whose preceding row is a codebook header, then reads rows until
# the table ends. Returns a codebook-vars data.frame or NULL.
.extract_markdown_codebook <- function(path, src, observed = list()) {
  ln <- tryCatch(readLines(path, warn = FALSE), error = function(e) NULL)
  if (is.null(ln) || !length(ln)) return(NULL)
  ln <- iconv(ln, to = "UTF-8", sub = "")
  ln[is.na(ln)] <- ""
  rule_re <- "^[[:space:]]*[|][[:space:]:-]*[-][[:space:]:|-]*$"
  rules <- grep(rule_re, ln)
  rules <- rules[rules > 1L]
  if (!length(rules)) return(NULL)

  cells <- function(x) {
    x <- sub("^[[:space:]]*[|]", "", x)
    x <- sub("[|][[:space:]]*$", "", x)
    trimws(unlist(strsplit(x, "[|]", fixed = FALSE)))
  }
  out <- list()
  for (i in rules) {
    hdr <- cells(ln[i - 1L])
    hdr <- hdr[nzchar(hdr)]
    if (length(hdr) < 2L || is.null(.find_codebook_cols(hdr))) next
    body <- list()
    j <- i + 1L
    while (j <= length(ln) && grepl("[|]", ln[j])) {
      rw <- cells(ln[j])
      if (any(nzchar(rw))) {
        length(rw) <- length(hdr)          # pad/truncate to the header width
        body[[length(body) + 1L]] <- rw
      }
      j <- j + 1L
    }
    if (!length(body)) next
    df <- as.data.frame(do.call(rbind, body), stringsAsFactors = FALSE)
    names(df) <- hdr
    df[] <- lapply(df, function(z) { z[is.na(z)] <- ""; z })
    one <- .extract_structured_codebook(df, src, observed)
    if (!is.null(one) && nrow(one) > 0) out[[length(out) + 1L]] <- one
  }
  if (!length(out)) return(NULL)
  dplyr::bind_rows(out)
}

# First non-empty scalar among a set of candidate field names, case-insensitively.
.json_field <- function(entry, names_want) {
  nm <- names(entry)
  if (is.null(nm)) return(NA_character_)
  for (w in names_want) {
    j <- which(tolower(nm) == w)
    if (!length(j)) next
    v <- entry[[j[1]]]
    if (is.null(v) || length(v) != 1 || is.list(v)) next
    v <- trimws(as.character(v))
    if (!is.na(v) && nzchar(v)) return(v)
  }
  NA_character_
}

# Encode a JSON codebook's value-label field. Real files use several shapes:
#   [{"name": "1", "label": "Male"}, ...]   (name = code, label = meaning)
#   {"1": "Male", "2": "Female"}            (plain object)
#   ["1 = Male", "2 = Female"]              (strings)
# In the corpus the per-entry `label` is FREQUENTLY EMPTY and the meaning lives
# in `name` ("ja"/"nein"), so an empty label falls back to using `name` as its
# own label rather than dropping the pair.
.json_value_labels <- function(vl) {
  if (is.null(vl) || length(vl) == 0) return(NA_character_)
  if (is.list(vl) && !is.null(names(vl)) && !any(vapply(vl, is.list, logical(1)))) {
    return(.encode_value_labels(names(vl), unlist(vl, use.names = FALSE)))
  }
  codes <- character(0); labs <- character(0)
  for (v in vl) {
    if (is.list(v)) {
      cd <- .json_field(v, c("name", "value", "code", "level"))
      lb <- .json_field(v, c("label", "meaning", "text"))
      if (is.na(cd)) next
      codes <- c(codes, cd)
      labs  <- c(labs, if (is.na(lb)) cd else lb)   # empty label -> code names itself
    } else if (length(v) == 1 && !is.na(v)) {
      pr <- .vl_split_pairs(as.character(v))
      if (!is.null(pr) && length(pr$lhs)) { codes <- c(codes, pr$lhs); labs <- c(labs, pr$rhs) }
    }
  }
  if (length(codes) < 1) return(NA_character_)
  .encode_value_labels(codes, labs)
}

# Parse a JSON codebook: an array of per-variable objects, or an object wrapping
# one under a key such as "variables" / "variableMeasured" (schema.org, as the
# `codebook` R package and Psych-DS emit). Field names are matched leniently
# because no single convention dominates. Returns a codebook-vars data.frame or
# NULL when the file is not a per-variable codebook.
.extract_json_codebook <- function(path, src) {
  j <- tryCatch(jsonlite::fromJSON(path, simplifyVector = FALSE),
                error = function(e) NULL)
  if (is.null(j)) return(NULL)
  # Unwrap a container object; schema.org puts the list under variableMeasured.
  if (is.list(j) && !is.null(names(j))) {
    for (k in c("variableMeasured", "variables", "columns", "fields",
                "codebook", "items")) {
      hit <- which(tolower(names(j)) == tolower(k))
      if (length(hit) && is.list(j[[hit[1]]])) { j <- j[[hit[1]]]; break }
    }
  }
  if (!is.list(j) || length(j) == 0) return(NULL)
  entries <- Filter(function(e) is.list(e) && !is.null(names(e)), j)
  if (length(entries) < 2) return(NULL)

  var <- vapply(entries, .json_field, character(1),
                c("name", "variable", "variable_name", "varname", "column", "id"))
  lab <- vapply(entries, .json_field, character(1),
                c("label", "description", "title", "variable_label", "definition"))
  itm <- vapply(entries, .json_field, character(1),
                c("item_text", "question", "question_text", "wording", "prompt"))
  # A label may be absent while the item wording carries the meaning.
  lab <- ifelse(is.na(lab) & !is.na(itm), itm, lab)
  keep <- !is.na(var) & nzchar(var) & !is.na(lab) & nzchar(lab)
  if (sum(keep) < 2) return(NULL)

  vl <- vapply(entries, function(e) {
    f <- names(e)[tolower(names(e)) %in% c("value_label", "value_labels",
                                           "values", "levels", "categories")]
    if (!length(f)) return(NA_character_)
    .json_value_labels(e[[f[1]]])
  }, character(1))

  data.frame(
    codebook_variable = var[keep],
    label             = lab[keep],
    codebook_source   = src,
    group             = NA_character_,
    value_labels      = vl[keep],
    missing_values    = vapply(vl[keep], .missing_from_value_labels,
                               character(1), USE.NAMES = FALSE),
    question          = itm[keep],
    coding_instructions = NA_character_
  )
}

# Parse a multi-sheet spreadsheet codebook, format-agnostically.
#
# Excel (.xlsx/.xls, via readxl) and OpenDocument (.ods, via readODS) codebooks
# are structurally identical -- several sheets, a header row, one variable per
# row -- so all of the logic lives here and only the READER differs. `sheets()`
# returns sheet names; `read(sheet, header)` returns one sheet as a data.frame,
# with `header = FALSE` giving the un-headered grid.
#
# Every sheet is explored, not just the first: a codebook often keeps its scale
# item lists on separate tabs (an IPIP-NEO sheet beside a general "Codebook"
# sheet). Per sheet: try the named-header parser, then a header-row lookahead
# for sheets whose first row is a prose title ("Codebook for Studies 1-4",
# "NOTES") which pushes the real header down, then a positional fallback.
# Results are combined and `src` records the sheet so a variable can be traced.
.extract_spreadsheet_codebook <- function(sheets, read, src, observed,
                                          header_lookahead) {
  sh_names <- tryCatch(sheets(), error = function(e) character(0))
  if (length(sh_names) == 0) sh_names <- NA_character_   # single default read
  parsed <- list()
  for (sh in sh_names) {
    df <- tryCatch(read(sh, TRUE), error = function(e) NULL)
    if (is.null(df)) next
    ssrc <- if (is.na(sh)) src else paste0(src, " [", sh, "]")
    one <- .extract_structured_codebook(df, ssrc, observed)
    if (is.null(one)) {
      hdrless <- tryCatch(read(sh, FALSE), error = function(e) NULL)
      if (!is.null(hdrless) && nrow(hdrless) > 1) {
        for (k in seq_len(min(nrow(hdrless) - 1L, header_lookahead))) {
          hdr <- trimws(as.character(unlist(hdrless[k, ])))
          if (is.null(.find_codebook_cols(hdr))) next
          sub <- hdrless[seq(k + 1L, nrow(hdrless)), , drop = FALSE]
          names(sub) <- hdr
          rownames(sub) <- NULL
          one <- .extract_structured_codebook(sub, ssrc, observed)
          if (!is.null(one)) break
        }
      }
    }
    if (is.null(one)) one <- .extract_codebook_positional(df, ssrc)
    if (!is.null(one) && nrow(one) > 0) parsed[[length(parsed) + 1L]] <- one
  }
  if (length(parsed) > 0) dplyr::bind_rows(parsed) else NULL
}

.extract_structured_codebook <- function(df, src, observed = list()) {
  if (is.null(df) || nrow(df) == 0 || ncol(df) < 2) return(NULL)
  cols <- .find_codebook_cols(names(df))
  if (is.null(cols)) return(NULL)
  rows <- df[nzchar(trimws(as.character(df[[cols$var_col]]))), , drop = FALSE]
  if (nrow(rows) == 0) return(NULL)
  # Drop a CONTENT-FREE repeat: a variable name that already appeared in an
  # earlier row, restated here with a blank label. Some codebook exports
  # duplicate their whole variable list many times over with the label column
  # stripped on every repeat after the first (seen on a corrupted OSF
  # README_VariableLegend.csv: ~65 real rows followed by ~1,000,000 blank rows,
  # among which the same ~65 names recur every ~16,384 rows with an empty
  # label) -- inflating a real handful of variables into thousands of rows that
  # add nothing, all the way through matching and any LLM tier downstream.
  # Keeps the FIRST occurrence of every name regardless of its label (so a
  # codebook that is genuinely sparse -- one row per variable, sometimes with no
  # description at all -- is untouched), and keeps any row whose label is
  # non-blank even when the name repeats (so real per-group restatements, e.g.
  # the same variable documented again under a second study heading with its
  # own real description, survive).
  name_key <- trimws(as.character(rows[[cols$var_col]]))
  lab_blank <- !nzchar(trimws(as.character(rows[[cols$lab_col]])))
  rows <- rows[!(duplicated(name_key) & lab_blank), , drop = FALSE]
  if (nrow(rows) == 0) return(NULL)

  # Optional DDI-derived columns: value labels / coding, question text,
  # Each is parsed per row when its column is present.
  val_col <- .find_value_label_col(names(df))
  q_col   <- .find_question_col(names(df))
  ci_col  <- .find_coding_instruction_col(names(df))
  na_str  <- function(x) { x <- trimws(as.character(x)); ifelse(nzchar(x), x, NA_character_) }

  # `observed` maps a variable name -> a sample of that column's real values, so
  # a text-coded cell ("M = Male") can be resolved against the data instead of
  # guessed. Absent (the usual case here, since a codebook file is parsed before
  # any data is read), such cells stay unparsed.
  value_labels <- if (!is.na(val_col)) {
    vn <- trimws(as.character(rows[[cols$var_col]]))
    vapply(seq_len(nrow(rows)), function(i) {
      .parse_value_label_text(as.character(rows[[val_col]])[i], observed[[vn[i]]])
    }, character(1))
  } else rep(NA_character_, nrow(rows))
  # Missing scheme: any code whose LABEL reads as missingness ("99 = Refused"),
  # plus a dedicated missing-values column when the codebook declares one
  # ("-9 = not answered"). The explicit column wins where both are present,
  # since it states the author's intent rather than inferring it from wording.
  missing_values <- vapply(value_labels, .missing_from_value_labels,
                           character(1), USE.NAMES = FALSE)
  m_col <- .find_missing_value_col(names(df))
  if (!is.na(m_col)) {
    declared <- vapply(as.character(rows[[m_col]]), function(x) {
      if (is.na(x) || !nzchar(trimws(x))) return(NA_character_)
      # "-9 = not answered" -> a coded scheme; a bare "-9" -> codes only.
      pr <- .vl_split_pairs(x)
      if (!is.null(pr) && length(pr$lhs)) {
        num_l <- mean(.vl_is_numeric(pr$lhs))
        if (num_l >= 0.8) return(.encode_value_labels(pr$lhs, pr$rhs))
        if (mean(.vl_is_numeric(pr$rhs)) >= 0.8)
          return(.encode_value_labels(pr$rhs, pr$lhs))
      }
      codes <- trimws(unlist(strsplit(x, "\\s*[;,|]\\s*")))
      codes <- codes[nzchar(codes) & .vl_is_numeric(codes)]
      if (!length(codes)) return(NA_character_)
      .encode_missing_values(codes)
    }, character(1), USE.NAMES = FALSE)
    missing_values <- ifelse(is.na(declared), missing_values, declared)
  }

  data.frame(
    codebook_variable = as.character(rows[[cols$var_col]]),
    label             = as.character(rows[[cols$lab_col]]),
    codebook_source   = src,
    group             = NA_character_,
    value_labels      = value_labels,
    missing_values    = missing_values,
    question          = if (!is.na(q_col)) na_str(rows[[q_col]]) else NA_character_,
    coding_instructions = if (!is.na(ci_col)) na_str(rows[[ci_col]]) else NA_character_
  )
}

# Extract embedded variable labels from a haven-read data.frame (SPSS/Stata/SAS).
# Returns NULL if no labelled columns found. Caller adds parse_method = "haven".
# `group` scopes these labels to the ONE study/file they were embedded in
# (data_check's structure_df$group for this file) -- passing NA_character_
# (the default) leaves them unscoped, which match_column_labels() then applies
# to every column of that name PAPER-WIDE regardless of source file. That is
# correct when a genuinely paper-wide label truly applies everywhere, but it
# also means one file's mislabelled/anomalous embedded label (e.g. a JASP
# free-text factorization the .looks_like_freetext_labels() guard missed)
# would otherwise leak onto unrelated files' same-named columns in a
# different study. Callers that know the file's group should pass it.
.extract_haven_labels <- function(df, src, group = NA_character_) {
  labels <- vapply(names(df), function(col) {
    lbl <- attr(df[[col]], "label")
    if (is.null(lbl)) NA_character_ else trimws(as.character(lbl[1]))
  }, character(1))
  # Value labels + declared missing values are useful even for columns without a
  # variable label, so harvest them for every column and keep any column that has
  # EITHER a label or a code list.
  vlmv <- lapply(names(df), function(col) .haven_value_labels(df[[col]]))
  value_labels   <- vapply(vlmv, function(x) x$value_labels %||% NA_character_, character(1))
  missing_values <- vapply(vlmv, function(x) x$missing_values %||% NA_character_, character(1))

  keep <- (!is.na(labels) & nzchar(labels)) | !is.na(value_labels)
  if (!any(keep)) return(NULL)
  data.frame(
    codebook_variable = names(df)[keep],
    label             = labels[keep],
    codebook_source   = src,
    group             = group,
    value_labels      = value_labels[keep],
    missing_values    = missing_values[keep],
    question          = NA_character_,
    coding_instructions = NA_character_
  )
}

# Does a line look like a codebook DEFINITION -- a short leading identifier, a
# separator (colon, equals, tab, or a 2+ space column gap), then descriptive
# text? Used to decide whether a PDF is worth sending to the LLM at all.
.cb_is_definition_line <- function(x) {
  grepl("^[ ]{0,8}[A-Za-z_][A-Za-z0-9_.$#-]{0,40}[ ]*([:=]|[ ]{2,}|\t)[ ]*[^ ].{3,}$",
        x, perl = TRUE)
}

# Decide whether a PDF's text is worth sending to the LLM, and return it if so.
#
# A PDF is only processed when its FIRST `probe_pages` pages already contain at
# least `min_defs` definition-looking lines on some page. Rationale: an LLM call
# on a PDF is the most expensive route we have, and a document whose opening
# pages hold no variable definitions is overwhelmingly a narrative coding manual,
# a survey printout, or a 375-page institutional report -- not a codebook we can
# use. Refusing those outright is deliberate policy, not an approximation: we do
# not send a huge PDF anywhere on the chance something useful appears late.
#
# The test counts definition lines rather than measuring their PROPORTION. That
# distinction matters: PDF codebooks are frequently wrapped multi-column tables
# where one variable's row spans a dozen physical lines, only the first of which
# looks like a definition. A proportion-based threshold reads those continuation
# lines as noise and rejects exactly the value-label content that makes a PDF
# codebook worth reading; a count does not.
#
# Returns the file's non-blank lines when the gate passes, or character(0) when
# the PDF should be skipped.
.pdf_codebook_lines <- function(path, probe_pages = 5L, min_defs = 5L,
                                max_pages = 60L) {
  if (!requireNamespace("pdftools", quietly = TRUE)) return(character(0))
  pages <- tryCatch(pdftools::pdf_text(path), error = function(e) NULL)
  if (is.null(pages) || !length(pages)) return(character(0))

  # Hard page ceiling, checked BEFORE the content probe. A book-length PDF is
  # never worth sending: the two 375-page WVS reports and the 232-page GIPO
  # codebooks in the corpus would each cost 50-200 LLM calls. The probe alone
  # cannot catch them -- the WVS report's page 2 is a centred TITLE page whose
  # layout incidentally matches the definition pattern 6 times, which is over
  # the threshold, so it passed the content gate while being exactly the file
  # we least want to send.
  if (length(pages) > max_pages) return(character(0))

  # pdf_text() is a single LOCAL read -- the whole gate costs no API calls.
  probe <- utils::head(pages, probe_pages)
  n_def <- vapply(probe, function(t) {
    ln <- unlist(strsplit(t, "\n"))
    ln <- ln[nzchar(trimws(ln))]
    if (!length(ln)) return(0L)
    sum(.cb_is_definition_line(ln))
  }, integer(1))
  if (max(c(0L, n_def)) < min_defs) return(character(0))

  ln <- unlist(strsplit(paste(pages, collapse = "\n"), "\n"))
  ln[nzchar(trimws(ln))]
}

# Strip RTF control codes from a string, returning plain text.
.strip_rtf <- function(text) {
  text <- gsub("\\\\[a-z]+\\-?[0-9]*\\s?", " ", text)
  text <- gsub("\\\\[^a-z\n]", " ", text)
  text <- gsub("[{}]", "", text)
  text <- gsub("\\s+", " ", text)
  trimws(text)
}

# Extract plain text from a rich-text or binary codebook file (docx/pdf/rtf/
# odt). Returns "" on any failure or missing optional dependency.
.extract_rich_text <- function(path, ext) {
  tryCatch({
    switch(ext,
      docx = {
        if (!requireNamespace("officer", quietly = TRUE)) return("")
        doc  <- officer::read_docx(path)
        summ <- officer::docx_summary(doc)
        txt  <- as.character(summ$text)
        paste(txt[nzchar(trimws(txt))], collapse = "\n")
      },
      pdf = {
        if (!requireNamespace("pdftools", quietly = TRUE)) return("")
        paste(pdftools::pdf_text(path), collapse = "\n")
      },
      rtf = {
        .strip_rtf(paste(readLines(path, warn = FALSE), collapse = "\n"))
      },
      odt = {
        tmp <- tempfile()
        on.exit(unlink(tmp, recursive = TRUE), add = TRUE)
        dir.create(tmp)
        tryCatch({
          utils::unzip(path, files = "content.xml", exdir = tmp)
          xml_path <- file.path(tmp, "content.xml")
          if (!file.exists(xml_path)) return("")
          raw <- paste(readLines(xml_path, warn = FALSE), collapse = "\n")
          txt <- gsub("<[^>]+>", " ", raw)
          txt <- gsub("&amp;", "&", txt, fixed = TRUE)
          txt <- gsub("&lt;", "<", txt, fixed = TRUE)
          txt <- gsub("&gt;", ">", txt, fixed = TRUE)
          txt <- gsub("&apos;", "'", txt, fixed = TRUE)
          txt <- gsub("&quot;", '"', txt, fixed = TRUE)
          trimws(gsub("\\s+", " ", txt))
        }, error = function(e) "")
      },
      ""
    )
  }, error = function(e) "")
}

#' Parse a codebook file into variable definitions
#'
#' Rule-based codebook reader. Handles structured tables (CSV/TSV/Excel with a
#' variable-name column and a label column, including wide-format transposition
#' and multi-row header scanning), and embedded haven labels (SPSS/Stata). For
#' rich-text formats (docx/pdf/rtf/odt) it extracts plain text. Files that yield
#' no structured definitions return their raw text lines (character vector) so
#' the caller can route them to an LLM when `llm_use(TRUE)`.
#'
#' @param path path to a codebook/readme file
#' @param header_lookahead rows to scan for a header in multi-level CSVs
#' @param group the file's study/experiment group (data_check's
#'   `structure_df$group` for this file), scoping the definitions it yields to
#'   that ONE study. `NA_character_` (the default) leaves them unscoped, which
#'   `match_column_labels()` then applies to every same-named column
#'   PAPER-WIDE -- correct for a genuinely paper-wide codebook, but it also lets
#'   one study's codebook (e.g. a `condition` variable coded 1-4 in Study A)
#'   leak onto an unrelated same-named column in Study B (coded 1-6). Callers
#'   that know the file's group should pass it. Mirrors `.extract_haven_labels`'s
#'   `group` argument.
#'
#' @returns a data.frame of variable definitions (`codebook_variable`, `label`,
#'   `codebook_source`, `group`, `parse_method`); a character vector of text
#'   lines when only unstructured text is available; or `NULL` on failure.
#' @export
#' @keywords internal
parse_codebook <- function(path, header_lookahead = 5L, observed = list(),
                           group = NA_character_) {
  if (!file.exists(path)) return(NULL)
  ext <- tolower(tools::file_ext(path))
  src <- basename(path)

  result <- tryCatch(
    switch(ext,
      json = {
        .extract_json_codebook(path, src)
      },
      md = , markdown = , rmd = , qmd = {
        # A pipe table in a README/vignette is fully structured; parse it rather
        # than sending the prose to the LLM. Falls through to raw lines when the
        # file has no codebook table.
        .extract_markdown_codebook(path, src, observed)
      },
      csv = , tsv = , dat = {
        # A JSON codebook is sometimes shipped with a .csv extension (both
        # codebook_rawdata.csv and .json in the corpus are byte-identical JSON).
        # Sniff the first non-blank character and route rather than handing a
        # perfectly structured file to the LLM as raw text.
        first <- tryCatch({
          h <- readLines(path, n = 5L, warn = FALSE)
          h <- trimws(h[nzchar(trimws(h))])
          if (length(h)) substr(h[1], 1, 1) else ""
        }, error = function(e) "")
        if (first %in% c("[", "{")) {
          jres <- .extract_json_codebook(path, src)
          if (!is.null(jres)) return(within(jres, parse_method <- "structured"))
        }
        sep <- if (ext == "tsv") "\t" else .sniff_delimiter(path)
        # A UTF-8 BOM (EF BB BF) must be declared, not repaired downstream. It is
        # valid UTF-8, so the invalid-encoding check below does NOT fire on it,
        # but the three bytes stay glued to the first header cell -- turning
        # "Name" into "Name", which no header pattern matches. Reading with
        # "UTF-8-BOM" strips it. (Before this, the BOM bytes made iconv() report
        # invalid input, triggering the latin1 re-read below, which re-encoded
        # them as the visible "i>?" and broke header detection outright: files
        # with an identical header parsed or failed purely on BOM presence.)
        enc <- {
          b <- tryCatch(readBin(path, "raw", 3L), error = function(e) raw(0))
          if (length(b) == 3L && b[1] == as.raw(0xef) &&
              b[2] == as.raw(0xbb) && b[3] == as.raw(0xbf)) "UTF-8-BOM" else ""
        }
        raw <- tryCatch(
          utils::read.delim(path, sep = sep, header = FALSE,
                            check.names = FALSE, fileEncoding = enc),
          error = function(e) NULL
        )
        if (is.null(raw) || nrow(raw) == 0) {
          NULL
        } else {
          has_invalid <- any(vapply(raw, function(col) {
            is.character(col) &&
              any(is.na(iconv(col, from = "UTF-8", to = "UTF-8")))
          }, logical(1)))
          if (has_invalid) {
            raw <- tryCatch(
              utils::read.delim(path, sep = sep, header = FALSE,
                                check.names = FALSE, fileEncoding = "latin1"),
              error = function(e) NULL
            )
          }
          if (is.null(raw) || nrow(raw) == 0) {
            NULL
          } else {
            # Wide-format detection: variables as columns, stats as rows. If
            # >=50% of first-column values are known statistic names, transpose.
            wide_stats <- c("mean", "sd", "se", "min", "max", "median", "n")
            col1 <- trimws(tolower(as.character(raw[, 1])))
            col1 <- col1[nzchar(col1)]
            if (length(col1) > 0 && mean(col1 %in% wide_stats) >= 0.5) {
              var_names  <- as.character(raw[1, ])
              stat_names <- as.character(raw[, 1])
              traw <- as.data.frame(t(raw[, -1, drop = FALSE]))
              names(traw) <- stat_names[-1]
              raw <- cbind(data.frame(variable = var_names[-1]), traw)
              rownames(raw) <- NULL
            }
            header_row <- NA_integer_
            for (k in seq_len(min(nrow(raw), header_lookahead))) {
              if (!is.null(.find_codebook_cols(trimws(as.character(raw[k, ]))))) {
                header_row <- k
                break
              }
            }
            if (is.na(header_row)) {
              NULL
            } else {
              names(raw) <- trimws(as.character(raw[header_row, ]))
              df <- raw[seq(header_row + 1L, nrow(raw)), , drop = FALSE]
              rownames(df) <- NULL
              .extract_structured_codebook(df, src, observed)
            }
          }
        }
      },
      ods = , fods = {
        # OpenDocument spreadsheet -- LibreOffice/OpenOffice's native format and
        # the default for anyone not using Excel. Same structure as .xlsx, so it
        # shares .extract_spreadsheet_codebook(); only the reader differs.
        # readODS is optional: without it the file falls through to the LLM tier.
        #
        # NOTE .ods (spreadsheet) is not .odt (text document); .odt is handled
        # with the rich-text formats, where only prose can be recovered.
        if (!requireNamespace("readODS", quietly = TRUE)) {
          NULL
        } else {
          .extract_spreadsheet_codebook(
            sheets = function() readODS::list_ods_sheets(path),
            read = function(sh, header)
              as.data.frame(readODS::read_ods(
                path, sheet = if (is.na(sh)) 1 else sh, col_names = header,
                .name_repair = "unique_quiet")),
            src = src, observed = observed, header_lookahead = header_lookahead)
        }
      },
      xlsx = , xls = {
        if (!requireNamespace("readxl", quietly = TRUE)) {
          NULL
        } else {
          .extract_spreadsheet_codebook(
            sheets = function() readxl::excel_sheets(path),
            read = function(sh, header)
              as.data.frame(if (is.na(sh))
                readxl::read_excel(path, col_names = header,
                                   .name_repair = "unique_quiet")
              else
                readxl::read_excel(path, sheet = sh, col_names = header,
                                   .name_repair = "unique_quiet")),
            src = src, observed = observed, header_lookahead = header_lookahead)
        }
      },
      sav = , dta = , sas7bdat = {
        if (!requireNamespace("haven", quietly = TRUE)) {
          NULL
        } else {
          df <- switch(ext,
            sav      = haven::read_sav(path),
            dta      = haven::read_dta(path),
            sas7bdat = haven::read_sas(path))
          res <- .extract_haven_labels(as.data.frame(df), src)
          if (!is.null(res)) attr(res, ".is_haven") <- TRUE
          res
        }
      },
      jasp = {
        # A .jasp carries its own variable/value labels (measurement level +
        # value coding), so it serves as its own codebook. import_jasp() attaches
        # haven-style attributes, so the SAME extractor used for .sav applies.
        df <- tryCatch(import_jasp(path)$data, error = function(e) NULL)
        if (is.null(df)) NULL else {
          res <- .extract_haven_labels(df, src)
          if (!is.null(res)) attr(res, ".is_haven") <- TRUE
          res
        }
      },
      omv = {
        # A .omv (jamovi) carries its own variable/value labels too, exactly like
        # a .jasp -- import_omv() attaches the same haven-style attributes, so the
        # shared .extract_haven_labels() extractor applies with no special-casing.
        df <- tryCatch(import_omv(path)$data, error = function(e) NULL)
        if (is.null(df)) NULL else {
          res <- .extract_haven_labels(df, src)
          if (!is.null(res)) attr(res, ".is_haven") <- TRUE
          res
        }
      },
      qsf = {
        # A Qualtrics survey-definition file: parse_qsf() reads the question
        # wording and response options straight from the survey object. It sets
        # its own parse_method ("qsf"), preserved by the override below.
        parse_qsf(path)
      },
      pdf = {
        # Gated: only PDFs whose opening pages already show variable definitions
        # are handed to the LLM (see .pdf_codebook_lines). character(0) means
        # "deliberately skipped", and is returned as such so the caller does
        # not fall through to the readLines() text dump at the end of this
        # function.
        .pdf_codebook_lines(path)
      },
      docx = , rtf = , odt = {
        text <- .extract_rich_text(path, ext)
        if (nchar(trimws(text)) < 10) NULL else strsplit(text, "\n")[[1]]
      },
      NULL
    ),
    error = function(e) NULL
  )

  # Rich-text formats hand back a character vector of lines for the LLM tier.
  if (is.character(result) && !is.data.frame(result)) return(result)

  if (!is.null(result) && is.data.frame(result) && nrow(result) > 0) {
    # A parser that already stamped its own method (e.g. parse_qsf -> "qsf")
    # keeps it; otherwise haven files are "haven" and the rest "structured".
    if (!"parse_method" %in% names(result) ||
        all(is.na(result$parse_method)))
      result$parse_method <- if (isTRUE(attr(result, ".is_haven"))) "haven"
                             else "structured"
    attr(result, ".is_haven") <- NULL
    # Scope every definition from this file to the caller-supplied study group
    # (see the `group` parameter doc above) -- every extractor above sets its own
    # `group` column to NA_character_ internally, so stamping it here in ONE
    # place, after dispatch, is simpler than threading `group` through each of
    # them individually.
    if ("group" %in% names(result) && !is.na(group) && nzchar(group))
      result$group <- group
    return(result)
  }

  # No structured definitions: return raw lines so the caller can try the LLM.
  tryCatch(readLines(path, warn = FALSE), error = function(e) NULL)
}

# -- Qualtrics survey-definition (.qsf) parsing --------------------------------
# A .qsf is JSON: one object with SurveyEntry (survey metadata) and
# SurveyElements (an array of elements). The question elements have
# Element == "SQ"; their Payload carries the wording and response options. The
# exported data column name is the Payload's DataExportTag; matrix / multi-answer
# questions expand into one column per Choice, named <DataExportTag>_<choiceKey>
# (an explicit ChoiceDataExportTags overrides the suffix). We reconstruct those
# names so the labels join data_check's columns, but the reconstruction is not
# guaranteed to match every export version's headers (Qualtrics has inserted an
# "x" into some matrix column names -- see ropensci/qualtRics#144); a row that
# finds no matching data column simply contributes no label. Everything here is
# deterministic -- no LLM, no network, jsonlite only.

# Strip HTML tags / entities from Qualtrics display text to plain prose.
.qsf_strip_html <- function(x) {
  if (is.null(x) || length(x) == 0) return(NA_character_)
  x <- as.character(x)[1]
  if (is.na(x)) return(NA_character_)
  x <- gsub("<[^>]+>", " ", x)          # tags
  x <- gsub("&nbsp;", " ", x, fixed = TRUE)
  x <- gsub("&amp;", "&", x, fixed = TRUE)
  x <- gsub("&lt;", "<", x, fixed = TRUE)
  x <- gsub("&gt;", ">", x, fixed = TRUE)
  x <- gsub("[[:space:]]+", " ", x)     # collapse whitespace (incl. newlines)
  x <- trimws(x)
  if (nzchar(x)) x else NA_character_
}

# The display text of a Qualtrics Choices / Answers option object. Each option is
# keyed by its code and holds a `Display` string (occasionally a nested list).
.qsf_option_display <- function(opt) {
  d <- opt$Display %||% opt$display %||% NULL
  if (is.list(d)) d <- unlist(d, use.names = FALSE)[1]
  .qsf_strip_html(d)
}

# Encode a Choices- or Answers-style option list (code -> option object) as the
# same value-labels JSON haven uses ({"1":"Strongly disagree",...}), so it flows
# through the identical codebook / OSD machinery.
.qsf_value_labels <- function(opts) {
  if (is.null(opts) || !length(opts)) return(NA_character_)
  codes  <- names(opts)
  labels <- vapply(opts, .qsf_option_display, character(1))
  .encode_value_labels(codes, labels)
}

# Reconstruct the export column name for one choice of a matrix / multi-answer
# question, matching exactly what Qualtrics writes to the CSV header. `tag` is
# the question DataExportTag ("SV", "POWER.PP1"); `choice_tag` is that choice's
# ChoiceDataExportTag when present (else NA); `code` is the raw choice key.
# Qualtrics stores the choice tag in one of two shapes:
#   (a) the FULL column name Qualtrics exports for this choice -- which may or may
#       not share the question's prefix. Examples: "SV_1" (shares stem "SV"),
#       "POWER.PP1_1" (shares "POWER.PP1"), and -- critically -- "STATUS.PP1_8"
#       for a question tagged "POWER.PP1" (a matrix whose choice tags carry a
#       DIFFERENT alpha stem than the question). All three ARE the CSV column.
#   (b) a bare per-choice suffix: a pure number ("1", "12") or a short reserved
#       token ("TEXT"), which must be appended to the question tag: "Q3_1".
# The old rule only recognised (a) when the choice tag started with the question
# tag, so a differently-stemmed choice tag ("STATUS.PP1_8") was wrongly prefixed
# to "POWER.PP1_STATUS.PP1_8" and never matched the data. Correct test: a choice
# tag is a FULL column name when it carries its OWN alphabetic stem (a letter
# followed later by a separator), i.e. it is more than a bare numeric/reserved
# suffix. Then use it verbatim; otherwise append it to the question tag.
.qsf_export_col <- function(tag, choice_tag, code) {
  tag <- trimws(as.character(tag))
  if (!is.na(choice_tag) && nzchar(choice_tag)) {
    ct <- trimws(choice_tag)
    # (a1) shares the question's stem -> full column name, verbatim.
    if (startsWith(tolower(ct), tolower(paste0(tag, "_"))) ||
        identical(tolower(ct), tolower(tag)))
      return(ct)
    # (a2) carries its OWN alpha stem then a separator (STATUS.PP1_8, Q17_2):
    # this IS the exported column name even though it does not share `tag`.
    if (grepl("[A-Za-z].*[._-]", ct, perl = TRUE))
      return(ct)
    # (b) a bare numeric/reserved suffix -> append to the question tag.
    return(paste0(tag, "_", ct))
  }
  paste0(tag, "_", code)
}

#' Parse a Qualtrics survey-definition file (.qsf) into codebook variables
#'
#' Reads the survey object's questions and returns one row per data column the
#' export would produce, carrying the item wording and the coded response
#' options -- the same shape as the other `parse_codebook()` back-ends, so the
#' rows join the codebook / scale pipeline unchanged. The `group` column holds
#' each question's `DataExportTag` stem, a high-confidence scale-block signal.
#'
#' Column-name reconstruction: simple questions use the `DataExportTag`; matrix
#' and multi-answer questions expand to `<DataExportTag>_<choiceCode>` (honouring
#' `ChoiceDataExportTags` when present). For a matrix (Likert) question the
#' `Choices` are the items (one column each) and the `Answers` are the shared
#' response scale points (the value labels); for single/multiple choice the
#' `Choices` are themselves the value labels.
#'
#' @param path path to a `.qsf` file
#'
#' @returns a data.frame of variable definitions (`codebook_variable`, `label`,
#'   `codebook_source`, `group`, `value_labels`, `missing_values`, `question`,
#'   `parse_method`), or `NULL` when the file is not a parseable QSF
#'   or yields no questions.
#' @export
#' @keywords internal
parse_qsf <- function(path) {
  if (!file.exists(path)) return(NULL)
  src <- basename(path)
  j <- tryCatch(
    jsonlite::fromJSON(readChar(path, file.info(path)$size, useBytes = TRUE),
                       simplifyVector = FALSE),
    error = function(e) NULL)
  if (is.null(j) || is.null(j$SurveyElements)) return(NULL)

  sq <- Filter(function(e) identical(e$Element, "SQ"), j$SurveyElements)
  if (!length(sq)) return(NULL)

  rows <- list()
  # `label` is the concise per-column wording (the matrix item, the choice), used
  # for matching and OSD translations; `question` is the full question stem
  # (shared across a matrix's items). For a simple question the two coincide.
  # `scale_group` is the DataExportTag stem -- the authoritative scale-block
  # signal. It is deliberately NOT the `group` column: `group` means the
  # experiment/study scope to match_column_labels, and the stem is not that.
  add <- function(var, label, scale_group, value_labels = NA_character_,
                  question = NULL) {
    var <- trimws(as.character(var))
    if (!nzchar(var)) return(invisible())
    rows[[length(rows) + 1L]] <<- data.frame(
      codebook_variable = var,
      label             = label %||% NA_character_,
      codebook_source   = src,
      group             = NA_character_,
      value_labels      = value_labels %||% NA_character_,
      missing_values    = NA_character_,
      question          = question %||% label %||% NA_character_,
      coding_instructions = NA_character_,
      scale_group       = scale_group %||% NA_character_
    )
  }

  for (e in sq) {
    p <- e$Payload
    if (is.null(p)) next
    tag <- p$DataExportTag %||% p$QuestionID %||% NULL
    if (is.null(tag) || !nzchar(trimws(as.character(tag)))) next
    tag <- trimws(as.character(tag))
    qtext <- .qsf_strip_html(p$QuestionText)
    qtype <- as.character(p$QuestionType %||% "")
    selector <- as.character(p$Selector %||% "")
    choices <- p$Choices
    answers <- p$Answers
    # Per-choice export tags carry the export column suffix. Build the full
    # export column name from the question tag and the choice tag exactly as
    # Qualtrics writes it to the CSV. Qualtrics stores ChoiceDataExportTags in
    # one of two shapes: the FULL column name already including the stem
    # ("SV_1", "POWER.PP1_1") or a bare per-choice suffix ("1"). The previous
    # code always did paste0(tag, "_", choicetag), which doubled the stem for
    # the first shape ("SV" + "SV_1" -> "SV_SV_1") so those columns never
    # matched the data. .qsf_export_col() reconciles both shapes: when the
    # choice tag already begins with the question tag it IS the column name;
    # otherwise the tag is prepended. Bare numeric codes fall back to tag_code.
    ctags <- p$ChoiceDataExportTags
    export_col <- function(code) {
      ct <- if (is.list(ctags) && !is.null(ctags[[code]]) &&
                nzchar(trimws(as.character(ctags[[code]]))))
        trimws(as.character(ctags[[code]])) else NA_character_
      .qsf_export_col(tag, ct, code)
    }

    is_matrix <- grepl("matrix", qtype, ignore.case = TRUE)
    is_multi  <- grepl("MAVR|MACOL|MSB", selector, ignore.case = TRUE)

    if (is_matrix && !is.null(choices) && length(choices)) {
      # One column per item (Choice); shared response labels come from Answers.
      # label = the item statement; question = the shared stem.
      vl <- .qsf_value_labels(answers)
      for (code in names(choices)) {
        stmt <- .qsf_option_display(choices[[code]])
        add(export_col(code), stmt %||% qtext, tag, vl,
            question = qtext)
      }
    } else if (is_multi && !is.null(choices) && length(choices)) {
      # Check-all-that-apply: one binary column per selectable choice.
      # label = the choice; question = the shared stem.
      for (code in names(choices)) {
        opt <- .qsf_option_display(choices[[code]])
        add(export_col(code), opt %||% qtext, tag,
            question = qtext)
      }
    } else if (!is.null(choices) && length(choices)) {
      # Single-answer choice question: one column, Choices are the value labels.
      add(tag, qtext, tag, .qsf_value_labels(choices))
    } else {
      # Text entry / other: wording only, no coded options.
      add(tag, qtext, tag)
    }
  }

  if (!length(rows)) return(NULL)
  out <- dplyr::bind_rows(rows)
  out$parse_method <- "qsf"
  out
}

# Map free-text experiment context strings to canonical group codes, e.g.
# "Experiment 1" -> "ex1", "Study 2a" -> "ex2a", "Pilot 1" -> "pilot1".
.infer_group <- function(context_str) {
  vapply(context_str, function(s) {
    if (is.null(s) || is.na(s) || !nzchar(trimws(as.character(s))))
      return(NA_character_)
    s <- trimws(as.character(s))
    m <- regmatches(s, regexpr("(?i)pilot\\s*(\\d+[a-z]?)", s, perl = TRUE))
    if (length(m) > 0 && nzchar(m)) {
      num <- sub("(?i)pilot\\s*", "", m, perl = TRUE)
      return(paste0("pilot", tolower(num)))
    }
    m <- regmatches(s, regexpr("(?i)(experiment|study)\\s*(\\d+[a-z]?)", s, perl = TRUE))
    if (length(m) > 0 && nzchar(m)) {
      num <- sub("(?i)(experiment|study)\\s*", "", m, perl = TRUE)
      return(paste0("ex", tolower(num)))
    }
    NA_character_
  }, character(1), USE.NAMES = FALSE)
}

#' Match data columns against codebook variable definitions (rules only)
#'
#' For each column in `columns_df`, find codebook variables whose normalised
#' name matches, respecting experiment-group scoping. Resolves multiple
#' definitions by haven priority, then rule-based label-equivalence
#' (`normalize_label`); genuinely differing labels are flagged
#' `conflicting_definition`. The LLM tiers (fuzzy matching, semantic merge) are
#' applied separately by codebook_check when `llm_use(TRUE)`.
#'
#' @param columns_df data columns to label (needs `paper_id`, `source_file`,
#'   `column_name`; optional `group`/`experiment_group`)
#' @param codebook_vars_df parsed codebook variables (from [parse_codebook()])
#'
#' @returns a data.frame with one row per input column: `paper_id`,
#'   `source_file`, `column_name`, `group`, `label`, `codebook_variable`,
#'   `label_source`, `label_status`, `label_method`.
#' @export
#' @keywords internal
match_column_labels <- function(columns_df, codebook_vars_df) {
  col_group <- if ("group" %in% names(columns_df)) columns_df$group else
               if ("experiment_group" %in% names(columns_df)) columns_df$experiment_group else
               rep(NA_character_, nrow(columns_df))

  make_empty <- function(status = "unlabelled") {
    data.frame(
      paper_id          = columns_df$paper_id,
      source_file       = columns_df$source_file,
      column_name       = columns_df$column_name,
      group             = col_group,
      label             = NA_character_,
      codebook_variable = NA_character_,
      label_source      = NA_character_,
      label_status      = status,
      label_method      = NA_character_,
      value_labels      = NA_character_,
      missing_values    = NA_character_,
      question          = NA_character_,
      coding_instructions = NA_character_,
      scale_group       = NA_character_
    )
  }

  if (is.null(columns_df) || nrow(columns_df) == 0) return(make_empty())
  if (is.null(codebook_vars_df) || nrow(codebook_vars_df) == 0)
    return(make_empty())

  norm_col <- normalize_varname(columns_df$column_name)

  # Expand range notation (e.g. "V1-V10") into individual variable rows.
  range_pat  <- "^([A-Za-z]*)\\s*(\\d+)\\s*[-\u2013]\\s*(\\d+)$"
  range_rows <- grep(range_pat, codebook_vars_df$codebook_variable, perl = TRUE)
  if (length(range_rows) > 0) {
    expanded <- Filter(Negate(is.null), lapply(range_rows, function(i) {
      parts <- regmatches(
        codebook_vars_df$codebook_variable[i],
        regexec(range_pat, codebook_vars_df$codebook_variable[i], perl = TRUE)
      )[[1]]
      prefix <- parts[2]; start <- as.integer(parts[3]); end <- as.integer(parts[4])
      if (is.na(start) || is.na(end) || start > end) return(NULL)
      row <- codebook_vars_df[i, , drop = FALSE]
      do.call(rbind, lapply(seq(start, end), function(nn) {
        row$codebook_variable <- paste0(prefix, nn)
        row
      }))
    }))
    if (length(expanded) > 0)
      codebook_vars_df <- rbind(
        codebook_vars_df[-range_rows, , drop = FALSE],
        do.call(rbind, expanded)
      )
  }

  norm_var <- normalize_varname(codebook_vars_df$codebook_variable)
  n <- nrow(columns_df)
  label_out <- cbk_var_out <- src_out <- label_method_out <- rep(NA_character_, n)
  status_out <- rep("unlabelled", n)
  # DDI-derived per-variable properties carried from the matched codebook rows.
  vl_out <- mv_out <- q_out <- ci_out <- rep(NA_character_, n)
  # QSF scale-block stem (parse_qsf's scale_group), carried onto the data column.
  sg_out <- rep(NA_character_, n)
  # First non-NA value of a codebook column across the applicable matches (used
  # to carry value_labels/missing_values/question onto the data column).
  first_present <- function(rows, col)
    if (col %in% names(rows)) {
      v <- rows[[col]][!is.na(rows[[col]]) & nzchar(as.character(rows[[col]]))]
      if (length(v) > 0) as.character(v[1]) else NA_character_
    } else NA_character_

  # A codebook variable must belong to the SAME paper as the column it labels.
  # Without this, a column named e.g. "age" or "condition" (extremely common
  # across unrelated studies) would match ANY paper's codebook variable of the
  # same normalised name when this function runs once across a whole
  # paperlist -- columns_df/codebook_vars_df both span every paper in that
  # case, not just one. columns_df always carries paper_id (from data_check),
  # and codebook_check.R stamps it onto codebook_vars_df too -- but this
  # function is also called directly in tests with a minimal codebook_vars_df
  # that has no paper_id column at all, so that case falls back to the
  # pre-fix behaviour (match by name only) rather than silently matching
  # nothing.
  col_paper_id <- columns_df$paper_id %||% rep(NA_character_, n)
  has_var_paper_id <- "paper_id" %in% names(codebook_vars_df)
  var_paper_id <- if (has_var_paper_id) codebook_vars_df$paper_id else NULL

  for (i in seq_len(n)) {
    name_idx <- if (has_var_paper_id) {
      which(norm_var == norm_col[i] & var_paper_id == col_paper_id[i])
    } else {
      which(norm_var == norm_col[i])
    }
    if (length(name_idx) == 0) next
    cg <- col_group[i]

    matches  <- codebook_vars_df[name_idx, , drop = FALSE]
    scoped   <- matches[!is.na(matches$group), , drop = FALSE]
    unscoped <- matches[ is.na(matches$group), , drop = FALSE]
    same_group   <- scoped[!is.na(scoped$group) & scoped$group == cg, , drop = FALSE]
    applicable   <- rbind(unscoped, same_group)
    other_scoped <- scoped[!is.na(scoped$group) & scoped$group != cg, , drop = FALSE]

    if (nrow(applicable) == 0) {
      if (nrow(other_scoped) > 0) {
        status_out[i]  <- "ambiguous_experiment"
        label_out[i]   <- paste(unique(other_scoped$label), collapse = " | ")
        cbk_var_out[i] <- paste(unique(other_scoped$codebook_variable), collapse = " | ")
        src_out[i]     <- paste(unique(other_scoped$codebook_source), collapse = " | ")
      }
      next
    }

    distinct_labels <- unique(applicable$label)
    if (length(distinct_labels) > 1) {
      haven_rows <- if ("parse_method" %in% names(applicable))
        applicable[!is.na(applicable$parse_method) &
                     applicable$parse_method == "haven", , drop = FALSE] else
        applicable[0, , drop = FALSE]
      norm_labels <- normalize_label(distinct_labels)
      if (nrow(haven_rows) > 0) {
        status_out[i]       <- "labelled"
        label_out[i]        <- haven_rows$label[which.max(nchar(haven_rows$label))]
        cbk_var_out[i]      <- haven_rows$codebook_variable[1]
        src_out[i]          <- paste(unique(haven_rows$codebook_source), collapse = " | ")
        label_method_out[i] <- "haven_priority"
      } else if (length(unique(norm_labels)) == 1) {
        status_out[i]       <- "labelled"
        label_out[i]        <- distinct_labels[which.max(nchar(distinct_labels))]
        cbk_var_out[i]      <- applicable$codebook_variable[1]
        src_out[i]          <- paste(unique(applicable$codebook_source), collapse = " | ")
        label_method_out[i] <- "merged_rules"
      } else {
        status_out[i]  <- "conflicting_definition"
        label_out[i]   <- paste(distinct_labels, collapse = " | ")
        cbk_var_out[i] <- paste(unique(applicable$codebook_variable), collapse = " | ")
        src_out[i]     <- paste(unique(applicable$codebook_source), collapse = " | ")
      }
    } else {
      status_out[i]  <- "labelled"
      label_out[i]   <- distinct_labels[1]
      cbk_var_out[i] <- applicable$codebook_variable[1]
      src_out[i]     <- paste(unique(applicable$codebook_source), collapse = " | ")
    }

    # Carry the DDI-derived properties from the matched codebook rows onto the
    # data column (independent of which label won: a variable's code list /
    # question is the same whichever source described it).
    vl_out[i]   <- first_present(applicable, "value_labels")
    mv_out[i]   <- first_present(applicable, "missing_values")
    q_out[i]    <- first_present(applicable, "question")
    ci_out[i]   <- first_present(applicable, "coding_instructions")
    sg_out[i]   <- first_present(applicable, "scale_group")
  }

  # -- Deterministic .qsf QUESTION-TAG fallback --------------------------------
  # A Qualtrics matrix / text-entry question exports one column per cell, and the
  # export cell naming does not always match the .qsf's reconstructed item name
  # (a text-entry matrix Q1914 exports Q1914_1_1, Q1914_2_1, ... while the .qsf
  # records Q1914_4, Q1914_5). Exact-name matching then leaves those columns
  # unlabelled. But the .qsf still tells us, with CERTAINTY, which QUESTION the
  # column belongs to: its `scale_group` is the question's DataExportTag, and the
  # column name begins with that tag. So we match on the question TAG (a prefix
  # relationship), not the exact item name, and assign the QUESTION-level label
  # and value labels. This never guesses a specific wrong item -- it claims only
  # the question, which the shared tag makes certain. Uses the LONGEST matching
  # .qsf tag so a column is attributed to the most specific question. Runs only on
  # still-unlabelled columns and only against .qsf-sourced scale_group tags.
  #
  # EXCEPT: Qualtrics also auto-exports per-question PARADATA columns under the
  # same tag prefix -- <tag>_First.Click / _Last.Click / _Page.Submit /
  # _Click.Count record response TIMING in seconds, not an answer. They are not
  # "the question" in any sense the label/value_labels could correctly describe
  # (claiming the question's response codes, e.g. a 1-4 Likert range, onto a
  # column of continuous seconds produces a false "out of range" flag on nearly
  # every value downstream in data_check_scale_values()). These suffixes are
  # Qualtrics-reserved and never legitimate item suffixes, so they are excluded
  # from the tag match entirely rather than mislabelled.
  qsf_paradata_re <- "_(First\\.Click|Last\\.Click|Page\\.Submit|Click\\.Count)$"
  qsf_tags <- if (all(c("scale_group", "question") %in% names(codebook_vars_df))) {
    keep <- if ("parse_method" %in% names(codebook_vars_df))
      !is.na(codebook_vars_df$parse_method) &
        codebook_vars_df$parse_method == "qsf" else
      rep(TRUE, nrow(codebook_vars_df))
    sg <- codebook_vars_df$scale_group
    keep <- keep & !is.na(sg) & nzchar(sg)
    unique(sg[keep])
  } else character(0)

  if (length(qsf_tags) > 0) {
    # First codebook row per tag carries the question wording / value labels.
    tag_row <- match(qsf_tags, codebook_vars_df$scale_group)
    # Try longest tags first so the most specific question wins.
    ord <- order(nchar(qsf_tags), decreasing = TRUE)
    for (i in which(status_out == "unlabelled")) {
      cn <- columns_df$column_name[i]
      if (grepl(qsf_paradata_re, cn, perl = TRUE, ignore.case = TRUE)) next
      hit <- NA_integer_
      for (t in ord) {
        tg <- qsf_tags[t]
        if (startsWith(cn, paste0(tg, "_")) || identical(cn, tg)) { hit <- t; break }
      }
      if (is.na(hit)) next
      r <- tag_row[hit]
      status_out[i]       <- "labelled"
      label_out[i]        <- codebook_vars_df$question[r] %||% codebook_vars_df$label[r]
      cbk_var_out[i]      <- qsf_tags[hit]
      src_out[i]          <- codebook_vars_df$codebook_source[r]
      label_method_out[i] <- "qsf_question_tag"
      vl_out[i]   <- codebook_vars_df$value_labels[r]
      q_out[i]    <- codebook_vars_df$question[r]
      sg_out[i]   <- qsf_tags[hit]
    }
  }

  label_method_out[status_out == "labelled" & is.na(label_method_out)] <- "rules"

  data.frame(
    paper_id          = columns_df$paper_id,
    source_file       = columns_df$source_file,
    column_name       = columns_df$column_name,
    group             = col_group,
    label             = label_out,
    codebook_variable = cbk_var_out,
    label_source      = src_out,
    label_status      = status_out,
    label_method      = label_method_out,
    value_labels      = vl_out,
    missing_values    = mv_out,
    question          = q_out,
    coding_instructions = ci_out,
    scale_group       = sg_out
  )
}

# -- Data-quality checks (native, used by data_validate) -----------------------
#
# Clean-room reimplementations of common data-screening checks. Each returns a
# list(problem = <logical>, message = <chr>, values = <flagged values or NULL>),
# so callers can treat them uniformly. Deliberately dependency-free base R (the
# equivalent checks in the dataReporter package are GPL-2 and pull in
# robustbase + an S3 framework; the logic itself is small, so we own it here).

# Conventional numeric codes that disguise missingness in shared data. These are
# only ever flagged when they sit OUTSIDE the column's real data (a scale's valid
# range, or far from the bulk) -- the list nominates candidates; the "detached
# from the data" test in data_check_scale_values decides. So codes that are
# plausible real values (97 in a 0-100 score, 99 in an age) do not fire unless
# they are genuinely out of place.
#
# Three real-world families, scaled by field width:
#   - 9x-block: consecutive high codes for don't-know / refused / not-applicable
#     (memisc 97/98/99; SPSS defaults; many social-science surveys)
#   - repeated-digit 8- and 7-families (Statistics Canada, WVS: 8=DK, 7=skip)
#   - extreme repeated placeholders at wide widths
# Deliberately EXCLUDED: single digits 7/8/9 (valid Likert points -- the scale
# detector catches an out-of-range 9 in context), -1 (very often a legitimate
# score, e.g. a difference score or a bipolar scale point), and single-digit
# negatives -7/-8/-9 (legitimate values on bipolar -k..k rating scales).
# Only -99 and -999 are kept from the negative family: they are the two forms
# actually attested as common user codings (SPSS/Stata guidance); the wider and
# 9x/8x negative variants had no source and are omitted.
.data_missing_sentinels <- c(
  # 9x-block (don't know / refused / not applicable)
  97, 98, 99,
  997, 998, 999,
  9997, 9998, 9999,
  99997, 99998, 99999,
  # repeated-digit 8- and 7-families
  88, 888, 8888, 88888,
  77, 777, 7777, 77777,
  # extreme repeated placeholders at wide field widths
  999999, 888888, 777777, 99999999,
  # the two attested negative codings
  -99, -999
)

# Could an out-of-scale value `v` be a keying TYPO of an in-scale value? Returns
# the most plausible intended value (inside [lo, hi]) or NA. Covers the common
# fat-finger patterns: a repeated digit (33 -> 3, 55 -> 5), a doubled/trailing
# digit (25 -> 2 or 5), a dropped/added minus, or an extra leading digit.
.scale_typo_of <- function(v, lo, hi) {
  if (is.na(v) || v %in% lo:hi) return(NA_integer_)
  cand <- integer(0)
  av <- abs(v)
  # The digit-manipulation candidates below (each single digit; drop the
  # leading/trailing digit) only make sense for an INTEGER-valued typo (a `33`
  # keyed instead of `3`) -- restricted to whole-number v, since as.character()
  # on a non-integer includes the literal decimal point ("3.5" -> "3", ".",
  # "5"), and as.integer(".") throws "NAs introduced by coercion". Confirmed as
  # a real, reachable warning: data_check_scale_values() passes every
  # out-of-range value here regardless of whether it is a whole number, so a
  # column with a stray decimal value (e.g. 3.5 on an otherwise-integer 1-7
  # scale) hit this on every run.
  if (v == round(v)) {
    ds <- strsplit(as.character(av), "")[[1]]
    if (length(ds) >= 2) {
      # each single digit (33->3, 25->2 or 5, 105->1/0/5)
      cand <- c(cand, as.integer(ds))
      # drop the leading digit (25 -> 5, 105 -> 5), drop the trailing (25 -> 2)
      cand <- c(cand, as.integer(substring(as.character(av), 2)),
                as.integer(substring(as.character(av), 1, nchar(as.character(av)) - 1)))
    }
  }
  # sign flip (a -3 typed on a 1..7 scale, or a 3 that should be -3 on a bipolar)
  # -- still meaningful for a non-integer v, so not gated above.
  cand <- c(cand, -v)
  cand <- unique(cand[!is.na(cand)])
  inside <- cand[cand >= lo & cand <= hi]
  if (!length(inside)) return(NA_integer_)
  # prefer the candidate closest to v's magnitude order (single digit of v)
  inside[which.min(abs(inside - (av %% 10)))]
}

#' Flag values that fall outside a rating scale's valid range
#'
#' A rating scale (Likert / rating item) has a small set of consecutive valid
#' integer levels. Any value outside that set is a data problem, and this check
#' both flags it and, for each value, offers the most likely explanation:
#' \itemize{
#'   \item a **missing-data code** left as a number (a `-99` / `999` in the
#'     sentinel list, or a codebook-**declared** missing code) -- recode to `NA`;
#'   \item a **keying typo** of an in-scale value (a `33` for `3`, a `55` for
#'     `5`) -- the probable intended value is named;
#'   \item otherwise an **unexplained** out-of-range value to review.
#' }
#' The valid range is ground truth when `valid_values` / `valid_range` are
#' supplied (e.g. from a codebook), otherwise inferred by `.detect_likert_scale`.
#' A column that is not a rating scale (continuous, many-level, non-integer, too
#' few rows) has no fixed range and is not flagged here -- unbounded variables
#' (age, reaction time) have no principled "valid range" to violate.
#'
#' Ground truth is trusted only when it is actually PLAUSIBLE for this column's
#' data: at least `min_ground_truth_coverage` of the non-missing values must
#' already fall inside the declared range. A codebook/`.qsf` variable can be
#' mismatched to the wrong data column (a cross-study name collision, or a
#' Qualtrics timing/paradata column inheriting its question's response codes --
#' see the `.qsf` question-tag fallback in `match_column_labels()`); when that
#' happens the declared range explains almost none of the data, and trusting it
#' anyway turns nearly every real value into a false "out of range" flag. When
#' coverage is too low, ground truth is DISCARDED for this column (not merely
#' widened) and the range falls back to `.detect_likert_scale` inference, same
#' as if no ground truth had been supplied at all.
#'
#' This unifies the former `data_check_out_of_range` and
#' `data_check_miscoded_missing`: one detector run, one finding per column.
#'
#' @param x a numeric vector
#' @param sentinels candidate missing-data sentinel codes
#' @param declared optional codebook-declared missing codes (ground truth)
#' @param valid_values optional enumerated valid codes (ground truth)
#' @param valid_range optional `c(lo, hi)` valid range (ground truth)
#' @param n_max max number of values to list in the message
#' @param min_ground_truth_coverage minimum fraction of non-missing values that
#'   must already fall inside a declared `valid_values`/`valid_range` for it to
#'   be trusted; below this, ground truth is discarded in favour of inference
#' @returns list(problem, message, values, lower, upper, classes) where `classes`
#'   labels each flagged value "missing", "typo:<intended>", or "unexplained"
#' @export
#' @keywords internal
data_check_scale_values <- function(x, sentinels = .data_missing_sentinels,
                                    declared = NULL, valid_values = NULL,
                                    valid_range = NULL, n_max = 10,
                                    min_ground_truth_coverage = 0.5) {
  none <- list(problem = FALSE, message = "", values = NULL,
               lower = NA_real_, upper = NA_real_, classes = character(0))
  if (!is.numeric(x)) return(none)
  xv <- x[!is.na(x) & !is.nan(x) & is.finite(x)]
  if (length(xv) == 0) return(none)

  # -- Establish the valid range: ground truth, else inferred scale ------------
  if (!is.null(valid_values) && length(valid_values)) {
    vv <- sort(unique(as.numeric(valid_values)))
    vv <- vv[is.finite(vv)]
    if (length(vv) == 0) return(none)

    # Some codebooks carry only the labeled anchor points of a bounded rating
    # scale (e.g., 1/9 for a 1-9 semantic differential, or 1/5/9 when a middle
    # point is also labeled) rather than every level. If interior integer
    # values not in `vv` are actually present in the data, interpret that as a
    # contiguous scale range rather than a literal discrete code set -- otherwise
    # every unlisted interior value (2, 3, 4, 6, 7, 8) gets flagged as "outside"
    # a range whose own reported bounds (lo/hi = min/max(vv)) contain it.
    vv_int <- all(vv == round(vv))
    if (vv_int && length(vv) >= 2L && diff(range(vv)) >= 2L) {
      x_int <- sort(unique(xv[xv == round(xv)]))
      has_interior <- any(x_int > min(vv) & x_int < max(vv) & !(x_int %in% vv))
      valid_set <- if (has_interior) seq.int(min(vv), max(vv)) else vv
    } else {
      valid_set <- vv
    }

    lo <- min(valid_set); hi <- max(valid_set)

    # For contiguous non-negative rating scales, anchor a sparsely observed
    # floor to the natural start (0 when 0 is observed, otherwise 1).
    is_contig_int <- all(valid_set == round(valid_set)) &&
      length(valid_set) >= 2L && all(diff(valid_set) == 1)
    if (is_contig_int && lo >= 0) {
      x_int <- sort(unique(xv[xv == round(xv)]))
      natural_floor <- if (0 %in% x_int) 0 else 1
      if (lo > natural_floor && lo <= natural_floor + 2L) {
        lo <- natural_floor
        valid_set <- seq.int(lo, hi)
      }
    }
  } else if (!is.null(valid_range) && length(valid_range) == 2 &&
             all(is.finite(valid_range))) {
    lo <- min(valid_range); hi <- max(valid_range)
    valid_set <- lo:hi
  } else {
    lo <- hi <- valid_set <- NULL   # signals "run inference below"
  }

  # -- Ground truth must actually explain most of the data ---------------------
  # A declared range that covers only a small slice of the column is very likely
  # attached to the WRONG column (see the function doc), not evidence that most
  # of the data is broken. Discard it and fall back to inference rather than
  # flagging the majority of the column.
  if (!is.null(valid_set) && mean(xv %in% valid_set) < min_ground_truth_coverage)
    lo <- hi <- valid_set <- NULL

  if (is.null(valid_set)) {
    sc <- .detect_likert_scale(xv)
    if (is.null(sc)) return(none)          # not a scale -> no range to violate
    lo <- sc$lo; hi <- sc$hi
    valid_set <- lo:hi
  }

  out <- sort(unique(xv[!(xv %in% valid_set)]))
  if (length(out) == 0)
    return(list(problem = FALSE, message = "", values = NULL,
                lower = lo, upper = hi, classes = character(0)))

  # -- Classify each out-of-scale value ----------------------------------------
  declared_num <- if (!is.null(declared)) as.numeric(declared) else numeric(0)
  classify <- function(v) {
    if (v %in% declared_num) return("missing")
    if (v %in% sentinels)    return("missing")
    typo <- .scale_typo_of(v, lo, hi)
    if (!is.na(typo))        return(paste0("typo:", typo))
    "unexplained"
  }
  classes <- vapply(out, classify, character(1))

  describe <- function(v, cls) {
    if (cls == "missing") sprintf("%s (looks like a missing-data code -> recode to NA)", v)
    else if (startsWith(cls, "typo:"))
      sprintf("%s (looks like a typo of %s)", v, sub("^typo:", "", cls))
    else sprintf("%s (outside the scale, cause unclear)", v)
  }
  shown_i <- seq_len(min(length(out), n_max))
  parts <- vapply(shown_i, function(i) describe(out[i], classes[i]), character(1))
  msg <- sprintf(
    "%d value%s outside the %d-%d scale: %s%s",
    length(out), plural(length(out)), lo, hi,
    paste(parts, collapse = ", "),
    if (length(out) > n_max) ", ..." else "")
  list(problem = TRUE, message = msg, values = out,
       lower = lo, upper = hi, classes = classes)
}

#' Flag Tukey (IQR) outliers in a numeric vector
#'
#' Values below Q1 - k*IQR or above Q3 + k*IQR. This is the symmetric boxplot
#' rule; a skew-aware (medcouple) variant can be added later.
#'
#' @param x a numeric vector
#' @param k IQR multiplier (default 1.5)
#' @param n_max max number of flagged values to list in the message
#' @returns list(problem, message, values, lower, upper)
#' @export
#' @keywords internal
data_check_outliers <- function(x, k = 1.5, n_max = 10) {
  none <- list(problem = FALSE, message = "", values = NULL,
               lower = NA_real_, upper = NA_real_)
  if (!is.numeric(x)) return(none)
  x <- x[!is.na(x) & !is.nan(x)]
  if (length(x) < 4) return(none)
  qs <- stats::quantile(x, c(0.25, 0.75), names = FALSE)
  iqr <- qs[2] - qs[1]
  if (iqr == 0) return(none)
  lower <- qs[1] - k * iqr
  upper <- qs[2] + k * iqr
  out <- unique(x[x < lower | x > upper])
  if (length(out) == 0)
    return(list(problem = FALSE, message = "", values = NULL,
                lower = lower, upper = upper))
  shown <- utils::head(sort(out), n_max)
  list(problem = TRUE,
       message = sprintf("%d outlier value%s outside [%.3g, %.3g]: %s%s",
                         length(out), plural(length(out)), lower, upper,
                         paste(signif(shown, 4), collapse = ", "),
                         if (length(out) > n_max) ", ..." else ""),
       values = out, lower = lower, upper = upper)
}

#' Flag a constant or near-constant column
#'
#' @param x a vector
#' @param threshold if the most common non-NA value covers at least this
#'   fraction, the column is near-constant
#' @returns list(problem, message, values)
#' @export
#' @keywords internal
data_check_constant <- function(x, threshold = 0.99) {
  x <- x[!is.na(x)]
  if (length(x) == 0)
    return(list(problem = FALSE, message = "", values = NULL, near = FALSE))
  tab <- sort(table(x), decreasing = TRUE)
  top_frac <- tab[[1]] / length(x)
  if (length(tab) == 1)
    return(list(problem = TRUE,
                message = sprintf("Column is constant: every value is \"%s\".",
                                  names(tab)[1]),
                values = names(tab)[1], near = FALSE))
  if (top_frac >= threshold)
    return(list(problem = TRUE,
                message = sprintf("Near-constant: %.0f%% of values are \"%s\".",
                                 100 * top_frac, names(tab)[1]),
                values = names(tab)[1], near = TRUE))
  list(problem = FALSE, message = "", values = NULL, near = FALSE)
}

#' Flag a column with no observed values
#'
#' All values are NA (or, for text, blank/whitespace-only). Such a column
#' usually means a variable that never recorded anything or an export
#' artifact, and it is invisible to [data_check_constant()] which strips NAs.
#'
#' @param x a vector
#' @returns list(problem, message, values)
#' @export
#' @keywords internal
data_check_empty <- function(x) {
  none <- list(problem = FALSE, message = "", values = NULL)
  n <- length(x)
  if (n == 0) return(none)
  filled <- if (is.numeric(x)) !is.na(x) else
    !is.na(x) & nzchar(trimws(as.character(x)))
  if (any(filled)) return(none)
  list(problem = TRUE,
       message = sprintf("Column is empty: all %d value%s %s missing.",
                         n, plural(n), if (n == 1) "is" else "are"),
       values = NULL)
}

#' Does a column name look like an experimental design variable?
#'
#' Matches names built from design/condition tokens (condition, group,
#' treatment, arm, dose, manipulation, intervention), requiring a word
#' boundary so e.g. "charm" does not match "arm". Used to decide whether a
#' constant column is suspicious: a design variable with one value suggests
#' the file was filtered to a single condition before export.
#'
#' @param col a column name
#' @returns logical
#' @export
#' @keywords internal
data_check_design_name <- function(col) {
  grepl("(?i)(^|[._ -])(cond(ition)?|grp|group|treat(ment)?|arm|dose|manip(ulation)?|intervention)([._ -]|[0-9]|$)",
        col, perl = TRUE)
}

#' Flag an SPSS "Select Cases" filter variable
#'
#' SPSS's Select Cases dialog creates a 0/1 variable named `filter_$`
#' (mangled to `filter_.` or `filter_` by some importers). Its presence
#' matters to a re-user either way: if it is constant at 1 the file was
#' saved after deleting unselected cases, so the shared data are a
#' pre-filtered subset; if it varies, the reported analyses likely used only
#' the selected rows and the filter must be re-applied to reproduce them.
#'
#' @param col the column name
#' @param x the column's values
#' @returns list(problem, message, values)
#' @export
#' @keywords internal
data_check_spss_filter <- function(col, x) {
  none <- list(problem = FALSE, message = "", values = NULL)
  if (!grepl("(?i)^filter_[$._]?$", col, perl = TRUE)) return(none)
  v <- suppressWarnings(as.numeric(x))
  v <- v[!is.na(v)]
  if (length(v) == 0) return(none)
  n_sel <- sum(v == 1)
  msg <- if (n_sel == length(v)) {
    "SPSS \"Select Cases\" filter variable: every row is selected (value 1), so the file appears to have been saved after deleting unselected cases -- the shared data are a pre-filtered subset of what was collected."
  } else {
    sprintf("SPSS \"Select Cases\" filter variable: %d of %d rows are selected (value 1). The reported analyses likely used only the selected rows; re-apply this filter to reproduce them.",
            n_sel, length(v))
  }
  list(problem = TRUE, message = msg,
       values = c(selected = n_sel, total = length(v)))
}

#' Flag categorical levels that differ only by letter case
#'
#' e.g. "Male" and "male" -- likely the same category entered inconsistently.
#'
#' @param x a character or factor vector
#' @returns list(problem, message, values)
#' @export
#' @keywords internal
data_check_case_issues <- function(x) {
  none <- list(problem = FALSE, message = "", values = NULL)
  if (is.numeric(x)) return(none)
  x <- as.character(x)
  x <- unique(x[!is.na(x) & nzchar(trimws(x))])
  if (length(x) == 0) return(none)
  lower <- tolower(x)
  dup <- lower[duplicated(lower)]
  if (length(dup) == 0) return(none)
  groups <- vapply(unique(dup), function(l)
    paste(x[lower == l], collapse = "/"), character(1))
  list(problem = TRUE,
       message = sprintf("Categories differing only by case: %s",
                         paste(groups, collapse = "; ")),
       values = groups)
}

#' Flag values with leading or trailing whitespace
#'
#' Padded values (e.g. "Male " vs "Male") silently split a category. Flags the
#' affected values in a character/factor column.
#'
#' @param x a character or factor vector
#' @returns list(problem, message, values)
#' @export
#' @keywords internal
data_check_whitespace <- function(x) {
  none <- list(problem = FALSE, message = "", values = NULL)
  if (is.numeric(x)) return(none)
  x <- as.character(x)
  x <- x[!is.na(x)]
  padded <- unique(x[x != trimws(x) & nzchar(trimws(x))])
  if (length(padded) == 0) return(none)
  list(problem = TRUE,
       message = sprintf("%d value%s with leading/trailing whitespace: %s",
                         length(padded), plural(length(padded)),
                         paste(utils::head(sprintf('"%s"', padded), 10),
                               collapse = ", ")),
       values = padded)
}

#' Flag a mostly-numeric column stored as text
#'
#' When a column read as character is mostly numbers but has a few values that
#' do not parse (e.g. "n/a", ">100", "50 approx"), those dirty cells forced the
#' whole column to text -- a data-quality problem in the source, not a read
#' error. A *fully* numeric text column is not flagged here: the file readers
#' auto-type clean numeric columns, so an all-numeric character column would
#' indicate a reader problem rather than a data problem.
#'
#' @param x a character or factor vector
#' @param threshold minimum fraction of non-empty values that must parse as
#'   numeric for the column to be considered "mostly numeric"
#' @param n_max max number of non-numeric values to list
#' @returns list(problem, message, values)
#' @export
#' @keywords internal
data_check_numeric_in_text <- function(x, threshold = 0.8, n_max = 10) {
  none <- list(problem = FALSE, message = "", values = NULL)
  if (is.numeric(x)) return(none)
  x <- trimws(as.character(x))
  x <- x[!is.na(x) & nzchar(x)]
  if (length(x) < 5) return(none)
  # Treat comma-decimals as numeric too (European formatting).
  num <- suppressWarnings(as.numeric(gsub(",", ".", x, fixed = TRUE)))
  frac_num <- mean(!is.na(num))
  # Mostly-but-not-fully numeric: contamination worth reporting.
  if (frac_num < threshold || frac_num >= 1) return(none)
  bad <- unique(x[is.na(num)])
  list(problem = TRUE,
       message = sprintf("Column is %.0f%% numeric but %d value%s cannot be parsed: %s",
                         100 * frac_num, length(bad), plural(length(bad)),
                         paste(utils::head(sprintf('"%s"', bad), n_max),
                               collapse = ", ")),
       values = bad)
}

#' Flag a problematic column name
#'
#' Column names travel: they become variable names in analysis scripts, chunk
#' labels and figure file names in generated codebooks, and keys in metadata
#' files. A name that contains characters that are illegal in file names
#' (`< > : " / \ | ? *`), control characters (tabs, newlines), leading/trailing
#' whitespace, or that runs to hundreds of characters cannot be used in those
#' places without modification -- tools either fail (e.g. a figure file cannot
#' be created on Windows) or silently rename the variable so it no longer
#' matches the shared data. Good practice is short names built from letters,
#' digits and underscores.
#'
#' Such names typically signal an upstream problem: a file whose header was not
#' parsed as intended (e.g. a whole header line captured as a single "name"),
#' or export settings that leaked formatting into the header.
#'
#' The length threshold is not arbitrary: 64 bytes is the maximum variable-name
#' length SPSS supports
#' (<https://www.ibm.com/docs/en/spss-statistics/32.0.0?topic=view-variable-names>),
#' and SAS and Stata cap names at 32 characters
#' (<https://www.stata.com/manuals/rlimits.pdf>), so a name over 64 characters
#' cannot be imported into any of the three major statistical packages without
#' being renamed -- after which it no longer matches the shared data or its
#' documentation. (DDI-Codebook's `var@name` documentation still notes names
#' are "usually up to eight characters, following the rules of SAS and SPSS" --
#' a legacy of those systems' old limits, not a modern recommendation, so DDI
#' imposes no constraint of its own.)
#'
#' This check only warns; nothing is renamed or dropped. (`convert_codebook()`
#' separately excludes columns whose name would push a generated figure's file
#' path past Windows' 260-character limit -- the one case where a name makes
#' rendering impossible; that budget depends on the output path, so it is
#' computed there, not here.)
#'
#' @param col_name the column name
#' @param max_chars names longer than this are flagged as excessively long;
#'   the default is SPSS's 64-byte maximum variable-name length (see Details)
#' @returns list(problem, message, values)
#' @export
#' @keywords internal
data_check_colname <- function(col_name, max_chars = 64L) {
  none <- list(problem = FALSE, message = "", values = NULL)
  nm <- as.character(col_name)
  if (length(nm) != 1 || is.na(nm)) return(none)

  issues <- character(0)
  # Characters that are illegal in Windows file names (and unsafe everywhere).
  illegal <- regmatches(nm, gregexpr('[<>:"/\\\\|?*]', nm))[[1]]
  # Control characters (tab, newline, carriage return, ...).
  ctrl <- regmatches(nm, gregexpr("[[:cntrl:]]", nm))[[1]]
  if (length(illegal) > 0)
    issues <- c(issues, sprintf("characters not allowed in file names (%s)",
                                paste(unique(sprintf('"%s"', illegal)),
                                      collapse = ", ")))
  if (length(ctrl) > 0)
    issues <- c(issues, sprintf("%d control character%s (tab/newline)",
                                length(ctrl), plural(length(ctrl))))
  if (nm != trimws(nm))
    issues <- c(issues, "leading/trailing whitespace")
  if (nchar(nm) > max_chars)
    issues <- c(issues, sprintf(
      "a length of %d characters (SPSS allows at most 64, SAS and Stata 32, so this name cannot be imported there without renaming)",
      nchar(nm)))
  if (length(issues) == 0) return(none)

  bad_chars <- unique(c(illegal, ctrl))
  list(problem = TRUE,
       message = sprintf(
         "Column name has %s. Such names break when reused as file names or in code; prefer short names of letters, digits and underscores.%s",
         paste(issues, collapse = "; "),
         if (length(bad_chars) > 0)
           " A name like this can also mean the file's header was not parsed as intended." else ""),
       values = if (length(bad_chars) > 0) bad_chars else NULL)
}

#' Flag column names that collide after sanitization
#'
#' Many tools replace the special characters in a variable name with `_` or
#' drop them: R's `make.names()`, SPSS/SAS/Stata on import, and generated
#' codebooks (section ids, figure file names). Two columns whose names differ
#' *only* in special characters -- e.g. the phoneme symbols `t'` and a
#' t-with-diacritic, which both sanitize to `t_` -- therefore become
#' indistinguishable the moment the data leave the original file, and links or
#' merged results silently point at the wrong variable. Identical duplicate
#' names collide trivially and are flagged too.
#'
#' The sanitization mirrored here (every character that is not a Unicode
#' letter or digit becomes `_`) is the one the codebook package uses for its
#' section ids, where collisions surface as pandoc "Duplicate identifier"
#' warnings.
#'
#' @param col_names character vector of a file's column names
#' @returns a named list mapping each colliding column name to a message
#'   (empty list when all names stay distinct)
#' @export
#' @keywords internal
data_check_colname_collisions <- function(col_names) {
  nms <- as.character(col_names)
  key <- gsub("[^\\p{L}\\p{N}]", "_", nms, perl = TRUE)
  out <- list()
  for (k in unique(key[duplicated(key)])) {
    members <- nms[key == k]
    for (i in which(key == k)) {
      others <- unique(members[members != nms[i]])
      out[[nms[i]]] <- sprintf(
        "Column name becomes \"%s\" when special characters are replaced, the same as %s: tools that sanitize names (R's make.names(), SPSS/SAS/Stata import, codebook section links) cannot tell these columns apart.",
        k,
        if (length(others) == 0)
          sprintf("%d other identically named column%s",
                  sum(members == nms[i]) - 1L,
                  plural(sum(members == nms[i]) - 1L))
        else
          paste(sprintf('"%s"', utils::head(others, 5)), collapse = ", "))
    }
  }
  out
}

# -- Personal / disclosure information -----------------------------------------
# These checks flag columns that may hold information that should not be shared
# openly (personally identifiable information, PII). They are intentionally
# conservative -- a hit is a "review this before sharing" prompt, not proof of a
# violation. The value regexes are standard patterns (vendored so metacheck
# takes no dependency); a matched pattern is reported, never the matching value
# itself, so the report does not itself leak the PII.

# Standard value patterns. Following the approach used by mature detectors
# (e.g. Microsoft Presidio), each pattern is classed by how specific it is:
#
#   "specific" patterns (email, IP, SSN, credit card) are distinctive enough
#   that a SINGLE valid match warrants a "review before sharing" flag -- for
#   disclosure, one real email leaking is already a problem, so requiring a
#   fraction of the column would be a dangerous false negative;
#
#   "broad" patterns would collide with ordinary data (dates, codes, long
#   integers), so they would require a FRACTION of the column plus a validation
#   step. No broad pattern is currently enabled (the former phone pattern was
#   removed for false-positives on timestamps).
#
# A raw regex match is necessary but not sufficient: patterns with a validator
# (credit card -> Luhn) must also pass it, which keeps ordinary numbers from
# tripping the flag.
.pii_value_patterns <- list(
  email = list(
    regex = "(?i)\\b[A-Z0-9._%+-]+@[A-Z0-9.-]+\\.[A-Z]{2,}\\b",
    kind  = "specific"),
  # IPv4 with each octet 0-255.
  ip_address = list(
    regex = "\\b(?:(?:25[0-5]|2[0-4]\\d|1?\\d?\\d)\\.){3}(?:25[0-5]|2[0-4]\\d|1?\\d?\\d)\\b",
    kind  = "specific"),
  # US SSN: 3-2-4 with separators, excluding obvious non-SSN (000/666/9xx area,
  # 00 group, 0000 serial).
  ssn = list(
    regex = "\\b(?!000|666|9\\d\\d)\\d{3}-(?!00)\\d{2}-(?!0000)\\d{4}\\b",
    kind  = "specific"),
  # Credit-card-like: 13-16 digits in even 4-digit groups (space/dash) or one
  # unbroken run, not part of a longer digit/decimal string. Must also pass
  # .pii_card_ok(): a real issuer prefix AND the Luhn checksum.
  credit_card = list(
    regex = "(?<![\\d.])(?:\\d{13,16}|\\d{4}[ -]\\d{4}[ -]\\d{4}[ -]\\d{1,4})(?![\\d.])",
    kind  = "specific", validate = ".pii_card_ok")
  # NOTE: a phone pattern was removed. It was "broad" and collided heavily with
  # date/time strings (e.g. Qualtrics StartDate/EndDate timestamps), producing
  # false positives on essentially every survey export, and modern studies
  # rarely collect phone numbers. The remaining value patterns are all
  # "specific" with validators, so a hit is almost always a real identifier.
)

# Luhn checksum: the check most card issuers use.
#
# On its own this is NOT a strong filter. Luhn is a single check digit, so a
# random digit run passes it one time in ten -- measured at 9.7-10.2% across
# random 13/14/15/16-digit strings, and the same for sequential ids, epoch
# millisecond timestamps and MTurk-style numeric ids. Since a credit-card hit
# flags the whole column on ONE match, a column of ~20 long numeric ids would
# trip it by chance alone. Both credit-card flags on a 120-file corpus were
# false positives on millisecond timestamps ("1595957929810"), with no true
# positives. Use .pii_card_ok(), which adds the issuer-prefix test.
.pii_luhn_ok <- function(s) {
  d <- as.integer(strsplit(gsub("[^0-9]", "", s), "")[[1]])
  n <- length(d)
  if (n < 13 || n > 16) return(FALSE)
  d <- rev(d)
  d[seq(2, n, by = 2)] <- d[seq(2, n, by = 2)] * 2
  d[d > 9] <- d[d > 9] - 9
  sum(d) %% 10 == 0
}

# Does this look like a real payment card? Issuer prefix (IIN) AND Luhn.
#
# Every card scheme assigns a fixed leading range, so a number that passes Luhn
# but begins 15959... is not a card whatever its checksum says. Requiring both
# means a coincidental digit run has to satisfy the 1-in-10 checksum AND land in
# one of these narrow ranges, which is what removes the timestamp/id collisions.
# Length is checked per scheme too (Amex is 15 digits, Visa 13 or 16, ...), so a
# 16-digit number starting "34" is rejected rather than counted as Amex.
.pii_card_ok <- function(s) {
  d <- gsub("[^0-9]", "", s)
  n <- nchar(d)
  if (n < 13 || n > 16) return(FALSE)
  p <- function(k) as.integer(substr(d, 1, k))
  scheme <-
    (p(1) == 4 && n %in% c(13, 16)) ||                       # Visa
    (p(2) >= 51 && p(2) <= 55 && n == 16) ||                 # Mastercard
    (p(4) >= 2221 && p(4) <= 2720 && n == 16) ||             # Mastercard (2-series)
    (p(2) %in% c(34, 37) && n == 15) ||                      # American Express
    (p(4) == 6011 && n == 16) ||                             # Discover
    (p(2) == 65 && n == 16) ||                               # Discover
    (p(3) >= 644 && p(3) <= 649 && n == 16) ||               # Discover
    (p(2) %in% c(36, 38) && n %in% c(14, 16)) ||             # Diners Club
    (p(3) >= 300 && p(3) <= 305 && n == 14) ||               # Diners Club (carte blanche)
    (p(4) >= 3528 && p(4) <= 3589 && n == 16)                # JCB
  if (!scheme) return(FALSE)
  .pii_luhn_ok(s)
}


# Column-name tokens that suggest the column identifies a person, even when the
# values look innocuous. Matched case-insensitively against normalised names.
# NOTE: the bare "name" token was removed -- it is a sub-string of many
# non-personal column names (experimentName, trial_name, fileName, videoName,
# conditionName, variable name, ...) and produced mostly false positives. The
# specific person-name compounds below (firstname/lastname/surname/fullname)
# are retained because they reliably indicate a real person's name.
.pii_name_tokens <- c(
  # -- person name --
  "firstname", "lastname", "surname", "fullname",
  # nl / de / fr / es / it / pt / nordic. "nombre" (es) is included even though
  # it is a substring of French "nombreux": this check ASKS FOR REVIEW rather
  # than asserting a breach, and the same trade-off is already accepted for the
  # English tokens ("addressed" and "street_view_task" match "address"/"street").
  "naam", "voornaam", "achternaam", "tussenvoegsel", "geslachtsnaam",
  "vorname", "nachname", "familienname",
  "prenom", "nomdefamille",
  "nombre", "apellido",
  "cognome", "sobrenome", "nomecompleto",
  "fornavn", "etternavn", "efternavn", "fornamn", "sukunimi", "etunimi",
  # -- email / phone --
  "email", "e-mail", "phone", "mobile", "telephone", "fax",
  "phonenumber", "mobilenumber", "cellphone", "cellnumber",
  "epost", "correoelectronico",
  "telefoon", "telefon", "telefono", "telefone", "puhelin",
  "mobiel", "handynummer",
  # Compound phone-number forms. These languages join the words with no
  # separator, so the token has to be listed whole -- splitting cannot recover
  # "telefoon" + "nummer" from "telefoonnummer".
  "telefoonnummer", "mobielnummer", "gsmnummer",
  "telefonnummer", "telefonnummber", "rufnummer", "mobilnummer",
  "numerotelephone", "numerodetelephone", "numeroportable",
  "numerotelefono", "numeromovil", "telefononumero",
  "numerodetelefone", "telemovel",
  "puhelinnumero", "telefonnumer",
  # -- address / postcode --
  "address", "street", "zipcode", "zip", "postcode", "postalcode",
  "adres", "adresse", "direccion", "indirizzo", "endereco", "osoite",
  "postleitzahl", "codepostal", "codigopostal", "postnummer",
  "woonplaats", "wohnort", "straat", "strasse",
  # -- national / government id --
  "ssn", "socialsecurity", "passport", "nationalid", "taxid",
  "bsn", "rijksregisternummer", "sozialversicherungsnummer",
  "numerosecuritesociale", "dni", "codicefiscale", "personnummer",
  "henkilotunnus", "paspoort", "reisepass", "passeport", "pasaporte",
  # Date of birth. HIPAA's Safe Harbor rule treats "all elements of dates
  # (except year) directly related to an individual" as an identifier, naming
  # birth date explicitly, and the statistical-disclosure-control literature
  # (sdcMicro's own guidance) lists date of birth alongside sex and postcode as
  # the classic quasi-identifier combination. Year of birth is included too:
  # Safe Harbor permits keeping the YEAR alone for people under 90, so it is
  # weaker evidence than a full date -- but the SDC literature still calls year
  # of birth highly identifying in combination, and metacheck only surfaces
  # this for review rather than asserting a breach.
  "dob", "dateofbirth", "birthdate", "birthday", "birthyear",
  "yearofbirth", "yob",
  # Non-English forms, matching the multilingual coverage the age/gender
  # patterns already carry (leeftijd/alter, geslacht).
  "geboortedatum", "geboortejaar", "geburtsdatum", "geburtsjahr",
  "datenaissance", "fechanacimiento", "datadinascita",
  # -- technical / financial --
  "ipaddress", "ip", "mac", "creditcard", "iban", "bankaccount",
  "kontonummer", "bankrekening", "ibannummer", "kreditkarte",
  "cartebancaire", "tarjetacredito",
  # -- location --
  "latitude", "longitude", "lat", "lon", "lng", "geolocation", "gps",
  "breitengrad", "laengengrad", "ortsangabe",
  # -- account / handle --
  "username", "userid", "handle", "initials",
  "gebruikersnaam", "benutzername", "initialen"
)

#' Flag values that match a personal-information pattern
#'
#' Scans a column's values for standard PII patterns (email, IP address, SSN,
#' credit-card-like). Reports which pattern matched and how many values, never
#' the matching values themselves (so the report does not leak the PII).
#'
#' All current patterns are *specific*: they flag on a single validated match,
#' because for disclosure one real identifier is already a problem. A raw regex
#' match is necessary but not sufficient -- the credit-card pattern must also pass
#' a Luhn checksum, which keeps ordinary numbers from tripping the flag. (A
#' broad-pattern path with a per-column fraction threshold, `broad_min_frac`, is
#' retained for future patterns but is currently unused.)
#'
#' @param x a vector (coerced to character)
#' @param broad_min_frac for broad patterns, the minimum fraction of non-empty
#'   values that must match for the column to be flagged (currently unused)
#' @returns list(problem, message, values) -- `values` is the matched pattern
#'   name(s), not the data
#' @export
#' @keywords internal
data_check_pii_values <- function(x, broad_min_frac = 0.30) {
  none <- list(problem = FALSE, message = "", values = NULL)
  x <- trimws(as.character(x))
  x <- x[!is.na(x) & nzchar(x)]
  if (length(x) < 3) return(none)

  hits <- character(0)
  for (nm in names(.pii_value_patterns)) {
    spec <- .pii_value_patterns[[nm]]
    m <- grepl(spec$regex, x, perl = TRUE)
    matched <- x[m]
    if (length(matched) == 0) next

    # Validate the matched values, when the pattern has a validator.
    if (!is.null(spec$validate)) {
      vfun <- get(spec$validate, mode = "function")
      matched <- matched[vapply(matched, vfun, logical(1))]
      if (length(matched) == 0) next
    }
    n_valid <- length(matched)
    frac <- n_valid / length(x)

    # Specific patterns: a single validated match is enough (a leaked email is
    # already a disclosure). Broad patterns: require a fraction of the column.
    flag <- if (identical(spec$kind, "specific")) n_valid >= 1
            else frac >= broad_min_frac
    if (flag)
      hits <- c(hits, sprintf("%s (%d value%s, %.0f%%)", nm, n_valid,
                              plural(n_valid), 100 * frac))
  }
  if (length(hits) == 0) return(none)
  list(problem = TRUE,
       message = sprintf("Values look like personal information: %s. Review before sharing.",
                         paste(hits, collapse = "; ")),
       values = sub(" .*$", "", hits))
}

# Split a column name into word tokens. This is the whole point of the PII
# name check working at all: the previous version stripped every separator
# ("phone_number" -> "phonenumber") and then substring-matched, which cannot
# tell a name that IS a token from a name that merely CONTAINS one. That is why
# "microphone", "headphones", "phoneme", "phonetic", "saxophone", "automobile"
# and "addressed" all flagged as personal information.
#
# Splits on punctuation/underscores AND camelCase, so "RecipientFirstName"
# becomes recipient/first/name and "IPAddress" becomes ip/address.
.pii_split_name <- function(x) {
  # camelCase boundary: phoneNumber -> phone Number
  a <- gsub("(?<=[a-z0-9])(?=[A-Z])", " ", x, perl = TRUE)

  # Where a run of capitals ends is genuinely ambiguous, and no single rule gets
  # both of these right:
  #   IPAddress  -> the run ends BEFORE a capitalised word  ("IP" + "Address")
  #   ZIPcode    -> the run ends BEFORE lowercase           ("ZIP" + "code")
  # Reading either one alone mis-splits the other ("ZI"+"Pcode", "IPA"+"ddress"),
  # so both readings are produced and a token only has to appear in ONE of them.
  # The spurious single letters this leaves ("r", "ecipient") are harmless: no
  # PII token is one character long.
  s1 <- gsub("(?<=[A-Z])(?=[A-Z][a-z])", " ", a, perl = TRUE)
  s2 <- gsub("(?<=[A-Z])(?=[a-z])",      " ", a, perl = TRUE)

  p <- c(strsplit(tolower(s1), "[^a-z0-9]+")[[1]],
         strsplit(tolower(s2), "[^a-z0-9]+")[[1]])
  unique(p[nzchar(p)])
}

#' Flag a column whose name suggests personal information
#'
#' Matches a column name against tokens that typically identify a person (name,
#' email, address, date of birth, national id, ip, coordinates, ...) in English
#' and the main European languages. Complements [data_check_pii_values()]:
#' catches identifying columns whose values look ordinary (e.g. a
#' `participant_name` free-text field).
#'
#' Matching is WORD-based, not substring-based. The name is split into words on
#' separators and camelCase, and a word must EQUAL a token. Three additions keep
#' real names matching:
#'
#' * adjacent words are re-joined and tested ("first_name" -> "firstname"),
#'   because the token list holds compounds rather than the bare word "name",
#'   which would otherwise match `trial_name` / `file_name` / `condition_name`;
#' * the whole separator-free name is tested, so languages that compound without
#'   separators still match ("telefoonnummer", "geboortedatum");
#' * matching is case-insensitive throughout.
#'
#' Measured against 2113 real column names and a decoy set of ordinary research
#' vocabulary, this cut false positives from 14 to 2 while missing nothing the
#' previous substring rule caught.
#'
#' @param col_name the column name
#' @returns list(problem, message, values)
#' @export
#' @keywords internal
data_check_pii_name <- function(col_name) {
  none <- list(problem = FALSE, message = "", values = NULL)
  if (is.null(col_name) || is.na(col_name) || !nzchar(col_name)) return(none)

  words <- .pii_split_name(col_name)
  norm  <- gsub("[^a-z0-9]", "", tolower(col_name))
  if (!length(words) && !nzchar(norm)) return(none)

  cand <- words
  # Adjacent joins: "first name" -> "firstname", "date of birth" ->
  # "dateofbirth". Pairs and triples cover every compound in the token list.
  if (length(words) >= 2)
    cand <- c(cand, vapply(seq_len(length(words) - 1),
                           function(i) paste0(words[i], words[i + 1]),
                           character(1)))
  if (length(words) >= 3)
    cand <- c(cand, vapply(seq_len(length(words) - 2),
                           function(i) paste0(words[i], words[i+1], words[i+2]),
                           character(1)))
  # The full name with separators removed, for no-separator compounds.
  if (nzchar(norm)) cand <- c(cand, norm)

  matched <- unique(cand[cand %in% .pii_name_tokens])
  if (!length(matched)) return(none)

  list(problem = TRUE,
       message = sprintf("Column name suggests personal information (matched: %s). Review before sharing.",
                         paste(matched, collapse = ", ")),
       values = matched)
}

#' Flag a column that holds geographic coordinates
#'
#' A precise coordinate pins a participant to a place, so it is disclosure risk
#' even when every other column is anonymous. Detected from the column NAME
#' (word-split, so `LocationLatitude` and `gps_lat` are recognised, not only a
#' bare `lat`), then confirmed two ways.
#'
#' Confirmation matters because the name alone is ambiguous: in psychology data
#' `lat` is as likely to be latency or a lateralisation index as latitude, and
#' `lon` can be a loneliness scale. Two requirements filter those out:
#'
#' * the values must lie inside the coordinate range (+/-90 for a latitude, +/-180
#'   for a longitude), which rejects a latency in milliseconds; and
#' * a matching PARTNER column must exist in the same file -- a latitude needs a
#'   longitude beside it. A real coordinate is always a pair, whereas a latency,
#'   a loneliness score or a Latin-square code never has a `lon` sibling. This
#'   is what separates them, since the coordinate ranges are so wide that almost
#'   any bounded measurement fits inside them.
#'
#' `gps`, `geolocation` and `coordinate` name a coordinate outright and need no
#' partner.
#'
#' @param col_name the column name
#' @param x the column's values
#' @param sibling_names the other column names in the same file, used to find
#'   the partner column. When `NULL` (a caller checking one column with no file
#'   context) the partner requirement is skipped, so the check behaves as it did
#'   before -- name plus value range.
#' @returns list(problem, message, values)
#' @export
#' @keywords internal
data_check_pii_geo <- function(col_name, x, sibling_names = NULL) {
  none <- list(problem = FALSE, message = "", values = NULL)

  words <- .pii_split_name(col_name %||% "")
  if (!length(words)) return(none)

  lat_words <- c("lat", "latitude", "breitengrad")
  lon_words <- c("lon", "lng", "longitude", "laengengrad")
  # Named outright as a coordinate: no partner column needed.
  solo_words <- c("gps", "geolocation", "coordinate", "coordinates", "geocode")

  is_lat  <- any(words %in% lat_words)
  is_lon  <- any(words %in% lon_words)
  is_solo <- any(words %in% solo_words)
  if (!is_lat && !is_lon && !is_solo) return(none)

  # Values must be in coordinate range. A latitude is bounded tighter than a
  # longitude, so each is checked against its own limit.
  num <- suppressWarnings(as.numeric(gsub(",", ".", as.character(x), fixed = TRUE)))
  num <- num[!is.na(num)]
  if (length(num) >= 3) {
    lim <- if (is_lat && !is_lon) 90 else 180
    if (!all(num >= -lim & num <= lim)) return(none)
  }

  # A lat/lon column must have its partner in the same file. Skipped when the
  # caller passed no sibling names, and never required for a `gps`-style name.
  if (!is_solo && !is.null(sibling_names)) {
    sib <- unique(unlist(lapply(sibling_names, .pii_split_name)))
    want <- if (is_lat) lon_words else lat_words
    if (!any(sib %in% want)) return(none)
  }

  list(problem = TRUE,
       message = "Column name suggests geographic coordinates. Review before sharing.",
       values = "geo")
}

#' Flag a free-text column that may contain incidental personal information
#'
#' Open-ended typed responses (comments, explanations, descriptions) can contain
#' names, places, or other identifying detail, so they warrant a "review before
#' sharing" prompt. The aim is to flag genuine typed prose only -- not any long,
#' varied string. Long values that are *not* prose (numeric matrices with blank
#' headers, IDs, hashes, URLs, file paths, base64) are common in research data
#' and previously produced false positives, so a column is flagged only when its
#' typical value actually reads like written language:
#'
#' * long enough (`min_median_chars`),
#' * varied enough to be responses rather than a repeated category
#'   (`min_unique_frac`),
#' * **multi-word** -- most values contain whitespace between words, and
#' * **predominantly alphabetic** -- letters, not mostly digits/punctuation.
#'
#' @param x a character or factor vector
#' @param min_median_chars typical (median) length above which a column may be
#'   free text
#' @param min_unique_frac minimum fraction of distinct values (prose is rarely
#'   repeated; a coded category is)
#' @param min_multiword_frac minimum fraction of values that contain more than
#'   one word (a space between word characters)
#' @param min_alpha_frac minimum share of alphabetic characters in the typical
#'   value (screens out numeric/ID/hash/URL columns)
#' @returns list(problem, message, values)
#' @export
#' @keywords internal
data_check_pii_freetext <- function(x, min_median_chars = 40,
                                    min_unique_frac = 0.8,
                                    min_multiword_frac = 0.6,
                                    min_alpha_frac = 0.5) {
  none <- list(problem = FALSE, message = "", values = NULL)
  if (is.numeric(x)) return(none)
  x <- trimws(as.character(x))
  x <- x[!is.na(x) & nzchar(x)]
  if (length(x) < 5) return(none)

  med <- stats::median(nchar(x))
  uniq_frac <- length(unique(x)) / length(x)
  if (med < min_median_chars || uniq_frac < min_unique_frac) return(none)

  # Real prose is multi-word: most values have a space between word characters.
  multiword_frac <- mean(grepl("\\w\\s+\\w", x))
  if (multiword_frac < min_multiword_frac) return(none)

  # And it is mostly letters, not digits/punctuation (rejects numeric matrices,
  # IDs, hashes, URLs, base64). Measured on the typical (median-length) value.
  typical <- x[order(abs(nchar(x) - med))][1]
  n_alpha <- nchar(gsub("[^A-Za-z]", "", typical))
  alpha_frac <- if (nchar(typical) > 0) n_alpha / nchar(typical) else 0
  if (alpha_frac < min_alpha_frac) return(none)

  list(problem = TRUE,
       message = sprintf("Free-text column (median %.0f characters, %.0f%% distinct) may contain names or other personal detail. Review before sharing.",
                         med, 100 * uniq_frac),
       values = NULL)
}

# -- Demographic-column detection ----------------------------------------------
# Detect the three demographic variables that almost every human-subjects study
# collects: age, gender/sex, and race/ethnicity. Used by data_check (to tag the
# column) and data_validate (to report which demographics a file contains).
#
# Detection requires NAME and VALUES to agree: the column NAME must look like the
# demographic, AND the VALUES must be consistent with it. Name alone is too weak
# (a column literally called "age" that holds free text is not usable age data)
# and values alone are ambiguous (a 1/2 column is as likely a condition code as
# a sex code). Requiring both keeps false positives low -- the aim is a column a
# reviewer can trust is really participant age / gender / race.

# Column-name tokens per demographic, matched against the normalised name
# (lowercase, punctuation stripped). Whole-name match OR a word-boundary token
# match, so `participant_age` and `age_years` hit but `page` / `agent` do not
# (handled by the boundary regex below, not these bare tokens).
.demographic_name_tokens <- list(
  age    = c("age", "agejaren", "ageyears", "ageyrs", "leeftijd", "alter"),
  gender = c("gender", "sex", "geslacht", "genderidentity", "sexgender",
             "gendersex"),
  race   = c("race", "ethnicity", "ethnic", "raceethnicity", "ethnicgroup",
             "raceeth", "hispanic", "raza", "etnia")
)

# Anchored name regexes: the token must be the whole name or a standalone word
# within it (separated by _ . - space or a case boundary), so it does not fire
# inside unrelated words. Built once from the token lists above.
.demographic_name_regex <- list(
  # age: exclude common false friends where "age" is a substring (percentage,
  # image, page, average, agent, storage, damage, usage, coverage, language).
  age    = "(?i)(^|[^a-z])(age|leeftijd|alter)([^a-z]|$)|(?i)age[_.-]?(years|yrs|jaren)|(?i)(years|yrs)[_.-]?age",
  gender = "(?i)(^|[^a-z])(gender|sex|geslacht)([^a-z]|$)",
  race   = "(?i)(^|[^a-z])(race|ethnicity|ethnic|hispanic|raza|etnia)([^a-z]|$)"
)

# Do a column's VALUES look like this demographic? Conservative value checks
# that CONFIRM a name match; they are not used to detect on their own.
.demographic_values_ok <- function(kind, x) {
  x_chr <- trimws(as.character(x))
  x_chr <- x_chr[!is.na(x_chr) & nzchar(x_chr)]
  if (length(x_chr) < 3) return(TRUE)   # too few values to contradict the name

  if (kind == "age") {
    # Numeric (allow comma decimals) and almost all within a human-age range.
    num <- suppressWarnings(as.numeric(gsub(",", ".", x_chr, fixed = TRUE)))
    frac_num <- mean(!is.na(num))
    if (frac_num < 0.8) return(FALSE)
    v <- num[!is.na(num)]
    # Drop common missing-data sentinels before the range test so a genuine age
    # column carrying a -99 / 999 code is not rejected by that single value.
    v <- v[!v %in% .data_missing_sentinels]
    if (length(v) == 0) return(FALSE)
    # Ages are 0-120; allow a small tail of remaining miscodes.
    mean(v >= 0 & v <= 120) >= 0.9
  } else if (kind == "gender") {
    # Either a small set of textual categories that read as sex/gender, or a
    # low-cardinality numeric coding (1/2, 0/1/2, ...).
    u <- unique(tolower(x_chr))
    gender_words <- c("m", "f", "male", "female", "man", "woman", "men",
                      "women", "boy", "girl", "nonbinary", "non-binary",
                      "nb", "other", "trans", "transgender", "genderqueer",
                      "prefer not to say", "prefernottosay", "pnts", "n/a",
                      "unknown", "d", "diverse", "man/vrouw", "vrouw", "man",
                      "intersex", "agender", "fluid", "questioning")
    hit_frac <- mean(u %in% gender_words)
    is_lowcard_numeric <- {
      num <- suppressWarnings(as.numeric(x_chr))
      all(!is.na(num)) && length(unique(num)) <= 4 &&
        all(num == round(num)) && all(num >= 0 & num <= 9)
    }
    hit_frac >= 0.6 || is_lowcard_numeric
  } else if (kind == "race") {
    # Race/ethnicity is categorical with a modest number of levels; if numeric,
    # a low-cardinality coding. Reject long free text and high-cardinality.
    x2 <- x_chr
    if (length(unique(x2)) > 30) return(FALSE)
    med_chars <- stats::median(nchar(x2))
    if (med_chars > 60) return(FALSE)   # long prose is not a race category
    num <- suppressWarnings(as.numeric(x2))
    if (all(!is.na(num)))
      return(length(unique(num)) <= 25 && all(num == round(num)))
    TRUE
  } else {
    FALSE
  }
}

#' Detect whether a column holds participant age, gender/sex, or race/ethnicity
#'
#' A content-based classifier for the three demographic variables collected by
#' almost every human-subjects study. A column is tagged only when its NAME
#' looks like the demographic AND its VALUES are consistent with it (see
#' `.demographic_values_ok`), which keeps false positives low: a `condition`
#' column coded 1/2 is not flagged as gender, and an `age` column of free text
#' is not treated as usable age data.
#'
#' Complements [data_col_type()] (which gives a structural type such as
#' continuous/categorical): this adds a *semantic* label used by `data_check`
#' (reported in the column table) and `data_validate` (which reports the
#' demographics a file contains). Detection is name-driven, so a demographic
#' under a cryptic name (e.g. `q3`) is intentionally not caught here -- that is
#' the LLM classifier's job.
#'
#' @param col_name the column's name
#' @param x the column's values
#'
#' @returns `"age"`, `"gender"`, or `"race"` when the column matches one of
#'   them, else `NA_character_`.
#' @export
#' @keywords internal
#'
#' @examples
#' data_check_demographic("age", c(23, 45, 31, 29))
#' data_check_demographic("gender", c("Male", "Female", "Female", "Male"))
#' data_check_demographic("condition", c(1, 2, 1, 2))   # NA (name does not match)
data_check_demographic <- function(col_name, x) {
  if (is.null(col_name) || length(col_name) != 1 || is.na(col_name) ||
      !nzchar(col_name)) return(NA_character_)
  # Guard against a non-UTF-8 name reaching the perl regexes below.
  if (is.na(iconv(col_name, "UTF-8", "UTF-8")))
    col_name <- iconv(col_name, "latin1", "UTF-8", sub = "")

  for (kind in names(.demographic_name_regex)) {
    if (grepl(.demographic_name_regex[[kind]], col_name, perl = TRUE) &&
        .demographic_values_ok(kind, x))
      return(kind)
  }
  NA_character_
}

# -- Column facets (orthogonal properties, DDI-style) --------------------------
# A data column has several INDEPENDENT properties, and collapsing them into one
# `col_type` enum (the old model) conflated things that are not alternatives:
# how the value is stored, what measurement level it is on, and what it actually
# measures. Following DDI (which separates RepresentedVariable representation,
# @classificationLevel, the Variable->Concept link, VariableRole and UnitType) we
# describe each column with orthogonal facets instead:
#
#   representation     numeric | text | datetime | code | empty
#                      (how the value is stored/represented)
#   measurement_level  nominal | ordinal | interval | ratio | NA
#                      (Stevens level; DDI @classificationLevel)
#   concept            reaction_time | accuracy | age | gender | race | likert |
#                      condition | id | date | timestamp | NA
#                      (what the column measures; DDI Variable->Concept)
#   role               identifier | measure | condition | timestamp | measure
#                      (how it functions in the dataset; DDI VariableRole)
#   unit               seconds | milliseconds | years | NA (DDI UnitType)
#   quality            ok | empty | constant | near_constant (data state)
#   parse_note         NA | comma_decimal | mostly_numeric
#                      (a representation quirk, NOT a type -- was a fake col_type)
#
# `data_col_facets()` derives these from the existing rule primitive
# `data_col_type()` (kept internal so its battle-tested edge cases -- UTF-8
# guard, date threshold, comma-decimal, text-length -- are preserved) plus the
# concept detectors below. Rules run always; the LLM (in data_check) only fills
# facets the rules left NA.

# Concept detector: name+value agreement, same discipline as the demographic
# detector. Returns a concept code or NA. Order matters -- the first match wins,
# so specific concepts (reaction_time) are tried before generic ones.
#
# All four name checks below match against the LOWERCASED but UN-stripped
# column name, with (^|[^a-z])...([^a-z]|$) boundary anchoring -- the same
# style already used by .RT_NAME_RE/.ACC_NAME_RE/cond_name in
# .detect_task_columns() below (which .concept_is_rt/.concept_is_accuracy now
# call directly, rather than maintaining a second, divergent copy of the same
# pattern). An earlier version matched against .qualtrics_key(col_name) --
# fully alnum-stripped, which destroys every separator a real column name has
# (e.g. "response_time_break" -> "responsetimebreak"), so word-boundary
# anchoring was IMPOSSIBLE on that stripped form; bare "rt"/"time"/"condition"
# substrings matched inside ordinary words ("effort", "Thwart", "cohort",
# "Timeline", "conditions_diabetes") purely because those substrings happened
# to appear. Verified against ~40,000 real column names from the cached
# corpus: the old patterns false-matched dozens of unrelated columns per
# concept (e.g. every "*Thwart" scale item as reaction_time, a medical-history
# checklist "conditions_asthma"/"conditions_diabetes"/... as condition/group
# assignment); this fix eliminates those without losing genuine matches.

# Reaction/response time: a numeric column named rt/latency/response time whose
# values are plausible durations. We do not fix the unit here (ms vs s); that is
# the `unit` facet, inferred separately.
.concept_is_rt <- function(col_name, x) {
  if (!grepl(.RT_NAME_RE, tolower(col_name), perl = TRUE)) return(FALSE)
  num <- suppressWarnings(as.numeric(gsub(",", ".", as.character(x), fixed = TRUE)))
  num <- num[!is.na(num)]
  if (length(num) < 3) return(TRUE)          # name is strong enough on its own
  # Durations are non-negative; a column with many negatives is not an RT.
  mean(num >= 0) >= 0.95
}

# Accuracy/correctness: a 0/1 (or boolean, or correct/incorrect) column named
# acc/correct/hit/error.
.concept_is_accuracy <- function(col_name, x) {
  if (!grepl(.ACC_NAME_RE, tolower(col_name), perl = TRUE)) return(FALSE)
  v <- tolower(trimws(as.character(x)))
  v <- v[!is.na(v) & nzchar(v)]
  if (length(v) < 3) return(TRUE)
  u <- unique(v)
  num <- suppressWarnings(as.numeric(u))
  is01 <- all(!is.na(num)) && all(num %in% c(0, 1))
  is_bool <- all(u %in% c("true", "false", "correct", "incorrect", "hit",
                          "miss", "yes", "no", "right", "wrong"))
  is01 || is_bool
}

# Condition/group assignment: a low-cardinality column named condition/group/
# treatment/arm/cond. Kept deliberately name-driven (values look like any other
# categorical), so it never steals a genuine gender/accuracy column.
.concept_is_condition <- function(col_name, x) {
  grepl("(^|[^a-z])(cond|condition|group|treatment|arm|manipulation|between|within)([^a-z]|$)",
        tolower(col_name), perl = TRUE)
}

# Date/time formats seen in real psychology repositories, ordered so the
# DATETIME set is always tried first: a full timestamp also parses as a bare
# date (strptime ignores trailing text), so testing dates first would classify
# every timestamp as a date. Checked against values pulled from a cache of 849
# real OSF repositories -- the two PsychoPy/compact forms below are there
# because a plain "\\d{1,2}:\\d{2}" test misses them:
#   "2020-05-19_16h20.01.792"  PsychoPy writes the hour separator as `h`
#   "2022_Feb_08_1523"         compact, no separator between hour and minute
.DATETIME_FMTS <- c(
  "%Y-%m-%dT%H:%M:%S", "%Y-%m-%d %H:%M:%S", "%Y-%m-%d %H:%M",
  "%d/%m/%Y %H:%M:%S", "%m/%d/%Y %H:%M:%S", "%Y/%m/%d %H:%M:%S",
  "%d-%m-%Y %H:%M:%S", "%Y-%m-%d_%Hh%M",    "%Y_%b_%d_%H%M"
)
.DATE_FMTS <- c(
  "%Y-%m-%d", "%d/%m/%Y", "%m/%d/%Y", "%Y/%m/%d", "%d-%m-%Y",
  "%m/%d/%y", "%d/%m/%y", "%d %b %Y", "%B %d, %Y"
)

# What fraction of `v` parses under the best-fitting of `fmts`? The best SINGLE
# format is used rather than "parsed by any", because a column is written in one
# format -- allowing a mixture would let two half-matching formats add up to a
# spurious pass.
.parse_frac <- function(v, fmts) {
  v <- as.character(v)
  v <- v[!is.na(v) & nzchar(v)]
  if (!length(v)) return(0)
  best <- 0
  for (f in fmts) {
    p <- suppressWarnings(as.POSIXct(v, format = f, tz = "UTC"))
    best <- max(best, mean(!is.na(p)))
    if (best >= 1) break
  }
  best
}

# Timestamp (a clock time / datetime the event happened) vs a plain date. Both
# have representation `datetime`; the concept distinguishes a full timestamp
# (has a time component) from a calendar date.
#
# Decided by PARSING the values, not by pattern-matching a time component. A
# bare-numeric column (likert responses, participant ids, reaction times in ms,
# ages, four-digit years) parses under none of these formats, so this cannot
# steal a non-date column -- verified against each of those cases explicitly.
.concept_is_timestamp <- function(col_name, x) {
  name_ok <- grepl("(^|[^a-z])(time|timestamp|datetime|onset|startdate|enddate|recordeddate)([^a-z]|$)",
                   tolower(col_name), perl = TRUE)
  if (!name_ok) return(FALSE)
  .parse_frac(x, .DATETIME_FMTS) >= 0.5
}

# A calendar date with no clock time (date of birth, test date, a bare
# collection date). Checked AFTER .concept_is_timestamp() in data_col_concept(),
# so a column carrying a full timestamp is never reduced to "date".
.concept_is_date <- function(col_name, x) {
  name_ok <- grepl("(^|[^a-z])(date|dob|birth|birthday|geboorte|dated)([^a-z]|$)|(^|[^a-z])(day|dag)([^a-z]|$)",
                   tolower(col_name), perl = TRUE)
  if (!name_ok) return(FALSE)
  # Must NOT look like a timestamp (that is the other concept), and must parse
  # as a date. The datetime test comes first for the same ordering reason the
  # format vectors are ordered: a timestamp satisfies both.
  if (.parse_frac(x, .DATETIME_FMTS) >= 0.5) return(FALSE)
  .parse_frac(x, .DATE_FMTS) >= 0.5
}

#' Detect the substantive concept a column measures
#'
#' A content classifier for the *concept* facet (what the column measures),
#' independent of how it is stored or its measurement level. Uses name+value
#' agreement like [data_check_demographic()], which it wraps for the demographic
#' concepts. Rules-only and deterministic; concepts under cryptic names are left
#' `NA` for the LLM tier in `data_check` to fill.
#'
#' @param col_name the column's name
#' @param x the column's values
#'
#' @returns one of `"reaction_time"`, `"accuracy"`, `"condition"`, `"age"`,
#'   `"gender"`, `"race"`, `"timestamp"`, or `NA_character_`. (`id`, `date` and
#'   `likert` concepts are assigned by [data_col_facets()] from the role /
#'   representation / measurement level, not here.)
#' @export
#' @keywords internal
data_col_concept <- function(col_name, x) {
  if (is.null(col_name) || length(col_name) != 1 || is.na(col_name) ||
      !nzchar(col_name)) return(NA_character_)
  if (is.na(iconv(col_name, "UTF-8", "UTF-8")))
    col_name <- iconv(col_name, "latin1", "UTF-8", sub = "")

  if (.concept_is_rt(col_name, x))        return("reaction_time")
  if (.concept_is_accuracy(col_name, x))  return("accuracy")
  demo <- data_check_demographic(col_name, x)
  if (!is.na(demo))                       return(demo)
  # Timestamp before date: a full timestamp satisfies both tests, and the more
  # specific concept wins.
  if (.concept_is_timestamp(col_name, x)) return("timestamp")
  if (.concept_is_date(col_name, x))      return("date")
  if (.concept_is_condition(col_name, x)) return("condition")
  # NOTE: no `likert` rule here, deliberately. .is_likert_item() (used for scale
  # BLOCK detection) tests values only -- whole numbers, 3-11 distinct levels, a
  # narrow range -- which describes a rating item and a trial counter equally
  # well. Validated against 3145 real columns from 120 cached repositories: it
  # fired 389 times, and the most frequent hits were `block`, `round`,
  # `Trial Number`, PsychoPy loop indices (resp_loop.thisIndex/.thisN), stimulus
  # ids (arcade_id_L/R) and key-press codes (resp_*.keys) -- not rating scales.
  # Genuine items ("british_entitative_1_1") were a small minority. A single
  # column carries too little signal; .detect_scale_blocks() gets this right
  # because it sees a RUN of same-prefix columns sharing one response range.
  NA_character_
}

# Map the old rule primitive's col_type onto a (representation, level) pair.
# This is where the conflated enum is untangled into two orthogonal facets.
.coltype_to_facets <- function(ct, is_numeric_hint) {
  switch(ct %||% "unknown",
    empty       = c(rep = "empty",    lvl = NA_character_),
    constant    = c(rep = NA_character_, lvl = NA_character_),  # rep unknown w/o values
    binary      = c(rep = NA_character_, lvl = "nominal"),
    date        = c(rep = "datetime", lvl = NA_character_),
    text        = c(rep = "text",     lvl = NA_character_),
    id          = c(rep = "text",     lvl = "nominal"),
    continuous                    = c(rep = "numeric", lvl = "ratio"),
    continuous_comma_decimal      = c(rep = "numeric", lvl = "ratio"),
    continuous_outliers_excluded  = c(rep = "numeric", lvl = "ratio"),
    c(rep = NA_character_, lvl = NA_character_)
  )
}

#' Describe a data column as orthogonal facets (DDI-style)
#'
#' Replaces the single `col_type` enum with independent properties, so the
#' numeric character of a column (how it is stored, its measurement level) is
#' kept separate from what it measures (its concept) and how it functions (its
#' role). See the facet vocabulary in the "Column facets" section of this file.
#'
#' Derives representation, measurement level, role, quality and a parse note from
#' the rule primitive [data_col_type()] (preserving its edge cases), and the
#' concept from [data_col_concept()]. The `id`/`date` concepts are inferred here
#' from the role / representation, and `likert` from `in_scale_block` (see that
#' argument). `unit` is left `NA` for concepts whose unit is not implied
#' (an LLM/codebook can fill it); `reaction_time` seeds `seconds`/`milliseconds`
#' from the value magnitude.
#'
#' @param col_name the column's name
#' @param values the column's values
#' @param in_scale_block whether this column belongs to a detected scale block
#'   (a run of consecutive same-prefix columns sharing a response range -- see
#'   `.detect_scale_blocks()`), which is what makes it a `likert` item. Requires
#'   the whole data frame to determine, so the caller supplies it; `NA` (the
#'   default) means unknown and leaves the concept alone. A column's own values
#'   cannot decide this -- whole numbers over a narrow range describe a trial
#'   counter as readily as a rating item.
#'
#' @returns a list with `representation`, `measurement_level`, `concept`,
#'   `role`, `unit`, `quality`, `parse_note`, plus the numeric helpers carried
#'   over from [data_col_type()] (`numeric_values`, `n_coerced`, `is_numeric`,
#'   `ambiguous`) so `data_check` can compute statistics and target the LLM.
#' @export
#' @keywords internal
#'
#' @examples
#' data_col_facets("RT", c(543, 612, 498, 701))
#' data_col_facets("subject_id", c("s01", "s02", "s03"))
data_col_facets <- function(col_name, values, in_scale_block = NA) {
  prim <- data_col_type(col_name, values)      # the rule primitive
  ct   <- prim$col_type
  x_noNA <- values[!is.na(values)]
  n_noNA <- length(x_noNA)
  n_unique <- length(unique(x_noNA))

  # representation + measurement_level from the (untangled) col_type.
  f <- .coltype_to_facets(ct, prim$is_numeric)
  representation <- unname(f["rep"])
  measurement_level <- unname(f["lvl"])

  # A constant/binary column's representation is decided by its actual storage.
  if (is.na(representation) && n_noNA > 0) {
    num <- suppressWarnings(as.numeric(gsub(",", ".", as.character(x_noNA),
                                            fixed = TRUE)))
    representation <- if (mean(!is.na(num)) >= 0.8) "numeric" else "text"
  }

  # quality: constant/empty are a data STATE, not a type. (near_constant is a
  # data_validate finding; here we surface the exact-constant/empty case.)
  quality <- if (identical(ct, "empty") || n_noNA == 0) "empty"
             else if (identical(ct, "constant") || n_unique == 1) "constant"
             else "ok"

  # concept (rules); fall through to structural concepts.
  concept <- data_col_concept(col_name, values)

  # role: an id column is an identifier; a timestamp/date column is temporal;
  # everything else defaults to a measure. condition concept -> condition role.
  role <- if (identical(ct, "id")) "identifier"
          else if (identical(concept, "timestamp") || identical(ct, "date")) "timestamp"
          else if (identical(concept, "condition")) "condition"
          else "measure"

  # Structural concepts that follow from other facets rather than name+value:
  if (is.na(concept)) {
    if (identical(role, "identifier")) concept <- "id"
    else if (identical(ct, "date"))    concept <- "date"
    else if (identical(representation, "datetime")) concept <- "timestamp"
  }

  # Likert: decided by whether the column belongs to a detected SCALE BLOCK (a
  # run of consecutive same-prefix columns sharing a response range -- see
  # .detect_scale_blocks()), not by the column's own values.
  #
  # The values alone cannot carry this. .is_likert_item() asks whether a column
  # holds whole numbers with 3-11 distinct levels in a narrow range, which
  # describes a rating item and a trial counter equally well. Over 3145 columns
  # from 120 real repositories, using it per-column claimed 242 columns that are
  # plainly not scales -- `round`, `block`, PsychoPy loop indices
  # (resp_loop.thisIndex/.thisN/.thisTrialN), stimulus ids (arcade_id_L/R) and
  # key-press codes (resp_*.keys). Over the same corpus, block detection claimed
  # 133 columns forming 6 groups, each inspected and confirmed a real instrument
  # (UPPS-P's 50 items, three 25-item rating batteries, DotsCol1-5, IQ_short1-3).
  #
  # Not a measured error rate: there is no hand-labelled ground truth here, and
  # the corpus is one sample of 120 files. Known structural limits -- a scale
  # whose items are not CONSECUTIVE columns, one whose item names do not share a
  # prefix after stripping a trailing number (bfi_agree_1 / bfi_extra_1), or one
  # shorter than .scale_min_items -- are missed by construction.
  #
  # `in_scale_block` is therefore supplied by the caller, which has the whole
  # data frame. NA (the default, for a caller describing a single column with no
  # frame context) means "unknown" and claims nothing, rather than falling back
  # to the value-only test that was measured to be wrong.
  if (is.na(concept) && isTRUE(in_scale_block)) {
    concept <- "likert"
    measurement_level <- "ordinal"
  }

  # Concept-implied measurement level: a categorical concept is nominal even
  # when the rules could not decide the level from values alone (e.g. a gender
  # column with >2 spellings did not hit the binary rule).
  if (is.na(measurement_level) &&
      concept %in% c("gender", "race", "accuracy", "condition"))
    measurement_level <- "nominal"

  # Non-numeric values cannot be interval or ratio: those levels require a
  # meaningful distance between values, which text does not have. A column whose
  # values do not parse as numbers is therefore nominal (or ordinal, but only a
  # codebook can say which -- and where an ordering IS derivable the rules above
  # have already set `ordinal` from a scale block, so anything reaching here has
  # no evidence of order).
  #
  # This is the rule that replaced the measurement-level LLM pass: it was being
  # handed exactly these columns -- `object_label` (Wine, Hammock, Binoculars),
  # `event` (onload, subject, mouse), `ll_amt` (the literal string "NULL") --
  # under a prompt asking for the level of a NUMERIC column.
  #
  # `representation == "empty"` is excluded: an all-NA column has no values to
  # be nominal ABOUT, and gets its own stub entry downstream.
  if (is.na(measurement_level) && !isTRUE(prim$is_numeric) &&
      !identical(representation, "empty") && length(x_noNA) > 0) {
    nn <- suppressWarnings(as.numeric(gsub(",", ".", as.character(x_noNA),
                                           fixed = TRUE)))
    # "Does not parse as numeric" is judged on the values actually present, so a
    # mostly-text column with a few stray numbers still counts as text.
    if (mean(!is.na(nn)) < 0.5) measurement_level <- "nominal"
  }

  # unit: implied by a few concepts; NA otherwise.
  unit <- NA_character_
  if (identical(concept, "reaction_time")) {
    num <- suppressWarnings(as.numeric(gsub(",", ".", as.character(x_noNA),
                                            fixed = TRUE)))
    num <- num[!is.na(num) & num > 0]
    # Median RT above ~100 is almost certainly milliseconds; below, seconds.
    unit <- if (length(num) > 0 && stats::median(num) >= 100) "milliseconds"
            else if (length(num) > 0) "seconds" else NA_character_
    if (is.na(measurement_level)) measurement_level <- "ratio"
  } else if (identical(concept, "age")) {
    unit <- "years"
    if (is.na(measurement_level)) measurement_level <- "ratio"
  }

  # parse_note: the representation quirk the old fake col_types encoded.
  parse_note <- if (identical(ct, "continuous_comma_decimal")) "comma_decimal"
                else if (identical(ct, "continuous_outliers_excluded")) "mostly_numeric"
                else NA_character_

  list(
    representation    = representation,
    measurement_level = measurement_level,
    concept           = concept,
    role              = role,
    unit              = unit,
    quality           = quality,
    parse_note        = parse_note,
    # carried over for data_check's statistics + LLM targeting:
    numeric_values = prim$numeric_values,
    n_coerced      = prim$n_coerced,
    is_numeric     = prim$is_numeric,
    ambiguous      = prim$ambiguous
  )
}

# Thresholds for the "is this a usable rectangular dataset?" test below. Named
# so they can be tuned as the check is run on more repositories. Derived from a
# set of human-coded qualitative worksheets (prose columns ~45-50%, missingness
# ~90%+) versus their study's real dataset (~3% prose, ~2% missing).
.tabular_prose_high  <- 0.70   # overwhelmingly free-text -> not a dataset on its own
.tabular_prose_mid   <- 0.40   # the coding-sheet middle ground: both must hold
.tabular_miss_mid    <- 0.40   # (missingness only corroborates prose; never alone)

#' Is a read-in data frame a usable rectangular dataset?
#'
#' `readxl`/`read.delim` will happily read a human-formatted coding worksheet
#' (interleaved "Code N: description" free-text columns + sparse 0/1 indicators,
#' summary/legend rows at the bottom) into a data frame -- but the result is not a
#' rectangular *dataset*: most "columns" are prose annotations and most cells are
#' structurally empty. Extracting columns from such a file yields junk (the
#' "Code"-prefixed columns) and sending them to the LLM wastes calls on non-data.
#'
#' This detects that case from facets `data_check` has already computed, using
#' two signals combined with tiered rules (not a single AND-gate, so an extreme
#' value on one axis can exclude a file on its own):
#'
#' * **prose fraction** -- share of columns that are free text: `representation`
#'   is `"text"`, the `concept` is not a recognised text kind (id/date/timestamp),
#'   and the column is high-cardinality (distinct/non-missing > 0.5), i.e. a
#'   genuine free-text field, not a small set of category labels.
#' * **missingness fraction** -- share of columns that are >50% missing.
#'
#' Exclude when the file is almost entirely empty, or overwhelmingly free text,
#' or moderately both. A file with an ordinary structure (a few open-response
#' columns, or a legitimately sparse but numeric design) trips none of these.
#'
#' @param facets a list of per-column facet lists, as produced by
#'   [data_col_facets()] (one per column, in column order).
#' @param df the read-in data frame the facets describe.
#'
#' @returns a list with `usable` (logical) and, when `FALSE`, a human-readable
#'   `reason` naming the signals that fired.
#' @export
#' @keywords internal
.tabular_usable <- function(facets, df) {
  p <- length(facets)
  if (p == 0 || is.null(df) || nrow(df) == 0)
    return(list(usable = FALSE, reason = "the file has no data rows or columns"))

  is_prose <- vapply(seq_len(p), function(j) {
    f <- facets[[j]]
    if (!identical(f$representation, "text")) return(FALSE)
    if (isTRUE(f$concept %in% c("id", "date", "timestamp"))) return(FALSE)
    x <- df[[j]]
    nonNA <- x[!is.na(x)]
    length(nonNA) > 0 && (length(unique(nonNA)) / length(nonNA)) > 0.5
  }, logical(1))

  miss_hi <- vapply(seq_len(p), function(j) mean(is.na(df[[j]])) > 0.5, logical(1))

  prose_frac <- mean(is_prose)
  miss_frac  <- mean(miss_hi)
  pct <- function(x) sprintf("%.0f%%", 100 * x)

  # Tiered rules. NOTE: high missingness ALONE does NOT exclude. Legitimate
  # branched / planned-missing surveys (e.g. a Qualtrics export where each
  # respondent sees only their condition's questions) are 90%+ missing but are
  # real NUMERIC data -- excluding them drops the actual dataset (and its scales).
  # We exclude only when the file is overwhelmingly FREE TEXT, or moderately free
  # text AND mostly empty (the human coding-worksheet signature). Missingness is
  # corroborating, never sufficient.
  if (prose_frac >= .tabular_prose_high)
    return(list(usable = FALSE, reason = sprintf(
      "%s of columns are free text, not variables", pct(prose_frac))))
  if (prose_frac >= .tabular_prose_mid && miss_frac >= .tabular_miss_mid)
    return(list(usable = FALSE, reason = sprintf(
      "%s of columns are free text and %s are mostly empty -- this looks like a coding worksheet, not a rectangular dataset",
      pct(prose_frac), pct(miss_frac))))

  list(usable = TRUE, reason = NA_character_)
}

# -- Qualtrics survey-export detection -----------------------------------------
# Qualtrics CSV/TSV exports have a fixed, distinctive shape: a set of reserved
# response-metadata columns (StartDate, Duration (in seconds), Finished, ...)
# that are the same across every survey, and -- for the "use choice text" export
# -- a multi-row header (machine names, then human question text, then an
# `ImportId` JSON row). We detect the file as Qualtrics from those metadata
# names and/or the ImportId row, strip the junk header rows so the data types
# correctly, and tag the metadata columns so data_validate can report the things
# that ARE reliably extractable from any Qualtrics file (completion time,
# preview/unfinished rows, recording window, which PII fields are present).
#
# We deliberately do NOT try to interpret the substantive question/scale columns
# here -- that is the scale-block detector's job (a different unit).

# Reserved Qualtrics metadata column names, mapped to a semantic tag. Names are
# matched case-insensitively after stripping non-alphanumerics, so "Duration (in
# seconds)" and "Duration..in.seconds." (R-mangled) both hit. The tag drives both
# reporting and the multi-row-header fix.
.qualtrics_meta_cols <- c(
  startdate            = "qualtrics_start",
  enddate              = "qualtrics_end",
  status               = "qualtrics_status",
  ipaddress            = "qualtrics_ip",
  progress             = "qualtrics_progress",
  durationinseconds    = "qualtrics_duration",
  finished             = "qualtrics_finished",
  recordeddate         = "qualtrics_recorded",
  responseid           = "qualtrics_responseid",
  recipientlastname    = "qualtrics_recipient",
  recipientfirstname   = "qualtrics_recipient",
  recipientemail       = "qualtrics_email",
  externaldatareference = "qualtrics_externalref",
  externalreference    = "qualtrics_externalref",
  locationlatitude     = "qualtrics_lat",
  locationlongitude    = "qualtrics_lon",
  distributionchannel  = "qualtrics_channel",
  userlanguage         = "qualtrics_language"
)

# Normalise a column name to its Qualtrics lookup key: lowercase, drop anything
# non-alphanumeric. "Duration (in seconds)" -> "durationinseconds".
.qualtrics_key <- function(nm) gsub("[^a-z0-9]", "", tolower(nm))

# Map each column name of a data frame to its Qualtrics metadata tag (or NA).
.qualtrics_tag_cols <- function(col_names) {
  keys <- vapply(col_names, .qualtrics_key, character(1), USE.NAMES = FALSE)
  unname(.qualtrics_meta_cols[keys])
}

# Recover the scale-block stem of a Qualtrics export column from its name alone,
# for a file that has NO .qsf. Qualtrics matrix/multi questions export as
# <stem>_<int> (TIPI_1, BSQ_10); the shared <stem> is the block. Returns the stem
# for a "<letters...>_<digits>" name whose stem starts with >= 2 letters, else NA.
# Reserved Qualtrics metadata columns (StartDate, Duration, ...) are never items.
.qualtrics_col_stem <- function(nm) {
  if (is.na(nm) || !nzchar(nm)) return(NA_character_)
  if (!is.na(.qualtrics_tag_cols(nm))) return(NA_character_)  # metadata column
  m <- regmatches(nm, regexec("^(.*[A-Za-z].*)_([0-9]+)$", nm))[[1]]
  if (length(m) != 3) return(NA_character_)
  stem <- m[2]
  if (sum(grepl("[A-Za-z]", strsplit(stem, "")[[1]])) < 2) return(NA_character_)
  stem
}

# Which columns are Qualtrics display-order (randomisation) metadata? Qualtrics
# writes one `<Question>_DO_<...>` column per question whose choices/loops were
# randomised, recording the presentation order. These are export-only: they have
# no SQ entry in the .qsf and no analytic value, so they should not be matched to
# a codebook or sent to the LLM. Matched as a delimiter-bounded `_DO_` segment so
# a substantive column that merely contains the letters "do" is never caught.
# Caller must gate on data_check_is_qualtrics() -- this is a name test only.
.qualtrics_is_display_order <- function(col_names) {
  grepl("_DO(_|$)", col_names, perl = TRUE)
}

#' Detect whether a data frame is a Qualtrics survey export
#'
#' Fires when the columns include enough of Qualtrics' reserved response-metadata
#' names (StartDate, EndDate, Progress, Duration (in seconds), Finished,
#' RecordedDate, ResponseId, DistributionChannel, ...) that the file is
#' unambiguously a Qualtrics export -- these exact names essentially never
#' co-occur outside Qualtrics. The `ResponseId` column (values like `R_xxxxx`)
#' or a leftover `ImportId` JSON header cell is treated as corroborating.
#'
#' @param df a data.frame (a read tabular file)
#' @param min_meta minimum number of distinct metadata columns required
#'
#' @returns `TRUE` when `df` looks like a Qualtrics export, else `FALSE`.
#' @export
#' @keywords internal
data_check_is_qualtrics <- function(df, min_meta = 4L) {
  if (is.null(df) || ncol(df) == 0) return(FALSE)
  tags <- .qualtrics_tag_cols(names(df))
  n_meta <- length(unique(stats::na.omit(tags)))
  if (n_meta >= min_meta) return(TRUE)
  # Corroboration for borderline files (a heavily-renamed export): a ResponseId
  # column whose values are Qualtrics response ids (R_ + base62), or an ImportId
  # JSON cell surviving in the first rows.
  if (n_meta >= 2L) {
    rid <- names(df)[.qualtrics_key(names(df)) == "responseid"]
    if (length(rid) > 0) {
      v <- as.character(df[[rid[1]]])
      v <- v[!is.na(v) & nzchar(v)]
      if (length(v) > 0 && mean(grepl("^R_[A-Za-z0-9]{6,}$", v)) >= 0.5)
        return(TRUE)
    }
    # An ImportId cell survives in the first rows. read.delim strips the
    # surrounding quotes, so match the bare token, not a quoted one.
    if (any(vapply(df, function(col)
      any(grepl("ImportId", as.character(utils::head(col, 3)), fixed = TRUE)),
      logical(1)))) return(TRUE)
  }
  FALSE
}

# Is a row a Qualtrics secondary-header row (not real data)? The "use choice
# text" export writes, below the machine-name header: (row 1) the human question
# text, and (row 2) an `{"ImportId":...}` JSON blob. Read as data, these force
# every column to character. We detect such a row so data_read_head can drop it.
#
# A row is a secondary header when it carries the ImportId JSON, OR when it
# repeats the reserved metadata *labels* (e.g. a cell literally reading
# "Duration (in seconds)" or "Start Date") that Qualtrics puts in the question-
# text row for its own metadata columns.
.qualtrics_is_header_row <- function(row_vals) {
  vals <- trimws(as.character(row_vals))
  vals <- vals[!is.na(vals) & nzchar(vals)]
  if (length(vals) == 0) return(FALSE)
  # `ImportId` marks the JSON row. read.delim strips the JSON's quotes, so match
  # the bare token rather than the quoted `"ImportId"`.
  if (any(grepl("ImportId", vals, fixed = TRUE))) return(TRUE)
  # Question-text row: Qualtrics labels its own metadata columns with prose
  # versions of their names ("Start Date", "Response ID", "Recipient Email").
  #
  # Counted ABSOLUTELY, not as a fraction of the row. A fraction fails on
  # exactly the surveys that matter most: Qualtrics writes a FIXED ~17 metadata
  # columns however long the questionnaire is, so the longer the survey the
  # smaller that fraction gets. Measured on a real 139-column export, 13 cells
  # matched unmistakable Qualtrics labels (Start Date, Response ID, Recipient
  # Email, Distribution Channel, ...) -- a fraction of 0.094, far below the 0.3
  # this used to require. The row was therefore not recognised, the loop in
  # data_strip_qualtrics_header() stopped at row 1, and the `ImportId` row
  # BEHIND it (which this function does detect) was never reached. Both junk
  # rows survived into the data, leaving every rating column a character vector
  # of question text -- which is why a 25-item rating battery classified as
  # `ratio` text rather than an ordinal scale.
  #
  # The absolute floor mirrors data_check_is_qualtrics()'s own `min_meta = 4L`,
  # which already counts metadata columns absolutely for the same reason.
  #
  # A narrow frame cannot reach 4: a 3-column export has at most 3 metadata
  # cells, so an absolute-only rule silently never strips it. When there are
  # fewer than 4 columns, fall back to requiring that nearly all of them are
  # metadata labels -- on such a frame that is just as diagnostic, because a row
  # of genuine data would have to consist entirely of strings matching reserved
  # Qualtrics label names.
  label_keys <- .qualtrics_key(vals)
  n_meta <- sum(label_keys %in% names(.qualtrics_meta_cols))
  if (length(vals) >= 4L) n_meta >= 4L else n_meta == length(vals)
}

#' Strip Qualtrics secondary-header rows and re-type the columns
#'
#' A Qualtrics "use choice text" export has extra header rows (human question
#' text, then an `ImportId` JSON row) directly under the machine-name header.
#' `read.delim` reads the machine names as the header but keeps those two rows as
#' the first data rows, which forces every column to character. This drops any
#' leading rows that look like Qualtrics header rows (see
#' `.qualtrics_is_header_row`) and coerces columns that are now fully numeric
#' back to numeric, so the rest of `data_check` types the file correctly.
#'
#' @param df a data.frame read from a Qualtrics export (machine names as header)
#' @param max_strip maximum number of leading rows to consider stripping
#'
#' @returns the cleaned data.frame (unchanged if no header rows are found).
#' @export
#' @keywords internal
data_strip_qualtrics_header <- function(df, max_strip = 2L) {
  if (is.null(df) || nrow(df) == 0) return(df)
  drop <- 0L
  for (i in seq_len(min(max_strip, nrow(df)))) {
    if (.qualtrics_is_header_row(df[i, , drop = TRUE])) drop <- i else break
  }
  if (drop == 0L) return(df)
  df <- df[-seq_len(drop), , drop = FALSE]
  rownames(df) <- NULL
  # Columns that are now fully numeric (the junk text row was what made them
  # character) get coerced back, so data_col_type / stats treat them as numeric.
  for (j in seq_along(df)) {
    if (!is.character(df[[j]])) next
    v <- trimws(df[[j]])
    nonempty <- v[!is.na(v) & nzchar(v)]
    if (length(nonempty) == 0) next
    num <- suppressWarnings(as.numeric(nonempty))
    if (all(!is.na(num))) df[[j]] <- suppressWarnings(as.numeric(v))
  }
  df
}

# -- Leading metadata-row / offset-header repair -------------------------------
# The Qualtrics strip above handles junk rows BELOW a correct header. The inverse
# defect is common too: the real header sits one or more rows DOWN because the top
# of the sheet is a banner, a blank row, or a units row. The reader then takes
# that top row as the header -- inventing placeholder names (...4, V3, Unnamed: 2)
# for its blank cells, or promoting a units row to names -- and every downstream
# check (typing, scale detection, data_validate, PII) sees a corrupted table.
#
# The CDA (contralateral delay activity) EEG files are the motivating case: the
# true header (Participant, Reject, Condition, then millisecond time points) is in
# row 2, so the reader produced CDA...4 ... CDA...113 from one stray top-row label
# spread across blank-header columns, and the wide time series was mistaken for an
# 80-item psychometric scale.

# Coerce to numeric without erroring on invalid UTF-8 bytes. data_read_head()
# repairs UTF-8 before calling the promotion, but the helper stays self-defensive
# so it cannot error a whole read when handed unrepaired text.
.as_num_safe <- function(x) {
  suppressWarnings(as.numeric(iconv(x, to = "UTF-8", sub = "")))
}

# Column names a reader invents for BLANK/duplicate headers: readxl/tidyverse
# `...N`, base R `V1`/`X.1`/`X`, this file's own `col_N` fallback, and pandas
# `Unnamed: N`. A high share of these is the signature of a blank or partial top
# row read as the header.
.is_placeholder_name <- function(x) {
  x <- trimws(as.character(x))
  grepl("^.*\\.\\.\\.\\d+$", x) |                     # readxl `...N` (incl. STEM...N,
                                                       # where one real top-row cell
                                                       # was spread across blanks:
                                                       # CDA...4, CDA...5, ...)
    grepl("^(V\\d+|X(\\.\\d+)?|col_\\d+)$", x) |       # base R make.names / fallback
    grepl("^Unnamed:?\\.?\\s*\\d+$", x, ignore.case = TRUE) |  # pandas
    !nzchar(x)                                          # bare blank
}

# Fraction of columns that are internally type-CONSISTENT over the given rows: a
# column counts when its non-empty cells are ALL numeric or ALL non-numeric. This
# is the core signal for locating a header. A header cell is a type-outlier at the
# top of its column (a text label above a numeric column), so a table read WITHOUT
# a header -- with the real header row still sitting in the body -- has LOW
# consistency; removing exactly the header row makes the columns consistent.
#
# We measure clean-numeric fraction specifically, because the offset defect this
# repairs (banner/units/blank row above the header) leaves the columns below as
# genuine numeric data. An all-text table (a codebook) has 0 numeric columns at
# every drop, so no drop improves it and nothing is promoted -- the correct outcome.
.numeric_col_fraction <- function(df) {
  if (is.null(df) || ncol(df) == 0 || nrow(df) == 0) return(0)
  ok <- vapply(df, function(col) {
    v <- trimws(as.character(col))
    v <- v[nzchar(v) & !is.na(v)]
    if (length(v) == 0) return(FALSE)
    all(!is.na(.as_num_safe(v)))
  }, logical(1))
  mean(ok)
}

# Fraction of a row's non-empty cells that repeat another cell in the same row.
# A real header is near-unique (~0); a banner row (one label written/merged across
# many columns, e.g. CDA...CDAx110) is near-1. Used only to DESCRIBE what was
# stripped, for the researcher-facing message -- not to decide the header.
.row_duplication <- function(vals) {
  v <- trimws(as.character(vals)); v <- v[nzchar(v) & !is.na(v)]
  if (length(v) == 0) return(0)
  1 - length(unique(v)) / length(v)
}

# Is a single row JUNK sitting above the real header -- a banner / blank / title /
# units row, or a stale reader-mangled header rather than a real column header? A
# header names columns: it is well filled, near-unique, and free of reader
# placeholders. Junk fails one of those in a characteristic way:
#  - near-empty: almost all cells blank (a spacer row, or a title in one cell);
#  - heavily duplicated: one label repeated across many columns (CDA...CDAx110), a
#    banner or a merged cell read unmerged;
#  - mostly placeholders: a stale mangled header baked into a converted file
#    (...1, ...2, CDA...4, CDA...5 -- a prior read's invented names saved as row 1).
# `body_numeric` is the type-consistency of the data below; a row is only junk in
# CONTEXT of a consistent body, so an all-text table (codebook) is never stripped.
.is_junk_above_header <- function(vals, body_numeric,
                                  max_filled = 0.5, min_dup = 0.6,
                                  min_placeholder = 0.5) {
  filled <- mean(nzchar(trimws(as.character(vals))) & !is.na(vals))
  dup    <- .row_duplication(vals)
  ph     <- mean(.is_placeholder_name(vals))
  # Only strip rows above a body that is itself reasonably well-typed -- otherwise
  # we have no evidence the columns below are real data (guards all-text files).
  if (body_numeric < 0.3) return(FALSE)
  filled <= max_filled || dup >= min_dup || ph >= min_placeholder
}

#' Locate the true header row of a table read WITHOUT a header
#'
#' Given the first several physical rows of a file (each a character vector, the
#' file read so that NO row was consumed as a header), find which row is the real
#' column header. Combines two signals, because no single one covers every file:
#'
#'  1. **Type-consistency of the body below.** A text/banner/units row on top of
#'     numeric columns makes them look mixed; dropping it makes them cleanly
#'     numeric (see `.numeric_col_fraction()`). Strong when the data are numeric
#'     (the CDA/behavior sheets), weak when the body is mostly text.
#'  2. **Junk rows above.** A banner (one label repeated across columns) or a
#'     near-empty spacer row is not a header (see `.is_junk_above_header()`). This
#'     carries the cases where the numeric jump is weak -- e.g. the CDA banner row
#'     of `CDA` x 110 sitting above a header that only relabels 3 of 113 columns.
#'
#' The header is the SMALLEST `h` such that every row above `h` is junk, the body
#' below `h` is type-consistent, and promoting `h` does not make the typing worse.
#' When no leading row is junk, the header is row 1 (already correct).
#'
#' @param rows a list of character vectors (one per physical row), all the same
#'   length; typically the first `max_scan + 1` rows of a headerless read.
#' @param max_scan how many leading rows to consider stripping as metadata.
#'
#' @returns a list: `header_row` (1-based index of the true header, 1 = already
#'   correct / no offset), `stripped` (character vectors of the metadata rows
#'   above it), and `improved` (body type-consistency gained by the promotion).
#' @export
#' @keywords internal
.detect_header_row <- function(rows, max_scan = 4L) {
  n <- length(rows)
  if (n < 2) return(list(header_row = 1L, stripped = list(), improved = 0))
  ncols <- length(rows[[1]])
  if (ncols < 2) return(list(header_row = 1L, stripped = list(), improved = 0))

  as_df <- function(rs) {
    m <- do.call(rbind, lapply(rs, function(r) {
      length(r) <- ncols; as.character(r)
    }))
    as.data.frame(m, stringsAsFactors = FALSE)
  }
  # body_numeric(h) = type-consistency of the rows strictly below candidate header
  # h, for h = 1..scan_n. h = 1 means "row 1 is the header".
  scan_n <- min(as.integer(max_scan) + 1L, n - 1L)
  body_numeric <- vapply(seq_len(scan_n), function(h) {
    body <- rows[(h + 1L):n]
    if (length(body) == 0) return(NA_real_)
    .numeric_col_fraction(as_df(body))
  }, numeric(1))

  # Walk down from the top: strip a leading row only while it is junk, judged
  # against the body that would remain if the NEXT row were the header. Stop at
  # the first non-junk row -- that is the true header.
  strip <- 0L
  while (strip < scan_n - 1L &&
         .is_junk_above_header(rows[[strip + 1L]], body_numeric[strip + 2L])) {
    strip <- strip + 1L
  }
  if (strip < 1L)
    return(list(header_row = 1L, stripped = list(), improved = 0))

  # The row we would PROMOTE must itself look like a header: it needs real
  # variable NAMES -- non-empty, non-numeric, non-placeholder tokens. A headerless
  # numeric file (every row is data, e.g. 1|1|3|2 or a column-index row NA|1|2|3)
  # has no such row, so stripping its first row and using the next (numeric) row as
  # names would FABRICATE a header. Refuse unless the promoted row is textual.
  hdr_vals <- trimws(as.character(rows[[strip + 1L]]))
  hdr_real <- hdr_vals[nzchar(hdr_vals) & !is.na(hdr_vals) &
                         !toupper(hdr_vals) %in%
                           c("NA", "NAN", "NULL", "N/A", "INF", "-INF", ".") &
                         !.is_placeholder_name(hdr_vals)]
  # A token is textual (a real name) only when it does NOT parse as a number.
  # `.as_num_safe("NaN")` returns NaN -- itself NA under is.na() -- so a numeric NaN
  # cell must not be mistaken for text; the NA-like filter above already drops it.
  num <- .as_num_safe(hdr_real)
  hdr_text <- hdr_real[is.na(num) & !is.nan(num)]
  if (length(hdr_text) < 2L)
    return(list(header_row = 1L, stripped = list(), improved = 0))

  # Only accept if the promotion did not WORSEN typing (it may merely relabel a
  # few columns, as in CDA, so we require >= not >).
  base_numeric <- body_numeric[1]                       # body if row 1 were header
  new_numeric  <- body_numeric[strip + 1L]              # body below the true header
  if (is.na(new_numeric) || new_numeric < base_numeric)
    return(list(header_row = 1L, stripped = list(), improved = 0))

  list(header_row = strip + 1L,
       stripped   = rows[seq_len(strip)],
       improved   = new_numeric - base_numeric)
}

#' Promote a mis-placed header row and drop leading metadata rows
#'
#' When a banner / blank / units / repeated-label row sits ABOVE the real header,
#' the reader takes that top row as the header (inventing `...N` names, or spreading
#' one label -- `CDA` merged across 110 columns -- into `CDA...1 ... CDA...110`). This
#' finds the true header among the first few rows via [.detect_header_row()],
#' promotes it to the column names, drops it and everything above, and re-types the
#' freed columns. It is the inverse of [data_strip_qualtrics_header()] (which
#' strips junk rows BELOW a correct header).
#'
#' The input must be read so that the real header is still a DATA row (i.e. read
#' headerless, or with the reader's own header as row-0 metadata). Pass the rows
#' via `raw_rows` (the file re-read headerless as character) so the detector can
#' see the row the reader swallowed as the header; without it the function falls
#' back to treating `df`'s own first rows as the scan window.
#'
#' @param df the data.frame as normally read (reader-assigned names)
#' @param raw_rows optional list of character vectors: the first rows of the file
#'   read WITHOUT a header, so the reader-swallowed header row is visible. When
#'   supplied it is the authoritative scan window.
#' @param max_scan how many leading rows to consider as metadata above the header
#'
#' @returns a list with `df` (possibly re-headed), `promoted` (1-based count of
#'   metadata rows removed above the header, 0 = unchanged), and `stripped`
#'   (character vectors of those removed rows, for reporting).
#' @export
#' @keywords internal
data_promote_header_row <- function(df, raw_rows = NULL, max_scan = 4L) {
  if (is.null(df) || nrow(df) < 2 || ncol(df) < 2)
    return(list(df = df, promoted = 0L, stripped = list()))

  # Scan window: the headerless rows if given (so the reader's own header row is
  # included as a candidate), else the reader-headed frame's first rows.
  if (!is.null(raw_rows) && length(raw_rows) >= 2) {
    rows <- lapply(raw_rows, as.character)
    det  <- .detect_header_row(rows, max_scan = max_scan)
    if (det$header_row <= 1L)
      return(list(df = df, promoted = 0L, stripped = list()))
    # header_row is 1-based over raw_rows; rows above it (header_row - 1) are the
    # reader-swallowed header + any metadata. The new names come from that row.
    hdr <- det$header_row
    new_names <- as.character(rows[[hdr]])
  } else {
    # Fallback: the reader already consumed a header, so df's row i corresponds to
    # raw row i+1. Prepend the current names as the row-0 header candidate.
    header_as_row <- as.character(names(df))
    body_rows <- lapply(seq_len(min(max_scan, nrow(df))),
                        function(i) as.character(df[i, , drop = TRUE]))
    rows <- c(list(header_as_row), body_rows)
    det  <- .detect_header_row(rows, max_scan = max_scan)
    if (det$header_row <= 1L)
      return(list(df = df, promoted = 0L, stripped = list()))
    hdr <- det$header_row
    new_names <- as.character(rows[[hdr]])
  }

  n_strip   <- length(det$stripped)
  new_names <- trimws(new_names)
  new_names[is.na(new_names) | !nzchar(new_names)] <- ""
  new_names <- make.unique(ifelse(nzchar(new_names),
                                  new_names, paste0("V", seq_along(new_names))))

  # Rebuild the body from the rows below the true header. Prefer the authoritative
  # raw_rows path (re-read below the header); otherwise slice df.
  if (!is.null(raw_rows) && length(raw_rows) >= 2) {
    # df was read with the WRONG header, so df row j = raw row j+1. The true
    # header is raw row `hdr`, so keep df rows from (hdr - 1) onward, dropping the
    # header row itself: df rows (hdr) .. end.
    keep_from <- hdr                      # df index of first true data row
    candidate <- df[keep_from:nrow(df), , drop = FALSE]
  } else {
    candidate <- df[hdr:nrow(df), , drop = FALSE]
  }
  if (length(new_names) == ncol(candidate)) names(candidate) <- new_names
  rownames(candidate) <- NULL

  # Re-coerce columns that are now fully numeric (the metadata rows above were what
  # forced them to character). Mirrors data_strip_qualtrics_header().
  for (j in seq_along(candidate)) {
    if (!is.character(candidate[[j]])) next
    v <- trimws(candidate[[j]]); ne <- v[!is.na(v) & nzchar(v)]
    if (length(ne) == 0) next
    if (all(!is.na(.as_num_safe(ne))))
      candidate[[j]] <- .as_num_safe(v)
  }

  list(df = candidate, promoted = n_strip, stripped = det$stripped)
}

# -- Trial-level (paradata) source-format detection ----------------------------
# Several data formats record per-trial PARADATA (response times, trial/stimulus
# indices) alongside each response. Detected here so codebook_check can route that
# paradata to a Behaverse `trial` document (see R/behaverse-convert.R) instead of
# mislabelling it as psychometric scales. Each detector keys on the reserved
# column vocabulary that identifies its format, mirroring data_check_is_qualtrics.

# -- Reader: E-Prime text export -----------------------------------------------
# E-Prime exports are UTF-16 (or BOM) text: a header block of `Field: value`
# lines, then repeating `Level: 3` frames (one per Trial), each a run of
# `Field: value` lines. Object timing is `<obj>.RT` (ms) or, if absent,
# `<obj>.RTTime - <obj>.OnsetTime` (absolute clock). Parses one file to a list of
# per-trial named lists, plus the header (Subject, Experiment).
# Does this file's content look like an E-Prime export? E-Prime writes a fixed
# header block whose markers are unmistakable. Content-based, because E-Prime's
# .txt extension is far too ambiguous to classify on the name alone.
.eprime_is_export <- function(path) {
  head_lines <- text_peek(path, n = 30L)
  if (!length(head_lines)) return(FALSE)
  any(grepl("^\\*\\*\\*\\s*Header Start", head_lines)) ||
    (any(grepl("^(Experiment|Subject):", head_lines)) &&
       any(grepl("^LevelName:", head_lines)))
}

# Is this file a trial-level format (Behaverse / Inquisit / E-Prime / jsPsych)?
# CHEAP: reads only the header (or the first lines for E-Prime), not the whole
# file, so screening hundreds of per-participant files is fast. Used by data_check
# to hold trial-level files OUT of the per-file tabular extractor and route them to
# the Behaverse accumulator instead -- otherwise 200 per-participant E-Prime files
# would become 200 separate "datasets" rather than one merged instrument.
.bh_is_trial_level_file <- function(path) {
  if (length(path) != 1L || is.na(path) || !file.exists(path)) return(FALSE)
  ext <- tolower(tools::file_ext(path))
  if (ext %in% c("txt", "edat", "edat2") && .eprime_is_export(path)) return(TRUE)
  # Only DELIMITED-TEXT files can be a trial-level table, so only those are worth
  # a read.csv sniff. Reading a binary/document (.docx/.pdf/.sav/.xlsx/...) as CSV
  # produces "invalid input / embedded nulls" warnings and can return garbage, so
  # gate on the extension first. (E-Prime's own extensions were handled above.)
  # NOT ".log": a Stata plain-text log genuinely could be a trial-level table by
  # this extension alone, but no real corpus example has ever been found to
  # confirm a sniffer against, so .log is left out here and classed
  # "documentation" in .ext_registry (R/data_check_helpers.R) instead of
  # guessed at.
  .bh_sniff_exts <- c("csv", "tsv", "dat", "iqdat", "txt")
  if (!ext %in% .bh_sniff_exts) return(FALSE)
  # A one-row header read is enough for the data-frame detectors.
  hdr <- tryCatch(
    utils::read.csv(path, check.names = FALSE, nrows = 1L,
                    fileEncoding = "UTF-8-BOM",
                    sep = if (ext == "iqdat") "\t" else ","),
    error = function(e) NULL)
  if (is.null(hdr) || ncol(hdr) == 0) return(FALSE)
  data_check_is_behaverse(hdr) || data_check_is_inquisit(hdr) ||
    data_check_is_jspsych(hdr)
}

#' Detect trial-level behavioural-data source formats
#'
#' Each function reports whether a data frame is an export from a particular
#' trial-level format, from its reserved column names. Used by codebook_check to
#' recognise paradata (response times, trial/stimulus channels) and normalise it
#' to the Behaverse `trial` schema rather than treating it as scale items.
#'
#' - `data_check_is_behaverse()` -- native Behaverse tidy long form (an
#'   `instrument_id` column plus Response channels such as `response_numeric`,
#'   `response_time`, `trial_index`).
#' - `data_check_is_inquisit()` -- Millisecond Inquisit `.iqdat` (`subject`,
#'   `blockcode`, `trialcode`, `latency`).
#' - `data_check_is_jspsych()` -- jsPsych (`trial_type`, `rt`, and `trial_index`
#'   or `time_elapsed`).
#' - `data_check_is_psychopy()` -- PsychoPy Builder (`<loop>.thisN`/`.thisRepN`
#'   loop counters, `<comp>.started`/`.stopped` timing, or `psychopyVersion` /
#'   `frameRate` / `expName`); machinery is matched by these suffixes, not a
#'   fixed column list, because PsychoPy column names are study-specific.
#'
#' E-Prime is detected from file text, not a data frame (see
#' `.bh_parse_eprime()`), because its export is a header + `Level: 3` frames
#' rather than a flat table.
#'
#' @param df a data.frame (a read tabular file)
#'
#' @returns `TRUE` when `df` matches the format, else `FALSE`.
#' @export
#' @keywords internal
data_check_is_behaverse <- function(df) {
  if (is.null(df) || ncol(df) == 0) return(FALSE)
  nm <- names(df)
  has <- function(x) x %in% nm
  # instrument_id plus at least one Behaverse Response channel, OR the wide-pivot
  # channel suffix (<item>_response_numeric_i1) that a wide Behaverse export uses.
  channels <- c("response_numeric", "response_time", "response_validation_time",
                "trial_index", "response_option_index", "stimulus_type")
  if (has("instrument_id") && any(vapply(channels, has, logical(1))))
    return(TRUE)
  any(grepl("_response_numeric_i[0-9]+$", nm)) &&
    any(grepl("_(response_time|trial_index)_i[0-9]+$", nm))
}

#' @rdname data_check_is_behaverse
#' @export
#' @keywords internal
data_check_is_inquisit <- function(df) {
  if (is.null(df) || ncol(df) == 0) return(FALSE)
  nm <- tolower(names(df))
  # Inquisit's fixed per-trial columns co-occur essentially only in .iqdat files.
  sum(c("subject", "blockcode", "trialcode", "latency") %in% nm) >= 3L
}

#' @rdname data_check_is_behaverse
#' @export
#' @keywords internal
data_check_is_jspsych <- function(df) {
  if (is.null(df) || ncol(df) == 0) return(FALSE)
  nm <- names(df)
  "trial_type" %in% nm && "rt" %in% nm &&
    any(c("trial_index", "time_elapsed", "internal_node_id") %in% nm)
}

#' @rdname data_check_is_behaverse
#' @export
#' @keywords internal
data_check_is_psychopy <- function(df) {
  if (is.null(df) || ncol(df) == 0) return(FALSE)
  nm <- names(df)
  # PsychoPy Builder output is unmistakable from its component/loop naming: a
  # loop-counter suffix (<loop>.thisN / .thisIndex / .thisRepN / .thisTrialN) or
  # a component timing suffix (<comp>.started / .stopped), or the run-metadata
  # columns PsychoPy always writes (psychopyVersion / frameRate / expName). The
  # column names are study-specific, so PsychoPy is recognised by these SUFFIXES
  # and reserved names, not a fixed column list.
  any(grepl("[.]this(N|Index|RepN|TrialN)$", nm)) ||
    any(grepl("[.](started|stopped)$", nm)) ||
    any(c("psychopyVersion", "frameRate", "expName") %in% nm)
}

# -- Likert scale-block detection ----------------------------------------------
# Shared by data_validate (careless responding) and codebook_check (LLM scale
# identification). A "scale block" is a run of adjacent Likert-type columns that
# share a variable-name prefix, i.e. one psychometric scale (PANAS_1..10).
#
# See also "Task column detection" below, which is the behavioural-task
# counterpart: tasks do not produce Likert blocks, so they are invisible here
# and need their own detector.

# Minimum items for a block to count as a scale. Set to 3 to catch genuine short
# scales (e.g. 3-item subscales) at the cost of a few more noisy small fragments;
# the dictionary/LLM naming stage filters those out (an un-nameable group stays
# unnamed rather than becoming a false scale). Shared by scale grouping and the
# careless-responding detector (.dv_careless_min_items).
.scale_min_items <- 3L

# Is a column a plausible Likert item? Integer-valued, 3-11 distinct levels (2
# is binary, not Likert), spanning a narrow range within a plausible bound.
#
# Deliberately does NOT key on the exact observed min-max: within one scale,
# different items reach different extremes, so per-column range varies (item A
# 1-4, item B 2-5) even on a shared metric. Keying on exact range would split a
# single scale into fragments; membership is decided by name prefix instead.
.is_likert_item <- function(x) {
  if (!is.numeric(x)) {
    xn <- suppressWarnings(as.numeric(as.character(x)))
    if (length(xn) == 0) return(FALSE)
    na_frac <- mean(is.na(xn))
    if (!is.finite(na_frac) || na_frac > 0.2) return(FALSE)
    x <- xn
  }
  x <- x[!is.na(x)]
  if (length(x) < 10) return(FALSE)
  if (any(x != round(x))) return(FALSE)
  u <- unique(x)
  length(u) >= 3 && length(u) <= 11 &&
    diff(range(u)) <= 12 && min(u) >= -5 && max(u) <= 100
}

# -- Task column detection (reaction time / accuracy) --------------------------
# A behavioural task does not produce a Likert block, so `.detect_scale_blocks`
# cannot see one: task data is either one row per TRIAL (subject, trial,
# condition, rt, correct) or one aggregated column per condition
# (stroop_rt_congruent). These helpers detect the two column kinds a task
# almost always yields, so codebook_check can recognise a task in the data the
# way it recognises a scale.
#
# The detection is deliberately name-AND-value based. A name alone is too weak
# ("correct" could be anything) and values alone are too weak (any positive
# continuous column resembles an RT). Both must agree.

# Name patterns. `\brt\b` needs the word boundary: "start", "sort" and "party"
# all contain "rt" as a substring.
.RT_NAME_RE  <- "(^|[^a-z])(rt|reaction[._ -]?time|response[._ -]?time|latency|resp[._ -]?time)([^a-z]|$)"
.ACC_NAME_RE <- "(^|[^a-z])(acc|accuracy|correct|iscorrect|is[._ -]?correct|error|errors|hit|hits|miss|misses)([^a-z]|$)"

# Does a column look like a REACTION TIME by its values?
# Positive, continuous-ish, right-skewed and wide -- the inverse of the Likert
# signature in `.is_likert_item`. Deliberately does not test skew directly: a
# small trial count makes skew unstable, whereas "many distinct positive values
# over a wide range" is robust.
.looks_like_rt <- function(x) {
  if (!is.numeric(x)) {
    xn <- suppressWarnings(as.numeric(as.character(x)))
    if (length(xn) == 0 || mean(is.na(xn)) > 0.2) return(FALSE)
    x <- xn
  }
  x <- x[!is.na(x) & is.finite(x)]
  if (length(x) < 10) return(FALSE)
  if (any(x < 0)) return(FALSE)                 # RTs are never negative
  u <- unique(x)
  # Many distinct values, and a spread no rating scale reaches. Accepts both
  # milliseconds (200-3000) and seconds (0.2-3.0), hence the two-armed test.
  if (length(u) < 10) return(FALSE)
  rng <- diff(range(x))
  frac_distinct <- length(u) / length(x)
  (rng > 12 && stats::median(x) > 20) ||        # ms-like
    (any(x != round(x)) && rng > 0.05 && stats::median(x) < 60)  # s-like
}

# Does a column look like ACCURACY by its values?
# Either binary correctness (0/1, TRUE/FALSE) or a proportion in [0, 1].
.looks_like_accuracy <- function(x) {
  if (is.logical(x)) return(sum(!is.na(x)) >= 10)
  if (!is.numeric(x)) {
    xn <- suppressWarnings(as.numeric(as.character(x)))
    if (length(xn) == 0 || mean(is.na(xn)) > 0.2) return(FALSE)
    x <- xn
  }
  x <- x[!is.na(x) & is.finite(x)]
  if (length(x) < 10) return(FALSE)
  u <- unique(x)
  if (all(u %in% c(0, 1))) return(TRUE)                       # binary correct
  all(x >= 0 & x <= 1) && length(u) > 2                       # proportion
}

# Classify every column of `df` as an RT / accuracy / condition column, or not a
# task column at all. Returns a data.frame with one row per column:
#   column_name, kind ("rt" | "accuracy" | "condition" | ""), by_name, by_value
#
# `kind` is only set when the NAME and the VALUES agree, except for `condition`,
# which is name-and-shape based (a low-cardinality non-numeric column whose name
# says condition/block/trial_type).
.detect_task_columns <- function(df) {
  empty <- data.frame(column_name = character(0), kind = character(0),
                      by_name = logical(0), by_value = logical(0))
  if (is.null(df) || !is.data.frame(df) || ncol(df) == 0) return(empty)
  nm  <- names(df)
  key <- tolower(nm)

  rt_name  <- grepl(.RT_NAME_RE,  key, perl = TRUE)
  acc_name <- grepl(.ACC_NAME_RE, key, perl = TRUE)
  rt_val   <- vapply(df, .looks_like_rt,       logical(1))
  acc_val  <- vapply(df, .looks_like_accuracy, logical(1))

  # A condition column: named like one, few distinct values, many rows.
  cond_name <- grepl("(^|[^a-z])(condition|cond|block|trial[._ -]?type|congruen\\w*|stimulus[._ -]?type)([^a-z]|$)",
                     key, perl = TRUE)
  cond_shape <- vapply(df, function(x) {
    v <- x[!is.na(x)]
    length(v) >= 10 && length(unique(v)) >= 2 && length(unique(v)) <= 8
  }, logical(1))

  kind <- rep("", length(nm))
  # Order matters: accuracy is tested first because an `acc` column of 0/1 also
  # passes no RT test, but an `rt` column of small seconds could look like a
  # proportion. Requiring the NAME to agree keeps the two apart.
  kind[acc_name & acc_val]  <- "accuracy"
  kind[rt_name  & rt_val]   <- "rt"
  kind[cond_name & cond_shape & kind == ""] <- "condition"

  data.frame(column_name = nm, kind = kind,
             by_name  = rt_name | acc_name | cond_name,
             by_value = rt_val | acc_val)[kind != "", , drop = FALSE]
}

# Is a column a plausible per-trial ACCURACY item -- one column per trial, scored
# right/wrong? The task analogue of `.is_likert_item`, and deliberately its
# complement: `.is_likert_item` requires >= 3 distinct values, so a binary
# column fails it and a block of them is invisible to `.detect_scale_blocks`.
# That is the gap this closes: `raven_1..18` or `iat_1..20` scored 0/1 is a real
# item battery that the scale detector cannot see.
#
# Binary only (0/1, TRUE/FALSE). A 2-level column that is not 0/1 (e.g. 1/2
# coding, or "left"/"right") is NOT accepted: it is as likely to be a group
# variable, and mistaking a condition for an accuracy item would be worse than
# missing it.
.is_accuracy_item <- function(x) {
  if (is.logical(x)) return(sum(!is.na(x)) >= 10)
  if (!is.numeric(x)) {
    xn <- suppressWarnings(as.numeric(as.character(x)))
    if (length(xn) == 0 || mean(is.na(xn)) > 0.2) return(FALSE)
    x <- xn
  }
  x <- x[!is.na(x)]
  if (length(x) < 10) return(FALSE)
  u <- sort(unique(x))
  identical(as.numeric(u), c(0, 1))
}

# Detect ACCURACY BLOCKS: maximal runs of adjacent binary columns sharing a name
# prefix (raven_1..18, iat_1..20). Mirrors `.detect_scale_blocks` exactly --
# same contiguity assumption, same prefix rule, same minimum size -- but keyed on
# `.is_accuracy_item` instead of `.is_likert_item`.
#
# A minimum of `.task_acc_min_items` (8) applies rather than `.scale_min_items`
# (3), because binary columns are common in survey data for reasons that have
# nothing to do with tasks: three adjacent yes/no demographics would otherwise
# be read as a 3-item accuracy battery. A real trial-level battery is long.
.task_acc_min_items <- 8L

.detect_accuracy_blocks <- function(df, min_items = .task_acc_min_items) {
  if (is.null(df) || !is.data.frame(df) || ncol(df) == 0) return(list())
  ok <- vapply(df, .is_accuracy_item, logical(1))
  nm <- names(df)
  blocks <- list(); start <- NA_integer_; cur_pre <- NA_character_
  flush <- function(endi) {
    if (!is.na(start) && (endi - start + 1L) >= min_items)
      blocks[[length(blocks) + 1L]] <<- seq.int(start, endi)
    start <<- NA_integer_; cur_pre <<- NA_character_
  }
  for (i in seq_along(nm)) {
    if (!ok[[i]]) { flush(i - 1L); next }
    pre <- .scale_name_prefix(nm[[i]])
    if (is.na(start)) { start <- i; cur_pre <- pre; next }
    if (!identical(pre, cur_pre)) { flush(i - 1L); start <- i; cur_pre <- pre }
  }
  flush(length(nm))
  blocks
}

# Does `df` look like task data at all? TRUE when it has an RT or accuracy
# column, or an accuracy block. Used to decide whether to run the task matcher
# on a file.
.is_task_data <- function(df) {
  tc <- .detect_task_columns(df)
  if (any(tc$kind %in% c("rt", "accuracy"))) return(TRUE)
  length(.detect_accuracy_blocks(df)) > 0
}

# Variable-name prefix: strip a trailing item number (bfi_1 -> bfi, RSE10 -> rse)
# so PANAS_1..10 and RSE_1..5 are recognised as two scales even when adjacent
# and on the same response range.
.scale_name_prefix <- function(nm) {
  p <- sub("[._-]?[0-9]+$", "", nm)
  p <- sub("[._-]+$", "", p)
  tolower(p)
}

# Pooled response range of a set of item columns, as a "min-max" label (e.g.
# "1-7").
.scale_block_range <- function(block) {
  v <- unlist(lapply(block, function(x)
    suppressWarnings(as.numeric(as.character(x)))), use.names = FALSE)
  v <- v[!is.na(v)]
  if (length(v) == 0) return("?")
  paste0(min(v), "-", max(v))
}

# Detect Likert scale blocks in a data frame: maximal runs of adjacent Likert
# columns sharing a name prefix. Returns a list of integer column-index vectors,
# one per block of at least `min_items` items. Scales are assumed contiguous
# (holds for typical survey exports, Q1_1, Q1_2, ...); a prefix change or a
# non-Likert column breaks a run.
.detect_scale_blocks <- function(df, min_items = .scale_min_items) {
  ok <- vapply(df, .is_likert_item, logical(1))
  nm <- names(df)
  blocks <- list(); start <- NA_integer_; cur_pre <- NA_character_
  flush <- function(endi) {
    if (is.na(start)) return(invisible())
    if (endi - start + 1L >= min_items)
      blocks[[length(blocks) + 1L]] <<- seq.int(start, endi)
  }
  for (j in seq_along(ok)) {
    p <- if (isTRUE(ok[[j]])) .scale_name_prefix(nm[[j]]) else NA_character_
    same <- !is.na(p) && identical(p, cur_pre)
    if (!same) {
      flush(j - 1L)
      cur_pre <- p
      start <- if (!is.na(p)) j else NA_integer_
    }
  }
  flush(length(ok))
  blocks
}

# Is a prefix group a RATING-LIKE block, judged from data_check's per-column
# statistics (no file re-read)? This is broader than `.detect_scale_blocks`,
# which only accepts small-integer Likert items and so misses 0-100 slider /
# percentage rating scales (values like 11, 95, 71). It exists to gate what the
# OSD exporter is allowed to WRITE -- named or unnamed -- so that a coherent rating
# block is kept while genuine non-scales (probabilities, model parameters) are
# rejected.
#
# A block qualifies when, pooled across its columns:
#   * at least 60% of its columns are numeric (a scale block is numeric ratings,
#     not free text / ids);
#   * the pooled minimum is >= -1 -- rejects unbounded model parameters that go
#     negative (e.g. alpha/beta weights spanning -52 .. +10);
#   * the pooled maximum is > 1 -- rejects [0,1] quantities (probabilities,
#     posterior means) that are NOT ratings, and
#   * the pooled maximum is <= 100 -- the upper bound of a plausible rating
#     envelope (0-100 sliders, 1-7 Likert, 0-10 scales all pass; a summed total
#     or a count that runs into the hundreds does not).
# `cols` are the block's column names; `source_file` scopes the lookup so a
# same-named column in another file is not mixed in.
.scale_block_is_ratinglike <- function(cols, source_file, columns_df) {
  if (is.null(columns_df) || !nrow(columns_df) ||
      !all(c("source_file", "column_name", "min", "max") %in% names(columns_df)))
    return(FALSE)
  key  <- paste(columns_df$source_file, columns_df$column_name, sep = "\x01")
  want <- paste(source_file, cols, sep = "\x01")
  idx  <- which(key %in% want)
  if (length(idx) < .scale_min_items) return(FALSE)

  mn <- suppressWarnings(as.numeric(columns_df$min[idx]))
  mx <- suppressWarnings(as.numeric(columns_df$max[idx]))
  numeric_frac <- mean(is.finite(mn) & is.finite(mx))
  if (!is.finite(numeric_frac) || numeric_frac < 0.6) return(FALSE)

  lo <- suppressWarnings(min(mn, na.rm = TRUE))
  hi <- suppressWarnings(max(mx, na.rm = TRUE))
  if (!is.finite(lo) || !is.finite(hi)) return(FALSE)
  lo >= -1 && hi > 1 && hi <= 100
}
