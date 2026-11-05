# Helpers for the reproducibility_check module. This module answers "can the
# paper's code be run on its data?" in two phases.
#
# STATIC helpers (never run downloaded code): collect the dependencies a run
# would need, rewrite the file paths that do not work in the repository as the
# authors organised it, work out the order the scripts must run in, and diagnose
# why a referenced input is unavailable. Built on the static analysis already in
# code_check.R (code_library_names, code_file_refs, code_parse_r) and the files
# data_check lists.
#
# EXECUTION helpers (run only when the module is called with execute = TRUE):
# repro_materialize_layout / repro_write_scripts build a throwaway copy of the
# repository, repro_install_deps installs the declared packages into a
# throwaway library, and repro_run_scripts runs each script in an isolated
# subprocess (callr) with a per-script timeout, capturing its outcome and output.
# Running downloaded code is a deliberate, opt-in action; see those functions.

#' Collect the package dependencies a set of code files declare
#'
#' Walks each code file with [code_library_names()] and returns the distinct
#' packages the paper's code loads, tagged with how each was referenced and
#' whether it is installable from CRAN, from a named GitHub/URL source, or is a
#' base/recommended package already shipped with R (so a run need not install
#' it). This is the input list a run would install; it is deliberately
#' name-only, because static analysis cannot know which *version* was used —
#' running against the latest packages and observing a break is itself a
#' finding (it argues the authors should have pinned versions).
#'
#' GitHub/URL sources are read from the code text, not the package name: a call
#' `remotes::install_github("user/repo")` names the source `user/repo`, which is
#' matched back to the package by the repo's basename. When a script pins a ref
#' (`user/repo@v1.2`), the ref is kept.
#'
#' @param code_text the code text for a single file (character vector), OR a list
#'   of such vectors (one per file) to pool across files
#' @param lang the language (only R declares installable packages here)
#'
#' @returns a data frame with columns `package`, `source` (`cran`, `bioc`,
#'   `github`, `url`, or `base`), `ref` (the source path/ref for github/url,
#'   else NA), and `base` (logical, TRUE for a base/recommended package). One
#'   row per distinct package. Empty frame (same columns) when none are found.
#' @export
#'
#' @examples
#' code_text <- c(
#'   "library(dplyr)",
#'   "remotes::install_github('tidyverse/ggplot2@v3.4.0')",
#'   "x <- stats::sd(1:10)"
#' )
#' repro_dependencies(code_text)
repro_dependencies <- function(code_text, lang = "R") {
  empty <- data.frame(package = character(0), source = character(0),
                      ref = character(0), base = logical(0))
  if (is.null(code_text)) return(empty)

  # Pool a list of per-file texts into one search, keeping the union of names.
  if (is.list(code_text)) {
    parts <- lapply(code_text, repro_dependencies, lang = lang)
    parts <- Filter(function(d) is.data.frame(d) && nrow(d) > 0, parts)
    if (length(parts) == 0) return(empty)
    out <- dplyr::bind_rows(parts)
    # A package named both bare (library) and via a github source: keep the
    # github row (it carries the real install source), drop the duplicate.
    out <- out[order(out$package, match(out$source, c("github", "url", "cran", "base"))), ]
    out[!duplicated(out$package), , drop = FALSE]
  } else {
    if (!identical(lang, "R")) return(empty)

    # Package names loaded/installed, from the shared static extractor.
    names_df <- code_library_names(code_text, "R")
    pkgs <- unique(names_df$package)
    if (length(pkgs) == 0) return(empty)

    # GitHub / URL install sources named in the text, matched to a package by
    # the repo basename. install_github("user/repo@ref") -> package "repo".
    joined <- paste(code_text, collapse = "\n")
    gh_pat  <- "install_github\\s*\\(\\s*['\"]([^'\"]+)['\"]"
    url_pat <- "install\\.packages\\s*\\(\\s*['\"](https?://[^'\"]+)['\"]"
    gh  <- regmatches(joined, gregexpr(gh_pat,  joined, perl = TRUE))[[1]]
    url <- regmatches(joined, gregexpr(url_pat, joined, perl = TRUE))[[1]]
    gh_refs  <- sub(gh_pat,  "\\1", gh,  perl = TRUE)
    url_refs <- sub(url_pat, "\\1", url, perl = TRUE)
    # groundhog.library("github::user/repo", date), and the same without github::
    gh_hog_pat <- paste0("['\"]github::([\\w.-]+/[^'\"]+)['\"]|",
                         "groundhog\\.library\\s*\\(\\s*(?:pkg\\s*=\\s*)?['\"]([\\w.-]+/[^'\"]+)['\"]")
    gh_hog <- regmatches(joined, gregexpr(gh_hog_pat, joined, perl = TRUE))[[1]]
    gh_refs <- c(gh_refs, sub(gh_hog_pat, "\\1\\2", gh_hog, perl = TRUE))
    # repo basename (drop owner and @ref) is the installed package name
    gh_pkg  <- sub("@.*$", "", basename(gh_refs))
    url_pkg <- sub("[_.].*$", "", basename(url_refs))   # pkg_1.0.tar.gz -> pkg

    base_pkgs <- .repro_base_packages()
    bioc_pkgs <- .repro_bioc_packages()

    src <- rep("cran", length(pkgs))
    ref <- rep(NA_character_, length(pkgs))
    src[pkgs %in% base_pkgs] <- "base"
    # A Bioconductor package is never on CRAN, so install.packages() always
    # fails for it with "there is no package called 'X'" even though the
    # package genuinely exists and installs fine via BiocManager -- see
    # .repro_bioc_packages(). Applied AFTER base (a base package is never also
    # a Bioconductor one) but BEFORE the github/url overrides below, so a
    # script that explicitly names a GitHub/URL source for a Bioconductor
    # package (a development version, say) still keeps that more specific
    # source rather than being relabelled "bioc".
    src[pkgs %in% bioc_pkgs & !pkgs %in% base_pkgs] <- "bioc"
    for (k in seq_along(gh_pkg)) {
      hit <- pkgs == gh_pkg[k]
      if (any(hit)) { src[hit] <- "github"; ref[hit] <- gh_refs[k] }
    }
    for (k in seq_along(url_pkg)) {
      hit <- pkgs == url_pkg[k]
      if (any(hit)) { src[hit] <- "url"; ref[hit] <- url_refs[k] }
    }

    data.frame(package = pkgs, source = src, ref = ref,
               base = pkgs %in% base_pkgs)
  }
}

# The base + recommended packages shipped with R. A run never needs to install
# these, so they are tagged `base` and excluded from the install set. Resolved
# from the running R installation so it stays correct across R versions.
.repro_base_packages <- function() {
  ip <- tryCatch(utils::installed.packages(priority = c("base", "recommended")),
                 error = function(e) NULL)
  if (is.null(ip)) c("base", "methods", "utils", "stats", "graphics",
                     "grDevices", "datasets", "tools")
  else rownames(ip)
}

# A bundled, static list of common Bioconductor package names -- NOT a live
# query (BiocManager::available()/the Bioconductor JSON manifest would need
# network access at classification time, for a question that is answered just
# as well by a name lookup), and NOT a hard dependency: BiocManager is only
# ever used conditionally, at INSTALL time (see repro_install_deps()'s "bioc"
# branch), never here. This list exists purely so repro_dependencies() can
# tell a CRAN-absent, Bioconductor-only package (edgeR, phyloseq, DESeq2, ...)
# apart from a package that is simply unavailable anywhere -- confirmed as the
# single largest identifiable cause of install failures in a real corpus run
# (40 of 140 dependency_unavailable failures were Bioconductor packages
# classified "cran" and so never attempted via BiocManager::install()).
# Deliberately a fixed list rather than exhaustive: Bioconductor has ~2000+
# packages, but only the common ones actually showing up in analysis code
# (as opposed to Bioconductor's own internal infrastructure packages) need
# to be recognised here; an unlisted Bioconductor package still falls
# through to "cran" (misclassified, but no worse than before this fix) rather
# than the list being maintained as if it must be complete.
.repro_bioc_packages <- function() {
  c(
    "edgeR", "DESeq2", "limma", "phyloseq", "dada2", "LEA", "affy", "affyio",
    "AnnotationDbi", "AnnotationHub", "Biobase", "BiocGenerics",
    "BiocParallel", "BiocVersion", "biomaRt", "Biostrings", "BSgenome",
    "ComplexHeatmap", "ConsensusClusterPlus", "cummeRbund", "DelayedArray",
    "DESeq", "DiffBind", "DOSE", "edgeR", "EnhancedVolcano", "ensembldb",
    "enrichplot", "fgsea", "flowCore", "GenomeInfoDb", "GenomicAlignments",
    "GenomicFeatures", "GenomicRanges", "genefilter", "GEOquery", "ggtree",
    "goseq", "GSEABase", "GSVA", "Gviz", "HTSeq", "IRanges", "KEGGREST",
    "limma", "M3C", "MAGeCKFlute", "makecdfenv", "MassSpecWavelet",
    "MEDIPS", "methylKit", "minfi", "MotifDb", "mzR", "org.Hs.eg.db",
    "org.Mm.eg.db", "pathview", "PICS", "preprocessCore", "qvalue",
    "Rgraphviz", "Rhtslib", "rhdf5", "rtracklayer", "Rsamtools",
    "Rsubread", "S4Vectors", "scater", "scran", "SingleCellExperiment",
    "SummarizedExperiment", "sva", "topGO", "TxDb.Hsapiens.UCSC.hg19.knownGene",
    "TxDb.Hsapiens.UCSC.hg38.knownGene", "variancePartition", "vsn",
    "WGCNA", "XVector", "zlibbioc", "clusterProfiler", "ChIPseeker",
    "ChIPQC", "csaw", "DECIPHER", "deseq2", "monocle", "muscat", "Rbowtie",
    "Rhisat2", "Rsubread", "seqinr", "shinyMethyl", "slingshot",
    "tximport", "tximeta", "biovizBase", "regioneR", "karyoploteR",
    "VariantAnnotation", "STRINGdb", "graph", "RBGL", "limmaGUI",
    "beadarray", "lumi", "oligo", "crlmm", "snpStats", "IlluminaHumanMethylation450kanno.ilmn12.hg19",
    "IlluminaHumanMethylationEPICanno.ilm10b4.hg19", "FlowSOM", "CATALYST",
    "diffloop", "GenomicInteractions", "InteractionSet", "bsseq",
    "methylumi", "wateRmelon", "phyloseq", "microbiome", "ANCOMBC",
    "mixOmics", "MOFA2", "BiocSingular", "batchelor", "scDblFinder",
    "scuttle", "zellkonverter", "DropletUtils", "destiny", "MAST",
    "ShortRead", "SNPRelate", "decontam", "arrayQualityMetrics", "baySeq", "EBImage",
    "EBSeq", "gdsfmt", "OmicCircos", "pcaMethods", "rain", "SeqArray"
  ) |> unique()
}

#' Find file paths built at runtime with sprintf()/paste()/paste0()/file.path()
#'
#' A path is sometimes not written as a literal string but assembled from
#' variables — `read_csv(sprintf("%s/%s/x.csv", wd, wd_data))`. This is fragile
#' authoring (it silently breaks whenever the code runs from a different
#' working directory than the author's own), and it also defeats
#' [code_file_refs()]'s literal-string extraction: the "reference" it returns
#' is really the RAW FORMAT STRING (`"%s/%s/x.csv"`, placeholders and all), so a
#' naive substring replacement of that text later corrupts the call — the
#' surrounding `sprintf(...)` keeps its now-meaningless trailing arguments,
#' `, wd, wd_data)`, uselessly attached after the plan's target path was
#' spliced in as if it were the whole first argument.
#'
#' This finds every such call in a script, and reports enough to fix it
#' properly: the call's own full text (so the whole expression, not just the
#' quoted substring, can be replaced), the raw format string, and — when every
#' placeholder's corresponding argument is a simple variable resolvable to a
#' literal string via [.repro_simple_string_vars()] — the format string with
#' those values substituted in, which is what basename-matching should key on
#' instead of the placeholder-laden original.
#'
#' Only `%s`/`%d` placeholders (the common case in practice; a `sprintf` with a
#' width/precision modifier like `%05d` is deliberately out of scope, since a
#' zero-padded numeric ID is unlikely to appear in a bare data-file reference)
#' are substituted; anything else in the format leaves that call unresolved
#' (returned with `resolved = NA`), never guessed. The arguments after the
#' format string are split on a plain comma, so an argument that is ITSELF a
#' call containing a comma (`sprintf("%s.csv", paste0(a, b))`) is not split
#' correctly either — this only means that argument then fails to resolve to
#' any known variable (falling back to `resolved = NA`, the same safe
#' unresolved outcome as any other unrecognised argument), never a wrong guess.
#'
#' @param code_text the code text for a single file (character vector)
#'
#' @returns a data frame with `call_text` (the full matched call, e.g.
#'   `sprintf("%s/%s/x.csv", wd, wd_data)`), `fmt` (the raw format string),
#'   `resolved` (the format string with resolvable placeholders substituted, or
#'   NA if any placeholder could not be resolved), and `line` (1-based line
#'   number the call starts on). Empty frame when the script has no such calls.
#' @keywords internal
.repro_format_call_refs <- function(code_text) {
  empty <- data.frame(call_text = character(0), fmt = character(0),
                      resolved = character(0), line = integer(0))
  if (is.null(code_text) || length(code_text) == 0) return(empty)
  nc <- code_remove_comments(code_text, "R")
  joined <- paste(nc, collapse = "\n")

  # A call to one of the format/join functions, containing a quoted string with
  # a file-like extension somewhere in its arguments. Matched with a manual
  # balanced-paren scan (not a single regex) because the argument list can
  # itself contain nested parens (e.g. another function call as an argument).
  fn_pat <- "\\b(sprintf|paste0|paste|file\\.path)\\s*\\("
  starts <- gregexpr(fn_pat, joined, perl = TRUE)[[1]]
  if (length(starts) == 1 && starts == -1) return(empty)
  lens <- attr(starts, "match.length")

  # Split into characters once for the scans below: substr(joined, i, i)
  # counts from the start of a string with non-ASCII characters on every
  # call, so a character walk was quadratic in the text's length (hours on a
  # 1-MB script; #481).
  chars <- strsplit(joined, "", fixed = TRUE)[[1]]
  n <- length(chars)
  rows <- lapply(seq_along(starts), function(k) {
    open_paren <- starts[k] + lens[k] - 1L   # index of the "(" itself
    # Balanced scan from the "(" to its matching ")", respecting quotes so a
    # ")" inside a quoted string is not mistaken for the call's own close.
    depth <- 0L; i <- open_paren; in_str <- NA_character_
    end <- NA_integer_
    while (i <= n) {
      ch <- chars[i]
      if (!is.na(in_str)) {
        if (ch == "\\") i <- i + 1L   # skip an escaped char inside the string
        else if (ch == in_str) in_str <- NA_character_
      } else if (ch %in% c("'", '"')) in_str <- ch
      else if (ch == "(") depth <- depth + 1L
      else if (ch == ")") { depth <- depth - 1L; if (depth == 0L) { end <- i; break } }
      i <- i + 1L
    }
    if (is.na(end)) return(NULL)   # unbalanced — leave alone, do not guess
    call_text <- substr(joined, starts[k], end)
    args_text <- substr(joined, open_paren + 1L, end - 1L)

    # Only calls whose FIRST quoted string looks like a file reference (has an
    # extension) are candidates — this is what makes a plain paste0("a", "b")
    # elsewhere in the code invisible to this scan.
    m <- regexpr("(['\"])((?:[^'\"\\\\]|\\\\.)*)\\1", args_text, perl = TRUE)
    if (m == -1) return(NULL)
    fmt <- regmatches(args_text, m)
    quoted_len <- nchar(fmt)   # includes the surrounding quotes
    fmt <- substr(fmt, 2, nchar(fmt) - 1)
    if (!grepl("\\.[A-Za-z0-9]{1,8}$", fmt)) return(NULL)
    # A bare sprintf()-style format specifier ("%.2e", "%.3f", "%05.2f") also
    # satisfies the "ends in a dotted extension-shape" check above by pure
    # coincidence — its own "%" + digits + "." + conversion letter looks
    # exactly like "<name>.<ext>". Checked on the BASENAME (not the whole
    # `fmt`), since a real path with a placeholder ("data/%s_data.csv") must
    # still be treated as a candidate — only the LAST path segment being a
    # bare format spec (no directory content, no literal filename text at
    # all) means this quoted string was never a file reference to begin with.
    # Confirmed as a real false positive against a real corpus paper's script
    # (sprintf("%.2e", est) reported "%.2e" as a file "not present in the
    # repository") — the SAME bug .repro_format_call_refs()'s sibling scan in
    # code_check.R's code_file_refs() was already fixed for; this is the
    # other of two independent places a format specifier can be
    # misidentified as a filename, missed the first time because this
    # function has its own, separate quoted-string extraction.
    if (grepl("^%[-+ 0#]*[0-9]*(\\.[0-9]+)?[diouxXeEfgGaAscp]$",
             basename(gsub("\\\\", "/", fmt)), perl = TRUE)) return(NULL)

    # Only calls with EXTRA arguments AFTER the quoted format string are what
    # this scan is for: a bare sprintf("Exp1A_Data.txt") with nothing else is a
    # plain literal in a no-op wrapper — code_file_refs()'s plain-literal path
    # already handles it correctly, and there is no trailing-argument-orphaning
    # risk to fix. `args_text` still carries the quotes around fmt (unlike
    # `fmt` itself, stripped above), so the remainder starts where the quoted
    # string ends. It was counted from the start of `args_text`, so a string
    # that is not the first argument was misread: `paste(wd, "data.csv", sep =
    # "/")` was not a format call (only its literal was rewritten, giving
    # "wd/<target>"), and `paste0(path, ".R")` was one, for a file ".R" (U133)
    after_fmt <- substr(args_text, as.integer(m) + quoted_len, nchar(args_text))
    if (!grepl("^\\s*,", after_fmt, perl = TRUE)) return(NULL)

    line <- 1L + lengths(regmatches(substr(joined, 1, starts[k]),
                                    gregexpr("\n", substr(joined, 1, starts[k]))))[1]

    # Attempt substitution of %s/%d placeholders with the values of simple
    # top-level string-literal variables (wd <- "..."; wd_data <- "..."),
    # taken in argument order after the format string.
    extra_args <- trimws(strsplit(sub("^\\s*,\\s*", "", after_fmt, perl = TRUE),
                                  ",")[[1]])
    resolved <- NA_character_
    if (grepl("%[sd]", fmt) && length(extra_args) > 0) {
      vars <- .repro_simple_string_vars(nc)
      vals <- vars[extra_args]
      if (!any(is.na(vals)) && length(vals) == length(regmatches(
        fmt, gregexpr("%[sd]", fmt))[[1]])) {
        r <- fmt
        for (v in vals) r <- sub("%[sd]", v, r, perl = TRUE)
        resolved <- r
      }
    }
    data.frame(call_text = call_text, fmt = fmt, resolved = resolved, line = line)
  })
  rows <- Filter(Negate(is.null), rows)
  if (length(rows) == 0) return(empty)
  dplyr::bind_rows(rows)
}

#' Simple top-level string-literal variable assignments in a script
#'
#' A minimal constant-folder: finds assignments of the exact shape
#' `name <- "literal"` (or `=`/`<<-`) at the START of a (comment-free) line —
#' the same "top level" approximation [repro_defined_vars()] uses — and returns
#' the literal each name was last assigned. Only a bare quoted string is
#' resolved (no concatenation, no function calls); anything else leaves that
#' name absent from the result, so a caller asking for an unresolvable name
#' gets `NA`, never a guess.
#'
#' @param code_text the code text for a single file, comments already stripped
#'
#' @returns a named character vector (name = literal value), possibly empty
#' @keywords internal
.repro_simple_string_vars <- function(code_text) {
  pat <- "^([.a-zA-Z][.a-zA-Z0-9_]*)\\s*(?:<<-|<-|=)\\s*(['\"])((?:[^'\"\\\\]|\\\\.)*)\\2\\s*$"
  m <- regmatches(code_text, regexec(pat, code_text, perl = TRUE))
  hits <- Filter(function(x) length(x) == 4, m)
  if (length(hits) == 0) return(character(0))
  nm  <- vapply(hits, `[[`, character(1), 2)
  val <- vapply(hits, `[[`, character(1), 4)
  stats::setNames(val, nm)[!duplicated(nm, fromLast = TRUE)]
}

#' Find a script's WRITE calls and redirect their targets into the sandbox
#'
#' A script writes intermediate/output files by relative path
#' (`write.csv("../Data/Cleaned/x.csv", ...)`, `saveRDS(m, "out/model.rds")`).
#' [repro_rewrite_paths()] only ever rewrites READ calls (`code_file_refs()`'s
#' `include_writes` defaults to `FALSE` there) — a write's own path is left
#' exactly as the author wrote it, so when the materialised sandbox's directory
#' layout does not match the original repo's (it is Psych-DS's layout, not the
#' author's), the write's target directory frequently does not exist at all,
#' and the write fails with "cannot open the connection" — a sandboxing
#' artefact, not a real reproducibility finding, confirmed against a real
#' corpus paper whose script wrote to `"../Data/Cleaned/..."` (a path that only
#' resolves relative to the AUTHOR's own original working directory).
#'
#' Every detected write call is redirected to `file.path(output_dir,
#' basename(...))` — a single flat folder inside the sandbox that
#' [repro_materialize_layout()] always creates, so the write always succeeds
#' regardless of the original relative structure. Two call shapes are handled:
#'
#' * a LITERAL quoted target (`write.csv("cleaned.csv", ...)`) — redirected
#'   directly, keeping the literal's own basename;
#' * a BARE VARIABLE target (`write.csv(cleaned_file_name, ...)`, the variable
#'   assigned on an earlier line, e.g. `cleaned_file_name <- paste0("../Data/
#'   Cleaned/task_level_", Sys.Date(), ".csv")`) — the ENTIRE write call is
#'   replaced with a fixed literal path, using the VARIABLE's own name (not its
#'   unknowable runtime value, which may depend on `Sys.Date()` or other
#'   non-literal pieces this scan cannot evaluate) plus the extension its
#'   assignment's own last quoted segment declares. This does not need to
#'   resolve the variable's exact string value — only that its assignment
#'   LOOKS like a file path (ends `.<ext>` in its last quoted piece) — so it
#'   still succeeds even when a placeholder (`Sys.Date()`) makes the true
#'   runtime value unknowable ahead of time.
#'
#' A write whose target cannot be traced to a literal or a simple top-level
#' assignment (built from a loop variable, a function argument, a data value)
#' is left UNREWRITTEN, the same "do not guess" policy
#' [repro_rewrite_paths()] already follows for reads.
#'
#' @param code_text the code text for a single file (character vector)
#'
#' @returns a data frame with one row per detected write call: `call_text` (the
#'   full original call), `replacement` (the full replacement call text, or
#'   `NA` when unresolved), `redirected_name` (the basename the write now
#'   targets, for the read-side basename search to also check), and `line`
#'   (1-based line the call starts on). Empty frame when the script has no
#'   resolvable write calls.
#' @keywords internal
#' @param lang `"R"` (default) or `"Python"` — selects the write-function
#'   names matched, and (for the path-like-variable scan) the assignment
#'   operator recognised: R's `<-`/`<<-`/`=` vs. Python's `=` only. Python's
#'   `open(path, "w")` is deliberately NOT included in `write_fns` here even
#'   though it is a write call (see [.py_open_writes()], which [repro_file_io()]
#'   uses): `open()` returns a file HANDLE, not the path itself — the
#'   call that actually names the path to redirect — so this function instead
#'   matches the handle variable's own `.write(...)`/`.writelines(...)` calls
#'   is out of scope for a text-level, no-interpreter rewrite (which handle
#'   a later `.write()` belongs to is not recoverable without actually
#'   running the code); `open()` is left unredirected here and any
#'   relative-path failure surfaces as a plain execute-time error instead,
#'   same as any other unresolved reference this function already leaves
#'   alone (see `hit` below).
.repro_redirect_writes <- function(code_text, lang = "R") {
  empty <- data.frame(call_text = character(0), replacement = character(0),
                      redirected_name = character(0), line = integer(0))
  if (is.null(code_text) || length(code_text) == 0) return(empty)
  nc <- code_remove_comments(code_text, lang)
  joined <- paste(nc, collapse = "\n")

  write_fns_for <- list(
    R = c("write[\\._][A-Za-z\\._0-9]*", "saveRDS", "save\\.image",
         "save", "ggsave", "export", "fwrite"),
    # to_<format> (to_csv/to_pickle/to_parquet/...), numpy's save/savetxt,
    # matplotlib's savefig, and pickle's/json's dump — see code_check.R's
    # lang_write_regex (code_file_refs()) for the same list, kept in sync.
    # open() is excluded — see this function's own `lang` docs above.
    Python = c("to_[A-Za-z_0-9]+", "savetxt", "savefig", "dump", "save")
  )
  write_fns <- write_fns_for[[lang]] %||% write_fns_for[["R"]]
  fn_pat <- paste0("\\b(", paste(write_fns, collapse = "|"), ")\\s*\\(")
  starts <- gregexpr(fn_pat, joined, perl = TRUE)[[1]]
  if (length(starts) == 1 && starts == -1) return(empty)
  lens <- attr(starts, "match.length")

  # Does an arbitrary expression's TEXT look like it builds a file path — its
  # LAST quoted segment ends in a dotted extension? Covers both a bare literal
  # ("a.csv") and a paste0()/sprintf()-built path ("dir/", Sys.Date(), ".csv"),
  # since only the FINAL quoted piece needs checking for a plain "prefix +
  # <computed> + extension" shape either way — shared by the assignment-RHS
  # scan below AND by a write call's own INLINE argument (`write.csv(df,
  # paste0("dir/", Sys.Date(), ".csv"))`, no intermediate variable at all),
  # since both are exactly the same "does this expression's text end with a
  # file-shaped quoted piece" question, just applied to different text spans.
  .repro_path_like_ext <- function(expr_text) {
    qs <- regmatches(expr_text, gregexpr("(['\"])((?:[^'\"\\\\]|\\\\.)*)\\1",
                                         expr_text, perl = TRUE))[[1]]
    if (!length(qs)) return(NA_character_)
    last_q <- substr(qs[[length(qs)]], 2, nchar(qs[[length(qs)]]) - 1)
    if (grepl("\\.[A-Za-z0-9]{1,8}$", last_q))
      sub(".*(\\.[A-Za-z0-9]{1,8})$", "\\1", last_q) else NA_character_
  }

  # Top-level assignments whose RHS looks path-like (see above) — covers
  # `x <- "a.csv"` and `x <- paste0("dir/", Sys.Date(), ".csv")` alike (R);
  # Python has only `=` (no `<-`/`<<-`), same restriction as
  # repro_defined_vars()/`.code_char_vector_vars()` elsewhere in this file.
  assign_pat <- if (identical(lang, "Python"))
    "^([.a-zA-Z_][.a-zA-Z0-9_]*)\\s*=(?!=)\\s*(.+)$" else
    "^([.a-zA-Z][.a-zA-Z0-9_]*)\\s*(?:<<-|<-|=)\\s*(.+)$"
  am <- regmatches(nc, regexec(assign_pat, nc, perl = TRUE))
  assigns <- Filter(function(x) length(x) == 3, am)
  path_like_vars <- character(0)
  var_ext <- character(0)
  for (a in assigns) {
    ext <- .repro_path_like_ext(a[[3]])
    if (!is.na(ext)) { path_like_vars[[a[[2]]]] <- TRUE; var_ext[[a[[2]]]] <- ext }
  }

  # Characters split once (see .repro_format_call_refs(); #481).
  chars <- strsplit(joined, "", fixed = TRUE)[[1]]
  n <- length(chars)
  rows <- lapply(seq_along(starts), function(k) {
    open_paren <- starts[k] + lens[k] - 1L
    depth <- 0L; i <- open_paren; in_str <- NA_character_
    end <- NA_integer_
    while (i <= n) {
      ch <- chars[i]
      if (!is.na(in_str)) {
        if (ch == "\\") i <- i + 1L
        else if (ch == in_str) in_str <- NA_character_
      } else if (ch %in% c("'", '"')) in_str <- ch
      else if (ch == "(") depth <- depth + 1L
      else if (ch == ")") { depth <- depth - 1L; if (depth == 0L) { end <- i; break } }
      i <- i + 1L
    }
    if (is.na(end)) return(NULL)   # unbalanced — leave alone, do not guess
    call_text <- substr(joined, starts[k], end)
    # The function name alone (just capture group 1) — sub(fn_pat, "\\1", ...)
    # would be wrong here: it replaces only the MATCHED SPAN (fn name + "(")
    # with the capture group, leaving the rest of call_text (the arguments)
    # appended unchanged, not isolating the name at all.
    fn_name <- regmatches(call_text, regexec(fn_pat, call_text, perl = TRUE))[[1]][2]
    args_text <- substr(joined, open_paren + 1L, end - 1L)
    line <- 1L + lengths(regmatches(substr(joined, 1, starts[k]),
                                    gregexpr("\n", substr(joined, 1, starts[k]))))[1]

    # Scan EVERY top-level (comma-separated) argument for the write target —
    # NOT just the first: the path is the first positional argument for some
    # write functions (write.csv(path, x)-style — actually rare), but the
    # SECOND for write.csv(x, path)/write.csv(x, file = path), and a NAMED
    # argument entirely for saveRDS(x, file = path). Scanning every argument
    # position (positional value or the part after `name =`) for whichever one
    # actually looks like a file target (a literal ending in an extension, or
    # a bare variable already known to be path-like) finds the real target
    # regardless of which argument position that particular function uses,
    # without hardcoding a per-function argument index.
    args_list <- .repro_split_args(args_text)
    hit <- NULL
    for (arg in args_list) {
      # the argument's name, kept in the replacement: dropped, `save(a, b, file
      # = "ab.RData")` became `save(a, b, "<out>/ab.RData")`, and save() took
      # the path for another object to save (U134)
      name_m <- regexpr("^[.a-zA-Z][.a-zA-Z0-9_]*\\s*=\\s*(?!=)", trimws(arg$text), perl = TRUE)
      arg_name <- paste0(sub("^(\\s*).*$", "\\1", arg$text),
                         if (name_m > 0) regmatches(trimws(arg$text), name_m) else "")
      val <- sub("^[.a-zA-Z][.a-zA-Z0-9_]*\\s*=\\s*(?!=)", "", trimws(arg$text), perl = TRUE)
      val <- trimws(val)

      lit <- regexpr("^(['\"])((?:[^'\"\\\\]|\\\\.)*)\\1$", val, perl = TRUE)
      if (lit != -1) {
        target <- substr(val, 2, nchar(val) - 1)
        if (grepl("\\.[A-Za-z0-9]{1,8}$", target)) {
          hit <- list(start = arg$start, end = arg$end,
                      redirected = basename(gsub("\\\\", "/", target)))
          break
        }
        next
      }
      if (grepl("^[.a-zA-Z][.a-zA-Z0-9_]*$", val) && !is.na(path_like_vars[val])) {
        hit <- list(start = arg$start, end = arg$end,
                    redirected = paste0(val, var_ext[[val]]))
        break
      }
      # An INLINE path-building call as the argument itself — no intermediate
      # variable at all (`write.csv(df, paste0("dir/", Sys.Date(), ".csv"))`,
      # the common `%>% write.csv(paste0(...), ...)` pipe shape; for Python,
      # `df.to_csv(os.path.join("dir", "x.csv"))`). Gated on the value
      # actually being a call to one of the known path-building functions
      # (not just "contains a quoted extension-shaped string somewhere"), so
      # an unrelated call whose LAST argument happens to be such a string
      # (rare, but not zero-risk) is not swept in by accident. Python
      # f-strings/.format() calls are NOT matched here — unlike
      # os.path.join(...), there is no reliable "this is a path-building
      # call" marker to gate on for either (an f-string is not a call at
      # all), so per this function's own `lang` docs, those are left
      # unresolved rather than guessed at.
      path_build_pat <- if (identical(lang, "Python"))
        "^(os\\.path\\.join|pathlib\\.Path|Path)\\s*\\(" else
        "^(paste0|paste|sprintf|file\\.path)\\s*\\("
      if (grepl(path_build_pat, val, perl = TRUE)) {
        ext <- .repro_path_like_ext(val)
        if (!is.na(ext)) {
          # No variable name to label the redirected file with (unlike the
          # bare-variable branch above) — the call's own function name plus
          # this write's position in the script is used instead, so two
          # different inline-built writes in the same file do not collide on
          # an identical redirected basename.
          hit <- list(start = arg$start, end = arg$end,
                      redirected = paste0("write_", k, ext))
          break
        }
      }
    }
    if (is.null(hit)) return(NULL)   # no argument resolves to a file target — leave alone

    replacement_arg <- paste0(arg_name, sprintf('"{{REPRO_OUTPUT}}%s"', hit$redirected))
    new_args <- paste0(substr(args_text, 1, hit$start - 1L), replacement_arg,
                       substr(args_text, hit$end + 1L, nchar(args_text)))
    data.frame(call_text = call_text,
              replacement = paste0(fn_name, "(", new_args, ")"),
              redirected_name = hit$redirected, line = line)
  })
  rows <- Filter(Negate(is.null), rows)
  if (length(rows) == 0) return(empty)
  dplyr::bind_rows(rows)
}

# Split an already-extracted argument-list string into its top-level (paren/
# bracket/quote-depth-0) comma-delimited arguments. Shared by
# .repro_redirect_writes()'s call parsing, which needs to scan every argument
# for the write target (not just the first — write.csv(x, path), saveRDS(x,
# file = path), and a first-argument path all occur in real code). Returns a
# list of list(text, start, end) — start/end are 1-based character positions
# WITHIN args_text (inclusive), so a caller can splice a replacement into the
# original string without re-matching. Empty list if args_text is empty.
.repro_split_args <- function(args_text) {
  if (!nzchar(args_text)) return(list())
  # Characters split once, not substr(args_text, i, i) per step (#481).
  chars <- strsplit(args_text, "", fixed = TRUE)[[1]]
  depth <- 0L; in_str <- NA_character_; n <- length(chars)
  arg_start <- 1L
  args <- list()
  i <- 1L
  while (i <= n) {
    ch <- chars[i]
    if (!is.na(in_str)) {
      if (ch == "\\") { i <- i + 2L; next }   # skip an escaped char inside the string
      if (ch == in_str) in_str <- NA_character_
    } else if (ch %in% c("'", '"')) in_str <- ch
    else if (ch %in% c("(", "[", "{")) depth <- depth + 1L
    else if (ch %in% c(")", "]", "}")) depth <- depth - 1L
    else if (ch == "," && depth == 0L) {
      args[[length(args) + 1L]] <- list(text = substr(args_text, arg_start, i - 1L),
                                        start = arg_start, end = i - 1L)
      arg_start <- i + 1L
    }
    i <- i + 1L
  }
  args[[length(args) + 1L]] <- list(text = substr(args_text, arg_start, n),
                                    start = arg_start, end = n)
  args
}

#' Rewrite a code file's data-file paths to the Psych-DS layout
#'
#' A script reads and writes data by relative path (`read_csv("raw/x.csv")`,
#' `saveRDS(m, "../out/model.rds")`). When the release is re-laid-out to
#' Psych-DS (data under `data/`, per-study `study-<group>/data/`), those paths no
#' lives, using the file→target plan `psychds_check` produced.
#'
#' Matching is by **basename** (a script's `../data/x.csv` and the repo's
#' `raw/x.csv` are the same file seen from different working directories, so the
#' prefix is ignored — the same choice `code_file_refs()` makes). When several
#' plan files share a basename (a `demographics.csv` in more than one study), the
#' ambiguity is resolved by **study group**: the candidate whose target path is
#' in the same study as the script (derived from the paths via
#' `.data_group_from_path()`) is chosen. Only when that still leaves more than
#' one candidate is the reference left unrewritten and flagged.
#'
#' A plan row for a converted tabular file (`psychds_check`'s `convert`/
#' `original_target` columns — a `.dta`/`.sav`/`.xlsx`/... source, whose
#' `target_path` is the re-exported `_data.csv`, with the untouched original
#' kept alongside at `original_target`) is matched **by the reference's own
#' extension**: a reference ending in the original's extension (`read_dta(
#' "math.dta")`) is rewritten to `original_target`, since a format-specific
#' reader needs the real file, not the CSV re-export — using `target_path`
#' there would silently point the reader at bytes it cannot correctly parse.
#' A reference that already names the CSV form (`"math_data.csv"`, or a bare
#' `.csv` extension) still rewrites to `target_path` as before.
#'
#' A path assembled at runtime with `sprintf()`/`paste()`/`paste0()`/
#' `file.path()` (see [.repro_format_call_refs()]) is matched the same way,
#' using the format string's own literal basename (with any resolvable
#' placeholder substituted via [.repro_simple_string_vars()]) — but since the
#' true "reference" there is the WHOLE call, not the quoted substring inside
#' it, such rows carry `is_call = TRUE` and `ref` holds the full call text
#' (`sprintf("%s/%s/x.csv", wd, wd_data)`), so [repro_write_scripts()] replaces
#' the entire expression rather than corrupting it with a substring swap that
#' would orphan the call's trailing arguments. A call whose placeholders could
#' not all be resolved is reported unmatched (`matched = FALSE`) rather than
#' guessed, and is separately flagged in the report as fragile authoring
#' practice regardless of whether it resolved.
#'
#' A basename can also collide across plan rows for a reason that is NOT real
#' ambiguity at all: a paper linking several OSF components that mirror each
#' other (see [repro_materialize_layout()]'s own de-duplication for the
#' execution-time counterpart of this) commonly lists the identical physical
#' file more than once, and the study-grouping step can assign DIFFERENT
#' (and for all-but-one, WRONG) groups to different mirrors of the same file
#' — e.g. one mirror's folder structure carries a "Study 1" hint the others
#' lack, so only that one gets grouped correctly. When `structure_df` is
#' supplied, candidates are first collapsed by the CONTENT HASH of their
#' resolved source files: byte-identical candidates count as ONE, using
#' whichever one's own group agrees with the majority (ties broken by the
#' first occurrence) — confirmed against a real corpus paper where 4 mirrors
#' of the same `Anonymized_2024-08-23.csv` were grouped ex1/ex1/ex1/ex3, which
#' made the existing "do all candidates already agree on one target"
#' shortcut fail (3 way agree, 1 disagrees) and fell through to `ambiguous =
#' TRUE` even though there was only ever one real file. Without
#' `structure_df` (or when a candidate's source cannot be resolved/read),
#' this collapsing step is skipped and candidates are compared as before.
#'
#' @param code_text the code text for a single file (character vector)
#' @param file_name the script's own name/path (used to derive its study group)
#' @param plan the `psychds_check` table: one row per file with `file_name`,
#'   `target_path`, `current_path`, and (for converted tabular files)
#'   `original_target`
#' @param lang the language: `"R"` or `"Python"` are rewritten; any other
#'   value returns the empty frame. Python's format-string call forms
#'   (f-strings, `.format()`, `os.path.join()`) are NOT resolved to a call
#'   row the way R's `sprintf()`/`paste()`/`file.path()` are (see
#'   `call_refs`/[.repro_format_call_refs()] below) — only a literal quoted
#'   path is rewritten for Python, so a path built by string
#'   concatenation/formatting is left alone and reported `matched = FALSE`
#'   rather than guessed at.
#' @param structure_df optional `data_check` structure table (`file_name`,
#'   `file_location`) — when supplied, enables the content-hash mirror
#'   collapsing described above. `NULL` (default) skips it.
#' @param script_path optional path of the script in its repository. When
#'   several files share the name a reference gives, the one the reference
#'   names counted from the script's own folder or a folder above it
#'   (`current_path` in `plan`) is the file it means. `NULL` (default) skips
#'   this; the folders in the reference's own path still count.
#'
#' @returns a data frame with one row per referenced path, columns `ref` (the
#'   path as written, or the full call text when `is_call`), `basename`,
#'   `matched` (logical, a plan file was found), `target` (its Psych-DS path,
#'   or NA), `ambiguous` (logical, several plan files matched and the study
#'   could not disambiguate), `n_candidates`, and `is_call` (logical, TRUE when
#'   `ref` is a whole `sprintf()`/`paste()`-family call rather than a literal
#'   path — see above), and `name_in_code` and `name_in_deposit` (both names when
#'   they differ, else NA). A reference with no exact match also matches the one
#'   plan file with the same extension and the same name apart from case, spaces
#'   and punctuation. Empty frame when the script references no files.
#' @export
#'
#' @examples
#' plan <- data.frame(
#'   file_name = c("demographics.csv", "scores.csv"),
#'   target_path = c("study-ex1/data/study-demographics_data.csv",
#'                   "study-ex1/data/study-scores_data.csv"),
#'   current_path = c("ex1/demographics.csv", "ex1/scores.csv")
#' )
#' code_text <- c('d <- read.csv("data/demographics.csv")')
#' repro_rewrite_paths(code_text, "ex1/analysis.R", plan)
repro_rewrite_paths <- function(code_text, file_name, plan, lang = "R",
                                structure_df = NULL, script_path = NULL) {
  empty <- data.frame(ref = character(0), basename = character(0),
                      matched = logical(0), target = character(0),
                      ambiguous = logical(0), n_candidates = integer(0),
                      is_call = logical(0))
  if (!lang %in% c("R", "Python") || is.null(code_text)) return(empty)
  if (is.null(plan) || nrow(plan) == 0 ||
      !all(c("file_name", "target_path") %in% names(plan))) return(empty)

  refs <- code_file_refs(code_text, lang)
  # A listed file's name held in a variable, as in f <- c("NetworkFunction.R")
  # and later source(f): the code_file_refs() scan only sees a name in the call.
  # Kept below only when it resolves to one file, so it adds no missing file and
  # no ambiguous reference.
  plan_base <- tolower(basename(gsub("\\\\", "/", plan$file_name)))
  assigned <- setdiff(.repro_assigned_names(code_text, lang), refs)
  assigned <- assigned[tolower(basename(gsub("\\\\", "/", assigned))) %in% plan_base]
  refs <- c(refs, assigned)
  # Format-string calls (sprintf()/paste()/paste0()/file.path()) are a SEPARATE
  # source of references: code_file_refs() already returns their raw format
  # string as a "reference" too (it matches the quoted-filename pattern), so
  # basename-matching still finds the right plan row from the placeholder-laden
  # text alone — but rewriting must replace the WHOLE call, not that substring,
  # or the call's trailing arguments are orphaned (see .repro_format_call_refs()
  # roxygen for the concrete case this fixes). Build the call rows here and
  # drop their raw format string from `refs`, so each such reference is
  # represented exactly once, as a call row. R only (see this function's own
  # `lang` docs) — a Python reference built by an f-string/.format()/
  # os.path.join() call is left as a literal-only match attempt instead.
  call_refs <- if (identical(lang, "R")) .repro_format_call_refs(code_text) else
    data.frame(call_text = character(0), fmt = character(0),
               resolved = character(0), line = integer(0))
  if (nrow(call_refs) > 0) refs <- setdiff(refs, call_refs$fmt)
  if (length(refs) == 0 && nrow(call_refs) == 0) return(empty)
  ref_base <- tolower(basename(gsub("\\\\", "/", refs)))
  ref_ext  <- tolower(tools::file_ext(ref_base))

  # Candidate plan files by basename. Only rows with a real (non-NA) target are
  # reachable in the release; NA-target rows (consumed archives) are not.
  has_target <- !is.na(plan$target_path) & nzchar(plan$target_path %||% "")

  # A converted row's ORIGINAL extension, so a reference naming that same
  # extension is known to want the original, not the CSV re-export.
  orig_target <- if ("original_target" %in% names(plan))
    plan$original_target else rep(NA_character_, nrow(plan))
  orig_ext <- tolower(tools::file_ext(orig_target %||% ""))

  # Pick the right column for a candidate row, given the reference's own
  # extension: the original when the reference names the pre-conversion
  # format, else the (possibly-CSV) target_path.
  pick_target <- function(cand_i, ref_e) {
    if (!is.na(orig_target[cand_i]) && nzchar(orig_target[cand_i]) &&
        nzchar(ref_e) && ref_e == orig_ext[cand_i]) orig_target[cand_i]
    else plan$target_path[cand_i]
  }

  # The script's own study group, for disambiguation.
  script_grp <- .data_group_from_path(file_name)

  # The one candidate a reference's own path names, among files with one name
  # (see .repro_path_named()).
  plan_current <- if ("current_path" %in% names(plan))
    sub("^(\\./|/)+", "", gsub("\\\\", "/", plan$current_path)) else rep(NA_character_, nrow(plan))
  named_by_path <- function(cand, ref) cand[.repro_path_named(plan_current[cand], ref, script_path)]

  # Resolve each PLAN ROW's own on-disk source (see .repro_plan_sources()),
  # for the content-hash mirror collapsing below. Only built when
  # structure_df was supplied; NA everywhere otherwise (skips collapsing
  # entirely, same as before this was added).
  plan_source_path <- if (!is.null(structure_df) &&
                          all(c("file_name", "file_location") %in% names(structure_df)))
    .repro_plan_sources(plan, structure_df) else rep(NA_character_, nrow(plan))
  plan_hash <- ifelse(is.na(plan_source_path), NA_character_,
                      unname(tools::md5sum(plan_source_path)))

  # Collapse candidates that are BYTE-IDENTICAL mirrors of the same real file
  # into one representative before any target/group comparison runs — a
  # basename-mirror duplicate whose study-grouping happened to disagree with
  # its sibling mirrors is not genuine ambiguity (see this function's own
  # roxygen for the confirmed real case). A hash-less candidate (structure_df
  # not supplied, or its source unreadable) is never collapsed with anything
  # — it keeps comparing exactly as before.
  collapse_by_hash <- function(cand) {
    h <- plan_hash[cand]
    has_hash <- !is.na(h)
    if (sum(has_hash) < 2) return(cand)   # nothing to collapse
    keep <- cand[!has_hash]                # hash-less candidates: untouched
    for (grp_h in unique(h[has_hash])) {
      grp_cand <- cand[has_hash][h[has_hash] == grp_h]
      if (length(grp_cand) == 1) { keep <- c(keep, grp_cand); next }
      # Among byte-identical mirrors, prefer whichever group the MAJORITY
      # agree on (a lone dissenting mirror is the mis-grouped one); ties (incl.
      # all-NA groups) keep the first occurrence, same "do not guess further"
      # spirit as the rest of this function.
      grp_of <- .data_group_from_path(plan$target_path[grp_cand])
      # a tie keeps the first listed group, as the comment says: table()
      # sorts its names, so a tie went to the alphabetically first (U135)
      seen <- unique(grp_of[!is.na(grp_of)])
      counts <- tabulate(match(grp_of, seen), nbins = length(seen))
      rep_i <- if (length(seen) > 0) grp_cand[which(!is.na(grp_of) &
                grp_of == seen[which.max(counts)])[1]] else grp_cand[1]
      keep <- c(keep, rep_i)
    }
    keep
  }

  # A file spelled differently in the code than in the deposit, which repositories
  # such as Dryad can cause on deposit (relative cover.csv, relative_cover.csv):
  # the one plan row with the same extension and the same name apart from case,
  # spaces and punctuation. A piece of a name, a placeholder, a name built from a
  # variable or a file the script writes itself can look like another file, so
  # is not compared; a call row has already worked out what it can of its name.
  plan_key <- .repro_name_key(plan_base)
  plan_ext <- tolower(tools::file_ext(plan_base))
  not_compared <- c(.repro_built_names(code_text, lang),
                    unlist(repro_file_io(list(script = code_text), lang)$writes))
  other_spelling <- function(base, literal = TRUE) {
    if ((literal && tolower(base) %in% not_compared) || grepl("^[._]|[%{}$*]", base) ||
        !nzchar(tools::file_ext(base))) return(integer(0))
    key <- .repro_name_key(base)
    if (!nzchar(key)) return(integer(0))
    cand <- collapse_by_hash(which(plan_key == key & plan_ext == tolower(tools::file_ext(base)) & has_target))
    if (length(unique(plan$target_path[cand])) == 1) cand[[1]] else integer(0)
  }

  rows <- lapply(seq_along(refs), function(i) {
    cand <- which(plan_base == ref_base[i] & has_target)
    cand <- collapse_by_hash(cand)
    n <- length(cand)
    if (n == 0) {
      other <- other_spelling(ref_base[i])
      if (length(other))
        return(data.frame(ref = refs[i], basename = ref_base[i], matched = TRUE,
                          target = pick_target(other, ref_ext[i]), ambiguous = FALSE,
                          n_candidates = 1L, is_call = FALSE))
      return(data.frame(ref = refs[i], basename = ref_base[i], matched = FALSE,
                        target = NA_character_, ambiguous = FALSE,
                        n_candidates = 0L, is_call = FALSE))
    }
    if (n == 1) {
      return(data.frame(ref = refs[i], basename = ref_base[i], matched = TRUE,
                        target = pick_target(cand, ref_ext[i]), ambiguous = FALSE,
                        n_candidates = 1L, is_call = FALSE))
    }
    # Several PLAN ROWS share the basename, but that is not necessarily several
    # possible TARGETS: the plan can list the same file more than once (e.g. it
    # is referenced from several places in the repository listing), and every
    # row still resolves to the identical Psych-DS path. That is not ambiguity —
    # disambiguating would be solving a problem that does not exist, and the
    # earlier code marked every such reference unresolved even though only one
    # answer was ever possible.
    cand_targets <- unique(vapply(cand, pick_target, character(1), ref_e = ref_ext[i]))
    if (length(cand_targets) == 1) {
      return(data.frame(ref = refs[i], basename = ref_base[i], matched = TRUE,
                        target = cand_targets, ambiguous = FALSE,
                        n_candidates = n, is_call = FALSE))
    }
    own <- named_by_path(cand, refs[i])
    if (length(own) == 1) {
      return(data.frame(ref = refs[i], basename = ref_base[i], matched = TRUE,
                        target = pick_target(own, ref_ext[i]), ambiguous = FALSE,
                        n_candidates = n, is_call = FALSE))
    }
    # Several candidates share the basename AND disagree on target: disambiguate
    # by study group. Prefer the candidate whose target path is in the script's
    # study; failing that, whose target is in the study the reference path
    # itself names.
    cand_grp <- .data_group_from_path(plan$target_path[cand])
    ref_grp <- .data_group_from_path(refs[i])
    pick <- integer(0)
    if (!is.na(script_grp)) pick <- cand[cand_grp == script_grp & !is.na(cand_grp)]
    if (length(pick) != 1 && !is.na(ref_grp))
      pick <- cand[cand_grp == ref_grp & !is.na(cand_grp)]
    if (length(pick) == 1) {
      data.frame(ref = refs[i], basename = ref_base[i], matched = TRUE,
                 target = pick_target(pick, ref_ext[i]), ambiguous = FALSE,
                 n_candidates = n, is_call = FALSE)
    } else {
      # Still ambiguous: do not guess. Flagged for the report; the script is run
      # unmodified for this reference.
      data.frame(ref = refs[i], basename = ref_base[i], matched = TRUE,
                 target = NA_character_, ambiguous = TRUE, n_candidates = n,
                 is_call = FALSE)
    }
  })

  # Format-string calls: match the SAME way, keyed on the resolved literal
  # basename when every placeholder resolved, else the raw format string's own
  # literal basename (works whenever the placeholders stand only for a
  # directory prefix, as observed in practice — the filename portion is fully
  # literal so basename-matching already finds the right plan row without
  # needing the placeholder resolved at all). `ref` is the CALL TEXT, not the
  # format string, so the caller replaces the whole expression.
  call_rows <- if (nrow(call_refs) > 0) lapply(seq_len(nrow(call_refs)), function(i) {
    key <- if (!is.na(call_refs$resolved[i])) call_refs$resolved[i] else call_refs$fmt[i]
    key_base <- tolower(basename(gsub("\\\\", "/", key)))
    key_ext  <- tolower(tools::file_ext(key_base))
    cand <- which(plan_base == key_base & has_target)
    n <- length(cand)
    if (n == 0) {
      other <- other_spelling(key_base, literal = FALSE)
      if (length(other))
        return(data.frame(ref = call_refs$call_text[i], basename = key_base,
                          matched = TRUE, target = pick_target(other, key_ext),
                          ambiguous = FALSE, n_candidates = 1L, is_call = TRUE))
    }
    if (n == 0)
      return(data.frame(ref = call_refs$call_text[i], basename = key_base,
                        matched = FALSE, target = NA_character_,
                        ambiguous = FALSE, n_candidates = 0L, is_call = TRUE))
    if (n == 1)
      return(data.frame(ref = call_refs$call_text[i], basename = key_base,
                        matched = TRUE, target = pick_target(cand, key_ext),
                        ambiguous = FALSE, n_candidates = 1L, is_call = TRUE))
    cand_targets <- unique(vapply(cand, pick_target, character(1), ref_e = key_ext))
    if (length(cand_targets) == 1)
      return(data.frame(ref = call_refs$call_text[i], basename = key_base,
                        matched = TRUE, target = cand_targets, ambiguous = FALSE,
                        n_candidates = n, is_call = TRUE))
    own <- if (!is.na(call_refs$resolved[i])) named_by_path(cand, key) else integer(0)
    if (length(own) == 1)
      return(data.frame(ref = call_refs$call_text[i], basename = key_base, matched = TRUE,
                        target = pick_target(own, key_ext), ambiguous = FALSE,
                        n_candidates = n, is_call = TRUE))
    cand_grp <- .data_group_from_path(plan$target_path[cand])
    key_grp <- .data_group_from_path(key)
    pick <- integer(0)
    if (!is.na(script_grp)) pick <- cand[cand_grp == script_grp & !is.na(cand_grp)]
    if (length(pick) != 1 && !is.na(key_grp))
      pick <- cand[cand_grp == key_grp & !is.na(cand_grp)]
    if (length(pick) == 1)
      data.frame(ref = call_refs$call_text[i], basename = key_base, matched = TRUE,
                target = pick_target(pick, key_ext), ambiguous = FALSE,
                n_candidates = n, is_call = TRUE)
    else
      data.frame(ref = call_refs$call_text[i], basename = key_base, matched = TRUE,
                target = NA_character_, ambiguous = TRUE, n_candidates = n,
                is_call = TRUE)
  }) else list()

  out <- dplyr::bind_rows(c(rows, call_rows))
  # a name in a variable counts only when it resolves to one file
  out <- out[!(out$ref %in% assigned & !out$is_call & (!out$matched | out$ambiguous)), , drop = FALSE]

  # Where the name in the code is not the name in the deposit, both names, so the
  # report can tell the authors. Only the name's case counts here too, since that
  # fails on a case-sensitive system.
  in_code <- basename(gsub("\\\\", "/", out$ref))
  in_code[out$is_call] <- ifelse(!is.na(call_refs$resolved), basename(gsub("\\\\", "/", call_refs$resolved)),
                                 basename(gsub("\\\\", "/", call_refs$fmt)))[
    match(out$ref[out$is_call], call_refs$call_text)]
  listed <- ifelse(out$is_call & grepl("[%{}$*]", in_code), NA_character_,
                   plan$file_name[match(out$target, plan$target_path)])
  from_original <- is.na(listed) & !is.na(out$target) & !(out$is_call & grepl("[%{}$*]", in_code))
  listed[from_original] <- plan$file_name[match(out$target[from_original], orig_target)]
  listed <- basename(gsub("\\\\", "/", listed))
  differs <- !is.na(listed) & !out$ambiguous & listed != in_code
  # as.character(): ifelse() of no rows gives logical(0), and joining that script's
  # rows with another's names then stopped the module for the paper
  out$name_in_code <- as.character(ifelse(differs, in_code, NA_character_))
  out$name_in_deposit <- as.character(ifelse(differs, listed, NA_character_))
  # Whether the name stands in a call that joins it to a folder of the code's own
  # (file.path(dir, "x.csv"), os.path.join(base, "x.csv")): such a name is left
  # as written when the file's path ends in it (.repro_keep_working_refs()). A
  # bare name read directly, after setwd("./data") was taken out, is not
  # (10.1111/1365-2664.14633; #568).
  joined_text <- paste(code_text, collapse = "\n")
  out$in_join <- vapply(seq_len(nrow(out)), function(i) {
    if (isTRUE(out$is_call[i])) return(FALSE)
    q <- gsub("([.\\\\+*?^$()|{}\\[\\]])", "\\\\\\1", out$ref[i])
    grepl(paste0("\\b(?:file\\.path|paste0?|os\\.path\\.join|here|Path|sprintf)\\s*\\([^()\n]*?[\"']", q, "[\"']"),
          joined_text, perl = TRUE)
  }, logical(1))
  out
}

# Which of several same-named files a reference means, by the path it gives. The
# file it names counted from the script's own folder, else from each folder above
# that, nearest first: a script often counts from the top of its project. Else the
# one file sharing the most of the reference's last folders, at least one, which
# also serves an absolute path from the author's computer. Case does not count.
# Returns an index into `paths`; none when that leaves no file or several, since
# the code is not guessed at.
.repro_path_named <- function(paths, ref, script_path = NULL) {
  paths <- tolower(gsub("\\\\", "/", paths))
  ref <- tolower(gsub("\\\\", "/", ref))
  if (!is.null(script_path) && !is.na(script_path) && nzchar(script_path)) {
    d <- dirname(tolower(gsub("\\\\", "/", script_path)))
    repeat {
      from_d <- .repro_norm_rel(if (d %in% c(".", "")) ref else file.path(d, ref))
      hit <- if (is.na(from_d)) integer(0) else which(paths == from_d)
      if (length(hit)) return(if (length(hit) == 1) hit else integer(0))
      if (d %in% c(".", "")) break
      d <- dirname(d)
    }
  }
  parts <- strsplit(ref, "/", fixed = TRUE)[[1]]
  parts <- parts[!parts %in% c("", ".", "..")]
  if (length(parts) < 2) return(integer(0))
  shared <- vapply(strsplit(paths, "/", fixed = TRUE), function(p) {
    n <- min(length(p), length(parts))
    if (n == 0 || anyNA(p)) return(0L)
    same <- rev(p)[seq_len(n)] == rev(parts)[seq_len(n)]
    if (all(same)) n else which(!same)[1] - 1L
  }, integer(1))
  best <- which(shared == max(shared))
  if (max(shared) >= 2 && length(best) == 1) best else integer(0)
}

# Text assigned to a variable, alone or in c(): names a later call may read.
.repro_assigned_names <- function(code_text, lang = "R") {
  if (identical(lang, "Python")) {
    m <- regmatches(code_text, regexec("^\\s*[A-Za-z_][A-Za-z0-9_.]*\\s*=\\s*[\"']([^\"']+)[\"']\\s*(#.*)?$", code_text))
    return(unique(vapply(m[lengths(m) > 1], `[`, character(1), 2)))
  }
  ex <- tryCatch(parse(text = code_text, keep.source = FALSE), error = function(e) NULL)
  found <- character(0)
  text_of <- function(x) {
    if (is.character(x) && length(x) == 1 && !is.na(x)) return(x)
    if (is.call(x) && identical(x[[1]], as.name("c"))) return(unlist(lapply(.repro_call_parts(x)[-1], text_of)))
    NULL
  }
  walk <- function(e) {
    if (is.name(e[[1]]) && as.character(e[[1]]) %in% c("<-", "=", "<<-") && length(e) == 3)
      found <<- c(found, text_of(e[[3]]))
    for (a in .repro_call_parts(e)) if (is.call(a)) walk(a)
  }
  for (e in ex) if (is.call(e)) walk(e)
  unique(found[nzchar(found)])
}

# The parts of a call, without empty arguments: x[1, ] has one, and using it stops
# with "argument is missing, with no default".
.repro_call_parts <- function(e) {
  parts <- as.list(e)
  parts[!vapply(seq_along(parts), function(k) identical(parts[[k]], quote(expr = )), logical(1))]
}

# A file name for comparing spellings: no extension, case, spaces or punctuation.
# A separator between two digits stays, so s1_2 and s12 remain different files.
# Not .repro_normalize_basename(), which removes leading numbers, so that
# 1_data.csv and 2_data.csv would count as one file.
.repro_name_key <- function(x) {
  x <- tolower(tools::file_path_sans_ext(iconv(x, "UTF-8", "UTF-8", sub = "")))
  x <- gsub("([0-9])[^[:alnum:]]+(?=[0-9])", "\\1|", x, perl = TRUE)
  gsub("[^[:alnum:]|]", "", x)
}

# File extensions of data, for saying that the deposit holds a data file in another
# format than the code reads: x.csv and x.html sharing a name is a coincidence.
.repro_data_exts <- c("csv", "tsv", "txt", "dat", "tab", "xls", "xlsx", "ods",
                      "rds", "rda", "rdata", "sav", "dta")

# The names a script reads under another spelling than the deposit's, one row per
# name, from repro_rewrite_paths() results (a named list, by script).
.repro_name_corrections <- function(rewrite_list) {
  rows <- lapply(names(rewrite_list), function(fn) {
    d <- rewrite_list[[fn]]
    if (is.null(d) || !nrow(d) || !"name_in_deposit" %in% names(d)) return(NULL)
    d <- unique(d[!is.na(d$name_in_deposit), c("name_in_code", "name_in_deposit"), drop = FALSE])
    if (!nrow(d)) return(NULL)
    data.frame(file_name = fn, name_in_code = d$name_in_code, name_in_deposit = d$name_in_deposit)
  })
  out <- dplyr::bind_rows(rows)
  if (!nrow(out)) return(data.frame(file_name = character(0), name_in_code = character(0),
                                    name_in_deposit = character(0)))
  out
}

# For `table`: a script's corrected names as text, NA when there are none.
.repro_names_corrected <- function(corrections, file_names) {
  vapply(file_names, function(fn) {
    d <- corrections[corrections$file_name == fn, , drop = FALSE]
    if (!nrow(d)) return(NA_character_)
    paste(sprintf("%s -> %s", d$name_in_code, d$name_in_deposit), collapse = "; ")
  }, character(1), USE.NAMES = FALSE)
}

# The report's note on these names, so authors can change their code; NULL if none.
.repro_name_report <- function(corrections, execute = FALSE) {
  n <- nrow(corrections)
  if (!n) return(NULL)
  c(sprintf(
    paste0("**%d file name%s in the code %s not the same as in the deposit**, in case, ",
           "spaces or punctuation, and exactly one file in the deposit fits. %s ",
           "this in a copy of the script so that it finds the file. The script itself ",
           "should be changed, because as written it does not find the file on every ",
           "system."),
    n, plural(n), plural(n, "is", "are"),
    if (isTRUE(execute)) "To run the code we corrected" else "To run the code we would correct"),
    scroll_table(data.frame(File = corrections$file_name,
                            `Name in the code` = corrections$name_in_code,
                            `Name in the deposit` = corrections$name_in_deposit,
                            check.names = FALSE), maxrows = 10))
}

# The rows of a repository listing (data_check's `structure`) that are not on
# disk but can be fetched: a file address, or an archive and the member in it.
.repro_fetchable <- function(structure_df) {
  if (is.null(structure_df) || !nrow(structure_df)) return(logical(0))
  loc <- structure_df$file_location %||% rep(NA_character_, nrow(structure_df))
  url <- structure_df$file_url %||% rep(NA_character_, nrow(structure_df))
  arc <- structure_df$archive_url %||% rep(NA_character_, nrow(structure_df))
  mem <- structure_df$archive_member %||% rep(NA_character_, nrow(structure_df))
  (is.na(loc) | !nzchar(loc)) &
    ((!is.na(url) & nzchar(url)) | (!is.na(arc) & nzchar(arc) & !is.na(mem)))
}

# Before a run: the files the repository lists, but that were not downloaded,
# which the code names, by file name, or by extension in a pattern such as
# `list.files(pattern = "\\.wav$")` or `glob("*.tif")`. The first download leaves
# out images, sound, video and documents from hosts that send files one by one
# (see .run_download_want()); a script that reads one gets it here (#568).
.repro_named_files <- function(structure_df, code_text) {
  want <- .repro_fetchable(structure_df)
  if (!any(want) || !length(code_text)) return(want)
  # bytes that are not UTF-8 stopped tolower() for the whole paper (#568)
  txt <- tolower(paste(.utf8_lines(code_text), collapse = "\n"))
  base <- tolower(basename(gsub("\\\\", "/", .utf8_lines(structure_df$file_name))))
  ext <- ifelse(grepl("\\.", base), sub(".*\\.", "", base), "")
  by_name <- vapply(base, function(b) grepl(b, txt, fixed = TRUE), logical(1), USE.NAMES = FALSE)
  exts <- unique(ext[want & nzchar(ext)])
  ext_named <- exts[vapply(exts, function(e)
    grepl(paste0("(\\\\{1,2}\\.|\\*\\.|['\"]\\.)", e, "\\b"), txt, perl = TRUE), logical(1))]
  want & (by_name | ext %in% ext_named)
}

# After a run: the names of the files scripts could not open, from their errors;
# with `full = TRUE` the paths as the errors give them.
.repro_unopened_files <- function(msgs, full = FALSE) {
  pat <- paste0("(?:cannot open (?:compressed )?file|No such file or directory:?|does not exist)",
                "[^'\"\n]*['\"]([^'\"\n]+)['\"]|['\"]([^'\"\n]+)['\"]:? No such file|['\"]([^'\"\n]+)['\"] does not exist")
  out <- unlist(lapply(.utf8_lines(msgs[!is.na(msgs)]), function(m) {
    k <- regmatches(m, gregexpr(pat, m, perl = TRUE))[[1]]
    vapply(k, function(x) { g <- regmatches(x, regexec(pat, x, perl = TRUE))[[1]][-1]; g[nzchar(g)][1] },
           character(1), USE.NAMES = FALSE)
  }))
  out <- gsub("\\\\", "/", out[!is.na(out)])
  unique(if (full) out else basename(out))
}

# The scripts that stopped on a file the repository lists but that was not
# downloaded, such as one over the size limit: one row per script and file
# (#568). Such a script was not tried, so it says nothing about the code. A file
# is matched by its name and the folder above it, as far as the error gives
# them; when a downloaded file matches as well, the script may have looked in
# the wrong place, and it stays a failure.
.repro_not_downloaded <- function(run_results, structure_df) {
  none <- data.frame(script = character(0), file = character(0), size = numeric(0))
  if (is.null(run_results) || !nrow(run_results) || is.null(structure_df) || !nrow(structure_df))
    return(none)
  loc <- structure_df$file_location %||% rep(NA_character_, nrow(structure_df))
  absent <- is.na(loc) | !nzchar(loc)
  if (!any(absent)) return(none)
  listed <- .utf8_lines(structure_df$file_path %||% structure_df$file_name)
  listed[is.na(listed)] <- .utf8_lines(structure_df$file_name)[is.na(listed)]
  parts <- function(p) { x <- strsplit(tolower(p), "/", fixed = TRUE)[[1]]; x[nzchar(x) & !x %in% c(".", "..")] }
  listed_parts <- lapply(gsub("\\\\", "/", listed), parts)
  ends_with <- function(lp, up) length(lp) >= length(up) && identical(utils::tail(lp, length(up)), up)
  size <- suppressWarnings(as.numeric(structure_df$file_size %||% rep(NA, nrow(structure_df))))
  rows <- lapply(which(run_results$outcome %in% "errored"), function(i) {
    unopened <- .repro_unopened_files(paste(run_results$error[i], run_results$stderr[i]), full = TRUE)
    hits <- unlist(lapply(unopened, function(u) {
      up <- utils::tail(parts(u), 2)
      if (!length(up)) return(integer(0))
      m <- vapply(listed_parts, ends_with, logical(1), up = up)
      if (any(m & !absent)) integer(0) else which(m & absent)
    }))
    if (!length(hits)) return(NULL)
    hits <- unique(hits)
    data.frame(script = run_results$file_name[i], file = listed[hits], size = size[hits])
  })
  out <- do.call(rbind, rows)
  if (is.null(out)) none else out
}

# The report's sentence on scripts that could not be tried, naming each file
# and, when it was over the limit, its size.
.repro_not_downloaded_text <- function(nd, max_file_size = 100) {
  if (is.null(nd) || !nrow(nd)) return(NULL)
  n <- length(unique(nd$script))
  files <- nd[!duplicated(nd$file), , drop = FALSE]
  too_large <- !is.na(files$size) & files$size > max_file_size * 1024^2
  desc <- ifelse(too_large, sprintf("%s (%s MB)", files$file, .repro_mb(files$size)), files$file)
  sprintf(paste0(" **%d script%s could not be tried, because a file %s need%s was not downloaded**: %s. ",
                 "%s not counted as %s that did not reproduce. The file%s: %s%s."),
          n, plural(n), plural(n, "it", "they"), plural(n, "s", ""),
          paste(unique(nd$script), collapse = ", "),
          plural(n, "It is", "They are"), plural(n, "a script", "scripts"),
          plural(nrow(files)), paste(desc, collapse = ", "),
          if (any(too_large)) sprintf("; files over %s MB are not downloaded", max_file_size) else "")
}

# Fetch rows `rows` of a repository listing with download_repo_files(), within
# the module's own size limits; returns the listing with the locations filled in.
.repro_fetch_files <- function(structure_df, rows, max_file_size = 100, max_download_size = 500,
                               cache = FALSE, skip_on_api_limit = FALSE) {
  if (!any(rows)) return(structure_df)
  dl <- tryCatch(download_repo_files(structure_df[rows, , drop = FALSE],
                                     max_file_size = max_file_size,
                                     max_download_size = max_download_size,
                                     repo_file_counts = table(structure_df$repo_url),
                                     cache = cache, skip_on_api_limit = skip_on_api_limit),
                 error = function(e) NULL)
  if (!is.null(dl)) structure_df$file_location[rows] <- dl$file_location
  structure_df
}

# The report's sentence on the paths that do not work in the repository as
# deposited and were pointed to the file (#568), so authors can change their code: an
# absolute path on their computer, a path from another folder, or a file that is
# elsewhere in the repository (see .repro_keep_working_refs()). One string, since
# the module adds sentences to it. `rewrite_list` holds a repro_rewrite_paths()
# result per script, named by script.
.repro_path_fix_text <- function(rewrite_list, execute = FALSE) {
  fixes <- do.call(rbind, lapply(names(rewrite_list), function(f) {
    d <- rewrite_list[[f]]
    if (!nrow(d)) return(NULL)
    ok <- d$matched %in% TRUE & !(d$ambiguous %in% TRUE) & !(d$as_written %in% TRUE) &
      !is.na(d$target) & d$target != d$ref
    if (!any(ok)) return(NULL)
    data.frame(file = f, ref = d$ref[ok], target = d$target[ok])
  }))
  n <- NROW(fixes)
  if (!n) return("Every data path in the code works in the repository as deposited.")
  shown <- utils::head(fixes, 10)
  sprintf(paste0("**%d data path%s in the code %s not work in the repository as deposited.** ",
                 "%s %s to where the file is, so the code could run: %s%s. ",
                 "The code should be changed, because as written it does not find %s."),
          n, plural(n), plural(n, "does", "do"),
          if (isTRUE(execute)) "We pointed" else "We would point", plural(n, "it", "them"),
          paste(sprintf("`%s` to `%s` (in %s)", shown$ref, shown$target, shown$file), collapse = "; "),
          if (n > 10) sprintf(", and %d more", n - 10) else "",
          plural(n, "the file", "these files"))
}

#' Determine the order code files must run in
#'
#' Scripts in a repository usually depend on each other: a preparation script
#' writes a file a later analysis script reads, or one `source()`s another.
#' Running them in the wrong order fails for reasons that have nothing to do with
#' reproducibility, so a run must order them first. This derives an order from
#' three signals, in decreasing strength:
#'
#' 1. **read-after-write** — if file A's code writes `x.csv` and file B's code
#'    reads `x.csv`, A must precede B (hard data dependency);
#' 2. **`source()` edges** — if A `source()`s B, B must precede A;
#' 3. **numeric filename prefixes** — `0_prep.R` before `1_analysis.R` (a weak
#'    tie-breaker, not a dependency).
#'
#' The read/write and source references come from the shared static analysis
#' ([code_file_refs()] and a source scan); matching is by basename (as
#' elsewhere). The dependency graph is topologically sorted; a cycle (A reads a
#' file B writes and vice versa) cannot be ordered and is reported.
#'
#' @param files a data frame with one row per code file, columns `file_name`,
#'   and `reads` / `writes` / `sources` list-columns of basenames (as produced
#'   by [repro_file_io()]). When `reads`/`writes`/`sources` are absent they are
#'   derived from a `code_text` list-column if present. An optional `file_path`
#'   column holds each file's path, and an optional `source_refs` list-column
#'   each `source()` reference as written, in the order of `sources`: when
#'   several files share the name a `source()` call gives, these decide which
#'   one it means, and if they cannot, no edge is added.
#'
#' @returns a data frame with columns `file_name`, `order` (1-based run
#'   position, NA when unplaceable), `depends_on` (comma-joined file_names that
#'   must run earlier), and `order_basis` (`dependency`, `numeric`, or `none`).
#'   Attribute `"cycle"` is a character vector of file_names in a dependency
#'   cycle (empty when acyclic); attribute `"ambiguous"` is TRUE when no ordering
#'   signal placed a multi-file set (they share `order = NA`, basis `none`).
#'   Attribute `"fuzzy_sources"` is a data frame (`from`, `to`) of `source()`
#'   edges that only resolved after normalising both basenames (a renumbered/
#'   reworded script reference — see [.repro_normalize_basename()]), empty
#'   when every `source()` reference matched an exact basename. Attribute
#'   `"source_edges"` is a data frame (`from`, `to`) of every resolved
#'   `source()` edge, exact or fuzzy: `from` is the sourced file, `to` the
#'   file that sources it.
#' @export
#'
#' @examples
#' files <- data.frame(file_name = c("1_analysis.R", "0_prep.R"))
#' files$reads   <- list("clean.csv", character(0))
#' files$writes  <- list(character(0), "clean.csv")
#' files$sources <- list(character(0), character(0))
#' repro_run_order(files)
repro_run_order <- function(files, extra_edges = NULL) {
  if (is.null(files) || nrow(files) == 0)
    return(data.frame(file_name = character(0), order = integer(0),
                      depends_on = character(0), order_basis = character(0)))

  n <- nrow(files)
  fname <- files$file_name
  base_name <- tolower(basename(gsub("\\\\", "/", fname)))

  io_col <- function(col) {
    if (col %in% names(files)) files[[col]]
    else replicate(n, character(0), simplify = FALSE)
  }
  reads   <- io_col("reads")
  writes  <- io_col("writes")
  sources <- io_col("sources")

  # Edge j -> i means j must run BEFORE i. Built from read-after-write and
  # source(), keyed on basename.
  before <- vector("list", n)   # before[[i]] = indices that must precede i
  for (i in seq_len(n)) before[[i]] <- integer(0)

  # read-after-write: writer of a basename precedes its readers.
  writer_of <- list()
  for (j in seq_len(n)) for (w in tolower(writes[[j]]))
    writer_of[[w]] <- c(writer_of[[w]], j)
  for (i in seq_len(n)) for (r in tolower(reads[[i]])) {
    w <- setdiff(writer_of[[r]] %||% integer(0), i)   # not self
    if (length(w)) before[[i]] <- union(before[[i]], w)
  }
  # source(): if i sources B, the file whose basename is B precedes i. Tried
  # first by EXACT basename (as before); when that finds nothing, retried on
  # a NORMALISED basename (see .repro_normalize_basename()) — a script-to-
  # script source() reference is the one basename-mismatch case with real
  # evidence behind it (a renumbered/reworded file, e.g. `source("001 - data
  # prep.R")` when the actual file is `"103 - Data Prep.R"` — see issue #392;
  # general DATA-file references are deliberately NOT given this treatment,
  # per that issue's own evidence that fuzzy matching does not generalise
  # there). A fuzzy-matched edge is recorded in the `"fuzzy_sources"`
  # attribute (from_file -> to_file pairs) so a caller can flag it as a
  # lower-confidence match rather than silently treating it the same as an
  # exact hit — the same "never silently guess" posture
  # repro_rewrite_paths()'s own `ambiguous` column already follows.
  fuzzy_sources <- list()
  source_edges <- list()   # every resolved source() edge, exact or fuzzy
  norm_base <- .repro_normalize_basename(base_name)
  paths <- if ("file_path" %in% names(files)) files$file_path else fname
  source_refs <- if ("source_refs" %in% names(files)) files$source_refs else sources
  for (i in seq_len(n)) for (k in seq_along(sources[[i]])) {
    s <- tolower(sources[[i]][[k]])
    j <- setdiff(which(base_name == s), i)
    # several files with this name: only the one the call names
    if (length(j) > 1) j <- .repro_sourced_file(j, source_refs[[i]][[k]], paths, i)
    if (!length(j)) {
      s_norm <- .repro_normalize_basename(s)
      j <- setdiff(which(nzchar(s_norm) & norm_base == s_norm), i)
      if (length(j) == 1) {
        fuzzy_sources[[length(fuzzy_sources) + 1L]] <- c(fname[j], fname[i])
      } else if (length(j) > 1) {
        j <- integer(0)   # ambiguous after normalising too: do not guess
      }
    }
    if (length(j)) {
      before[[i]] <- union(before[[i]], j)
      for (jj in j) source_edges[[length(source_edges) + 1L]] <- c(fname[jj], fname[i])
    }
  }

  # extra_edges: soft ordering edges inferred from execution (e.g. an
  # undefined-variable error implies the file defining that variable must run
  # first). A list of c(from_file_name, to_file_name) pairs; each adds a
  # "from precedes to" edge, matched to file positions by file_name.
  for (e in extra_edges %||% list()) {
    from <- which(fname == e[[1]]); to <- which(fname == e[[2]])
    from <- setdiff(from, to)   # never self
    if (length(from) && length(to))
      before[[to[[1]]]] <- union(before[[to[[1]]]], from)
  }

  # Kahn topological sort. `before[[i]]` are i's predecessors; build the forward
  # adjacency (`after[[j]]` = successors of j) and the in-degree, then repeatedly
  # emit a zero-in-degree node and decrement its successors. Any node never
  # reaching in-degree 0 sits in a cycle.
  after <- vector("list", n)
  for (i in seq_len(n)) after[[i]] <- integer(0)
  for (i in seq_len(n)) for (j in before[[i]]) after[[j]] <- c(after[[j]], i)
  indeg <- vapply(before, length, integer(1))

  # Deterministic tie-break among ready nodes. Real scripts carry their sequence
  # as digit runs anywhere in the name, not just a leading prefix: "0_prep.R",
  # but also "Exp1_02_preprocessing.R", "study2_1_import.R". So the numeric key
  # is the VECTOR of all digit runs in the name (Exp1_02 -> c(1, 2)), padded to a
  # common length and compared component-wise, which sorts Exp1_02 < Exp2_1
  # correctly. Files with no digits sort last, then alphabetically.
  digit_runs <- lapply(base_name, function(b)
    as.integer(regmatches(b, gregexpr("[0-9]+", b))[[1]]))
  has_num <- vapply(digit_runs, length, integer(1)) > 0
  maxk <- max(0L, vapply(digit_runs, length, integer(1)))
  # A matrix of padded numeric keys (NA-padded so no-digit names go last).
  num_key <- if (maxk > 0)
    do.call(rbind, lapply(digit_runs, function(v)
      c(v, rep(NA_integer_, maxk - length(v))))) else
    matrix(NA_integer_, nrow = n, ncol = 1)
  tie_key <- function(v) {
    args <- c(lapply(seq_len(ncol(num_key)), function(k) num_key[v, k]),
              list(base_name[v]))
    v[do.call(order, args)]
  }

  order_idx <- integer(0)
  placed <- rep(FALSE, n)
  ready <- tie_key(which(indeg == 0))
  while (length(ready) > 0) {
    node <- ready[[1]]; ready <- ready[-1]
    order_idx <- c(order_idx, node); placed[node] <- TRUE
    newly <- integer(0)
    for (i in after[[node]]) {
      indeg[i] <- indeg[i] - 1L
      if (indeg[i] == 0L) newly <- c(newly, i)
    }
    if (length(newly)) ready <- tie_key(c(ready, newly))
  }
  cycle_nodes <- which(!placed)

  # order_basis per file: `dependency` when it had an incoming edge (a real data
  # or source() dependency decided its position); else `numeric` when its name
  # carries digits that fed the ordering; else `none` (nothing distinguished it).
  had_edge <- vapply(before, function(x) length(x) > 0, logical(1))
  any_dependency <- any(had_edge)
  basis <- ifelse(had_edge, "dependency",
                  ifelse(has_num, "numeric", "none"))

  ord <- rep(NA_integer_, n)
  ord[order_idx] <- seq_along(order_idx)

  depends_on <- vapply(seq_len(n), function(i)
    paste(fname[before[[i]]], collapse = ", "), character(1))

  out <- data.frame(file_name = fname, order = ord,
                    depends_on = depends_on, order_basis = basis)
  attr(out, "cycle") <- fname[cycle_nodes]
  # Ambiguous when more than one file exists but nothing (no dependency edge and
  # no numbering in any filename) distinguishes their order.
  attr(out, "ambiguous") <- n > 1 && !any_dependency && !any(has_num)
  # source() edges resolved only after normalising both basenames (see above)
  # — a data frame of `from`/`to` file_name pairs, empty (0-row) when every
  # source() reference matched exactly. Distinct from an exact match so a
  # caller can flag it as lower-confidence, per issue #392.
  attr(out, "fuzzy_sources") <- if (length(fuzzy_sources))
    data.frame(from = vapply(fuzzy_sources, `[[`, character(1), 1),
              to   = vapply(fuzzy_sources, `[[`, character(1), 2),
              stringsAsFactors = FALSE) else
    data.frame(from = character(0), to = character(0))
  # Every source() edge, exact and fuzzy: `from` is the sourced file, `to` the
  # file that sources it. The module uses it to run a sourced script only
  # inside the script that sources it (see .repro_run_by_source()).
  attr(out, "source_edges") <- if (length(source_edges))
    data.frame(from = vapply(source_edges, `[[`, character(1), 1),
               to   = vapply(source_edges, `[[`, character(1), 2)) else
    data.frame(from = character(0), to = character(0))
  out
}

# Which of several same-named files `cand` a source() reference in script `from`
# names (see .repro_path_named()).
.repro_sourced_file <- function(cand, ref, paths, from) {
  cand[.repro_path_named(paths[cand], ref, paths[from])]
}

#' Which scripts run inside another script, through `source()`
#'
#' When `main.R` loads its packages and then runs `a.R` with `source()`, `a.R`
#' only works inside `main.R`: run on its own, in a fresh R process, it lacks
#' the packages and objects `main.R` set up first (the most common symptom is
#' `could not find function "%>%"`). Such a script is therefore not run on its
#' own; it runs as part of the script that sources it.
#'
#' A script counts as run through `source()` only when a chain of `source()`
#' calls leads to it from a script that IS run on its own (it parses, its
#' inputs are available, and nothing sources it). So two scripts that source
#' each other, or a script sourced only by a script that cannot run, are still
#' run on their own, as before.
#'
#' @param order_tbl [repro_run_order()] output (its `"source_edges"` attribute)
#' @param can_run named logical (by file_name): whether each script parses and
#'   has its inputs, i.e. would be run at all
#'
#' @returns a named list: for each script run through `source()`, the
#'   script(s) that source it. Empty when there are none.
#' @keywords internal
.repro_run_by_source <- function(order_tbl, can_run) {
  edges <- attr(order_tbl, "source_edges")
  if (is.null(edges) || !nrow(edges)) return(list())
  edges <- edges[edges$from != edges$to, , drop = FALSE]
  sourced <- unique(edges$from)
  # Start from the scripts that run on their own and follow source() calls
  # down: everything reached runs inside one of them.
  reached <- character(0)
  frontier <- setdiff(names(can_run)[can_run], sourced)
  while (length(frontier)) {
    nxt <- setdiff(edges$from[edges$to %in% frontier], reached)
    # A sourced script that does not parse, or lacks its inputs, keeps that
    # outcome of its own rather than being hidden inside the sourcing script.
    nxt <- nxt[nxt %in% names(can_run)[can_run]]
    reached <- c(reached, nxt)
    frontier <- nxt
  }
  out <- lapply(reached, function(f)
    unique(edges$to[edges$from == f & (edges$to %in% c(reached, names(can_run)[can_run]))]))
  stats::setNames(out, reached)
}

# Packages a script attaches itself (library(), require(), pacman::p_load()),
# in the order it attaches them -- not packages it only uses with `pkg::`.
.repro_attached_pkgs <- function(code_text) {
  libs <- tryCatch(code_library_names(code_text, "R"), error = function(e) NULL)
  if (is.null(libs) || !nrow(libs)) return(character(0))
  unique(libs$package[libs$source != "namespace"])
}

# Packages that export `%>%`, for when the package cannot be checked because
# it is not installed on this computer (a Docker run installs packages in the
# container only). "tidyverse" itself does not export it, but attaches dplyr,
# which does.
.repro_pipe_pkgs <- c("magrittr", "dplyr", "tidyr", "purrr", "stringr",
                      "tibble", "tidyverse")

#' Which `library()` calls to add for a missing function
#'
#' The module's corrective re-run calls this for a script that failed with
#' `could not find function "X"` when no script of the paper defines `X`. In
#' order:
#'
#' 1. When the paper's OTHER scripts attach a package that provides `X`, all
#'    the packages those scripts attach are returned, in the order they attach
#'    them. Such a script was written to run after them, in the same session
#'    (typically with `library(tidyverse)` in a script that runs first), and
#'    usually needs more than the one function that failed first: loading
#'    only magrittr for `%>%` would make the script fail one line later, on
#'    the first dplyr function.
#' 2. Otherwise, the one package among `candidates` that exports `X`
#'    ([.repro_find_export_pkg()]), as before.
#' 3. Otherwise, for `%>%` when no script of the paper attaches a package that
#'    provides it, `"magrittr"`.
#'
#' A package provides `X` when it is installed here and exports `X`, or, for
#' `%>%`, when it is one of `.repro_pipe_pkgs`, or it is `"tidyverse"`.
#'
#' @param name the missing function name (e.g. `"%>%"`)
#' @param user the file_name of the script that failed
#' @param code_text_list named list of every R script's code text
#' @param candidates candidate packages for step 2 (see the module)
#'
#' @returns a character vector of package names to attach (empty when none)
#' @keywords internal
.repro_missing_fn_pkgs <- function(name, user, code_text_list, candidates) {
  provides <- function(pkg) {
    if (identical(pkg, "tidyverse") ||
        (identical(name, "%>%") && pkg %in% .repro_pipe_pkgs)) return(TRUE)
    if (!requireNamespace(pkg, quietly = TRUE)) return(FALSE)
    name %in% tryCatch(getNamespaceExports(asNamespace(pkg)),
                       error = function(e) character(0))
  }
  own <- .repro_attached_pkgs(code_text_list[[user]] %||% character(0))
  others <- unique(unlist(lapply(setdiff(names(code_text_list), user), function(f)
    .repro_attached_pkgs(code_text_list[[f]])), use.names = FALSE))
  others <- setdiff(others, own)
  if (length(others) && any(vapply(others, provides, logical(1)))) return(others)

  pkg <- .repro_find_export_pkg(name, candidates)
  if (!is.na(pkg)) return(pkg)

  if (identical(name, "%>%") &&
      !any(vapply(unique(c(own, others)), provides, logical(1))))
    return("magrittr")
  character(0)
}

# Normalise a basename for FUZZY source()-target matching (repro_run_order()'s
# own use, see issue #392) — lowercase (caller already lowercases, done again
# here so this helper is safe standalone too), strip the extension, strip a
# leading numbering prefix (`01_`, `103 - `, `12.`), and collapse every
# remaining separator (space/dot/underscore/hyphen) so `"001 - data prep.R"`
# and `"103 - Data Prep.R"` normalise to the same key ("dataprep"). Applied
# ONLY to script-to-script source() targets, never to general data-file
# references — see repro_run_order()'s own comment for why (this issue's own
# evidence found fuzzy matching does not generalise to data files).
# @keywords internal
.repro_normalize_basename <- function(b) {
  b <- tolower(tools::file_path_sans_ext(b))
  b <- sub("^[0-9]+[\\s._-]*", "", b, perl = TRUE)
  gsub("[\\s._-]+", "", b, perl = TRUE)
}

#' Extract the files a script reads, writes, and sources
#'
#' A thin wrapper that runs the static reference scans over each code file and
#' returns the per-file read / write / source basenames [repro_run_order()]
#' needs. Reads/writes come from [code_file_refs()] split by call type; sources
#' from a scan for `source()` (R only — see `lang` below). All returned as
#' lowercased basenames.
#'
#' @param code_text_list a named list of code-text character vectors, one per
#'   file (names are the file_names), OR a named list of such lists when
#'   files are in more than one language (see `lang`)
#' @param lang the language each element of `code_text_list` is in: either a
#'   single string applied to every file (default `"R"`), or a named
#'   character vector keyed by file_name for a mixed-language set. Python has
#'   no `source()`-equivalent for reusing a SIBLING analysis script by
#'   relative path (its code-reuse mechanism, `import`, names an installed
#'   module, not a local file run_order must sequence), so `sources` is
#'   always empty for a Python file; `open(..., "w"/"a"/...)` is classified
#'   as a write only when that same line's own mode argument says so (a bare
#'   `open(x)` defaults to Python's own read mode and is otherwise
#'   ambiguous between the two).
#'
#' @returns a data frame with `file_name` and list-columns `reads`, `writes`,
#'   `sources` (each a character vector of basenames).
#' @export
repro_file_io <- function(code_text_list, lang = "R") {
  if (is.null(code_text_list) || length(code_text_list) == 0)
    return(data.frame(file_name = character(0)))

  fname <- names(code_text_list) %||% as.character(seq_along(code_text_list))
  lang_for <- if (length(lang) == 1) stats::setNames(rep(lang, length(fname)), fname)
    else lang[fname]

  # A read call vs a write call, from the call name preceding the quoted path.
  write_fns_for <- list(
    R = "\\b(write[._][A-Za-z._0-9]*|saveRDS|save|save\\.image|ggsave|export|fwrite)\\s*\\(",
    Python = "\\b(to_[A-Za-z_0-9]+|savetxt|savefig|dump|save)\\s*\\("
  )
  # open(...) is BOTH a read call (code_file_refs()'s Python branch) and a
  # potential write call, decided only by its own mode argument (see
  # .py_open_writes()).
  src_pat <- "source\\s*\\(\\s*['\"]([^'\"]+)['\"]"

  rows <- lapply(seq_along(code_text_list), function(k) {
    ct <- code_text_list[[k]]
    fn_lang <- lang_for[[k]] %||% "R"
    nc <- code_remove_comments(ct, fn_lang)
    # include_writes = TRUE: outputs must be visible here, because a file one
    # script writes is the input the next script reads. Without them `writes` is
    # always empty and every intermediate looks like a missing input.
    all_refs <- code_file_refs(nc, fn_lang, include_writes = TRUE)
    joined <- paste(nc, collapse = "\n")

    write_fns <- write_fns_for[[fn_lang]] %||% write_fns_for[["R"]]

    # Classify each ref per *occurrence*, not per file: a file can be written on
    # one line and read back on another (a cached intermediate), and it must then
    # count as both. Taking any-write-wins would drop such a file from `reads`.
    ref_base <- tolower(basename(gsub("\\\\", "/", all_refs)))
    is_write <- rep(FALSE, length(all_refs))
    is_read  <- rep(FALSE, length(all_refs))
    # A call over several lines is judged as a whole: its file name on a later
    # line has no call beside it, so `write.csv(x,` then `"out.csv")` counted as a
    # read (#568)
    io_regex <- paste0("\\b(read[._][A-Za-z._0-9]+|import(_list)?|fread|readRDS|load|readLines|",
                       "fromJSON|readtext|vroom|source|read_[A-Za-z_0-9]+|loadtxt|genfromtxt|",
                       "loadmat|open|", sub("^\\\\b\\(", "", sub("\\)\\\\s\\*\\\\\\($", "", write_fns)),
                       ")\\s*\\(")
    open_calls <- .code_open_calls(nc, io_regex)
    units <- c(if (length(open_calls$lines)) nc[-open_calls$lines] else nc, open_calls$calls)
    for (i in seq_along(all_refs)) {
      lines <- grep(all_refs[i], units, fixed = TRUE, value = TRUE)
      w <- grepl(write_fns, lines, perl = TRUE, ignore.case = TRUE)
      if (identical(fn_lang, "Python"))
        w <- w | vapply(lines, .py_open_writes, logical(1), USE.NAMES = FALSE)
      is_write[i] <- any(w)
      is_read[i]  <- any(!w)
    }
    reads  <- ref_base[is_read]
    writes <- ref_base[is_write]

    # source() is R-only (see this function's own roxygen for why Python's
    # import does not play the same role).
    if (identical(fn_lang, "R")) {
      srcs <- regmatches(joined, gregexpr(src_pat, joined, perl = TRUE))[[1]]
      src_refs <- sub(src_pat, "\\1", srcs, perl = TRUE)
      srcs <- tolower(basename(gsub("\\\\", "/", src_refs)))
    } else {
      src_refs <- srcs <- character(0)
    }

    # code_file_refs() lists source() among its "read" calls, so a sourced
    # script leaks into `reads`. A source() is a CODE dependency (handled by the
    # source-edge ordering), not a data input, so drop sourced files from reads —
    # otherwise a present, sourced .R file is later mis-reported as a missing
    # data input.
    reads <- setdiff(reads, srcs)

    data.frame(file_name = fname[k], stringsAsFactors = FALSE) |>
      (\(d) { d$reads <- list(unique(reads)); d$writes <- list(unique(writes))
              d$sources <- list(unique(srcs))
              d$source_refs <- list(src_refs[!duplicated(srcs)]); d })()
  })
  dplyr::bind_rows(rows)
}

#' Whether a line of Python opens a file for writing
#'
#' Only the mode argument of an `open()` call counts: the second positional
#' argument, or `mode =`. A mode is a write when it is `w`, `a`, `x` or `r+`,
#' with `b`, `t` or `+`. Any quoted word with a w, a or x in it used to count,
#' so `open(p, encoding = "ascii")` was a write and `open(p, "r+")` was not
#' (U240).
#'
#' @param line one line of Python code, comments removed
#'
#' @returns TRUE when an `open()` call on the line has a write mode
#' @keywords internal
.py_open_writes <- function(line) {
  starts <- gregexpr("\\bopen\\s*\\(", line, perl = TRUE)[[1]]
  if (starts[1] == -1) return(FALSE)
  chars <- strsplit(line, "", fixed = TRUE)[[1]]
  first <- starts + attr(starts, "match.length")   # first character after "("
  for (k in seq_along(starts)) {
    # the call's closing parenthesis, skipping quoted text and nested brackets
    depth <- 1L; in_str <- NA_character_; end <- NA_integer_; i <- first[k]
    while (i <= length(chars)) {
      ch <- chars[i]
      if (!is.na(in_str)) {
        if (ch == "\\") i <- i + 1L
        else if (ch == in_str) in_str <- NA_character_
      } else if (ch %in% c("'", '"')) in_str <- ch
      else if (ch %in% c("(", "[", "{")) depth <- depth + 1L
      else if (ch %in% c(")", "]", "}")) {
        depth <- depth - 1L
        if (depth == 0L) { end <- i; break }
      }
      i <- i + 1L
    }
    if (is.na(end) || end == first[k]) next
    args <- vapply(.repro_split_args(paste(chars[first[k]:(end - 1L)], collapse = "")),
                   function(a) trimws(a$text), character(1))
    named <- grepl("^[A-Za-z_][A-Za-z0-9_]*\\s*=(?!=)", args, perl = TRUE)
    is_mode <- named & grepl("^mode\\s*=", args)
    mode <- if (any(is_mode)) sub("^mode\\s*=\\s*", "", args[is_mode][1])
            else args[!named][2]
    if (!is.na(mode) &&
        grepl("^(['\"])(?=[^'\"]*[wax+])[rwaxbt+]+\\1$", mode, perl = TRUE))
      return(TRUE)
  }
  FALSE
}

#' Find the variables each script defines at top level
#'
#' When a script errors with `object 'X' not found`, the fix is usually to run
#' the script that *defines* `X` first. This scans each file for **top-level**
#' variable assignments — `x <- ...`, `x = ...`, `x <<- ...`, and
#' `assign("x", ...)` — so an undefined-variable error can be matched to the file
#' that would supply it (see the module's corrective re-run). Only assignments at
#' the start of a (comment-free) line are taken, which approximates "top level":
#' an assignment indented inside a `function`/`for` body is not a global the next
#' sourced script would see, and taking it would create false ordering edges.
#'
#' @param code_text_list a named list of code-text character vectors, one per
#'   file (names are the file_names)
#'
#' @returns a data frame with `file_name` and a list-column `defines` (character
#'   vector of variable names each file assigns at top level).
#' @export
repro_defined_vars <- function(code_text_list) {
  if (is.null(code_text_list) || length(code_text_list) == 0)
    return(data.frame(file_name = character(0)))
  fname <- names(code_text_list) %||% as.character(seq_along(code_text_list))

  # A top-level assignment: line start (no indentation => not inside a block),
  # a valid R name, then <-, <<-, or = (not ==). Also assign("name", ...).
  assign_pat <- "^([.a-zA-Z][.a-zA-Z0-9_]*)\\s*(<<-|<-|=)(?!=)"
  assign_fn  <- "^\\s*assign\\s*\\(\\s*['\"]([.a-zA-Z][.a-zA-Z0-9_]*)['\"]"

  rows <- lapply(seq_along(code_text_list), function(k) {
    nc <- code_remove_comments(code_text_list[[k]], "R")
    m1 <- regmatches(nc, regexpr(assign_pat, nc, perl = TRUE))
    v1 <- sub(paste0(assign_pat, ".*"), "\\1", m1, perl = TRUE)
    m2 <- regmatches(nc, regexpr(assign_fn, nc, perl = TRUE))
    v2 <- sub(paste0(assign_fn, ".*"), "\\1", m2, perl = TRUE)
    defines <- unique(c(v1[nzchar(v1)], v2[nzchar(v2)]))
    data.frame(file_name = fname[k], stringsAsFactors = FALSE) |>
      (\(d) { d$defines <- list(defines); d })()
  })
  dplyr::bind_rows(rows)
}

# A small curated safe-list of extremely common packages worth considering
# for the missing-library() corrective step (see .repro_find_export_pkg()
# and the module's own corrective re-run) even when a paper's OWN declared
# dependencies (repro_dependencies()) do not name them -- these are packages
# whose exported names turn up constantly in scripts that never library()
# them explicitly (a test file run standalone with no library(testthat); a
# %>% pipe with no library(magrittr)/library(dplyr)). Deliberately NOT a
# blind scan of every installed/CRAN package: the companion issue's own
# evidence found a blind scan ambiguous 13.5% of the time (e.g. %>% alone
# matches 69 installed packages) -- this list stays short and high-confidence
# on purpose, and the ambiguity check in .repro_find_export_pkg() still
# applies to it exactly as it does to a paper's own declared dependencies.
.repro_common_pkgs <- function() {
  c("testthat", "magrittr", "dplyr", "tidyr", "ggplot2", "purrr", "tibble",
    "stringr", "readr", "forcats", "data.table", "lubridate", "readxl", "nlme",
    "lme4", "emmeans", "lsmeans", "MASS", "ape", "picante", "shiny", "vegan")
}

#' Find the one installed package (if any) that unambiguously exports a name
#'
#' Used by the module's corrective re-run for a `could not find function "X"`
#' error that no file in the paper's own code defines (the sibling case to
#' [repro_defined_vars()]'s repo-file lookup, for when the missing symbol is a
#' PACKAGE export the author simply forgot to `library()`, not a local
#' variable/function). Deliberately scoped, not a blind search of every
#' installed or CRAN package: only `candidates` (the paper's own declared
#' dependencies plus [.repro_common_pkgs()]'s small curated safe-list) are
#' checked, so an ambiguous common name (`%>%`, `select`, ...) across the
#' package universe at large is never even considered — see this function's
#' caller for why a blind scan is unsafe (a real corpus check found `%>%`
#' alone exported by 69 installed packages).
#'
#' A candidate package that is not installed is silently skipped (not an
#' error): this only ever runs against packages already usable in the current
#' R installation, since `BiocManager`/`remotes`-style live lookups are out of
#' scope for a same-process check like this one.
#'
#' @param name the undefined symbol (e.g. `"test_that"`, `"%>%"`)
#' @param candidates character vector of candidate package names to check
#'   (already scoped by the caller — see above)
#'
#' @returns the single package name that exports `name`, or `NA_character_`
#'   when zero or more than one candidate does (never guessed)
#' @keywords internal
.repro_find_export_pkg <- function(name, candidates) {
  candidates <- unique(candidates[!is.na(candidates) & nzchar(candidates %||% "")])
  if (!length(candidates) || !nzchar(name %||% "")) return(NA_character_)
  hits <- character(0)
  for (pkg in candidates) {
    if (!requireNamespace(pkg, quietly = TRUE)) next   # not installed: skip
    exports <- tryCatch(getNamespaceExports(asNamespace(pkg)),
                        error = function(e) character(0))
    if (name %in% exports) hits <- c(hits, pkg)
  }
  hits <- unique(hits)
  if (length(hits) == 1) hits else NA_character_
}

# File names a script joins to a variable, as lower-case base names: the literal parts
# of paste(x, "field.csv"), paste0(x, "/field.csv") and, in Python, x + "field.csv". The
# whole name is known only when the code runs. A script that does not parse gives none.
.repro_built_names <- function(code_text, lang = "R") {
  if (is.null(code_text) || length(code_text) == 0) return(character(0))
  found <- if (identical(lang, "Python")) {
    nc <- paste(code_remove_comments(code_text, "Python"), collapse = "\n")
    hits <- regmatches(nc, gregexpr("\\+\\s*(['\"])[^'\"\n]+\\1", nc, perl = TRUE))[[1]]
    gsub("^\\+\\s*['\"]|['\"]$", "", hits, perl = TRUE)
  } else {
    walk <- function(e) {
      out <- character(0)
      if (deparse(e[[1]])[1] %in% c("paste", "paste0", "base::paste", "base::paste0")) {
        args <- as.list(e)[-1]
        if (!is.null(names(args))) args <- args[!nzchar(names(args))]
        literal <- vapply(args, is.character, logical(1))
        if (!all(literal)) out <- unlist(args[literal])
      }
      # e[[i]] is tested directly because x[, 1] holds an empty argument, which
      # cannot be passed on to a function of our own
      for (i in seq_along(e)) if (is.call(e[[i]])) out <- c(out, walk(e[[i]]))
      out
    }
    exprs <- tryCatch(parse(text = code_text, keep.source = FALSE), error = function(e) NULL)
    unlist(lapply(Filter(is.call, as.list(exprs)), walk), use.names = FALSE)
  }
  unique(tolower(basename(gsub("\\\\", "/", found %||% character(0)))))
}

# Which rows of a repro_rewrite_paths() table name a file that can be checked before
# the code runs. A name built at run time cannot: the row of a paste() or sprintf() call,
# a name from .repro_built_names() (`built`), a name with a placeholder ({name}.csv,
# %s.pdf), or a piece of one (_field.csv, .png). Such a name is only left out of the
# prediction, and the script is run in any case, so a real file called _data.csv does no
# harm here. A backslash is no such sign, because it separates folders on Windows.
.repro_ref_checkable <- function(d, built = character(0)) {
  is_call <- if ("is_call" %in% names(d)) d$is_call %in% TRUE else rep(FALSE, nrow(d))
  piece <- grepl("^[._]", d$basename) | !grepl(".", d$basename, fixed = TRUE)
  placeholder <- grepl("[%{}$*]|: ", d$ref)
  !is_call & !piece & !placeholder & !(tolower(d$basename) %in% built)
}

# The predicted missing files worth reporting. Without a run, all of them. After a run,
# only files the deposit does hold but the code could not use: code_check already lists
# files that are not in the repository, and the run shows what happened to each script.
.repro_inputs_to_report <- function(missing_inputs, execute = FALSE) {
  if (!isTRUE(execute) || nrow(missing_inputs) == 0) return(missing_inputs)
  missing_inputs[missing_inputs$status != "absent" | !is.na(missing_inputs$similar_to), , drop = FALSE]
}

# The predicted missing files that still count after a run: those a script names that
# did not run through. A script that ran without an error did not need its file, as when
# it reads the file only inside if (file.exists()).
.repro_inputs_after_run <- function(missing_inputs, unresolved, file_names, run_results) {
  if (is.null(run_results) || nrow(missing_inputs) == 0) return(missing_inputs)
  ran <- file_names %in% run_results$file_name[
    run_results$outcome %in% c("ran_ok", "run_by_source", "run_by_import")]
  needed <- unlist(unresolved[!ran], use.names = FALSE)
  missing_inputs[missing_inputs$basename %in% needed, , drop = FALSE]
}

# The traffic light before any error in a run is counted. Red is the "code cannot run
# only because size-gated inputs were withheld" story: some inputs missing, and all of
# them size-gated. `clean` says that nothing else in the static check found a problem.
.repro_static_light <- function(missing_inputs, clean) {
  n <- nrow(missing_inputs)
  if (n > 0 && sum(missing_inputs$status == "withheld_size") == n) "red"
  else if (clean && n == 0) "green"
  else "yellow"
}

#' Diagnose why a referenced input file is unavailable
#'
#' When a script reads a file that is not present, the reason matters: a file
#' deliberately withheld because it was too large to download is a different
#' finding from a file simply absent from the repository. This cross-references
#' the referenced basenames against the file plan and the download-skip records
#' `download_repo_files()` attaches, and classifies each unresolved reference.
#'
#' @param refs a character vector of referenced file basenames (unresolved reads)
#' @param plan the `psychds_check` table (file_name + target_path)
#' @param structure_df the `data_check` structure table (file_name +
#'   file_location + file_size)
#' @param skipped optional data frame of size-skipped files (the
#'   `oversize_skipped` / `gated` attributes of [download_repo_files()], with a
#'   `file_name` column)
#'
#' @returns a data frame with `basename`, `status` (one of `in_repo_not_downloaded`,
#'   `withheld_size`, `in_archive` (the file is in the repository only inside
#'   a .zip or .tar, so the code cannot read it as the package is), `absent`),
#'   `detail` (a human-readable reason), and
#'   `similar_to` (a candidate near-match basename from the repository's file
#'   list, or `NA` — only ever set for `absent` rows; see below).
#' @export
repro_missing_inputs <- function(refs, plan, structure_df, skipped = NULL) {
  refs <- unique(tolower(basename(gsub("\\\\", "/", refs))))
  empty <- data.frame(basename = character(0), status = character(0),
                      detail = character(0), similar_to = character(0))
  if (length(refs) == 0) return(empty)

  struct_base <- if (!is.null(structure_df) && "file_name" %in% names(structure_df))
    tolower(basename(gsub("\\\\", "/", structure_df$file_name))) else character(0)
  skip_base <- if (!is.null(skipped) && "file_name" %in% names(skipped))
    tolower(basename(gsub("\\\\", "/", skipped$file_name))) else character(0)
  # Files listed from inside an archive have its name in front of their path
  # ("data.zip/data/x.csv"); the archive is the path up to it (#520, item 2).
  arch_re <- "^(.*?[^/]+\\.(zip|tar|tgz|tar\\.gz))/.*$"
  struct_path <- if (!is.null(structure_df) && "file_path" %in% names(structure_df))
    gsub("\\\\", "/", structure_df$file_path) else rep(NA_character_, length(struct_base))
  struct_archive <- ifelse(!is.na(struct_path) & grepl(arch_re, struct_path, perl = TRUE),
                           sub(arch_re, "\\1", struct_path, perl = TRUE), NA_character_)

  # A file genuinely absent from the repository is sometimes actually PRESENT
  # under a near-identical name — a typo in the code, or a capitalisation/
  # separator mismatch ("Data.csv" referenced, "data.csv" present) — which is a
  # different (and more actionable) finding than "this file was never shared at
  # all". Tried only for an otherwise-`absent` reference (an exact match already
  # resolved above needs no fallback): first the SAME NAME in another data format
  # (a script asking for `data.csv` when only `data.xlsx` was shared — common
  # when authors describe a conversion in prose but never update the code), then
  # a small EDIT DISTANCE on the full basename (a typo/case/punctuation slip).
  # Never guesses across a large distance — a near-miss is surfaced as a
  # candidate for a human to confirm, not silently treated as a match.
  all_candidates <- unique(struct_base)
  find_similar <- function(b) {
    if (length(all_candidates) == 0) return(NA_character_)
    ext <- tolower(tools::file_ext(b))
    cand_ext <- tolower(tools::file_ext(all_candidates))
    # The same name in another format counts only between data formats: x.csv
    # and x.html sharing a name is a coincidence.
    key <- .repro_name_key(b)
    stem_hit <- all_candidates[.repro_name_key(all_candidates) == key & nzchar(key) &
                                 cand_ext != ext & ext %in% .repro_data_exts &
                                 cand_ext %in% .repro_data_exts]
    if (length(stem_hit) > 0) return(stem_hit[[1]])
    # Small edit distance on the full basename, among files of the same
    # extension. Threshold: at most 3 edits, and never more than a
    # quarter of the reference's own length, so a short name ("x.csv") cannot
    # match something unrelated purely because 3 edits is a large fraction of
    # it — same "do not guess" spirit as the rest of this file.
    max_dist <- min(3L, floor(nchar(b) / 4))
    if (max_dist < 1L) return(NA_character_)
    d <- utils::adist(b, all_candidates)[1, ]
    d[cand_ext != ext] <- Inf
    hit <- which(d <= max_dist)
    if (length(hit) == 0) return(NA_character_)
    all_candidates[hit[which.min(d[hit])]]
  }

  rows <- lapply(refs, function(b) {
    if (b %in% skip_base) {
      i <- which(skip_base == b)[1]
      # without a file_size column the size is left out: NULL[i] stopped the
      # function ("argument is of length zero") (U135)
      sz <- if ("file_size" %in% names(skipped))
        suppressWarnings(as.numeric(skipped$file_size[i])) else NA_real_
      mb <- if (is.finite(sz)) sprintf(" (%.0f MB)", sz / (1024 * 1024)) else ""
      return(data.frame(basename = b, status = "withheld_size",
                        detail = sprintf(
                          "referenced file is in the repository but was not downloaded%s: over the size cap",
                          mb), similar_to = NA_character_))
    }
    # in the repository only inside an archive: the code runs on the package
    # as it is, so it cannot read the file before someone unpacks it
    in_arch <- which(struct_base == b)
    if (length(in_arch) && all(!is.na(struct_archive[in_arch]))) {
      return(data.frame(basename = b, status = "in_archive",
                        detail = sprintf("referenced file is only inside %s; unpack it so the code finds it",
                                         paste(unique(struct_archive[in_arch]), collapse = ", ")),
                        similar_to = NA_character_))
    }
    if (b %in% struct_base) {
      i <- which(struct_base == b)[1]
      loc <- if ("file_location" %in% names(structure_df))
        structure_df$file_location[i] else NA_character_
      downloaded <- !is.na(loc) && nzchar(loc) && file.exists(loc %||% "")
      if (!downloaded)
        return(data.frame(basename = b, status = "in_repo_not_downloaded",
                          detail = "referenced file is listed in the repository but was not downloaded",
                          similar_to = NA_character_))
      # Present and downloaded — not actually missing; caller filters these out.
      return(data.frame(basename = b, status = "present",
                        detail = "referenced file is available",
                        similar_to = NA_character_))
    }
    similar <- find_similar(b)
    detail <- if (is.na(similar)) "referenced file is not present in the repository"
      else if (tolower(tools::file_ext(similar)) != tolower(tools::file_ext(b)))
        sprintf("the deposit holds %s, the code reads %s", similar, b)
      else sprintf("referenced file is not present in the repository, but a similarly-named file exists: %s",
                   similar)
    data.frame(basename = b, status = "absent", detail = detail,
              similar_to = similar)
  })
  out <- dplyr::bind_rows(rows)
  out[out$status != "present", , drop = FALSE]
}

#' Sniff whether a `.R`/`.Rmd` file that failed to parse is actually R
#'
#' A parse failure ([code_parse_r()], via `code_check`'s `parse_error`) is
#' usually a genuine defect in the paper's code — but occasionally the file
#' was never R source at all (a serialized JSON/HTML widget export saved with
#' a `.R` extension) or is a well-known non-R DSL (a JAGS/BUGS model
#' definition, see [.repro_is_jags_model()]). Reporting either of those as
#' "failed to parse" reads identically to a real syntax error in real
#' analysis code, giving a reader no way to tell "the author has a bug" from
#' "this file was never R to begin with".
#'
#' Deliberately conservative: only the UNAMBIGUOUS cases are flagged (a JSON
#' object/array spanning essentially the whole file, or a clear HTML/XML
#' doctype/tag at the very start) — checked on the RAW text, not something
#' `code_parse_r()` already tried to parse as R, so this never depends on
#' *why* the parse failed. Anything else (including a real, if unusual, R
#' script) is left alone and reported as a genuine parse failure, same as
#' before this existed.
#'
#' @param code_text the file's raw text (character vector, one element per
#'   line, as read by `code_read()`/`code_extract_r()`)
#'
#' @returns `NA_character_` when nothing matches (treat as a genuine parse
#'   failure), else one of `"JSON"`, `"HTML"`, `"XML"` naming what was
#'   detected
#' @keywords internal
.repro_content_sniff <- function(code_text) {
  if (is.null(code_text) || !length(code_text)) return(NA_character_)
  txt <- trimws(paste(code_text, collapse = "\n"))
  if (!nzchar(txt)) return(NA_character_)

  # HTML/XML: an unambiguous doctype/root-tag marker at the very start (after
  # stripping leading whitespace) — real R code never legitimately starts
  # this way (a `<` at the very start of a file, before any comment or
  # assignment, is not valid R syntax either).
  if (grepl("^<!DOCTYPE\\s+html", txt, ignore.case = TRUE, perl = TRUE)) return("HTML")
  if (grepl("^<html\\b", txt, ignore.case = TRUE, perl = TRUE)) return("HTML")
  if (grepl("^<\\?xml\\b", txt, ignore.case = TRUE, perl = TRUE)) return("XML")

  # JSON: the file starts with `{` or `[` AND the matching close (found by a
  # simple bracket-depth scan, respecting quotes) is at or near the very end
  # of the file — "spans essentially the whole file", not merely a `{` that
  # opens an R block at the top of an otherwise-real script (e.g. a script
  # that starts with `{ x <- 1; ... }` -- rare, but real R). Requiring the
  # closing bracket to land in roughly the last 1% of the text (or the last
  # 5 characters, whichever is larger) is what keeps this conservative.
  first_ch <- substr(txt, 1, 1)
  if (first_ch %in% c("{", "[")) {
    close_ch <- if (first_ch == "{") "}" else "]"
    # Characters split once: this walks a WHOLE file, and substr(txt, i, i)
    # made it quadratic in the file's length (#481).
    chars <- strsplit(txt, "", fixed = TRUE)[[1]]
    n <- length(chars)
    depth <- 0L; i <- 1L; in_str <- NA_character_; end <- NA_integer_
    while (i <= n) {
      ch <- chars[i]
      if (!is.na(in_str)) {
        if (ch == "\\") i <- i + 1L
        else if (ch == in_str) in_str <- NA_character_
      } else if (ch %in% c('"')) in_str <- ch   # JSON strings are double-quoted only
      else if (ch == first_ch) depth <- depth + 1L
      else if (ch == close_ch) { depth <- depth - 1L; if (depth == 0L) { end <- i; break } }
      i <- i + 1L
    }
    if (!is.na(end)) {
      tail_len <- n - end
      # A trailing `# roxygen`-style comment or blank lines after the closing
      # bracket would still be "essentially the whole file"; anything with
      # substantial trailing content is left alone (more likely a real R
      # block followed by more R code).
      if (tail_len <= max(5L, ceiling(n * 0.01))) return("JSON")
    }
  }
  NA_character_
}

#' Recognise a JAGS/BUGS model-definition file
#'
#' A JAGS/BUGS model (`model { ... }`) is written in JAGS's own modelling
#' language, not R, and is meant to be passed as a text file to
#' `rjags::jags.model()`/`R2jags::jags()`/`runjags::run.jags()`, never parsed
#' as R directly — authors frequently save one with a `.R` extension, which
#' then fails [code_parse_r()] and reads identically to a genuine broken R
#' script. Matched via three signals, any one of which is treated as
#' sufficient (the third — an actual calling reference from the paper's own
#' code — is the strongest, since it confirms the paper's own code treats
#' this file as a JAGS model rather than something to run directly):
#'
#' 1. the file's first non-comment, non-blank content is the token `model`
#'    followed by `{`, also on the next line, or a `data { ... }` block with a
#'    `model` block after it;
#' 2. its path contains a component named `jags_script`/`bugs_script`/
#'    `jags_model` (case-insensitive);
#' 3. it is referenced elsewhere in the SAME paper's own R code as the
#'    `file=`/first argument to `rjags::jags.model()`, `R2jags::jags()`, or
#'    `runjags::run.jags()`.
#'
#' @param code_text the file's raw text (character vector, one element per
#'   line)
#' @param file_name the file's own name/path (checked for a `jags_script`/
#'   `bugs_script`/`jags_model` path component)
#' @param other_code_text optional list of OTHER files' code text (e.g. the
#'   paper's other R files) to scan for a `jags.model()`/`jags()`/`run.jags()`
#'   call naming this file — `NULL` skips this (strongest) signal
#'
#' @returns `TRUE` when any signal matches, else `FALSE`
#' @keywords internal
.repro_is_jags_model <- function(code_text, file_name = "", other_code_text = NULL) {
  if (!is.null(code_text) && length(code_text)) {
    nc <- code_remove_comments(code_text, "R")
    nc <- nc[nzchar(trimws(nc))]
    if (length(nc)) {
      start <- paste(utils::head(nc, 2), collapse = " ")
      if (grepl("^\\s*model\\s*\\{", start, perl = TRUE)) return(TRUE)
      if (grepl("^\\s*data\\s*\\{", start, perl = TRUE) &&
          any(grepl("^\\s*model\\s*(\\{|$)", nc, perl = TRUE))) return(TRUE)
    }
  }
  if (nzchar(file_name %||% "") &&
      grepl("(^|[/\\\\])(jags_script|bugs_script|jags_model)([/\\\\]|$)",
           file_name, ignore.case = TRUE, perl = TRUE)) return(TRUE)

  if (!is.null(other_code_text) && length(other_code_text)) {
    fn_base <- basename(gsub("\\\\", "/", file_name %||% ""))
    call_pat <- paste0(
      "\\b(rjags::jags\\.model|jags\\.model|R2jags::jags|runjags::run\\.jags|run\\.jags)",
      "\\s*\\(\\s*(?:file\\s*=\\s*)?['\"]([^'\"]+)['\"]")
    for (ct in other_code_text) {
      if (is.null(ct) || !length(ct)) next
      joined <- paste(code_remove_comments(ct, "R"), collapse = "\n")
      m <- regmatches(joined, gregexpr(call_pat, joined, perl = TRUE))[[1]]
      if (!length(m)) next
      refs <- sub(paste0(".*", call_pat), "\\2", m, perl = TRUE)
      if (nzchar(fn_base) && fn_base %in% basename(gsub("\\\\", "/", refs)))
        return(TRUE)
    }
  }
  FALSE
}

# The functions an R file defines, when defining things is all it does: apart from
# functions, only library() calls, constants and the like. Empty otherwise, also for
# a file that does not parse. Such a file is not an analysis: run on its own it
# "succeeds" without doing anything. A function counts when a script uses it (#568).
.repro_function_names <- function(code_text) {
  ex <- tryCatch(parse(text = code_text, keep.source = FALSE), error = function(e) NULL)
  if (is.null(ex) || !length(ex)) return(character(0))
  literal <- function(x) is.atomic(x) || (is.call(x) && identical(x[[1]], as.name("c")) &&
                                            all(vapply(.repro_call_parts(x)[-1], literal, logical(1))))
  setup <- c("library", "require", "requireNamespace", "suppressPackageStartupMessages",
             "setGeneric", "setMethod", "setClass", "setValidity", "setRefClass")
  defs <- character(0)
  for (e in ex) {
    if (is.atomic(e)) next
    if (!is.call(e)) return(character(0))
    if (identical(deparse(e[[1]])[1], "utils::globalVariables")) next
    if (!is.name(e[[1]])) return(character(0))
    f <- as.character(e[[1]])
    if (f %in% setup) next
    if (!f %in% c("<-", "=", "<<-") || length(e) != 3 ||
        !(is.name(e[[2]]) || is.character(e[[2]]))) return(character(0))
    v <- e[[3]]
    if (is.call(v) && identical(v[[1]], as.name("function"))) defs <- c(defs, as.character(e[[2]]))
    else if (!literal(v)) return(character(0))
  }
  defs
}

# The scripts that use a file's functions: calling one by name, or sourcing the file.
.repro_function_users <- function(fn, functions, code_text_list, sources = list()) {
  others <- setdiff(names(code_text_list), fn)
  if (!length(functions) || !length(others)) return(character(0))
  called <- paste0("(^|[^A-Za-z0-9._])(", paste(gsub("([.])", "\\\\\\1", functions), collapse = "|"), ")\\s*\\(")
  base <- tolower(basename(gsub("\\\\", "/", fn)))
  others[vapply(others, function(o) {
    base %in% sources[[o]] ||
      any(grepl(called, iconv(code_text_list[[o]], "", "UTF-8", sub = ""), perl = TRUE))
  }, logical(1))]
}

# A package's own tests: run on their own, they test whether the package is
# installed, not whether the paper's analysis reproduces.
.repro_is_r_test <- function(paths) {
  grepl("(^|/)tests/testthat(/|\\.[Rr]$)", gsub("\\\\", "/", paths))
}

# The same for Python, and the file that installs a package. A name alone is not
# enough: model_test.py and predict_test.py can be analyses.
.repro_is_py_test <- function(path, code_text) {
  path <- tolower(gsub("\\\\", "/", path))
  base <- basename(path)
  if (!grepl("\\.py$", base)) return(FALSE)
  if (base == "conftest.py") return(TRUE)
  has <- function(pat) any(grepl(pat, code_text, perl = TRUE, useBytes = TRUE))
  if (base == "setup.py")
    return(has("^\\s*(import|from)\\s+(setuptools|distutils)\\b") && has("\\bsetup\\s*\\("))
  in_tests <- grepl("(^|/)tests?/", path)
  named <- grepl("^test_.*\\.py$|_test\\.py$|^tests?\\.py$", base)
  ((named || in_tests) && has("^\\s*(import|from)\\s+(pytest|unittest|nose|django\\.test)\\b")) ||
    (in_tests && has("^\\s*def\\s+test_"))
}

# The report's section on files that are not run on their own; NULL when none.
# `why` gives, for a file that is not part of the analysis, the sentence of
# .repro_software_kind() (NA for a package's tests and setup.py).
.repro_not_run_report <- function(file_names, kinds, used_by = NULL, why = NULL) {
  fn <- kinds %in% "defines_functions"
  ts <- kinds %in% "not_analysis"
  if (!any(fn) && !any(ts)) return(NULL)
  used <- if (is.null(used_by)) rep(NA_character_, length(kinds)) else used_by
  c("#### Files not run on their own",
    if (any(fn)) sprintf(
      paste0("%d file%s only define%s functions, so running %s on %s own says nothing: a ",
             "function counts when a script uses it. These files are not counted as scripts. ",
             "%d %s used by no script in the deposit."),
      sum(fn), plural(sum(fn)), plural(sum(fn), "s", ""), plural(sum(fn), "it", "them"),
      plural(sum(fn), "its", "their"), sum(is.na(used[fn])), plural(sum(is.na(used[fn])), "is", "are")),
    if (any(fn)) scroll_table(data.frame(
      File = file_names[fn],
      `Used by` = ifelse(is.na(used[fn]), "no script", used[fn]),
      check.names = FALSE), maxrows = 10),
    if (any(ts)) sprintf(
      paste0("%d file%s %s not part of the analysis but of software (a package, its tests or ",
             "documentation, a tool, a web application), so %s not run or counted as scripts."),
      sum(ts), plural(sum(ts)), plural(sum(ts), "is", "are"), plural(sum(ts), "it was", "they were")),
    if (any(ts)) scroll_table(data.frame(
      File = file_names[ts],
      Why = .repro_not_analysis_why(if (is.null(why)) rep(NA_character_, sum(ts)) else why[ts]),
      check.names = FALSE), maxrows = 10))
}

# ─────────────────────────────────────────────────────────────────────────────
# EXECUTION helpers (the `execute = TRUE` phase). Everything below actually runs
# downloaded code, so it is reached only when the caller explicitly opts in via
# reproducibility_check(execute = TRUE). Each script runs in an isolated
# subprocess (callr) so a crash cannot take down the R session; note this
# isolates crashes, NOT the filesystem or network — running untrusted code is a
# deliberate, gated action. The layout is materialised into a throwaway temp dir
# so the real psychds output and the download cache are never touched.
# ─────────────────────────────────────────────────────────────────────────────

# The file on disk behind each row of a plan, or NA when it has none.
#
# A plan that names each row's own source file (the package layout, see
# .repro_package_plan()) is used as it is. A plan made by psychds_file_plan()
# has one row per row of structure_df, in the same order, so row i's file is
# structure row i's. Only a plan that is neither (filtered, or built by hand)
# is looked up by file name, then by base name. The name lookup alone took
# the first file of that name: the demographics.csv of several OSF components
# all got the first component's content, and in repro_rewrite_paths() they
# then had the same content hash and were taken for copies of one file.
.repro_plan_sources <- function(plan, structure_df) {
  n <- nrow(plan)
  out <- rep(NA_character_, n)
  if (!n) return(out)
  exists_ <- function(p) !is.na(p) & nzchar(p %||% "") & file.exists(p %||% "")

  own <- if ("source" %in% names(plan)) plan$source else rep(NA_character_, n)
  has_own <- exists_(own)
  out[has_own] <- own[has_own]

  if (is.null(structure_df) ||
      !all(c("file_name", "file_location") %in% names(structure_df)))
    return(out)

  # psychds_file_plan() gives a row without a file name its path's base name
  s_name <- structure_df$file_name
  no_name <- is.na(s_name) | !nzchar(s_name)
  s_name[no_name] <- basename(structure_df$file_path %||% rep(NA_character_, nrow(structure_df)))[no_name]
  aligned <- nrow(structure_df) == n &&
    identical(as.character(plan$file_name), as.character(s_name))

  if (aligned) {
    loc <- structure_df$file_location
    use <- !has_own & exists_(loc)
    out[use] <- loc[use]
    return(out)
  }

  loc_lookup <- stats::setNames(structure_df$file_location, structure_df$file_name)
  # basename index, for when plan file_name and structure file_name differ by dir
  loc_base <- stats::setNames(loc_lookup, tolower(basename(names(loc_lookup))))
  for (i in which(!has_own)) {
    fn <- plan$file_name[i]
    if (is.na(fn)) next
    if (fn %in% names(loc_lookup) && exists_(loc_lookup[[fn]])) {
      out[i] <- loc_lookup[[fn]]; next
    }
    b <- tolower(basename(fn))
    if (b %in% names(loc_base) && exists_(loc_base[[b]])) out[i] <- loc_base[[b]]
  }
  out
}

#' Copy a paper's files into a throwaway directory, as a plan places them
#'
#' The execute phase runs the paper's code in the repository as the authors
#' organised it ([.repro_package_plan()]: every file at its own path), but the
#' module has only the *plan* (file → `target_path`) and the
#' current on-disk location of each source file (`data_check`'s `structure`).
#' This builds the tree the plan describes inside a fresh temp directory: for
#' each planned file with a real target and an available source copy, the source
#' is copied to `<root>/<target_path>`. Nothing outside `root` is written, and
#' `root` is the caller's to delete.
#'
#' @param plan the `psychds_check` table (`file_name`, `target_path`), or the
#'   package layout's plan ([.repro_package_plan()]), whose `source` column
#'   names each row's own file
#' @param structure_df the `data_check` structure table (`file_name`,
#'   `file_location`)
#' @param root the temp directory to build into (created if absent)
#'
#' @returns `root`, invisibly, with attribute `"materialised"`: a data frame of
#'   `target_path` / `source` / `ok` (whether the copy succeeded) rows.
#' @export
repro_materialize_layout <- function(plan, structure_df, root) {
  dir.create(root, recursive = TRUE, showWarnings = FALSE)
  # A flat folder every write call is redirected into (see
  # .repro_redirect_writes(), applied in repro_write_scripts()) — created
  # UNCONDITIONALLY, before the no-plan early return below, since a script can
  # write output even for a paper with no psychds_check plan at all (no data
  # files to lay out, but still code that writes a results file).
  dir.create(file.path(root, "output"), recursive = TRUE, showWarnings = FALSE)
  done <- data.frame(target_path = character(0), source = character(0),
                     ok = logical(0))
  if (is.null(plan) || nrow(plan) == 0 ||
      !all(c("file_name", "target_path") %in% names(plan)))
    return(invisible(structure(root, materialised = done)))

  # Each row's own source file (see .repro_plan_sources()): looked up by name,
  # the 20 log.csv files of 20 scan folders all got the first one's content.
  sources <- .repro_plan_sources(plan, structure_df)
  # A Git LFS pointer in place of the file, as GitHub's archives hold for files
  # kept in LFS: the copy of the same name and of the size the pointer states,
  # elsewhere in the deposit, is used instead. In 10.1111/1365-2664.14751 the
  # GitHub archive had pointers for the .RDS files, the Zenodo copy the files,
  # and five scripts stopped with "unknown input format" (#568).
  lfs <- .repro_lfs_swaps(sources)
  is_ptr <- sources %in% lfs$pointer
  if (any(is_ptr)) sources[is_ptr] <- lfs$file[match(sources[is_ptr], lfs$pointer)]

  has_target <- !is.na(plan$target_path) & nzchar(plan$target_path %||% "")
  rows <- lapply(which(has_target), function(i) {
    tgt <- plan$target_path[i]
    src <- sources[i]
    if (is.na(src))
      return(data.frame(target_path = tgt, source = NA_character_, ok = FALSE))
    dest <- file.path(root, .portable_path(tgt))
    dir.create(dirname(dest), recursive = TRUE, showWarnings = FALSE)
    ok <- isTRUE(file.copy(src, dest, overwrite = TRUE))
    # The copy is the code's to change: in the Docker sandbox the code runs
    # as another user, and a script that saves over a file the package holds
    # (raw_data/fig9/histogramroi_counts_matrix.npy, written by the author's
    # own run) failed with "Permission denied".
    if (ok) Sys.chmod(dest, "0666", use_umask = FALSE)
    data.frame(target_path = tgt, source = src, ok = ok)
  })

  # A converted tabular row (psychds_check's `convert`/`original_target`
  # columns — see R/data_check_helpers.R and repro_rewrite_paths()'s own
  # roxygen) ALSO needs its untouched ORIGINAL copied in, not just the
  # `_data.csv` re-export above: repro_rewrite_paths() rewrites a script's
  # read_dta()/read_sav()/read_excel() call to `original_target` specifically
  # BECAUSE that reader needs the real file, not the CSV. Without this, the
  # rewrite correctly names the right path, but nothing ever puts a file there
  # — confirmed directly against a real paper (a read_dta("math.dta") call,
  # correctly rewritten to "data/math.dta" (original_target keeps the file's
  # own real basename — see psychds_check.R's same_dir_real_name()), still
  # failed at execute time with "'data/math.dta' does not exist", because only
  # the target_path loop above had ever populated the sandbox).
  if ("original_target" %in% names(plan)) {
    has_orig <- !is.na(plan$original_target) & nzchar(plan$original_target %||% "")
    orig_rows <- lapply(which(has_orig), function(i) {
      tgt <- plan$original_target[i]
      src <- sources[i]
      if (is.na(src))
        return(data.frame(target_path = tgt, source = NA_character_, ok = FALSE))
      dest <- file.path(root, .portable_path(tgt))
      dir.create(dirname(dest), recursive = TRUE, showWarnings = FALSE)
      ok <- isTRUE(file.copy(src, dest, overwrite = TRUE))
      data.frame(target_path = tgt, source = src, ok = ok)
    })
    if (length(orig_rows)) rows <- c(rows, orig_rows)
  }

  done <- if (length(rows)) dplyr::bind_rows(rows) else done
  # and its folders, so a script can save a new file next to the data
  Sys.chmod(list.dirs(root, recursive = TRUE), "0777", use_umask = FALSE)
  invisible(structure(root, materialised = done, lfs = lfs))
}

# The Git LFS pointers among `sources` (files on disk) and, for each, the file
# of the same name, elsewhere among them, of the size the pointer states. A
# pointer with no such file is left out. Returns a data frame `pointer`, `file`.
.repro_lfs_swaps <- function(sources) {
  none <- data.frame(pointer = character(0), file = character(0))
  src <- unique(sources[!is.na(sources) & file.exists(sources %||% "")])
  if (!length(src)) return(none)
  size <- file.size(src)
  small <- src[!is.na(size) & size < 1024]
  if (!length(small)) return(none)
  stated <- vapply(small, function(f) {
    l <- tryCatch(readLines(f, n = 3, warn = FALSE), error = function(e) character(0))
    if (!length(l) || !startsWith(l[1], "version https://git-lfs.github.com/spec/")) return(NA_real_)
    s <- suppressWarnings(as.numeric(sub("^size ", "", grep("^size [0-9]+$", l, value = TRUE)[1])))
    if (is.na(s)) NA_real_ else s
  }, numeric(1))
  pointers <- small[!is.na(stated)]
  if (!length(pointers)) return(none)
  rows <- lapply(pointers, function(p) {
    same <- src[tolower(basename(src)) == tolower(basename(p)) & size == stated[[p]] & src != p]
    if (length(same)) data.frame(pointer = p, file = same[1]) else NULL
  })
  rows <- Filter(Negate(is.null), rows)
  if (!length(rows)) none else dplyr::bind_rows(rows)
}

#' The package layout's plan: every file in its own place
#'
#' The Psych-DS layout ([psychds_file_plan()]) gives the code only the data
#' files it recognised, renamed and moved into Psych-DS folders, with the
#' code's paths rewritten to match. That suits a study whose scripts read a
#' few tables, but code that reads folders of images or scan files, or imports
#' its own helper modules from a subfolder, then fails for files that are in
#' the package (`raw_data/scan1/img.tif` became `data/img.tif`;
#' `code/utils/helpers.py` became `analysis/helpers.py`, so `from utils import
#' helpers` failed). The package layout copies every file the run has (every
#' file of a local folder) to its own path in the package, so a path that
#' works in the package works in the sandbox; [repro_rewrite_paths()] still
#' rewrites a reference that does not (an absolute path on the author's
#' computer, a file in another folder) to where that file is.
#'
#' @param structure_df the `data_check` structure table (`file_name`,
#'   `file_path`, `file_location`)
#' @returns a plan with `file_name`, `current_path`, `target_path` (the file's
#'   own path in the package) and `source` (its file on disk), one row per
#'   path
#' @keywords internal
.repro_package_plan <- function(structure_df) {
  empty <- data.frame(file_name = character(0), current_path = character(0),
                      target_path = character(0), source = character(0))
  if (is.null(structure_df) || !nrow(structure_df) || !"file_name" %in% names(structure_df))
    return(empty)
  path <- if ("file_path" %in% names(structure_df)) structure_df$file_path else structure_df$file_name
  path[is.na(path) | !nzchar(path)] <- structure_df$file_name[is.na(path) | !nzchar(path)]
  path <- sub("^(\\./|/)+", "", gsub("\\\\", "/", path))
  source <- if ("file_location" %in% names(structure_df)) structure_df$file_location else NA_character_
  plan <- data.frame(file_name = structure_df$file_name, current_path = path,
                     target_path = path, source = source)
  plan <- plan[!is.na(plan$target_path) & nzchar(plan$target_path) &
                 !grepl("(^|/)\\.\\.(/|$)", plan$target_path) &
                 !duplicated(plan$target_path), , drop = FALSE]
  rownames(plan) <- NULL
  plan
}

#' Rewrite only the references that cannot work as written
#'
#' Every script runs in the repository as the authors organised it, with every
#' file at its own path, from the folder [.repro_run_folders()] chooses. A
#' reference is then:
#' * left as written when it names a file's path from the top
#'   (`"raw_data/scan1/log.csv"`), also when other files share the name (20
#'   scan folders each with a `log.csv`), and when it is a relative path that
#'   names no file and no file with its name was found, or the file found has a
#'   path that ends in the path as written: the code probably joins it with a
#'   folder of its own (`os.path.join(base, "fig7/x.csv")`), and rewriting it
#'   to the full path gave `processed_data/fig7/processed_data/fig7/x.csv`;
#' * rewritten to the one file with its name elsewhere in the repository when
#'   it names no file as written (`"./data/x.csv"` with the file in
#'   `exploratory/`): the authors moved or left out a folder when they
#'   deposited;
#' * rewritten to the file's path from the top when it names a file relative
#'   to the script's own folder (`source("helpers.R")`,
#'   `"../raw_data/scores.csv"`), since the script does not run from there;
#' * rewritten as [repro_rewrite_paths()] found, by the file's name, when it
#'   is an absolute path (`Z:/Strontium/...` on the author's computer) or
#'   goes up out of the package.
#'
#' A reference left as written is marked `as_written`; it is not counted as
#' rewritten, and not as a missing input, unless it names no file and no file
#' with its name is in the repository. Another exception is a file the
#' package has only inside an archive, read at the path unpacking it would
#' give: as the package is, that path does not exist, so the reference is
#' not found, and the file becomes a missing input (`in_archive` in
#' [repro_missing_inputs()]).
#'
#' @param rw a [repro_rewrite_paths()] result for one script
#' @param plan the package layout's plan ([.repro_package_plan()])
#' @param script_path the script's own path in the package
#' @param run_dir the folder the script runs from, from the top of the package
#'   (see [.repro_run_folders()]); a reference works as written when it names a
#'   file counted from there, and a rewritten path is counted from there too
#' @returns `rw` with a logical `as_written` column
#' @keywords internal
.repro_keep_working_refs <- function(rw, plan, script_path = NA_character_, run_dir = ".") {
  rw$as_written <- rep(FALSE, nrow(rw))
  if (!nrow(rw) || is.null(plan) || !nrow(plan)) return(rw)
  is_call <- if ("is_call" %in% names(rw)) rw$is_call %in% TRUE else rep(FALSE, nrow(rw))
  ref <- sub("^(\\./)+", "", gsub("\\\\", "/", rw$ref))
  absolute <- grepl("^([A-Za-z]:/|/|~)", ref)
  # the path from the top, when the reference names a file relative to the
  # script's own folder
  script_dir <- if (length(script_path) == 1 && !is.na(script_path) && nzchar(script_path))
    dirname(gsub("\\\\", "/", script_path)) else "."
  from_script <- vapply(ref, function(r) {
    if (grepl("^([A-Za-z]:/|/|~)", r)) return(NA_character_)
    p <- .repro_norm_rel(if (script_dir %in% c(".", "")) r else file.path(script_dir, r))
    if (!is.na(p) && p %in% plan$target_path) p else NA_character_
  }, character(1), USE.NAMES = FALSE)
  from_run <- vapply(ref, function(r) {
    if (grepl("^([A-Za-z]:/|/|~)", r)) return(NA_character_)
    .repro_norm_rel(if (run_dir %in% c(".", "")) r else file.path(run_dir, r))
  }, character(1), USE.NAMES = FALSE)
  in_place <- !is_call & !is.na(from_run) & from_run %in% plan$target_path
  relocate <- !in_place & !is_call & !is.na(from_script)
  up <- grepl("(^|/)\\.\\.(/|$)", ref)
  # a path built by sprintf()/file.path() is kept unless it holds an absolute path
  absolute_call <- is_call & grepl("[\"']([A-Za-z]:[/\\\\]|/|~)", rw$ref, perl = TRUE)
  keep <- in_place | (!relocate & ((is_call & !absolute_call) | (!is_call & !absolute & !up)))
  # a name spelled otherwise than the file's does not work as written
  if ("name_in_deposit" %in% names(rw)) keep <- keep & is.na(rw$name_in_deposit)
  tgt <- if ("target" %in% names(rw)) sub("^(\\./)+", "", gsub("\\\\", "/", rw$target)) else rep(NA_character_, nrow(rw))
  # A path that names no file, while repro_rewrite_paths() found the one file with
  # that name elsewhere in the repository: the authors moved or left out a folder
  # when they deposited (`./data/x.csv` with the file in `exploratory/`), and the
  # path is pointed there (#568). Not when the file's path ends in the path as
  # written: the code then probably joins it to a folder of its own
  # (`os.path.join(base, "fig7/x.csv")`).
  found <- !is.na(tgt) & nzchar(tgt) & rw$matched %in% TRUE & !(rw$ambiguous %in% TRUE)
  # A file at the top named as it is there is no joined path: the authors ran the
  # script from the top, while here it runs from its own folder. In
  # 10.1002/pan3.10310 the data, a deposit of its own, lies at the top, and
  # "./kilimanjaro_..._questionnaire.csv" was kept as written and not found (#568).
  joined <- found & endsWith(tolower(tgt), paste0("/", tolower(ref)))
  # a bare name counts as joined only when it stands in a joining call
  if ("in_join" %in% names(rw))
    joined <- joined & (grepl("/", ref, fixed = TRUE) | rw$in_join %in% TRUE)
  keep <- keep & !(!in_place & !is_call & !absolute & !up & found & !joined)
  # a path that names no file, with no file of its name in the repository, stays
  # as written and is a missing input
  absent <- keep & !in_place & !is_call & !(rw$matched %in% TRUE)
  # A file found by name only inside an archive ("data.zip/data/x.csv"),
  # read at the path unpacking the archive would give it ("data/x.csv"): the
  # code runs on the package as it is, where that path does not exist, so the
  # reference is not found, and the file is a missing input (#520, item 2).
  arch_re <- "(^|/)[^/]+\\.(zip|tar|tgz|tar\\.gz)/"
  unpacked <- sub(arch_re, "\\1", tgt)
  in_archive <- keep & !in_place & !is.na(tgt) & grepl(arch_re, tgt) &
    (unpacked == ref | endsWith(unpacked, paste0("/", ref)))
  keep <- keep & !in_archive

  rw$matched[relocate] <- TRUE
  rw$ambiguous[relocate] <- FALSE
  rw$target[relocate] <- from_script[relocate]
  rw$as_written <- keep
  rw$matched[keep] <- TRUE
  rw$ambiguous[keep] <- FALSE
  rw$target[keep] <- rw$ref[keep]
  rw$matched[in_archive] <- FALSE
  rw$target[in_archive] <- NA_character_
  rw$matched[absent] <- FALSE
  # a rewritten path counts from where the script runs
  moved <- !keep & !is.na(rw$target) & nzchar(rw$target)
  rw$target[moved] <- vapply(rw$target[moved], .repro_rel_from, character(1), dir = run_dir, USE.NAMES = FALSE)
  rw
}

# Folders on the authors' computer held in a variable (#568): `root <-
# "~/Data_to_publish/"` with `paste0(root, "2_data/")` and `source(paste0(root,
# "/1_scripts/0_FUNCTIONS.R"))` (10.1111/1365-2656.14164), or `dataDir <-
# "~/MGDrivE/Main/SoftwarePaper2"` with `file.path(dataDir, "Data/k.csv")`
# (10.1111/2041-210X.13318). The variable gets the folder of the deposit under
# which the most of the paths pasted to it exist, counted from `run_dir`, the
# folder the script runs from; the pasted pieces are then not rewritten on their
# own (they were, so "/1_scripts/0_FUNCTIONS.R" became "0_FUNCTIONS.R" after the
# author's folder). Only an absolute folder ("C:/...", "/...", "~/...") is
# changed, and only when at least one pasted path exists in the deposit. Returns
# a data frame: `value` (the folder as written), `target` (its new value) and a
# list-column `pieces` (the texts pasted to it, as written).
.repro_folder_vars <- function(code_text, plan, run_dir = ".") {
  empty <- data.frame(value = character(0), target = character(0))
  empty$pieces <- list()
  if (is.null(plan) || !nrow(plan) || !length(code_text)) return(empty)
  nc <- tryCatch(code_remove_comments(.utf8_lines(code_text), "R"), error = function(e) .utf8_lines(code_text))
  vars <- .repro_simple_string_vars(nc)
  vars <- vars[grepl("^([A-Za-z]:[/\\\\]|/|~)", vars) & !grepl("\\.[A-Za-z0-9]{1,8}$", vars)]
  if (!length(vars)) return(empty)
  joined <- paste(nc, collapse = "\n")
  files <- tolower(gsub("\\\\", "/", plan$target_path[!is.na(plan$target_path)]))
  folders <- character(0)
  d <- dirname(files)
  while (length(d <- setdiff(d[d != "."], folders))) {
    folders <- c(folders, d)
    d <- dirname(d)
  }
  known <- c(files, folders)
  rows <- lapply(names(vars), function(v) {
    vq <- gsub("([.\\\\])", "\\\\\\1", v)
    pat <- paste0("\\b(?:paste0|file\\.path|paste)\\s*\\(\\s*", vq, "\\s*,\\s*(['\"])([^'\"\n]*)\\1")
    hits <- regmatches(joined, gregexpr(pat, joined, perl = TRUE))[[1]]
    if (!length(hits)) return(NULL)
    pieces <- unique(sub(pat, "\\2", hits, perl = TRUE))
    rel <- sub("/+$", "", sub("^[/\\\\]+", "", gsub("\\\\", "/", pieces)))
    rel <- rel[nzchar(rel)]
    if (!length(rel)) return(NULL)
    # "../2_data/x.csv" counts from the folder too (10.1111/1365-2656.14164)
    score <- vapply(c(".", folders), function(f) {
      p <- vapply(if (f == ".") rel else file.path(f, rel), .repro_norm_rel, character(1), USE.NAMES = FALSE)
      sum(tolower(p[!is.na(p)]) %in% known)
    }, numeric(1))
    if (max(score) < 1) return(NULL)
    best <- c(".", folders)[score == max(score)]
    # several equally good: the one nearest the script's own folder, else none
    near <- best[startsWith(paste0(tolower(run_dir), "/"), paste0(best, "/")) | best == "."]
    pick <- if (length(best) == 1) best else if (length(near)) near[which.max(nchar(near))] else NA
    if (is.na(pick)) return(NULL)
    # the original spelling of the folder, as case can matter
    if (pick != ".") {
      tp <- gsub("\\\\", "/", plan$target_path[!is.na(plan$target_path)])
      under <- tp[startsWith(tolower(tp), paste0(pick, "/"))]
      if (!length(under)) return(NULL)
      pick <- substr(under[1], 1, nchar(pick))
    }
    to <- dirname(.repro_rel_from(if (pick == ".") "./x" else file.path(pick, "x"), run_dir))
    if (grepl("[/\\\\]$", vars[[v]])) to <- paste0(if (to == ".") "." else to, "/")
    out <- data.frame(value = vars[[v]], target = to)
    out$pieces <- list(pieces)
    out
  })
  rows <- Filter(Negate(is.null), rows)
  if (!length(rows)) return(empty)
  dplyr::bind_rows(rows)
}

# The folder here() should take as the project's top, for scripts that use it
# (#568). With no .Rproj, .here or the like in the deposit, here() starts in the
# folder the script runs from, so in 10.1111/1365-2664.14317 here("scripts",
# "functions.R") became /sandbox/scripts/scripts/functions.R. For each script,
# the folder at or above the one it runs from from which the most of its
# here("a", "b") paths exist; a `.here` file there makes here() start there. Not
# when a project marker already lies between. `code_text_list` and `run_dirs`
# (folders from the top) are per script; returns per script the folder, from the
# top in the deposit's spelling, or NA (no here() paths, or none found).
.repro_here_roots <- function(code_text_list, run_dirs, plan) {
  out <- stats::setNames(rep(NA_character_, length(code_text_list)), names(code_text_list))
  if (is.null(plan) || !nrow(plan) || !length(code_text_list)) return(out)
  tp <- gsub("\\\\", "/", plan$target_path[!is.na(plan$target_path)])
  files <- tolower(gsub("\\\\", "/", plan$target_path[!is.na(plan$target_path)]))
  folders <- character(0)
  d <- dirname(files)
  while (length(d <- setdiff(d[d != "."], folders))) {
    folders <- c(folders, d)
    d <- dirname(d)
  }
  known <- c(files, folders)
  markers <- unique(dirname(files[grepl("(^|/)(\\.here|[^/]*\\.rproj|description|\\.git)$", files)]))
  for (i in seq_along(code_text_list)) {
    txt <- paste(.utf8_lines(code_text_list[[i]]), collapse = "\n")
    calls <- regmatches(txt, gregexpr("\\bhere(?:::here)?\\s*\\(\\s*([\"'][^\"'\n]*[\"']\\s*,?\\s*)+\\)", txt, perl = TRUE))[[1]]
    if (!length(calls)) next
    paths <- vapply(calls, function(cl) {
      a <- regmatches(cl, gregexpr("[\"'][^\"'\n]*[\"']", cl))[[1]]
      paste(gsub("^[\"']|[\"']$", "", a), collapse = "/")
    }, character(1), USE.NAMES = FALSE)
    paths <- tolower(sub("/+$", "", gsub("/+", "/", paths)))
    chain <- tolower(run_dirs[[i]] %||% ".")
    while (!chain[length(chain)] %in% c(".", "")) chain <- c(chain, dirname(chain[length(chain)]))
    score <- vapply(chain, function(f) sum((if (f == ".") paths else file.path(f, paths)) %in% known), numeric(1))
    if (max(score) < 1) next
    pick <- chain[which(score == max(score))[1]]
    # a marker at or below the pick, on the way from the run folder, wins already
    between <- chain[seq_len(match(pick, chain))]
    if (any(between %in% markers)) next
    under <- tp[startsWith(tolower(tp), paste0(pick, "/"))]
    out[i] <- if (pick == ".") "." else if (length(under)) substr(under[1], 1, nchar(pick)) else NA_character_
  }
  out
}

# Apply .repro_folder_vars() to one script's rewrites: a row for each folder's
# new value, and the pasted pieces no longer rewritten on their own.
.repro_apply_folder_vars <- function(rw, fv) {
  if (is.null(fv) || !nrow(fv)) return(rw)
  if (nrow(rw)) rw <- rw[!rw$ref %in% unlist(fv$pieces), , drop = FALSE]
  add <- data.frame(ref = fv$value, basename = basename(gsub("\\\\", "/", fv$value)), matched = TRUE,
                    target = fv$target, ambiguous = FALSE, n_candidates = 1L, is_call = FALSE,
                    name_in_code = NA_character_, name_in_deposit = NA_character_, as_written = FALSE)
  dplyr::bind_rows(rw, add)
}

# A path from the top of the package, counted from folder `dir` of the package.
.repro_rel_from <- function(path, dir = ".") {
  if (dir %in% c(".", "")) return(path)
  a <- strsplit(gsub("\\\\", "/", dir), "/", fixed = TRUE)[[1]]
  b <- strsplit(gsub("\\\\", "/", path), "/", fixed = TRUE)[[1]]
  n <- min(length(a), length(b) - 1L)
  same <- if (n > 0) which(a[seq_len(n)] != b[seq_len(n)]) else integer(0)
  common <- if (length(same)) same[1] - 1L else n
  paste(c(rep("..", length(a) - common), b[seq.int(common + 1L, length(b))]), collapse = "/")
}

# The folder each script runs from, in the package layout. A path in code counts
# from the folder the authors ran it in, which is not recorded: their own folder,
# or the top of their project. Of 271 failures on a file in the listing, 96 would
# find it from the script's own folder and 59 from a folder above, also when the
# name is only known while the script runs, which no rewriting can follow (#568,
# item 4). So each script runs from the folder at or above its own from which the
# most of the files and folders it names exist. On a tie: the nearest folder with an
# .Rproj file, which is where RStudio runs a project, else the script's own folder.
# A script run with source() takes the folder of the script that sources it, since
# source() does not change the working folder.
# `places` are the scripts' paths from the top (.repro_script_places()); returns
# one folder per script from the top, "." being the top.
.repro_run_folders <- function(places, code_text_list, plan, order_tbl, lang = "R") {
  out <- rep(".", length(places))
  if (is.null(plan) || !nrow(plan)) return(out)
  files <- tolower(gsub("\\\\", "/", plan$target_path))
  folders <- character(0)
  d <- dirname(files)
  while (length(d <- setdiff(d[d != "."], folders))) {
    folders <- c(folders, d)
    d <- dirname(d)
  }
  rproj <- dirname(files[grepl("\\.rproj$", files)])
  known <- c(files, folders)
  for (i in seq_along(places)) {
    own <- dirname(tolower(gsub("\\\\", "/", places[i])))
    chain <- own
    while (!chain[length(chain)] %in% c(".", "")) chain <- c(chain, dirname(chain[length(chain)]))
    if (length(chain) == 1) next
    # every quoted text counts: only what exists in the package adds to the score,
    # and a name also appears outside read calls: os.listdir("raw/scan1")
    code <- iconv(code_text_list[[i]], "", "UTF-8", sub = "")
    code <- tryCatch(code_remove_comments(code, lang), error = function(e) code)
    quoted <- unlist(regmatches(code, gregexpr("[\"'][^\"'\n]+[\"']", code)))
    refs <- tolower(gsub("\\\\", "/", unique(gsub("^[\"']|[\"']$", "", quoted))))
    refs <- refs[!grepl("^([a-z]:/|/|~)|://|[%{}$*]", refs)]
    score <- vapply(chain, function(dir) {
      from <- vapply(refs, function(r) .repro_norm_rel(if (dir == ".") r else file.path(dir, r)),
                     character(1), USE.NAMES = FALSE)
      ok <- !is.na(from)
      # a file written into a folder of the project counts by its folder
      sum(from[ok] %in% known | (grepl("/", refs[ok], fixed = TRUE) & dirname(from[ok]) %in% folders))
    }, numeric(1), USE.NAMES = FALSE)
    best <- chain[score == max(score)]
    pick <- if (any(best %in% rproj)) best[best %in% rproj][1] else best[1]
    # the original spelling of the folder, as case can matter on the system that runs it
    depth <- length(chain) - match(pick, chain)
    orig <- dirname(gsub("\\\\", "/", places[i]))
    parts <- strsplit(orig, "/", fixed = TRUE)[[1]]
    out[i] <- if (pick == "." || depth <= 0) "." else paste(parts[seq_len(depth)], collapse = "/")
  }
  edges <- attr(order_tbl, "source_edges")
  if (!is.null(edges) && nrow(edges)) {
    fn <- names(code_text_list)
    for (round in 1:5) for (k in seq_len(nrow(edges))) {
      from <- match(edges$from[k], fn); to <- match(edges$to[k], fn)
      if (!is.na(from) && !is.na(to) && !edges$to[k] %in% edges$from) out[from] <- out[to]
    }
  }
  out
}

# Paths in a script that count from the script's own folder (package layout)
#
# Every script runs from the top of the package, but a script made in its own
# folder (or in an RStudio project, whose .Rproj folder is the working
# directory) writes `"Data/x_processed.csv"` into a `Data/` folder beside it.
# From the top that folder does not exist, and the write stopped with "Cannot
# open file for writing"; a later script reading the written file stopped too
# (#523, item 3). Reads of the package's own files are already rewritten
# (.repro_keep_working_refs()), but a file the code makes is not one of them.
#
# So every quoted relative path whose folder does not exist from the top, but
# does from the script's .Rproj folder or its own folder (in that order), is
# rewritten to its path from the top. A path given as a later piece of a
# joining call (`file.path(base, "fig7/x.csv")`, `os.path.join(...)`) is left
# alone: the code joins it with a folder of its own. `txt` is the script's
# lines, `script_path` its path from the top, and `plan` the package
# layout's plan (.repro_package_plan()); returns `txt` with those paths
# rewritten.
.repro_relocate_relative_paths <- function(txt, script_path, plan) {
  if (!length(txt) || is.null(plan) || !nrow(plan) ||
      is.na(script_path) || !nzchar(script_path)) return(txt)
  files <- gsub("\\\\", "/", plan$target_path)
  folders <- character(0)
  d <- dirname(files)
  while (length(d <- setdiff(d[d != "."], folders))) {
    folders <- c(folders, d)
    d <- dirname(d)
  }
  if (!length(folders)) return(txt)
  script_dir <- dirname(gsub("\\\\", "/", script_path))
  # the nearest folder at or above the script's that holds an .Rproj file
  rproj <- dirname(files[grepl("\\.Rproj$", files, ignore.case = TRUE)])
  up <- script_dir
  proj_dir <- NA_character_
  repeat {
    if (up %in% rproj) { proj_dir <- up; break }
    if (up == ".") break
    up <- dirname(up)
  }
  bases <- unique(stats::na.omit(c(proj_dir, script_dir)))
  bases <- bases[bases != "."]
  if (!length(bases)) return(txt)

  relocate <- function(s) {
    p <- gsub("\\\\", "/", s)
    if (grepl("^([A-Za-z]:/|/|~)|://", p) || !grepl("/", p, fixed = TRUE)) return(s)
    dir <- if (endsWith(p, "/")) sub("/+$", "", p) else dirname(p)
    top <- .repro_norm_rel(dir)
    # a path going up out of the top ("../Data/x.csv") is tried from the bases
    if (!is.na(top) && (top %in% c("", ".") || top %in% folders)) return(s)
    for (b in bases) {
      from_base <- .repro_norm_rel(file.path(b, dir))
      if (!is.na(from_base) && from_base %in% folders) {
        out <- .repro_norm_rel(file.path(b, p))
        return(if (endsWith(p, "/")) paste0(out, "/") else out)
      }
    }
    s
  }
  join_pat <- "(file\\.path|os\\.path\\.join|paste0?|Path|here)\\s*\\([^()]*,\\s*$"
  lit_pat <- "(['\"])([^'\"\n]*/[^'\"\n]*)\\1"
  for (i in grep(lit_pat, txt, perl = TRUE)) {
    m <- gregexpr(lit_pat, txt[i], perl = TRUE)
    hits <- regmatches(txt[i], m)[[1]]
    starts <- m[[1]]
    new <- vapply(seq_along(hits), function(k) {
      before <- substr(txt[i], 1, starts[k] - 1)
      if (grepl(join_pat, before, perl = TRUE)) return(hits[k])
      q <- substr(hits[k], 1, 1)
      inner <- substr(hits[k], 2, nchar(hits[k]) - 1)
      paste0(q, relocate(inner), q)
    }, character(1))
    regmatches(txt[i], m) <- list(new)
  }
  txt
}

# The first lines of a script's console output, which are searched for
# statistical results. read_r_output() reads about 700 lines a second, so a
# script that printed a growing table in a loop (millions of lines in its 300
# seconds) kept the check busy for most of an hour after the script had been
# stopped, and the job ran into its 60-minute limit with no report (#523,
# item 2: OSF sq8y6). `metacheck.repro_output_max_lines` (default 20000) sets
# the number of lines. `txt` is one string or a vector of lines; the result
# has the same form, with attribute `n_lines` (the number of lines before
# cutting) when it was cut.
.repro_output_head <- function(txt, max_lines = getOption("metacheck.repro_output_max_lines", 20000L)) {
  if (!length(txt) || anyNA(txt) || !is.finite(max_lines)) return(txt)
  if (length(txt) > 1) {
    if (length(txt) <= max_lines) return(txt)
    return(structure(txt[seq_len(max_lines)], n_lines = length(txt)))
  }
  nl <- gregexpr("\n", txt, fixed = TRUE)[[1]]
  if (nl[1] == -1 || length(nl) < max_lines) return(txt)
  structure(substr(txt, 1, nl[max_lines] - 1), n_lines = length(nl) + 1)
}

# The report's note on scripts whose output was searched only in part: `cut`
# is a named vector, script name -> its number of output lines. NULL when
# nothing was cut.
.repro_output_cut_note <- function(cut,
                                   max_lines = getOption("metacheck.repro_output_max_lines", 20000L)) {
  if (!length(cut)) return(NULL)
  sprintf("*Statistical results were searched for in the first %s lines of the output of %s, to keep the check within its time.*",
          format(max_lines, big.mark = ","),
          paste(sprintf("`%s` (%s lines)", names(cut), format(cut, big.mark = ",", trim = TRUE)),
                collapse = ", "))
}

# A relative path with "." and ".." resolved ("code/../raw_data/x.csv" ->
# "raw_data/x.csv"); NA when it goes above the top.
.repro_norm_rel <- function(path) {
  out <- character(0)
  for (part in strsplit(gsub("\\\\", "/", path), "/", fixed = TRUE)[[1]]) {
    if (part %in% c("", ".")) next
    if (part == "..") {
      if (!length(out)) return(NA_character_)
      out <- out[-length(out)]
    } else out <- c(out, part)
  }
  paste(out, collapse = "/")
}

#' Python files that other files of the run import
#'
#' A module such as `utils/data_handling.py` is not a script: it defines
#' functions for the scripts that import it, and run on its own it does
#' nothing, or fails on code meant to run only when imported. Like an R
#' script another script `source()`s, such a file is not run on its own. A
#' file with an `if __name__ == "__main__":` block is a script too, and still
#' runs.
#'
#' @param code_text_list named list of the Python files' code (names are
#'   file names)
#' @param paths the files' paths in the package (same order), for dotted
#'   imports (`from utils.data_handling import x`); `NULL` uses the names
#' @returns a named list: for each imported file, the files that import it
#' @keywords internal
.repro_py_imported <- function(code_text_list, paths = NULL) {
  fns <- names(code_text_list)
  if (!length(fns)) return(list())
  if (is.null(paths)) paths <- fns
  paths <- gsub("\\\\", "/", ifelse(is.na(paths) | !nzchar(paths), fns, paths))
  stem <- tools::file_path_sans_ext(basename(paths))
  # a package's __init__.py is imported by the package's (its folder's) name
  is_init <- stem == "__init__"
  stem[is_init] <- basename(dirname(paths[is_init]))
  # the module names each file imports: "a.b.c" gives a, b and c; "from . import
  # x" gives x
  imports <- lapply(code_text_list, function(txt) {
    txt <- txt[!grepl("^\\s*#", txt)]
    m1 <- regmatches(txt, regexec("^\\s*import\\s+([A-Za-z0-9_., ]+)", txt))
    m2 <- regmatches(txt, regexec("^\\s*from\\s+([A-Za-z0-9_.]+)\\s+import\\s+([A-Za-z0-9_, ()*]+)", txt))
    names1 <- unlist(lapply(m1, function(m) if (length(m) > 1) trimws(sub("\\s+as\\s+.*$", "", strsplit(m[[2]], ",")[[1]])) else NULL))
    names2 <- unlist(lapply(m2, function(m) {
      if (length(m) < 3) return(NULL)
      c(m[[2]], paste0(m[[2]], ".", trimws(sub("\\s+as\\s+.*$", "", strsplit(gsub("[()]", "", m[[3]]), ",")[[1]]))))
    }))
    found <- as.character(c(names1, names2))
    if (!length(found)) return(character(0))
    unique(unlist(strsplit(found, ".", fixed = TRUE)))
  })
  is_main <- vapply(code_text_list, function(txt) {
    any(grepl("^\\s*if\\s+__name__\\s*==\\s*['\"]__main__['\"]", txt))
  }, logical(1))
  # Of several files with one module name, a file imports the one beside it, or
  # the one in the folder it names (from a.utils import x).
  dirs <- dirname(paths)
  repeated <- stem %in% stem[duplicated(stem)]
  imports_it <- function(j, i) {
    j != i && stem[[i]] %in% imports[[j]] &&
      (!repeated[[i]] || dirs[[j]] == dirs[[i]] || basename(dirs[[i]]) %in% imports[[j]])
  }
  # Only a .py file can be imported. `import audio` named the package's module,
  # and a tutorial notebook called audio.ipynb was taken as imported, so it was
  # never run (#568).
  importable <- grepl("\\.py$", paths, ignore.case = TRUE)
  out <- list()
  for (i in seq_along(fns)) {
    if (is_main[[i]] || stem[[i]] %in% c("", ".") || !importable[[i]]) next
    by <- fns[vapply(seq_along(fns), imports_it, logical(1), i = i)]
    if (length(by)) out[[fns[[i]]]] <- unique(by)
  }
  out
}

# Apply each resolved (matched, non-ambiguous, real target) rewrite of
# repro_rewrite_paths() to a script's text as a literal replacement. Fixed
# (non-regex) to avoid metacharacter surprises in file paths. A plain
# reference replaces just the path; an `is_call` row (sprintf()/paste()-family
# — see repro_rewrite_paths()) replaces the ENTIRE call with a bare literal
# string, since `ref` there is the whole call text: substituting only a
# substring inside it would leave the call's other arguments (e.g. `wd,
# wd_data` after the format string) orphaned in a now-broken expression.
#
# A plain reference is replaced only where it stands in quotes ("x.csv",
# 'x.csv', or \"x.csv\" inside a notebook's JSON). Replaced wherever the text
# appeared, the reference x.csv also changed "old_x.csv" into
# "old_data/study-x_data.csv", a file that does not exist.
.repro_apply_rewrites <- function(txt, rw) {
  if (is.null(rw) || !nrow(rw)) return(txt)
  good <- rw$matched & !rw$ambiguous & !is.na(rw$target) & nzchar(rw$target)
  # a reference kept as written is left alone: for a call its target is
  # the call itself, which was put in quotes, so `sprintf("%s/x.xlsx", wd)`
  # became the unparseable `"sprintf("%s/x.xlsx", wd)"`
  # (Lakens/tue_datamanager_check#4, item 9)
  if ("as_written" %in% names(rw)) good <- good & !(rw$as_written %in% TRUE)
  is_call_col <- if ("is_call" %in% names(rw)) rw$is_call else rep(FALSE, nrow(rw))
  for (k in which(good)) {
    if (isTRUE(is_call_col[k])) {
      txt <- gsub(rw$ref[k], paste0('"', rw$target[k], '"'), txt, fixed = TRUE)
    } else {
      for (q in c('\\"', '"', "'"))
        txt <- gsub(paste0(q, rw$ref[k], q), paste0(q, rw$target[k], q), txt,
                    fixed = TRUE)
    }
  }
  txt
}

# Replace each setwd()/os.chdir() line of a script with a statement that does
# nothing, keeping the original visible in a comment that says why. Not a bare
# comment: when the call is the only statement of a block, a comment leaves
# the block empty -- Python stops with "IndentationError: expected an
# indented block" (a try:/if:/with: body), and in R `if (x)` followed by
# setwd() on the next line would take the NEXT statement as its body (#480).
#
# Matches the call at a statement start (allowing leading whitespace). Also
# base::setwd(), and chdir() after `from os import chdir` or under another
# module name (o.chdir()): only setwd( and os.chdir( were caught, so such a
# call ran and failed on the author's own path (tue_datamanager_check#4, item
# 5). Not mychdir( or print(chdir).
#
# `by` names who removed it, in the comment. Returns list(txt, n = the number
# replaced, paths = the quoted path each named).
.repro_neutralise_setwd <- function(txt, lang = "R", by = "reproducibility_check") {
  setwd_line_for <- list(
    R = "^(\\s*)((?:base::)?setwd\\s*\\(.*)$",
    Python = "^(\\s*)((?:[A-Za-z_][A-Za-z0-9_]*\\.)?chdir\\s*\\(.*)$"
  )
  setwd_arg_for <- list(
    R = "(?:base::)?setwd\\s*\\(\\s*['\"]([^'\"]+)['\"]",
    Python = "(?:[A-Za-z_][A-Za-z0-9_]*\\.)?chdir\\s*\\(\\s*['\"]([^'\"]+)['\"]"
  )
  setwd_line <- setwd_line_for[[lang]] %||% setwd_line_for[["R"]]
  setwd_arg  <- setwd_arg_for[[lang]] %||% setwd_arg_for[["R"]]

  is_setwd <- grepl(setwd_line, txt, perl = TRUE)
  n <- sum(is_setwd)
  paths <- character(0)
  if (n > 0) {
    hit_lines <- txt[is_setwd]
    m <- regmatches(hit_lines, regexpr(setwd_arg, hit_lines, perl = TRUE))
    paths <- sub(paste0(".*", setwd_arg, ".*"), "\\1", m[nzchar(m)], perl = TRUE)
    no_op <- if (identical(lang, "Python"))
      sprintf("\\1pass  # [%s removed os.chdir] \\2", by) else
      sprintf("\\1invisible(NULL)  # [%s removed setwd] \\2", by)
    txt[is_setwd] <- sub(setwd_line, no_op, txt[is_setwd], perl = TRUE)
  }
  list(txt = txt, n = n, paths = paths)
}

# groundhog.library(...) as the library() calls that load the same packages: the
# code for one such call. `vars` holds the literal vectors the script assigns
# (see .code_char_vector_vars()). A bare name that is not one of those may be a
# package or a variable computed in the script, which only the run can tell.
.repro_groundhog_as_library <- function(call, vars) {
  call <- sub("^groundhog::", "", call)
  arg <- .repro_split_args(sub("(?s)^[^(]*\\((.*)\\)$", "\\1", call, perl = TRUE))
  first <- if (length(arg) > 0) sub("^\\s*pkg\\s*=\\s*", "", trimws(arg[[1]]$text)) else ""
  if (grepl("^[.A-Za-z][.A-Za-z0-9_]*$", first) && !first %in% names(vars))
    return(sprintf(paste0('{ if (exists("%1$s") && is.character(%1$s)) ',
                          'for (.pkg in %1$s) library(.pkg, character.only = TRUE) ',
                          'else library("%1$s") }'), first))
  known <- vapply(names(vars), function(nm) paste0(nm, " <- ", deparse1(vars[[nm]])), character(1))
  pkgs <- unique(code_library_names(c(known, call), "R")$package)
  if (!length(pkgs)) return("invisible(NULL)")
  sprintf("{ %s }", paste(sprintf('library("%s")', pkgs), collapse = "; "))
}

# pacman::p_load(a, b) as library() calls: p_load installs what is missing, which
# cannot work during a run, and the module has installed these packages.
.repro_p_load_as_library <- function(call, vars = list()) {
  held <- regmatches(call, regexec("\\bchar\\s*=\\s*([.A-Za-z][.A-Za-z0-9_]*)", call, perl = TRUE))[[1]]
  if (length(held) > 1 && !held[2] %in% names(vars))
    return(sprintf("{ for (.pkg in %s) library(.pkg, character.only = TRUE) }", held[2]))
  known <- vapply(names(vars), function(nm) paste0(nm, " <- ", deparse1(vars[[nm]])), character(1))
  pkgs <- unique(tryCatch(code_library_names(c(known, call), "R")$package, error = function(e) character(0)))
  pkgs <- setdiff(pkgs, "pacman")
  if (!length(pkgs)) return("invisible(NULL)")
  sprintf("{ %s }", paste(sprintf('library("%s")', pkgs), collapse = "; "))
}

# What asks a person to choose a file, which nobody can during a check.
.repro_choose_pat <- "(?<![\\w.])(?:utils::|tcltk::|rstudioapi::)?(?:file\\.choose|choose\\.files|tk_choose\\.files|selectFile)\\s*\\((?:[^()]|\\([^()]*\\))*\\)"

# Fill in the file where a script asks a person to choose one, when the code says
# which: a comment on that line or just above it names a data file of the deposit,
# or the deposit holds only one data file (#568, item 18). Otherwise the call stays,
# and the script stops there with the outcome needs_a_person. Returns the text and
# what was filled in.
.repro_fill_file_choice <- function(code_text, data_files) {
  out <- list(txt = code_text, filled = character(0))
  if (!length(code_text) || !length(data_files)) return(out)
  txt <- code_text
  stems <- tools::file_path_sans_ext(data_files)
  for (i in which(grepl(.repro_choose_pat, txt, perl = TRUE) & !grepl("^\\s*#", txt))) {
    ctx <- txt[seq.int(max(1L, i - 2L), i)]
    ctx <- c(ctx[-length(ctx)][grepl("^\\s*#", ctx[-length(ctx)])],
             sub("^.*?#", "", txt[i][grepl("#", txt[i], fixed = TRUE)], perl = TRUE))
    named <- data_files[vapply(stems, function(s) nchar(s) >= 4 &&
      any(grepl(s, ctx, fixed = TRUE, useBytes = TRUE)), logical(1))]
    pick <- if (length(unique(named)) == 1) named[1] else if (!length(named) && length(data_files) == 1) data_files else NA_character_
    if (is.na(pick)) next
    new <- paste0('"', gsub('"', '\\\\"', pick), '"')
    m <- gregexpr(.repro_choose_pat, txt[i], perl = TRUE)
    n_call <- length(m[[1]])
    regmatches(txt[i], m) <- list(rep(new, n_call))
    out$filled <- c(out$filled, rep(pick, n_call))
  }
  out$txt <- txt
  out
}

# Whether an R script starts an interactive app at its top level (runApp() and the
# like): it would wait for a person until the time limit, with nothing to show for it.
.repro_starts_app <- function(code_text) {
  ex <- tryCatch(parse(text = code_text, keep.source = FALSE), error = function(e) NULL)
  if (is.null(ex)) return(FALSE)
  apps <- c("runApp", "shinyApp", "launch_shinystan", "runExample", "runGitHub", "runUrl",
            "shiny::runApp", "shiny::shinyApp", "shinystan::launch_shinystan")
  any(vapply(as.list(ex), function(e) is.call(e) && deparse(e[[1]])[1] %in% apps, logical(1)))
}

# Whether a Python script serves others: it waits for a connection until the time
# limit, with nothing to show for it (#568, item 22). `tcpserver.py` took 50 minutes.
# A program with a window waits for a person the same way: in
# 10.1111/1365-2435.14173 a Qt application waited 38 minutes, and 13 more
# scripts then had no time left (#568).
.repro_py_starts_server <- function(code_text) {
  nc <- code_text[!grepl("^\\s*#", code_text)]
  any(grepl(paste0("\\.serve_forever\\s*\\(|\\bsocketserver\\.|\\bapp\\.run\\s*\\(|\\buvicorn\\.run\\s*\\(|",
                   "\\.listen\\s*\\(\\s*\\d*\\s*\\).*$|\\bwhile True:.*\\.accept\\(|",
                   "\\bQ(?:Gui)?Application\\s*\\(|\\.mainloop\\s*\\(\\s*\\)|\\.MainLoop\\s*\\(\\s*\\)|",
                   "\\bpygame\\.display\\.set_mode\\s*\\("), nc, perl = TRUE, useBytes = TRUE))
}

# Whether a run's error shows that the code needs the internet, which is off during a
# Docker run, since this runs code nobody has checked (#568, item 21). A package
# that keeps data it downloads once, as tidyhydat keeps its HYDAT database, says
# so when the data is not there yet ("Run download_hydat()").
.repro_needs_internet_error <- function(txt) {
  grepl(paste0("cannot open URL|[Cc]ould not resolve host|Couldn't resolve host|Temporary failure in name resolution|",
               "Name or service not known|NameResolutionError|Max retries exceeded|urlopen error|",
               "ConnectionError|LocalEntryNotFound|Could not reach|Failed to connect|Network is unreachable|",
               "getaddrinfo failed|Timeout was reached|no internet|internet connection|",
               "Run download_hydat\\(\\)"),
        txt %||% "", perl = TRUE)
}

# The report's sentence that scripts could use the internet, with its limits (see
# .repro_download_limits()), so a reader knows that what they downloaded may differ
# from what the authors had.
.repro_internet_text <- function(net) {
  lim <- function(b, what) if (is.finite(b)) sprintf("%s MB %s", .repro_mb(b), what) else NULL
  limits <- c(lim(net$max_file, "per file"), lim(net$max_total, "for all scripts together"))
  paste0("The scripts could use the internet",
         if (length(limits)) paste0(", up to ", paste(limits, collapse = " and ")), ", ",
         "so what they downloaded may differ from what the authors had.")
}

.repro_needs_internet_text <- function(files, internet = FALSE) {
  n <- length(files)
  sprintf(" **%d script%s need%s the internet**, %s: %s. Keeping what they download in the deposit solves this.",
          n, plural(n), plural(n, "s", ""),
          if (internet) "and could not reach what they asked for" else "which is off during the run",
          paste(files, collapse = ", "))
}

.repro_download_stop_text <- function(files) {
  n <- length(files)
  sprintf(" **%d script%s %s stopped for downloading more than our limits allow**: %s. Keeping what they download in the deposit solves this.",
          n, plural(n), plural(n, "was", "were"), paste(files, collapse = ", "))
}

# Why a script did not run through, as one word, so reasons need not be recovered
# from messages afterwards (#568, item 24). NA for what is not a failure of the script.
.repro_failure_reason <- function(outcome, msg) {
  msg <- ifelse(is.na(msg), "", msg)
  has <- function(pat) grepl(pat, msg, perl = TRUE)
  out <- rep(NA_character_, length(outcome))
  out[outcome %in% c("dependency_unavailable")] <- "package"
  out[outcome %in% c("not_parsed", "python2")] <- "syntax"
  out[outcome %in% c("timed_out", "skipped_time_limit")] <- "time"
  out[outcome %in% c("needs_a_person")] <- "interactive"
  out[outcome %in% c("needs_internet")] <- "internet"
  # a download that was too large, or still arriving when the time ran out
  out[outcome %in% c("download_too_large")] <- "download"
  out[outcome %in% c("timed_out") & has("while a download was still arriving")] <- "download"
  out[outcome %in% c("skipped_missing_inputs")] <- "file"
  err <- outcome == "errored"
  set <- function(pat, label) out[err & is.na(out) & has(pat)] <<- label
  # provided by Snakemake when it runs the script, so only there
  set("object 'snakemake' not found|name 'snakemake' is not defined", "environment")
  set("there is no package called|ModuleNotFoundError|No module named|ImportError|package or namespace|Error: package ", "package")
  set("cannot open the connection|cannot open file|No such file|FileNotFoundError|does not exist|Cannot find the file|Could not find a root|cannot open compressed", "file")
  set("object '[^']*' not found|could not find function|NameError|is not defined", "undefined")
  set("SyntaxError|IndentationError", "syntax")
  set("shared object|ImproperlyConfigured|cannot cache function|cannot open display|X11|can only print from a screen device", "environment")
  # R itself crashed, in a package's compiled code: rgeos, installed from the
  # CRAN Archive, crashes on every geometry against the GEOS of the image (#568)
  set("caught segfault|Segmentation fault|core dumped|caught illegal operation", "environment")
  out[err & is.na(out) & !grepl("\\S", msg)] <- "no_message"
  out[err & is.na(out)] <- "other"
  out
}

# The packages the code loads that are on neither CRAN nor Bioconductor and that
# the install found nowhere else, since the code does not say where they come from
# (#389). character(0) when there are none.
.repro_source_unknown <- function(install_results) {
  if (is.null(install_results) || !nrow(install_results) || !"category" %in% names(install_results))
    return(character(0))
  unique(install_results$package[!install_results$installed &
                                   install_results$category %in% "source_unknown"])
}

.repro_source_unknown_text <- function(pkgs) {
  if (!length(pkgs)) return(NULL)
  n <- length(pkgs)
  sprintf(paste0("**%d package%s the code loads %s not on CRAN or Bioconductor, and the code ",
                 "does not say where %s from**: %s. We also looked in the CRAN Archive, on ",
                 "R-universe and in a list of known places. Saying where to install %s from, ",
                 "such as with `remotes::install_github()` or in the README, solves this."),
          n, plural(n), plural(n, "is", "are"), plural(n, "it comes", "they come"),
          paste(sprintf("`%s`", pkgs), collapse = ", "), plural(n, "it", "them"))
}

# Every reason .repro_failure_reason() gives, so each paper's row of summary_table
# has the same columns, also for a reason none of its scripts had.
.repro_reason_levels <- c("file", "package", "undefined", "syntax", "environment",
                          "internet", "download", "interactive", "time",
                          "no_message", "other")

# Per-paper counts for summary_table (#568, item 24): the scripts with a result
# of their own, the files of software that were not run (not analyses, counted
# apart), and for each reason how many scripts did not run through for it. A
# sourced or imported file, a model or function file and a Docker failure on our
# side are not scripts with a result of their own, nor is a script that stopped on
# a file we did not download (counted apart as well). NA when nothing was run.
.repro_run_counts <- function(run_results) {
  cols <- c("repro_scripts_n", "repro_not_analysis_n", "repro_not_downloaded_n",
            paste0("repro_fail_", .repro_reason_levels))
  if (is.null(run_results) || !nrow(run_results) || !"outcome" %in% names(run_results))
    return(as.data.frame(as.list(stats::setNames(rep(NA_integer_, length(cols)), cols))))
  outcome <- run_results$outcome
  reason <- if ("reason" %in% names(run_results)) run_results$reason else
    .repro_failure_reason(outcome, paste(run_results$error, run_results$stderr))
  counted <- !outcome %in% c("run_by_source", "run_by_import", "jags_model", "not_r_content",
                             "defines_functions", "not_analysis", "docker_failed",
                             "not_downloaded") &
    !is.na(outcome)
  fails <- vapply(.repro_reason_levels, function(r) sum(counted & reason %in% r), integer(1))
  as.data.frame(as.list(stats::setNames(
    c(sum(counted), sum(outcome %in% "not_analysis"), sum(outcome %in% "not_downloaded"), fails),
    cols)))
}

# One line per repository when a paper links several, so a reader can see when the
# failures lie in one of them, such as software by others that the paper only uses.
# Nothing is left out or labelled: a repository whose owner is no author is often the
# authors' own, and skipping those would hide 575 real failures (#568, item 8).
.repro_by_repository <- function(table) {
  if (is.null(table) || !nrow(table) || !all(c("repo_name", "outcome") %in% names(table))) return(NULL)
  counted <- !table$outcome %in% c("run_by_source", "run_by_import", "jags_model", "not_r_content",
                                   "defines_functions", "not_analysis", "not_downloaded") &
    !is.na(table$outcome)
  repos <- table$repo_name[counted]
  if (length(unique(repos[!is.na(repos)])) < 2) return(NULL)
  ok <- table$outcome[counted] == "ran_ok"
  c("#### Results by repository", scroll_table(data.frame(
    Repository = sort(unique(repos[!is.na(repos)])),
    Scripts = as.integer(table(repos)[sort(unique(repos[!is.na(repos)]))]),
    `Ran to the end` = vapply(sort(unique(repos[!is.na(repos)])), function(r) sum(ok & repos %in% r), integer(1), USE.NAMES = FALSE),
    check.names = FALSE), maxrows = 10))
}

# What the deposit does not hold, for the scripts that did not run through, with
# scripts apart from data: authors can act on this (#568, item 23). NULL when none.
.repro_absent_report <- function(absent) {
  absent <- absent[absent$status == "absent" & is.na(absent$similar_to), , drop = FALSE]
  if (!nrow(absent)) return(NULL)
  is_code <- grepl("\\.(r|rmd|qmd|py|ipynb|stan|jags|bugs)$", absent$basename, ignore.case = TRUE)
  list_some <- function(x) paste0(paste(sprintf("`%s`", utils::head(x, 25)), collapse = ", "),
                                  if (length(x) > 25) sprintf(" and %d more", length(x) - 25) else "")
  c("#### Files the deposit does not hold",
    if (any(!is_code)) sprintf(
      "The scripts that did not run through read %d data file%s that %s not in the deposit, and no script of the deposit writes: %s.",
      sum(!is_code), plural(sum(!is_code)), plural(sum(!is_code), "is", "are"), list_some(absent$basename[!is_code])),
    if (any(is_code)) sprintf(
      "They also run %d script%s that %s not in the deposit: %s.",
      sum(is_code), plural(sum(is_code)), plural(sum(is_code), "is", "are"), list_some(absent$basename[is_code])))
}

# Whether a run's error shows that the script waited for a person.
.repro_needs_person_error <- function(txt) {
  grepl(paste0("file choice cancelled|invalid 'description' argument|menu\\(\\) cannot be used non-interactively|",
               "could not find function \"(choose\\.files|choose\\.dir)\"|",
               "RStudio not running|tcltk|EOFError: EOF when reading a line|",
               "no display name and no \\$DISPLAY|couldn't connect to display|",
               # read.table("clipboard"): what a person copied (10.1111/1365-2656.14113)
               "for the clipboard must be|clipboard cannot be opened"),
        txt %||% "", perl = TRUE)
}

# Replace the calls of groundhog, checkpoint and renv in a script, since they
# would install packages themselves, which needs the network that is off during
# a Docker run, and put them outside the library the run uses. renv::restore()
# stopped a run for this reason (#523, item 4). The packages are installed
# before the run (see .repro_recorded_plan()), so:
# groundhog.library() becomes library(); renv::*(), checkpoint() and the calls that
# load or set up these packages become a statement that does nothing and names
# what was removed; require(renv) becomes TRUE, so that
# `if (!require(renv)) install.packages("renv")` installs nothing.
#
# The whole text, not line by line: a call can run over several lines. Text that
# cannot be searched is returned as it is. Returns list(txt, n = the number
# replaced).
.repro_neutralise_env_calls <- function(txt) {
  if (!length(txt)) return(list(txt = txt, n = 0L))
  joined <- paste(txt, collapse = "\n")
  args_pat <- "(?:[^()]++|\\((?:[^()]++|\\([^()]*+\\))*+\\))*+"
  n <- 0L
  swap <- function(joined, pat, repl) {
    m <- gregexpr(pat, joined, perl = TRUE)
    calls <- regmatches(joined, m)[[1]]
    if (!length(calls)) return(joined)
    n <<- n + length(calls)
    regmatches(joined, m) <- list(vapply(calls, repl, character(1), USE.NAMES = FALSE))
    joined
  }
  removed <- function(name) function(call)
    sprintf('invisible("reproducibility_check removed %s")', name)
  out <- tryCatch({
    vars <- .code_char_vector_vars(txt)
    joined <- swap(joined, paste0("(?<![\\w.])(?:groundhog::)?groundhog\\.library\\s*\\(", args_pat, "\\)"),
                   function(call) .repro_groundhog_as_library(call, vars))
    joined <- swap(joined, paste0("(?<![\\w.:])(?:groundhog::)?(?:set\\.groundhog\\.folder|meta\\.groundhog)\\s*\\(",
                                  args_pat, "\\)"), removed("a groundhog call"))
    joined <- swap(joined, paste0("(?<![\\w.:])renv::\\w+(?:\\$[\\w.]+)*\\s*\\(", args_pat, "\\)"),
                   removed("an renv call"))
    # A function of the script's own can be called checkpoint(), so only when
    # the script names the package.
    if (grepl("checkpoint::|(?:library|require)\\s*\\(\\s*['\"]?checkpoint", joined, perl = TRUE))
      joined <- swap(joined, paste0("(?<![\\w.:])(?:checkpoint::)?(?:create_|use_)?checkpoint\\s*\\(",
                                    args_pat, "\\)"), removed("a checkpoint call"))
    joined <- swap(joined, "(?<![\\w.])source\\s*\\([^)\n]*renv[^)\n]*activate[^)\n]*\\)+",
                   removed("renv's activate.R"))
    # install.packages() and the like stop on the read-only system and with the
    # network off (82 scripts in 25 papers, #568, item 15), and the module installs
    # what the scripts load before the run. Without quotes, since such a call also
    # stands inside messages: "please run install.packages('x')".
    installers <- paste0("(?:(?:utils|remotes|devtools|BiocManager|pak|renv)::)?",
                         "(?:install\\.packages|update\\.packages|install_github|install_version|install_url|",
                         "install_cran|install_bitbucket|install_local|install_git|biocLite|pkg_install|pak)")
    joined <- swap(joined, paste0("(?<![\\w.:$])", installers, "\\s*\\(", args_pat, "\\)"),
                   function(call) "invisible(NULL)")
    joined <- swap(joined, paste0("(?<![\\w.:$])BiocManager::install\\s*\\(", args_pat, "\\)"),
                   function(call) "invisible(NULL)")
    # as a function name: sapply(pkgs, install.packages)
    joined <- swap(joined, "(?<![\\w.:$\"'])(?:utils::)?install\\.packages(?![\\w.(\"'])", function(call) "invisible")
    joined <- swap(joined, paste0("(?<![\\w.:])(?:pacman::)?p_load\\s*\\(", args_pat, "\\)"),
                   function(call) .repro_p_load_as_library(call, vars))
    tools <- "['\"]?(?:groundhog|checkpoint|renv|pacman|remotes|devtools|BiocManager|pak)['\"]?"
    joined <- swap(joined, paste0("(?<![\\w.:])(?:base::)?(?:requireNamespace|require)\\s*\\(\\s*", tools,
                                  "\\s*(?:,[^()]*)?\\)"), function(call) "TRUE")
    joined <- swap(joined, paste0("(?<![\\w.:])(?:base::)?library\\s*\\(\\s*", tools,
                                  "\\s*(?:,[^()]*)?\\)"), removed("the loading of a package manager"))
    list(txt = strsplit(joined, "\n", fixed = TRUE)[[1]], n = n)
  }, error = function(e) list(txt = txt, n = 0L))
  out
}

# The same for a project's .Rprofile, which R runs when it starts: it usually
# holds source("renv/activate.R"), which downloads renv and changes the library
# the run uses. The rest of the file stays, since it can set options the analysis
# relies on. Returns the number of calls replaced.
.repro_neutralise_rprofile <- function(root) {
  # in every folder, since a script can run from its project's folder
  paths <- list.files(root, pattern = "^\\.Rprofile$", recursive = TRUE, all.files = TRUE, full.names = TRUE)
  n <- 0L
  for (path in paths) {
    txt <- tryCatch(readLines(path, warn = FALSE), error = function(e) NULL)
    if (is.null(txt)) next
    res <- .repro_neutralise_env_calls(txt)
    if (res$n > 0) tryCatch(writeLines(res$txt, path), error = function(e) NULL)
    n <- n + res$n
  }
  n
}

# Lines that load other scripts first, in the same session, each from its own run
# folder, since its paths count from there. `run_after` lists by script the scripts
# to load, and `run_tbl` is the repro_write_scripts() result. Errors stop the script,
# as they would have in the authors' session.
.repro_run_after_lines <- function(run_after, run_tbl, root) {
  if (!length(run_after)) return(NULL)
  from_top <- function(p) {
    p <- gsub("\\\\", "/", normalizePath(p, mustWork = FALSE))
    top <- gsub("\\\\", "/", normalizePath(root, mustWork = FALSE))
    if (identical(p, top)) "." else sub(paste0(top, "/"), "", p, fixed = TRUE)
  }
  q <- function(x) paste0('"', gsub('"', '\\\\"', x), '"')
  stats::setNames(lapply(names(run_after), function(user) {
    u <- match(user, run_tbl$file_name)
    vapply(run_after[[user]], function(def) {
      d <- match(def, run_tbl$file_name)
      def_dir <- from_top(run_tbl$run_dir[d])
      to_dir <- .repro_rel_from(if (def_dir == ".") "./x" else file.path(def_dir, "x"), from_top(run_tbl$run_dir[u]))
      to_dir <- if (dirname(to_dir) == ".") "." else dirname(to_dir)
      script <- .repro_rel_from(from_top(run_tbl$script_path[d]), def_dir)
      sprintf('local({ .old <- setwd(%s); on.exit(setwd(.old)); source(%s, local = globalenv()) })  # [reproducibility_check: load the script that defines what this one uses]',
              q(to_dir), q(script))
    }, character(1), USE.NAMES = FALSE)
  }), names(run_after))
}

# Defaults the code was written for that current R lacks, from a script's error
# (#568): before R 4.0, read.csv(), read.table() and data.frame() turned text into
# factors ("'relevel' only for (unordered) factors", "invalid 'labels'"), and
# code written on Windows reads data files in Latin-1 ("invalid multibyte
# string"). options(stringsAsFactors = TRUE) no longer has any effect in R 4.5.
# Returns, for each script that stopped on such an error, "factors", "latin1" or
# both; an empty list when none did.
.repro_old_defaults <- function(run_results) {
  if (is.null(run_results) || !nrow(run_results)) return(list())
  msg <- paste(run_results$error, run_results$stderr)
  stopped <- run_results$outcome %in% "errored"
  factors <- stopped & grepl(paste0("only for \\(unordered\\) factors|invalid 'labels'; length|",
                                    "is not a factor|level sets of factors are different|",
                                    "argument is not a factor|must be a factor"), msg, perl = TRUE)
  latin1 <- stopped & grepl("invalid multibyte string|is invalid in this locale|utf8towcs", msg, perl = TRUE)
  out <- lapply(which(factors | latin1), function(i) c(if (factors[i]) "factors", if (latin1[i]) "latin1"))
  stats::setNames(out, run_results$file_name[factors | latin1])
}

# The lines put at the top of a script for .repro_old_defaults()'s defaults. The
# default is changed in R's own functions, not only in the script: before R 4.0 it
# held inside packages too, and in 10.1111/1365-2435.13388 reshape2's dcast() gave
# the dates as text again, so the script still stopped. An argument the code
# gives itself still counts.
.repro_old_default_lines <- function(kinds) {
  c(if ("factors" %in% kinds) paste0(
      'local({ for (w in list(c("base", "data.frame"), c("base", "as.data.frame.character"), ',
      'c("base", "as.data.frame.list"), c("base", "rbind.data.frame"), c("base", "cbind.data.frame"), ',
      'c("utils", "read.table"))) for (env in list(asNamespace(w[1]), as.environment(paste0("package:", w[1])))) { ',
      'if (!exists(w[2], envir = env, inherits = FALSE)) next; f <- get(w[2], envir = env); ',
      'if (!"stringsAsFactors" %in% names(formals(f))) next; formals(f)$stringsAsFactors <- TRUE; ',
      'unlockBinding(w[2], env); assign(w[2], f, envir = env); lockBinding(w[2], env) } })',
      "  # [reproducibility_check: text read as factors, as before R 4.0]"),
    if ("latin1" %in% kinds)
      'options(encoding = "latin1")  # [reproducibility_check: data files written on Windows, in Latin-1]')
}

# The report's sentence on the scripts run again with the old defaults, and which
# of them then ran to the end.
.repro_old_defaults_text <- function(used, ran_ok = character(0)) {
  if (!length(used)) return(NULL)
  n <- length(used)
  kinds <- unique(unlist(used))
  what <- c(factors = "text read as factors, as R did before version 4.0",
            latin1 = "data files read as Latin-1, as written on Windows")[kinds]
  ok <- intersect(names(used), ran_ok)
  fix <- c(factors = "`stringsAsFactors = TRUE`", latin1 = "`fileEncoding = \"latin1\"`")[kinds]
  paste0(sprintf(" **%d script%s ran again with a default R had when the code was written** (%s), because %s, or a script that writes a file %s, stopped on a default that has changed since: %s.",
                 n, plural(n), paste(what, collapse = "; "), plural(n, "it", "they"),
                 plural(n, "it reads", "they read"), paste(names(used), collapse = ", ")),
         if (length(ok)) sprintf(" %s then ran to the end: %s.",
                                 plural(length(ok), "It", "These"), paste(ok, collapse = ", ")) else "",
         sprintf(" Setting %s in the code itself solves this.", paste(fix, collapse = " or ")))
}

# Make the folders of the relative paths in write calls, counted from where the
# script runs. Never above the sandbox. `wr` is a .repro_redirect_writes() result.
# A folder held in a variable that a write call uses counts too, given the
# script's lines `txt`: `dirF <- "../Figures/"` with `ggsave(paste0(dirF,
# "x.png"))` stopped on "Cannot find directory" (10.1111/1365-2435.14353; #568).
.repro_make_write_folders <- function(wr, root, run_dir = ".", txt = NULL) {
  if (!nrow(wr)) return(invisible(NULL))
  quoted <- unlist(regmatches(wr$call_text, gregexpr("[\"'][^\"'\n]*[/\\\\][^\"'\n]*\\.[A-Za-z0-9]{1,8}[\"']", wr$call_text)))
  in_vars <- character(0)
  if (length(txt)) {
    vars <- .repro_simple_string_vars(tryCatch(code_remove_comments(.utf8_lines(txt), "R"),
                                               error = function(e) .utf8_lines(txt)))
    vars <- vars[grepl("[/\\\\]$", vars)]
    used <- vapply(names(vars), function(v)
      any(grepl(paste0("(?<![.\\w])", gsub("\\.", "\\\\.", v), "(?![.\\w])"), wr$call_text, perl = TRUE)),
      logical(1))
    in_vars <- paste0(sub("[/\\\\]+$", "", vars[used]), "/x")
    # a path given as `file =` and the like, to a function not on the list of
    # writers: "tables/duration_LMM_revision.doc" (10.1111/1365-2435.14173)
    named <- unlist(regmatches(txt, gregexpr(
      "\\b(?:file|out|outfile|filename|output)\\s*=\\s*[\"'][^\"'\n]*[/\\\\][^\"'\n]*\\.[A-Za-z0-9]{1,8}[\"']",
      txt, perl = TRUE)))
    in_vars <- c(in_vars, sub("^[^\"']*[\"']", "", sub("[\"']$", "", named)))
  }
  for (q in unique(c(gsub("^[\"']|[\"']$", "", quoted), in_vars))) {
    q <- gsub("\\\\", "/", q)
    if (grepl("^([A-Za-z]:/|/|~)|://", q)) next
    p <- .repro_norm_rel(if (run_dir == ".") q else file.path(run_dir, q))
    if (is.na(p) || dirname(p) == ".") next
    dir.create(file.path(root, .portable_path(dirname(p))), recursive = TRUE, showWarnings = FALSE)
  }
  invisible(NULL)
}

#' Write path-rewritten scripts into the materialised layout
#'
#' Each script's referenced data paths are rewritten to their Psych-DS target
#' (via the per-file `repro_rewrite_paths()` result) so that, run from `root`,
#' the reads/writes resolve against the materialised tree. The rewritten script
#' text is written to `root/<script target_path>` (or, when the plan has no
#' target for the script itself, to `root/<basename>` at the top level so it is
#' still runnable). A reference left ambiguous or unmatched is written unchanged
#' — the static report already flagged it.
#'
#' @param code_text_list named list of per-file code text (names are file_names)
#' @param rewrite_list named list of `repro_rewrite_paths()` results, same order
#' @param plan the `psychds_check` table (to find each script's own target_path)
#' @param root the materialised layout root
#'
#' @details
#' A `setwd()` call in analysis code is bad practice: it hardcodes an assumption
#' about where the code runs (often an absolute path on the author's own machine,
#' e.g. `setwd("D:/Dropbox/...")`), and it overrides the working directory the
#' runner sets to the materialised layout — so the script's relative reads then
#' resolve against a directory that does not exist here, and the reproduction
#' fails for a reason that has nothing to do with the analysis. To run the code
#' we therefore **comment out every `setwd()` call** (recorded per file in
#' `setwd_removed` and flagged in the report). The transform is in the same
#' spirit as the path rewriting: it makes the author's script runnable in its new
#' location without changing the analysis. A script that only reproduces after a
#' `setwd()` is removed is reported as "reproducible after ignoring a `setwd()`
#' that should not have been in the code and needs to be fixed".
#'
#' @param inject_libs optional named character vector/list (names are
#'   `file_name`s already present in `code_text_list`) — one or more package
#'   names to inject as `library(<pkg>)` calls at the very top of that
#'   script's text before it is written. Used by the module's corrective
#'   re-run for a `could not find function "X"` error (see
#'   [.repro_missing_fn_pkgs()] for which packages) —
#'   reusing this function's existing text-rewrite plumbing rather than a
#'   separate rewrite path. `NULL` (default) injects nothing, unchanged from
#'   before this parameter existed. R only: there is no Python corrective
#'   re-run (the module's missing-dependency/undefined-name recovery is
#'   namespace-export-based — see `.repro_find_export_pkg()` — with no
#'   equivalent built for Python's `import` statements), so this is ignored
#'   for a file whose `lang` is `"Python"`.
#' @param lang the language each element of `code_text_list` is in: a single
#'   string applied to every file (default `"R"`), or a named character
#'   vector keyed by file_name for a mixed-language set (same shape as
#'   [repro_file_io()]'s own `lang`). Selects the write-redirect function
#'   names ([.repro_redirect_writes()]) and the working-directory override
#'   that is replaced with a statement that does nothing (`setwd()` becomes
#'   `invisible(NULL)` in R, `os.chdir()` becomes `pass` in Python).
#'   Python's font-substitution analogue is deliberately NOT built: an
#'   unavailable font is a matplotlib WARNING with a silent DejaVu Sans
#'   fallback, not the hard "invalid font type" failure
#'   `family =`/`base_family =` causes in R's graphics devices (the actual
#'   reason this transform exists for R) — rewriting Python's many
#'   `rcParams`/`plt.rc()`/`fontdict=` spellings would be solving a failure
#'   mode Python's own ecosystem does not have here.
#'
#' @param script_paths optional character vector, one per element of
#'   `code_text_list`: the path (relative to `root`) to write that script to,
#'   as the package layout ([.repro_package_plan()]) needs. `NULL` (default),
#'   or an `NA` element, places the script by its basename in `plan`, as
#'   before.
#' @param writes_in_place if `TRUE` (the package layout), a write stays where
#'   the code puts it, so a later script finds the file where it expects it;
#'   only a write to an absolute path or out of the package is redirected to
#'   `output/`. A quoted relative path whose folder exists from the script's
#'   own folder (or its `.Rproj` folder) but not from the top is first
#'   rewritten to its path from the top, for reads and writes alike.
#'   `FALSE` (default) redirects every write, as before.
#' @param run_dirs optional folder each script runs from, from `root`
#'   ([.repro_run_folders()]), which becomes its `run_dir`. `NULL` (default) runs
#'   every script from `root`.
#' @param prepend optional named list, by file name: lines to put at the top of a
#'   script, such as loading another script first ([.repro_run_after_lines()]).
#' @param replace_env_calls if `TRUE`, the calls to groundhog, checkpoint and renv
#'   in R code are replaced, since packages are installed before the run, and such
#'   a call would need the network (see `.repro_neutralise_env_calls()`).
#'   `FALSE` (default) leaves them.
#' @returns a data frame with `file_name`, `script_path` (absolute path written),
#'   `run_dir` (the working directory a run should use — always `root`),
#'   `setwd_removed` (count of `setwd()`/`os.chdir()` calls replaced with a
#'   statement that does nothing),
#'   `env_calls_replaced` (count of groundhog, checkpoint and renv calls replaced),
#'   `slashes_fixed` (the quoted Windows paths given `/`, as they were, joined by
#'   "; "; see `.repro_slash_paths()`),
#'   `setwd_paths` (comma-joined paths those calls named, for the warning),
#'   `family_replaced` (count of `family =`/`base_family =` named-font
#'   arguments replaced with `"sans"`, so a plot call does not fail with
#'   "invalid font type" for a font not registered on the machine running the
#'   sandbox), `family_detail` (semicolon-joined `<old> -> "sans"` text for
#'   each replacement), and `library_injected` (the package name(s) injected
#'   via `inject_libs` for that file, comma-joined, or `NA` when none was).
#' @export
repro_write_scripts <- function(code_text_list, rewrite_list, plan, root,
                                inject_libs = NULL, lang = "R",
                                script_paths = NULL, writes_in_place = FALSE,
                                replace_env_calls = FALSE, run_dirs = NULL, prepend = NULL) {
  fnames <- names(code_text_list)
  plan_base <- if (!is.null(plan) && "file_name" %in% names(plan))
    tolower(basename(plan$file_name)) else character(0)
  lang_for <- if (length(lang) == 1) stats::setNames(rep(lang, length(fnames)), fnames)
    else lang[fnames]

  script_target <- function(fn, i) {
    # The package layout gives each script its own path, so two scripts with
    # the same name (two __init__.py) do not land on one place.
    own <- if (!is.null(script_paths)) script_paths[[i]] else NA_character_
    if (!is.na(own) && nzchar(own)) return(own)
    # A script's own place in the tree: match its basename in the plan; else put
    # it at the tree root under its basename so it is still runnable.
    if (length(plan_base)) {
      m <- which(plan_base == tolower(basename(fn)) &
                   !is.na(plan$target_path) & nzchar(plan$target_path %||% ""))
      if (length(m) >= 1) return(plan$target_path[m[[1]]])
    }
    basename(fn)
  }

  rows <- lapply(seq_along(code_text_list), function(i) {
    fn  <- fnames[[i]]
    txt <- code_text_list[[i]]
    rw  <- rewrite_list[[fn]]
    # One line per script, like the run step's own: without it a slow rewrite
    # of a very large script could not be told apart from a hang (#481).
    message("[repro]   rewriting '", fn, "' (", sum(nchar(txt, type = "bytes")), " bytes) ...")
    fn_lang <- lang_for[[i]] %||% "R"
    txt <- .repro_apply_rewrites(txt, rw)
    slashed <- .repro_slash_paths(txt)
    txt <- slashed$txt

    # Redirect WRITE calls into the sandbox's flat output/ folder (see
    # .repro_redirect_writes()'s own roxygen for why: an unrewritten write
    # commonly targets a relative directory that does not exist in the
    # materialised layout — a sandboxing artefact, not a real reproducibility
    # finding — confirmed against a real corpus paper's script writing to
    # "../Data/Cleaned/..."). Applied on the WHOLE joined text (a write call
    # can wrap across lines, e.g. piped into write.csv() on its own line), then
    # re-split back into per-line form so the rest of this function's
    # line-vector operations (setwd detection below) still work unchanged.
    #
    # The output folder is given RELATIVE to the sandbox root ("output/"), not
    # as file.path(root, "output"): the root is a host path, and in the Docker
    # sandbox the same folder is mounted at /sandbox, so an absolute host path
    # does not exist inside the container and every script that wrote a file
    # failed with "cannot open the connection" (#502). Every run uses the root
    # as its working directory (run_dir, in both sandboxes; setwd()/os.chdir()
    # are neutralised below), which is also what the rewritten READ paths
    # (plan target paths, relative to the root) already rely on.
    # In the package layout, a path counted from the script's own (or its
    # .Rproj) folder is rewritten to its path from the top first, so such a
    # write stays in the package and a later script reads it there (#523)
    run_dir <- if (is.null(run_dirs)) "." else run_dirs[[i]]
    if (isTRUE(writes_in_place) && run_dir == ".")
      txt <- .repro_relocate_relative_paths(txt, script_target(fn, i), plan)
    wr <- all_wr <- .repro_redirect_writes(txt, lang = fn_lang)
    # In the package layout a write stays where the code puts it, so a later
    # script reads the file where it expects it; only a write to an absolute
    # path (the author's computer) or out of the package goes to output/.
    if (isTRUE(writes_in_place) && nrow(wr) > 0) {
      outside <- grepl("[\"']([A-Za-z]:[/\\\\]|/|~|\\.\\.[/\\\\])", wr$call_text, perl = TRUE)
      wr <- wr[outside, , drop = FALSE]
    }
    # A write stays in place, so a folder the deposit lacks must exist: an empty
    # output folder is often not deposited, and the write stopped (#568, item 20).
    if (isTRUE(writes_in_place)) .repro_make_write_folders(all_wr, root, run_dir, txt)
    if (nrow(wr) > 0) {
      joined_txt <- paste(txt, collapse = "\n")
      for (k in seq_len(nrow(wr))) {
        # output/ is at the root, so from a folder below it the path goes up first
        ups <- if (run_dir == ".") "" else strrep("../", length(strsplit(run_dir, "/", fixed = TRUE)[[1]]))
        repl <- gsub("{{REPRO_OUTPUT}}", paste0(ups, "output/"), wr$replacement[k], fixed = TRUE)
        joined_txt <- sub(wr$call_text[k], repl, joined_txt, fixed = TRUE)
      }
      txt <- strsplit(joined_txt, "\n", fixed = TRUE)[[1]]
    }

    # Replace setwd() lines so they cannot override the sandbox wd. Record
    # how many, and the path each named, for the report's warning.
    sw <- .repro_neutralise_setwd(txt, fn_lang)
    txt <- sw$txt
    setwd_n <- sw$n
    setwd_paths <- sw$paths

    env_n <- 0L
    if (isTRUE(replace_env_calls) && identical(fn_lang, "R")) {
      env <- .repro_neutralise_env_calls(txt)
      txt <- env$txt
      env_n <- env$n
    }

    # Replace a named font family (family = "Times New Roman", base_family =
    # "Arial", ...) with grid's own universal fallback "sans" — a specific
    # named font is very commonly NOT registered as usable on the machine
    # actually running the sandboxed subprocess (no font-registration setup
    # of its own, unlike the author's own interactive session where the OS
    # font is simply present), so the plot call fails with "invalid font
    # type" for a reason that has nothing to do with the analysis itself —
    # confirmed against a real corpus paper's theme_few(base_family = "Times
    # New Roman") failing exactly this way in the callr subprocess. "sans" is
    # grid's own documented generic family name, resolved by every graphics
    # device without needing the named font installed at all. Deliberately
    # narrow: only a `family =` / `base_family =` NAMED argument is touched
    # (not any quoted string that merely looks like a font name), the same
    # "do not guess beyond what is clearly the thing in question" restraint
    # the rest of this function's rewrites already follow. R only — see this
    # function's own `lang` docs for why Python has no equivalent here.
    family_pat <- "\\b(base_family|family)\\s*=\\s*(['\"])[^'\"]*\\2"
    joined_for_family <- paste(txt, collapse = "\n")
    family_m <- gregexpr(family_pat, joined_for_family, perl = TRUE)
    family_all <- if (identical(fn_lang, "R")) regmatches(joined_for_family, family_m)[[1]] else character(0)
    # A model's family is no font: glmmTMB(..., family = "nbinom2") became
    # family = "sans" and stopped (#568)
    is_font <- .repro_is_font_family(family_all)
    family_matches <- family_all[is_font]
    family_n <- length(family_matches)
    if (family_n > 0) {
      regmatches(joined_for_family, family_m) <- list(ifelse(
        is_font, sub(family_pat, "\\1 = \"sans\"", family_all, perl = TRUE), family_all))
      txt <- strsplit(joined_for_family, "\n", fixed = TRUE)[[1]]
    }

    # Inject a missing library() call at the very top of the script, when the
    # caller resolved an undefined-function error to exactly one package (see
    # this function's own `inject_libs` param docs above). Prepended, not
    # inserted at the error's own line: the injected package must be attached
    # before ANY of the script's code runs, since a `library()` earlier in
    # the same file could otherwise already have been attempted and failed
    # first (also, the corrective rerun only has a file-level signal — "this
    # script needs package P" — not a line number to target). R only — see
    # this function's own `inject_libs` docs above.
    # One or more packages per file (a script that relies on the packages
    # another script of the project attaches gets all of them).
    injected_pkg <- if (identical(fn_lang, "R") && !is.null(inject_libs) &&
                        fn %in% names(inject_libs))
      inject_libs[[fn]] else character(0)
    injected_pkg <- injected_pkg[!is.na(injected_pkg) & nzchar(injected_pkg)]
    if (length(injected_pkg))
      txt <- c(sprintf("library(%s)  # [reproducibility_check injected]", injected_pkg), txt)
    if (fn %in% names(prepend)) txt <- c(prepend[[fn]], txt)

    tgt  <- script_target(fn, i)
    dest <- file.path(root, .portable_path(tgt))
    dir.create(dirname(dest), recursive = TRUE, showWarnings = FALSE)
    # A script that cannot be written (a path too long for the system) is run and
    # reported as failed, and the paper's other scripts still run.
    written <- tryCatch({
      suppressWarnings(writeLines(txt, dest))
      TRUE
    }, error = function(e) FALSE)
    if (!written) message("[repro]   could not write '", fn, "' to ", dest)
    data.frame(file_name = fn, script_path = dest,
               run_dir = if (run_dir == ".") root else file.path(root, .portable_path(run_dir)),
               setwd_removed = setwd_n,
               env_calls_replaced = env_n,
               slashes_fixed = paste(slashed$paths, collapse = "; "),
               setwd_paths = paste(setwd_paths, collapse = ", "),
               family_replaced = family_n,
               family_detail = paste(
                 sprintf("%s -> \"sans\"", family_matches), collapse = "; "),
               library_injected = if (length(injected_pkg))
                 paste(injected_pkg, collapse = ", ") else NA_character_)
  })
  dplyr::bind_rows(rows)
}

# Stan code in the syntax Stan removed in version 2.33 (2023), in the newer form
# that means the same (#568): `#` comments, `<-`, `increment_log_prob()`,
# `get_lp()`, `fabs()`, `_log`/`_cdf_log`/`_ccdf_log` functions, and arrays
# declared after the name (`int y[N];`, `real[] x`). stanc() stopped on them in
# 12 scripts of 10.1111/1365-2656.12673 and 10.1111/1365-2656.13534. Returns the
# lines and the kinds of change made.
.repro_upgrade_stan <- function(txt) {
  if (!length(txt)) return(list(txt = txt, changes = character(0)))
  orig <- txt
  changes <- character(0)
  sub_all <- function(pat, rep, what) {
    new <- gsub(pat, rep, txt, perl = TRUE)
    if (!identical(new, txt)) changes <<- c(changes, what)
    txt <<- new
  }
  sub_all("(?<![/\"])#(?!include)", "//", "# comments")
  sub_all("\\s<-\\s", " = ", "<- assignments")
  sub_all("\\bincrement_log_prob\\s*\\((.*)\\)\\s*;", "target += \\1;", "increment_log_prob()")
  sub_all("\\bget_lp\\s*\\(\\s*\\)", "target()", "get_lp()")
  sub_all("\\bfabs\\s*\\(", "abs(", "fabs()")
  sub_all("_ccdf_log\\s*\\(", "_lccdf(", "_ccdf_log()")
  sub_all("_cdf_log\\s*\\(", "_lcdf(", "_cdf_log()")
  # name_log(y, a, b) -> name_lpdf(y | a, b), or _lpmf for a discrete distribution
  discrete <- c("bernoulli", "bernoulli_logit", "binomial", "binomial_logit", "beta_binomial",
                "hypergeometric", "categorical", "categorical_logit", "ordered_logistic",
                "ordered_probit", "neg_binomial", "neg_binomial_2", "neg_binomial_2_log",
                "poisson", "poisson_log", "multinomial", "discrete_range")
  continuous <- c("normal", "normal_id_glm", "std_normal", "student_t", "cauchy", "double_exponential",
                  "logistic", "gumbel", "skew_normal", "lognormal", "chi_square", "inv_chi_square",
                  "scaled_inv_chi_square", "exponential", "gamma", "inv_gamma", "weibull", "frechet",
                  "rayleigh", "pareto", "pareto_type_2", "beta", "beta_proportion", "von_mises",
                  "uniform", "multi_normal", "multi_normal_cholesky", "multi_normal_prec",
                  "multi_student_t", "dirichlet", "lkj_corr", "lkj_corr_cholesky", "wishart",
                  "inv_wishart", "exp_mod_normal", "skew_double_exponential")
  dist_pat <- paste0("\\b(", paste(c(discrete, continuous), collapse = "|"), ")_log\\s*\\(")
  for (i in grep(dist_pat, txt, perl = TRUE)) {
    line <- txt[i]
    repeat {
      m <- regexpr(dist_pat, line, perl = TRUE)
      if (m == -1) break
      name <- sub("_log\\s*\\($", "", regmatches(line, m))
      open <- m + attr(m, "match.length") - 1L
      chars <- strsplit(line, "", fixed = TRUE)[[1]]
      depth <- 0L; comma <- NA_integer_
      for (k in seq.int(open, length(chars))) {
        ch <- chars[k]
        if (ch %in% c("(", "[", "{")) depth <- depth + 1L
        else if (ch %in% c(")", "]", "}")) { depth <- depth - 1L; if (depth == 0L) break }
        else if (ch == "," && depth == 1L) { comma <- k; break }
      }
      suffix <- if (name %in% discrete) "_lpmf(" else "_lpdf("
      rest <- substring(line, open + 1L)
      if (!is.na(comma)) rest <- paste0(substr(line, open + 1L, comma - 1L), " |", substring(line, comma + 1L))
      line <- paste0(substr(line, 1L, m - 1L), name, suffix, rest)
    }
    if (!identical(line, txt[i])) { changes <- c(changes, "_log() functions"); txt[i] <- line }
  }
  # arrays: `int<lower=0> y[N];` -> `array[N] int<lower=0> y;`, and in function
  # arguments `real[] x` -> `array[] real x`
  types <- paste0("(?:int|real|complex|vector|row_vector|matrix|simplex|unit_vector|ordered|",
                  "positive_ordered|cov_matrix|corr_matrix|cholesky_factor_cov|cholesky_factor_corr)")
  dims <- "((?:[^\\[\\]]|\\[[^\\[\\]]*\\])+)"
  sub_all(paste0("^(\\s*)((?:data\\s+)?", types, "\\s*(?:<[^<>;]*>)?\\s*(?:\\[[^\\[\\]]*\\])?)\\s+",
                 "([A-Za-z_][A-Za-z0-9_]*)\\s*\\[", dims, "\\]"),
          "\\1array[\\4] \\2 \\3", "array declarations")
  sub_all(paste0("\\b((?:data\\s+)?", types, ")\\s*\\[(\\s*(?:,\\s*)*)\\]\\s+([A-Za-z_][A-Za-z0-9_]*)"),
          "array[\\2] \\1 \\3", "array arguments")
  list(txt = txt, changes = unique(changes))
}

# Give every Stan file in the sandbox the newer syntax (.repro_upgrade_stan());
# returns the files changed and how, as modifications rows (NULL when none).
.repro_upgrade_stan_files <- function(root) {
  files <- list.files(root, pattern = "\\.stan$", recursive = TRUE, full.names = TRUE, ignore.case = TRUE)
  rows <- lapply(files, function(f) {
    txt <- tryCatch(readLines(f, warn = FALSE), error = function(e) NULL)
    if (is.null(txt)) return(NULL)
    up <- .repro_upgrade_stan(txt)
    if (!length(up$changes)) return(NULL)
    writeLines(up$txt, f)
    data.frame(file_name = sub(paste0("^", gsub("([.\\\\+*?^$()|{}\\[\\]])", "\\\\\\1", normalizePath(root, "/", mustWork = FALSE)), "/"), "",
                               normalizePath(f, "/", mustWork = FALSE)),
               change_type = "stan_syntax",
               detail = paste("Stan from before 2.33:", paste(up$changes, collapse = ", ")))
  })
  rows <- Filter(Negate(is.null), rows)
  if (!length(rows)) NULL else dplyr::bind_rows(rows)
}

# Which `family = "..."` / `base_family = "..."` arguments name a font, not a
# model's family. `base_family` is always a font. A model family is a lowercase
# name (binomial, nbinom2, zero_inflated_poisson) or a function in stats (Gamma);
# a font has capitals or spaces (Times New Roman, Arial), or is one of R's
# generic fonts (serif, mono). `args` are the matched argument texts.
.repro_is_font_family <- function(args) {
  if (!length(args)) return(logical(0))
  value <- sub("^.*=\\s*['\"]([^'\"]*)['\"]$", "\\1", args, perl = TRUE)
  base <- grepl("^base_family", args)
  generic <- tolower(value) %in% c("serif", "sans", "mono", "symbol", "times", "helvetica",
                                   "arial", "courier", "palatino", "calibri", "verdana", "georgia")
  stats_fn <- vapply(value, function(v) nzchar(v) &&
                       exists(v, envir = asNamespace("stats"), mode = "function", inherits = FALSE),
                     logical(1), USE.NAMES = FALSE)
  model <- !generic & (stats_fn | grepl("^[a-z][a-z0-9_.]*$", value))
  base | !model
}

# Windows paths in quoted file names, "Data\\MatrixNoct.txt", get "/": on Linux
# the backslash is part of the name, so write.table() made one file named
# "Data\MatrixNoct.txt" beside the script and the scripts reading
# "Data/MatrixNoct.txt" stopped (#568). Only a quoted text that ends in a file
# extension and has no sign of a regular expression is changed, so
# list.files(pattern = "\\.csv$") stays as it is. Returns the lines and the
# paths changed, as they were.
.repro_slash_paths <- function(txt) {
  if (!length(txt)) return(list(txt = txt, paths = character(0)))
  pat <- "([\"'])((?:[^\"'\\\\\n]|\\\\\\\\)*\\\\\\\\(?:[^\"'\\\\\n]|\\\\\\\\)*\\.[A-Za-z0-9]{1,8})\\1"
  changed <- character(0)
  txt <- vapply(txt, function(line) {
    if (!grepl("\\\\\\\\", line)) return(line)
    m <- gregexpr(pat, line, perl = TRUE)[[1]]
    if (m[1] == -1) return(line)
    hits <- regmatches(line, list(m))[[1]]
    inner <- substr(hits, 2, nchar(hits) - 1)
    # a regular expression: an escaped dot, or one of ^ $ * + ? | ( ) [ ] { }
    path_like <- !grepl("\\\\\\\\\\.", inner, perl = TRUE) &
      !grepl("[\\^$*+?|()\\[\\]{}]", inner, perl = TRUE)
    if (!any(path_like)) return(line)
    new <- hits
    new[path_like] <- paste0(substr(hits[path_like], 1, 1),
                             gsub("\\\\\\\\", "/", inner[path_like]),
                             substr(hits[path_like], 1, 1))
    changed <<- c(changed, hits[path_like])
    regmatches(line, list(m)) <- list(new)
    line
  }, character(1), USE.NAMES = FALSE)
  list(txt = txt, paths = changed)
}

# The modifications rows for the Windows paths repro_write_scripts() changed, one
# per path; NULL when there were none.
.repro_slash_rows <- function(run_tbl) {
  if (is.null(run_tbl) || !nrow(run_tbl) || !"slashes_fixed" %in% names(run_tbl)) return(NULL)
  hit <- !is.na(run_tbl$slashes_fixed) & nzchar(run_tbl$slashes_fixed)
  if (!any(hit)) return(NULL)
  dplyr::bind_rows(lapply(which(hit), function(i) {
    p <- strsplit(run_tbl$slashes_fixed[i], "; ", fixed = TRUE)[[1]]
    data.frame(file_name = run_tbl$file_name[i], change_type = "windows_path",
               detail = sprintf("%s -> %s", p, gsub("\\\\\\\\", "/", p)))
  }))
}

# The report's part on Windows paths: a sentence and a table; NULL when none.
.repro_slash_report <- function(run_tbl) {
  rows <- .repro_slash_rows(run_tbl)
  if (is.null(rows)) return(NULL)
  n <- length(unique(rows$file_name))
  c(sprintf(paste0("**%d script%s use%s Windows paths**, with `\\` between folders, which ",
                   "works only on Windows. To run the code we put `/` instead, which works on ",
                   "every system, Windows included."),
            n, plural(n), plural(n, "s", "")),
    scroll_table(data.frame(File = rows$file_name, `Path in the code` = sub(" -> .*", "", rows$detail),
                            check.names = FALSE), maxrows = 10))
}

# The highest of several version strings; NA when none can be read as a version.
.repro_highest_version <- function(versions) {
  versions <- versions[!is.na(versions) & nzchar(versions)]
  valid <- vapply(versions, function(v)
    !is.na(package_version(v, strict = FALSE)), logical(1), USE.NAMES = FALSE)
  versions <- versions[valid]
  if (!length(versions)) return(NA_character_)
  highest <- versions[1]
  for (v in versions[-1]) if (package_version(v) > package_version(highest)) highest <- v
  highest
}

# Whether two version strings are the same version: "1.1-21" and "1.1.21" are.
.repro_same_version <- function(a, b) {
  if (length(a) != 1 || length(b) != 1 || is.na(a) || is.na(b)) return(FALSE)
  isTRUE(tryCatch(package_version(a) == package_version(b), error = function(e) FALSE))
}

# The version of a package in the given libraries, NA when it is not in them.
# packageDescription() with lib.loc reads the file, and so is not misled by a
# package already loaded in this R session (see repro_install_deps()).
.repro_installed_version <- function(pkg, lib_paths) {
  v <- suppressWarnings(tryCatch(
    utils::packageDescription(pkg, lib.loc = lib_paths, fields = "Version"),
    error = function(e) NA_character_))
  if (length(v) != 1 || is.na(v)) NA_character_ else as.character(v)
}

# The version of each package on a dated copy of CRAN, from Posit's package
# manager (the copies start in late 2017). NA for a package that was not on CRAN
# at that date. All NA, with attribute `failed` TRUE, when the copy could not be
# read: a date before the first copy, or no network.
.repro_date_versions <- function(pkgs, date) {
  url <- paste0("https://packagemanager.posit.co/cran/", date)
  ap <- tryCatch(suppressWarnings(utils::available.packages(
    repos = url, type = "source", filters = "duplicates")), error = function(e) NULL)
  failed <- is.null(ap) || nrow(ap) == 0
  out <- stats::setNames(rep(NA_character_, length(pkgs)), pkgs)
  if (!failed) out[] <- unname(ap[match(pkgs, rownames(ap)), "Version"])
  attr(out, "failed") <- failed
  out
}

#' The recorded version of each package the scripts load
#'
#' A repository can record the package versions its analysis used: an
#' `renv.lock` file, a `sessionInfo()` or `session_info()` record, or a date in a
#' `groundhog.library()` or `checkpoint()` call (see
#' [.code_version_pin_check()]). This matches those records to the packages the
#' scripts load, because only those are installed: a listed package no script
#' loads is left out, and a loaded package that is not listed keeps `NA` and is
#' installed at its current version.
#'
#' `renv.lock` comes first, then the `sessionInfo()` and `session_info()` records,
#' then a date, the latest one when there are several. Within one kind of record
#' the highest version wins when files disagree, since a run has one library. A
#' package whose record says it came from GitHub is installed from that commit.
#'
#' `groundhog`, `checkpoint` and `renv` are left out: the scripts' calls to them
#' are rewritten (see [repro_write_scripts()]), since they would install packages
#' themselves, which needs the network that is off during a run.
#'
#' Packages that come with R (`Matrix`, `MASS`, ...) keep R's own version, since
#' they are built for that R version, and are listed in `kept_with_r`.
#'
#' @param deps the frame from [repro_dependencies()]: `package`, `source`, `ref`,
#'   `base`
#' @param version_pin the result of [.code_version_pin_check()]
#' @param use_dates `FALSE` not to look up the versions for a date, which needs
#'   the network; such a package then keeps `NA`
#'
#' @returns `NULL` when nothing records a version of a package the scripts load,
#'   else a list: `deps` (the non-base rows of `deps`, with `version` and
#'   `recorded_from`), `kept_with_r` (data frame `package`, `version`), `dates`
#'   (the date used, or `character(0)`) and `date_failed` (`TRUE` when the date's
#'   versions could not be looked up)
#' @keywords internal
.repro_recorded_plan <- function(deps, version_pin, use_dates = TRUE) {
  if (is.null(deps) || !nrow(deps) || is.null(version_pin)) return(NULL)
  tools <- c("groundhog", "checkpoint", "renv")
  loaded <- setdiff(deps$package, tools)
  records <- list(version_pin$renv_packages, version_pin$session_packages)

  version <- stats::setNames(rep(NA_character_, length(loaded)), loaded)
  from <- version
  origin <- version
  remote <- version
  for (rec in records) {
    if (is.null(rec) || !nrow(rec)) next
    if (!"remote" %in% names(rec)) rec$remote <- NA_character_
    for (pkg in intersect(loaded[is.na(version)], rec$package)) {
      rows <- rec[rec$package == pkg, , drop = FALSE]
      v <- .repro_highest_version(rows$version)
      if (is.na(v)) next
      row <- rows[match(v, rows$version), , drop = FALSE]
      version[[pkg]] <- v
      from[[pkg]] <- row$file_name
      origin[[pkg]] <- row$source
      remote[[pkg]] <- row$remote
    }
  }

  dates <- sort(unique(version_pin$pin_dates$date %||% character(0)), decreasing = TRUE)
  date_failed <- FALSE
  if (length(dates) > 0 && isTRUE(use_dates)) {
    open <- names(version)[is.na(version) & !loaded %in% deps$package[deps$base]]
    if (length(open) > 0) {
      dv <- .repro_date_versions(open, dates[1])
      date_failed <- isTRUE(attr(dv, "failed"))
      if (date_failed)
        warning("reproducibility_check(): the package versions for the date ", dates[1],
                " could not be looked up, because copies of CRAN start in late 2017 or ",
                "the network could not be reached. Those packages are not installed at ",
                "recorded versions.", call. = FALSE)
      got <- open[!is.na(dv)]
      version[got] <- dv[got]
      from[got] <- paste("date", dates[1])
    }
  }
  if (all(is.na(version))) return(NULL)

  is_base <- deps$package %in% deps$package[deps$base]
  rec_base <- deps$package[is_base & deps$package %in% names(version)[!is.na(version)]]
  out <- deps[!deps$base & !deps$package %in% tools, , drop = FALSE]
  if (!any(!is.na(version[out$package]))) return(NULL)
  out$version <- unname(version[out$package])
  out$recorded_from <- unname(from[out$package])

  # Where the record says the package came from decides how it is installed,
  # unless the code itself names a GitHub or URL source.
  rec_src <- unname(origin[out$package])
  rec_remote <- unname(remote[out$package])
  is_gh <- !is.na(rec_src) & rec_src == "GitHub" & !is.na(rec_remote)
  out$source[is_gh] <- "github"
  out$ref[is_gh] <- rec_remote[is_gh]
  is_bioc <- !is.na(rec_src) & rec_src == "Bioconductor" & out$source == "cran"
  out$source[is_bioc] <- "bioc"

  list(deps = out,
       kept_with_r = data.frame(package = rec_base, version = unname(version[rec_base])),
       dates = if (length(dates) > 0) dates[1] else character(0),
       date_failed = date_failed)
}

# Which files or dates record the versions, as words for the warning and report.
.repro_recorded_sources <- function(version_pin) {
  dates <- version_pin$pin_dates$date %||% character(0)
  has_session <- nrow(version_pin$session_packages %||% data.frame()) > 0
  has_renv <- nrow(version_pin$renv_packages %||% data.frame()) > 0
  paste(c(if (has_renv) "an `renv.lock` file",
          if (has_session) "a `sessionInfo()` record",
          if (length(dates) > 0) sprintf("the date %s in a `groundhog` or `checkpoint` call",
                                         max(dates))),
        collapse = " and ")
}

# What installing recorded versions does not do, as sentences: the limitation
# stated in both the warning before a run and the report after it.
.repro_recorded_limits <- function(recorded, version_pin, sandbox, use_declared = FALSE) {
  declared <- version_pin$r_versions %||% character(0)
  r_kept <- !(sandbox == "docker" && isTRUE(use_declared) && length(declared) > 0)
  bioc <- recorded$deps$package[recorded$deps$source == "bioc" & !is.na(recorded$deps$version)]
  kept <- recorded$kept_with_r$package
  c(if (r_kept) sprintf(
      paste0("The R version is not changed: this run uses %s%s, so an old package may ",
             "not install, or may give other results than it gave the authors."),
      if (sandbox == "docker") "the R version of the Docker image"
      else paste("R", as.character(getRversion())),
      if (length(declared) > 0) sprintf(", while the repository records R %s", declared[1]) else ""),
    "Packages that the installed packages depend on are not at their recorded versions, which can also change results.",
    if (length(kept) > 0) sprintf("Packages that come with R keep R's own version: %s.",
                                  paste(kept, collapse = ", ")),
    if (length(bioc) > 0) sprintf(
      "Bioconductor versions follow the R version, so these get their current version: %s.",
      paste(bioc, collapse = ", ")))
}

# The warning before a run that installs recorded versions.
.repro_recorded_warning <- function(recorded, version_pin, sandbox, use_declared = FALSE) {
  n <- nrow(recorded$deps)
  n_listed <- sum(!is.na(recorded$deps$version))
  paste(c(
    sprintf(paste0("reproducibility_check(): the repository records package versions in ",
                   "%s, so %d of the %d package%s the scripts load will be installed at ",
                   "those versions. This can take a long time, because old versions are built from source."),
            .repro_recorded_sources(version_pin), n_listed, n, plural(n)),
    if (n_listed < n) sprintf("%d loaded package%s not listed and get%s the current version.",
                              n - n_listed, plural(n - n_listed, " is", "s are"),
                              plural(n - n_listed, "s", "")),
    .repro_recorded_limits(recorded, version_pin, sandbox, use_declared)), collapse = " ")
}

# The packages of a run with recorded versions, by what happened to them: TRUE
# per package in `exact` (at the recorded version), `replaced` (installed at
# another version) and `failed` (not installed), for the recorded packages.
.repro_recorded_counts <- function(recorded, install_results) {
  listed <- recorded$deps[!is.na(recorded$deps$version), , drop = FALSE]
  ir <- install_results[match(listed$package, install_results$package), , drop = FALSE]
  installed <- !is.na(ir$installed) & ir$installed
  same <- mapply(.repro_same_version, as.character(ir$version_installed %||% rep(NA, nrow(ir))),
                 listed$version, USE.NAMES = FALSE)
  same <- as.logical(same)
  list(listed = listed, results = ir, exact = installed & same,
       replaced = installed & !same, failed = !installed)
}

# One sentence for the summary and the checklist. NULL without recorded versions.
.repro_recorded_summary <- function(recorded, install_results) {
  if (is.null(recorded) || is.null(install_results) || !nrow(install_results)) return(NULL)
  counts <- .repro_recorded_counts(recorded, install_results)
  n <- nrow(counts$listed)
  sprintf(paste0("We installed %d of the %d package%s the scripts load and the repository ",
                 "lists at the recorded version, but did not change the R version, so ",
                 "this is not an exact copy of the authors' setup."),
          sum(counts$exact), n, plural(n))
}

# The section of the report on the recorded versions a run used. NULL without them.
.repro_recorded_report <- function(recorded, install_results, version_pin, sandbox,
                                   use_declared = FALSE) {
  if (is.null(recorded) || is.null(install_results) || !nrow(install_results)) return(NULL)
  counts <- .repro_recorded_counts(recorded, install_results)
  n <- nrow(counts$listed)
  not_listed <- recorded$deps$package[is.na(recorded$deps$version)]
  odd <- !counts$exact
  ir <- counts$results
  why <- ifelse(nzchar(ir$version_note %||% rep("", nrow(ir))), ir$version_note,
                ifelse(is.na(ir$message), "", ir$message))
  tb <- if (any(odd)) data.frame(
    Package = counts$listed$package[odd],
    Recorded = counts$listed$version[odd],
    Installed = ifelse(is.na(ir$version_installed[odd]), "not installed", ir$version_installed[odd]),
    Why = why[odd])
  c("#### Recorded package versions",
    paste(c(
      sprintf(paste0("The repository records package versions in %s. Of the %d package%s the ",
                     "scripts load and the repository lists, %d %s installed at the recorded ",
                     "version, %d got another version, and %d could not be installed."),
              .repro_recorded_sources(version_pin), n, plural(n), sum(counts$exact),
              plural(sum(counts$exact), "was", "were"), sum(counts$replaced), sum(counts$failed)),
      if (any(counts$replaced)) "A script that uses a package with another version is not an exact reproduction.",
      if (length(not_listed) > 0) sprintf(
        "The scripts also load %s, which %s not listed and got the current version.",
        paste(not_listed, collapse = ", "), plural(length(not_listed), "is", "are")),
      if (isTRUE(recorded$date_failed)) sprintf(
        "The versions for the date %s could not be looked up.", recorded$dates),
      .repro_recorded_limits(recorded, version_pin, sandbox, use_declared)), collapse = " "),
    if (!is.null(tb)) scroll_table(tb, maxrows = 15))
}

# How many packages the scripts load have a recorded version, without looking
# up the versions for a date, which needs the network.
.repro_recorded_n <- function(deps, version_pin) {
  if (is.null(deps) || !nrow(deps)) return(0L)
  recorded <- .repro_recorded_plan(deps, version_pin, use_dates = FALSE)
  dates <- version_pin$pin_dates$date %||% character(0)
  if (length(dates) > 0) sum(!deps$base & !deps$package %in% c("groundhog", "checkpoint", "renv"))
  else if (!is.null(recorded)) sum(!is.na(recorded$deps$version)) else 0L
}

# The recommendation after a run in which scripts failed, when the repository
# records versions that the run did not use. NULL otherwise. A shared server
# (see api_keys_use()) does not offer the setting, so there it says so.
.repro_recorded_advice <- function(deps, version_pin, run_results) {
  failed <- c("errored", "timed_out", "dependency_unavailable")
  if (is.null(run_results) || !any(run_results$outcome %in% failed)) return(NULL)
  if (.repro_recorded_n(deps, version_pin) == 0) return(NULL)
  paste(c(
    "Metacheck detected version information that can be used to install specific versions of software for the reproducibility run.",
    "We recommend retrying the reproducibility check with `recorded_versions = TRUE`.",
    "This will install the required R packages, if still available.",
    if (isTRUE(getOption("metacheck.keys.shared")))
      "As this takes a long time, this setting is not implemented on this shared server."),
    collapse = " ")
}

# What a run with execute = TRUE would install, for the report of a run without.
.repro_recorded_preview <- function(deps, version_pin) {
  n <- .repro_recorded_n(deps, version_pin)
  if (n == 0) return(NULL)
  sprintf(paste0("The repository records package versions in %s. A run with `execute = TRUE` ",
                 "would install %d package%s the scripts load at those versions, which can ",
                 "take a long time. It would not change the R version."),
          .repro_recorded_sources(version_pin), n, plural(n))
}

#' Install a paper's declared dependencies into a throwaway library
#'
#' Installs each non-base dependency into `lib_dir` (a temp library, never the
#' user's), so a run does not mutate the user's installed packages. CRAN packages
#' come from `install.packages()`; Bioconductor packages (`source == "bioc"`,
#' see [repro_dependencies()]/[.repro_bioc_packages()]) from
#' `BiocManager::install()`, installing `BiocManager` itself first if absent;
#' GitHub/URL sources from the ref the code named
#' (`remotes::install_github()` / `install.packages(url)`), honouring a pinned
#' `@ref` when present. Every install's outcome is recorded; installing runs
#' package build/configure scripts, so this is part of the gated execute phase.
#'
#' A row with a `version` is installed at that version with
#' `remotes::install_version()`, unless the libraries already hold it; packages
#' it depends on that are not there yet get their current version. When that
#' version cannot be installed, the current version is installed instead and
#' `version_note` says why. Such a row always goes into the throwaway `lib_dir`,
#' so an old version can never replace one in your own library. A Bioconductor
#' package always gets its current version, since Bioconductor versions follow
#' the R version.
#'
#' @param install_deps the module's `install_deps` frame (`package`, `source`,
#'   `ref`, and optionally `version`, from [.repro_recorded_plan()]), base
#'   packages already excluded
#' @param lib_dir the throwaway library path (created if absent); ignored for
#'   CRAN-source packages when `cran_to_main_lib = TRUE` (see below)
#' @param deadline the time ([Sys.time()] plus the time for installing) after
#'   which no further package is started; `NULL` (the default) sets no limit
#' @param cran_to_main_lib if `TRUE`, CRAN-source packages are installed into
#'   your DEFAULT library (no `lib` argument — same as calling
#'   `install.packages(pkg)` yourself) instead of the throwaway `lib_dir`, and so
#'   PERSIST after the run rather than being deleted with the sandbox. This
#'   trades a one-time, permanent change to your R library for install work that
#'   is shared across every later call needing the same package (useful when
#'   `reproducibility_check()` runs across many papers in one session/script: a
#'   CRAN package installed for an early paper is already present, and skipped
#'   as already-installed, for every later one). GitHub/URL sources are
#'   deliberately excluded from this and always go into the throwaway `lib_dir`
#'   regardless of this argument — those are arbitrary, unreviewed code you
#'   likely do not want permanently in your main library. Default `FALSE`
#'   (everything throwaway, the original behaviour).
#'
#' A CRAN-source package that `install.packages()` cannot find is not
#' necessarily a broken dependency: CRAN removes (archives) packages that stop
#' passing its checks, but every version ever published stays available as a
#' source tarball under the CRAN Archive
#' (`https://cran.r-project.org/src/contrib/Archive/<pkg>/`). When the live-CRAN
#' install fails, this retries once from the Archive's most recent version
#' before giving up — a package the paper's authors could install at the time
#' (and that later dropped off live CRAN) still installs, instead of the run
#' recording an "errored" script for a cause that has nothing to do with the
#' paper's code. The retry is recorded in `via_archive` so the report can say
#' the run used a stale-but-real version rather than the one the authors had.
#'
#' @returns a data frame with `package`, `source`, `installed` (logical),
#'   `message` (error text on failure, else ""), `via_archive` (logical,
#'   TRUE when installed from the CRAN Archive after a live-CRAN failure),
#'   `category` (see [.repro_classify_install_message()]; `NA` for a
#'   successful install), `version_recorded` (the version asked for, or `NA`),
#'   `version_installed` (the version now in the libraries, or `NA`) and
#'   `version_note` (why the recorded version was not installed, else "").
#' @export
repro_install_deps <- function(install_deps, lib_dir, cran_to_main_lib = FALSE,
                               deadline = NULL) {
  empty <- data.frame(package = character(0), source = character(0),
                      installed = logical(0), message = character(0),
                      via_archive = logical(0), category = character(0),
                      version_recorded = character(0),
                      version_installed = character(0),
                      version_note = character(0))
  if (is.null(install_deps) || nrow(install_deps) == 0) return(empty)
  versions <- install_deps$version %||% rep(NA_character_, nrow(install_deps))
  dir.create(lib_dir, recursive = TRUE, showWarnings = FALSE)
  old_lib <- .libPaths(); on.exit(.libPaths(old_lib), add = TRUE)
  .libPaths(c(lib_dir, old_lib))

  # A non-interactive session (Rscript, a Quarto render, a CI job) has repos set
  # to the unresolved placeholder "@CRAN@". Interactively R would prompt for a
  # mirror; here install.packages() just fails with "trying to use CRAN without
  # setting a mirror", so EVERY not-yet-installed dependency fails and the
  # paper's scripts then error on library() — which reads as the paper being
  # irreproducible when the real cause is this session's configuration. Set a
  # mirror for the duration of the installs when none is configured.
  repos <- getOption("repos")
  if (is.null(repos) || !length(repos) ||
      any(!nzchar(repos)) || any(repos == "@CRAN@")) {
    old_repos <- getOption("repos")
    on.exit(options(repos = old_repos), add = TRUE)
    options(repos = c(CRAN = "https://cloud.r-project.org"))
    message("[repro]     no CRAN mirror configured; using cloud.r-project.org ",
            "for this install.")
  }

  gh_avail <- requireNamespace("remotes", quietly = TRUE)

  bar <- pb(NA, "(:spin) :what")
  on.exit(bar$terminate(), add = TRUE)
  started <- Sys.time()

  rows <- lapply(seq_len(nrow(install_deps)), function(i) {
    pkg <- install_deps$package[i]
    src <- install_deps$source[i]
    ref <- install_deps$ref[i]
    version <- versions[i]
    bar$tick(1, list(what = .repro_install_what(i, nrow(install_deps), pkg, version, started)))
    # The time for installing has run out: a package that is left out may still
    # be found in another library when the scripts run.
    if (.repro_time_left(deadline) <= 0)
      return(data.frame(package = pkg, source = src, installed = FALSE,
                        message = "not started: the time for installing had run out",
                        via_archive = FALSE, category = "other",
                        version_recorded = version,
                        version_installed = .repro_installed_version(pkg, c(lib_dir, old_lib)),
                        version_note = ""))
    # The recorded version is already there, so there is nothing to do.
    have <- .repro_installed_version(pkg, c(lib_dir, old_lib))
    if (.repro_same_version(have, version)) {
      message("[repro]     '", pkg, "' ", version, " is already installed; skipping.")
      return(data.frame(package = pkg, source = src, installed = TRUE, message = "",
                        via_archive = FALSE, category = NA_character_,
                        version_recorded = version, version_installed = have,
                        version_note = ""))
    }
    # A CRAN package already installed in one of the REAL (pre-throwaway)
    # library paths needs no work — this is what makes cran_to_main_lib's reuse
    # across papers actually pay off, instead of every call re-installing it.
    # Checked against `old_lib` specifically (not requireNamespace()'s default
    # search), because requireNamespace() also returns TRUE for a namespace
    # that is merely already LOADED in this R session — which happens after
    # this function once installed a package into the throwaway `lib_dir` and
    # loaded it from there. That throwaway directory is deleted at the end of
    # the run, so a later call in the same session would report "already
    # installed" for a package that no longer exists anywhere on disk, and a
    # freshly spawned callr subprocess (which starts with none of this
    # session's loaded namespaces) would then fail to find it at all.
    cran_main <- identical(src, "cran") && isTRUE(cran_to_main_lib) && is.na(version)
    if (cran_main && length(find.package(pkg, lib.loc = old_lib, quiet = TRUE)) > 0) {
      message("[repro]     '", pkg, "' already installed (main library); skipping.")
      # the same columns as every other row, with category NA (U135)
      return(data.frame(package = pkg, source = src, installed = TRUE, message = "",
                        via_archive = FALSE, category = NA_character_,
                        version_recorded = version, version_installed = have,
                        version_note = ""))
    }
    # DEBUG (TEMPORARY — remove before release): quiet = FALSE so compilation
    # progress is visible; a slow install then reads as work, not a hang.
    message("[repro]     installing '", pkg, "'",
            if (!is.na(version)) paste0(" ", version), " (source: ", src,
            if (cran_main) ", into main library" else ", into throwaway library",
            ") ...")
    install_lib <- if (cran_main) old_lib[1] else lib_dir
    install_warnings <- character(0)
    version_note <- ""
    version_done <- FALSE
    if (!is.na(version) && identical(src, "bioc")) {
      version_note <- "Bioconductor versions follow the R version, so the current version was installed"
    } else if (!is.na(version) && identical(src, "cran")) {
      if (gh_avail) {
        version_done <- tryCatch({
          # force: without it a package already in lib_dir, put there as a
          # dependency of an earlier package, can be left at that version
          remotes::install_version(pkg, version = version, lib = lib_dir, upgrade = "never",
                                   dependencies = NA, force = TRUE, quiet = FALSE)
          .repro_same_version(.repro_installed_version(pkg, lib_dir), version)
        }, error = function(e) {
          version_note <<- conditionMessage(e)
          FALSE
        })
        if (!version_done && !nzchar(version_note))
          version_note <- paste0("the recorded version could not be installed on R ", getRversion())
      } else {
        version_note <- "the 'remotes' package is needed to install a recorded version"
      }
      if (!version_done)
        message("[repro]     '", pkg, "' ", version, " could not be installed (",
                version_note, "); installing the current version.")
    }
    res <- tryCatch({
      # Warnings are recorded (not silenced) for the load check below: see
      # .repro_not_loadable_msg().
      withCallingHandlers({
      if (version_done) {
        # installed above
      } else if (identical(src, "github")) {
        if (!gh_avail) stop("the 'remotes' package is needed to install GitHub sources")
        remotes::install_github(ref, lib = lib_dir, upgrade = "never",
                                quiet = FALSE)
      } else if (identical(src, "url")) {
        utils::install.packages(ref, lib = lib_dir, repos = NULL, quiet = FALSE)
      } else if (identical(src, "bioc")) {
        # BiocManager is deliberately NOT a hard dependency of this package
        # (see .repro_bioc_packages()'s own comment) -- installed here, into
        # the SAME throwaway lib_dir, only when an actual Bioconductor install
        # is needed. install.packages() (not remotes) is enough for this,
        # since BiocManager itself is an ordinary CRAN package.
        if (!requireNamespace("BiocManager", quietly = TRUE))
          utils::install.packages("BiocManager", lib = lib_dir, quiet = FALSE)
        BiocManager::install(pkg, lib = lib_dir, update = FALSE, ask = FALSE)
      } else {
        # Explicit lib=: .libPaths()[1] is `lib_dir` (the throwaway) at this
        # point, not R's real user library, so an unqualified install.packages()
        # call here would silently install into the throwaway and vanish at
        # the end of the run — the bug this whole block works around above.
        # old_lib[1] is the real library this session would install to
        # normally (captured before the throwaway push).
        utils::install.packages(pkg, lib = install_lib, quiet = FALSE)
      }
      }, warning = function(w)
        install_warnings <<- c(install_warnings, conditionMessage(w)))
      # Confirm it can actually be loaded (main library needs no lib.loc; the
      # throwaway paths were already prepended to .libPaths() above). Being where
      # it was installed is checked as well: the load check also passes for a copy
      # in the main library or a namespace already loaded, and a failed
      # install_github() does not stop. A package that is gone from CRAN may
      # still be in the main library, and the scripts can use it there.
      in_main <- identical(src, "cran") && is.na(version) &&
        !is.na(.repro_installed_version(pkg, old_lib))
      if ((!file.exists(file.path(install_lib, pkg, "DESCRIPTION")) && !in_main) ||
          (!requireNamespace(pkg, quietly = TRUE, lib.loc = lib_dir) &&
           !requireNamespace(pkg, quietly = TRUE)))
        stop(.repro_not_loadable_msg(pkg, install_warnings, .repro_load_error(pkg, lib_dir)))
      list(ok = TRUE, msg = "", via_archive = FALSE)
    }, error = function(e) list(ok = FALSE, msg = conditionMessage(e), via_archive = FALSE))

    # A CRAN-source package that failed is not necessarily broken code: CRAN
    # may simply no longer be SERVING it (removed for a check failure, a
    # dependency going away, ...) while every version it ever published still
    # sits in the Archive. Retry once from there before accepting the failure —
    # this is the one case worth a second attempt, since GitHub/URL failures
    # already point at a specific, user-named source with nothing else to try.
    if (!res$ok && identical(src, "cran")) {
      message("[repro]     '", pkg, "' not available on live CRAN; trying the ",
              "CRAN Archive (last published version) ...")
      res2 <- .repro_cran_archive_install(pkg, install_lib, lib_dir)
      if (res2$ok) {
        message("[repro]     '", pkg, "' installed from the CRAN Archive (",
                res2$version, ").")
        res <- list(ok = TRUE, msg = "", via_archive = TRUE)
      } else {
        message("[repro]     '", pkg, "' could not be installed from the ",
                "CRAN Archive either: ", res2$msg)
        res$msg <- paste0(res$msg, " (CRAN Archive retry also failed: ", res2$msg, ")")
      }
    }
    have <- .repro_installed_version(pkg, c(lib_dir, old_lib))
    if (res$ok && !is.na(version) && !.repro_same_version(have, version) && !nzchar(version_note))
      version_note <- "the source gave another version than the recorded one"
    data.frame(package = pkg, source = src, installed = res$ok, message = res$msg,
              via_archive = isTRUE(res$via_archive),
              category = if (isTRUE(res$ok)) NA_character_
                        else .repro_classify_install_message(res$msg),
              version_recorded = version,
              version_installed = have,
              version_note = version_note)
  })
  dplyr::bind_rows(rows)
}

# Lines for a script that runs without this package, to set where packages come
# from. The rocker/r-ver images of R before 4.0 name MRAN, which closed in 2023,
# so nothing could be installed there ("package 'x' is not available"). Posit
# keeps the same dated copies of CRAN.
.repro_repos_lines <- function() {
  c('repos <- getOption("repos")',
    'if (is.null(repos) || !length(repos) || any(!nzchar(repos)) || any(repos == "@CRAN@")) {',
    '  options(repos = c(CRAN = "https://cloud.r-project.org"))',
    '} else if (any(grepl("mran.microsoft.com", repos, fixed = TRUE))) {',
    '  options(repos = c(CRAN = sub("^.*/snapshot/", "https://packagemanager.posit.co/cran/", repos[1])))',
    '}')
}

# The order to install in: first the packages at their current version, then
# the recorded versions. Next to old versions, a current package can fail its
# own load test during the install: ggtree did, in a real repository. A conflict
# that is left then shows in the load check, which can repair it.
.repro_install_order <- function(deps) {
  current <- is.na(deps$version) | deps$source == "bioc"
  deps[order(!current), , drop = FALSE]
}

#' The script that checks whether the installed packages load
#'
#' Only the packages the scripts load are installed at their recorded versions,
#' next to the current versions of everything else. An old version can then keep
#' another package from loading (a current `ggplot2` next to a recorded `scales`
#' stopped with "namespace 'scales' 1.2.1 is being loaded, but >= 1.4.0 is
#' required"), or crash R when a current package uses it (a recorded `magrittr`
#' crashed `library(tidyverse)` on R 4.6.1). Both were found in real repositories.
#'
#' The script loads each package in a new R process, so a crash stops only that
#' process, and a namespace that an earlier package loaded does not hide a fault.
#' When a package does not load, a recorded version is set aside, so the current
#' version is used: the one the error names, or else the first recorded package
#' it depends on (or the package itself) without which it loads. A package that
#' does not load without them either is reported, and nothing is set aside for
#' it. The script runs without this package, in a container or in a new process,
#' and works from R 3.6.
#'
#' @param lib the library the packages were installed in
#' @param pkgs the packages the scripts load
#' @param recorded the packages installed at a recorded version
#' @param results_file where the script saves its results, after every package:
#'   a list of `set_aside` (`package`, `because`, `version_installed`, `note`) and
#'   `not_loading` (`package`, `message`)
#'
#' @returns the lines of the script
#' @keywords internal
.repro_load_check_lines <- function(lib, pkgs, recorded, results_file) {
  c(
    paste0("lib <- ", deparse1(lib)),
    paste0("pkgs <- ", deparse1(pkgs)),
    paste0("recorded <- ", deparse1(recorded)),
    paste0("results_file <- ", deparse1(results_file)),
    '.libPaths(c(lib, .libPaths()))',
    .repro_repos_lines(),
    # a folder without a DESCRIPTION file is not a package for R
    'aside <- file.path(lib, ".set_aside")',
    'dir.create(aside, showWarnings = FALSE)',
    'load_file <- tempfile(fileext = ".R")',
    'writeLines(c("args <- commandArgs(TRUE)", ".libPaths(c(args[1], .libPaths()))",',
    '             "for (p in args[-1]) suppressPackageStartupMessages(library(p, character.only = TRUE))"), load_file)',
    # away from the folder of the scripts, which can hold an .Rprofile
    'setwd(tempdir())',
    'quote_arg <- function(x) shQuote(x, type = if (.Platform$OS.type == "windows") "cmd" else "sh")',
    'try_load <- function(p) {',
    '  out <- suppressWarnings(system2(file.path(R.home("bin"), "Rscript"),',
    '                                  quote_arg(c(load_file, lib, p)), stdout = TRUE, stderr = TRUE, timeout = 300))',
    '  status <- attr(out, "status")',
    '  list(ok = is.null(status) || identical(as.integer(status), 0L), msg = paste(out, collapse = "\\n"))',
    '}',
    'in_lib <- function(p) file.exists(file.path(lib, p, "DESCRIPTION"))',
    'can_go <- function() recorded[vapply(recorded, in_lib, logical(1))]',
    # The current version comes from another library, or is installed.
    'other_copy <- function(p) length(find.package(p, lib.loc = setdiff(.libPaths(), lib), quiet = TRUE)) > 0',
    'use_current <- function(p) {',
    '  file.rename(file.path(lib, p), file.path(aside, p))',
    '  if (!other_copy(p)) try(utils::install.packages(p, lib = lib, quiet = TRUE), silent = TRUE)',
    '}',
    'put_back <- function(p) {',
    '  unlink(file.path(lib, p), recursive = TRUE)',
    '  file.rename(file.path(aside, p), file.path(lib, p))',
    '}',
    'depends_on <- function(p) {',
    '  db <- utils::installed.packages(lib.loc = .libPaths(), noCache = TRUE)',
    '  db <- db[!duplicated(db[, "Package"]), , drop = FALSE]',
    '  unique(unlist(tools::package_dependencies(p, db = db, which = c("Depends", "Imports", "LinkingTo"), recursive = TRUE)))',
    '}',
    'named_in <- function(msg) {',
    '  m <- regmatches(msg, regexec("namespace [^A-Za-z]*([A-Za-z][A-Za-z0-9.]*)[^A-Za-z0-9.]+[0-9.-]+ is (?:being|already) loaded, but",',
    '                               msg, perl = TRUE))[[1]]',
    '  if (length(m) > 1) m[2] else NA_character_',
    '}',
    'have_version <- function(p) {',
    '  v <- suppressWarnings(tryCatch(utils::packageDescription(p, lib.loc = .libPaths(), fields = "Version"),',
    '                                 error = function(e) NA_character_))',
    '  if (length(v) != 1 || is.na(v)) NA_character_ else as.character(v)',
    '}',
    'set_aside <- list(); not_loading <- list()',
    'save_results <- function() saveRDS(list(set_aside = do.call(rbind, set_aside),',
    '                                        not_loading = do.call(rbind, not_loading)), results_file)',
    'note <- function(p, because) {',
    '  why <- if (identical(p, because)) "the recorded version does not load on R " else',
    '    paste0("the recorded version keeps ", because, " from loading on R ")',
    '  set_aside[[length(set_aside) + 1L]] <<- data.frame(package = p, because = because,',
    '    version_installed = have_version(p), note = paste0(why, getRversion(), ", so the current version was used"),',
    '    stringsAsFactors = FALSE)',
    '  save_results()',
    '}',
    'repair <- function(p) {',
    '  r <- try_load(p)',
    '  if (r$ok) return(invisible(TRUE))',
    # the package the error names, as often as it names one
    '  repeat {',
    '    named <- named_in(r$msg)',
    '    if (is.na(named) || !named %in% can_go()) break',
    '    use_current(named); note(named, p)',
    '    r <- try_load(p)',
    '    if (r$ok) return(invisible(TRUE))',
    '  }',
    # An error that names nothing, or a crash: what p depends on first, then p.
    '  tries <- intersect(depends_on(p), can_go())',
    '  tries <- tries[vapply(tries, other_copy, logical(1))]',
    '  if (p %in% can_go()) tries <- c(tries, p)',
    '  for (x in tries) {',
    '    use_current(x)',
    '    if (try_load(p)$ok) { note(x, p); return(invisible(TRUE)) }',
    '    put_back(x)',
    '  }',
    '  if (length(tries) > 1) {',
    '    for (x in tries) use_current(x)',
    '    if (try_load(p)$ok) { for (x in tries) note(x, p); return(invisible(TRUE)) }',
    '    for (x in tries) put_back(x)',
    '  }',
    '  lines <- strsplit(r$msg, "\\n", fixed = TRUE)[[1]]',
    '  not_loading[[length(not_loading) + 1L]] <<- data.frame(package = p,',
    '    message = paste0("the package does not load: ", substr(paste(utils::head(lines[nzchar(lines)], 5), collapse = " "), 1, 400)),',
    '    stringsAsFactors = FALSE)',
    '  save_results()',
    '  invisible(FALSE)',
    '}',
    'for (p in pkgs) repair(p)',
    # A conflict can also show only when the packages are loaded together.
    'for (i in seq_along(pkgs)) {',
    '  r <- try_load(pkgs[!pkgs %in% vapply(not_loading, function(x) x$package, "")])',
    '  named <- named_in(r$msg)',
    '  if (r$ok || is.na(named) || !named %in% can_go()) break',
    '  use_current(named); note(named, "the other packages")',
    '}',
    'save_results()'
  )
}

#' Replace recorded versions that keep the installed packages from loading
#'
#' Runs the script of [.repro_load_check_lines()] where the scripts will run, and
#' puts its findings in the install results. Does nothing when no package was
#' installed at a recorded version, since then no recorded version can be at fault.
#'
#' @param install_results the result of [repro_install_deps()] or
#'   [repro_install_deps_docker()]
#' @param lib_dir the library the packages were installed in
#' @param image the Docker image, or NULL to run in a new process on this machine
#' @param timeout the time in seconds for the check
#'
#' @returns `install_results`, with `version_installed` and `version_note` updated
#'   for a replaced package, and `installed` FALSE for a package that does not load
#' @keywords internal
.repro_load_check <- function(install_results, lib_dir, image = NULL, timeout = 1800) {
  # in Docker the library is a volume (see .repro_docker_lib_mount()), not this folder
  in_lib <- if (!is.null(image) && !isFALSE(getOption("metacheck.docker_lib_volume", TRUE)))
    install_results$installed %in% TRUE else
    file.exists(file.path(lib_dir, install_results$package, "DESCRIPTION"))
  if (!any(in_lib)) return(install_results)
  at_recorded <- in_lib & install_results$installed & !is.na(install_results$version_recorded) &
    mapply(.repro_same_version, as.character(install_results$version_installed),
           install_results$version_recorded, USE.NAMES = FALSE)
  if (!any(at_recorded)) return(install_results)
  message("[repro]   Checking that the installed packages load (", sum(install_results$installed),
          " packages) ...")

  sandbox_dir <- tempfile("repro_load_check_")
  dir.create(sandbox_dir, recursive = TRUE)
  on.exit(unlink(sandbox_dir, recursive = TRUE), add = TRUE)
  in_docker <- !is.null(image)
  lib_path <- normalizePath(lib_dir, winslash = "/", mustWork = FALSE)
  lines <- .repro_load_check_lines(
    if (in_docker) "/rlib" else lib_path,
    install_results$package[install_results$installed],
    install_results$package[at_recorded],
    if (in_docker) "/sandbox/results.rds" else file.path(normalizePath(sandbox_dir, winslash = "/"), "results.rds"))
  script <- file.path(sandbox_dir, "check.R")
  con <- file(script, open = "wb", encoding = "UTF-8")
  writeLines(lines, con, useBytes = TRUE)
  close(con)

  container_name <- .repro_docker_container_name()
  res <- tryCatch(
    if (in_docker) {
      # as for the install: the network is on, because a replaced package may need installing
      processx::run("docker", c("run", "--rm", "--name", container_name,
                                "--user", .repro_docker_uid,
                                "--cap-drop", "ALL",
                                "--security-opt", "no-new-privileges",
                                "--pids-limit", "512",
                                .repro_docker_resource_args(),
                                "-v", paste0(sandbox_dir, ":/sandbox"),
                                "-v", .repro_docker_lib_mount(lib_dir, "/rlib", image = image),
                                image, "Rscript", "/sandbox/check.R"),
                    error_on_status = FALSE, timeout = timeout)
    } else {
      processx::run(file.path(R.home("bin"), "Rscript"), script,
                    error_on_status = FALSE, timeout = timeout)
    }, error = function(e) NULL)
  if (in_docker && (is.null(res) || isTRUE(res$timeout))) .repro_docker_stop(container_name)

  found <- tryCatch(readRDS(file.path(sandbox_dir, "results.rds")), error = function(e) NULL)
  if (is.null(found)) {
    warning("reproducibility_check(): the check that the installed packages load did not finish, ",
            "so a recorded version may keep a package from loading.", call. = FALSE)
    return(install_results)
  }
  .repro_load_check_apply(install_results, found)
}

# Puts what the load check found in the install results.
.repro_load_check_apply <- function(install_results, found) {
  gone <- found$set_aside
  for (i in seq_len(NROW(gone))) {
    row <- match(gone$package[i], install_results$package)
    if (is.na(row)) next
    install_results$version_installed[row] <- gone$version_installed[i]
    install_results$version_note[row] <- gone$note[i]
  }
  bad <- found$not_loading
  for (i in seq_len(NROW(bad))) {
    row <- match(bad$package[i], install_results$package)
    if (is.na(row)) next
    install_results$installed[row] <- FALSE
    install_results$message[row] <- bad$message[i]
    install_results$category[row] <- .repro_classify_install_message(bad$message[i])
  }
  install_results
}

# The failure message for a package that could not be loaded after its
# install step. install.packages() does not stop for a package it cannot
# find: it warns "package 'x' is not available for this version of R" and
# returns normally. Without this, the load check that follows reported
# "installed but package 'x' is not loadable" -- claiming an install that
# never happened, in wording .repro_classify_install_message() cannot place,
# so a name that is not a package at all was listed as an uncategorised
# failure (issue #421). The install step's own "not available" warning is the
# real reason, so it is reported instead when there is one. `reason` is what R
# said when loading failed (see .repro_load_error()).
.repro_not_loadable_msg <- function(pkg, warnings, reason = "") {
  unavailable <- grep("is not available", warnings, value = TRUE, fixed = TRUE)
  if (length(unavailable) > 0) return(paste(unique(unavailable), collapse = "; "))
  paste0("installed but package '", pkg, "' is not loadable", if (nzchar(reason)) paste0(": ", reason))
}

# What R says when a package does not load, on one line: often a system library
# that is missing, which "is not loadable" alone does not tell.
.repro_load_error <- function(pkg, lib_dir) {
  msg <- tryCatch({
    loadNamespace(pkg, lib.loc = c(lib_dir, .libPaths()))
    ""
  }, error = function(e) conditionMessage(e))
  substr(gsub("[[:space:]]+", " ", msg), 1, 300)
}

#' Classify why a package installation failed
#'
#' `repro_install_deps()`/`repro_install_deps_docker()` already capture the raw
#' error text for a failed package, but the raw text alone does not say what
#' KIND of failure it was — and the distinction matters for what a reader
#' should do about it: a package genuinely gone from CRAN with no successor is
#' a real, permanent problem (worth reporting as-is), a transient network
#' hiccup is worth an automatic retry rather than treating as conclusive, and a
#' missing compiler toolchain is an environment-setup gap, not a defect in the
#' paper's own code. None of these change whether the install COUNTS as a
#' failure (they still do, and still stay out of the traffic light — see the
#' module's own dependency-unavailable handling), only how informative the
#' report is about the subtype.
#'
#' Matched against the install error message's own text with a fixed set of
#' regexes, most-specific first, so a message that could plausibly match more
#' than one category (rare, but not impossible) is not misassigned to a looser
#' pattern. A message matching none of them is `"other"` — the raw `message`
#' column is always still the authoritative text; this is advisory framing on
#' top of it, not a replacement for reading the message.
#'
#' `install.packages()`'s own standard CRAN-unavailability message —
#' `"package 'x' is not available for this version of R"` — is CRAN's ONE
#' generic wording for two different real causes (the package was removed/
#' archived entirely, or a current version simply never supported this R
#' release) and does not itself say which one applies; confirmed directly
#' against real R output rather than assumed. Splitting those into two
#' categories from the message text alone would be a guess this classifier
#' does not make — both are reported as `"cran_unavailable"`, and the CRAN
#' Archive retry's own `via_archive` result elsewhere already provides the
#' real, non-guessed signal for the archived case ([repro_install_deps()]),
#' since a successful Archive install proves that specific cause rather than
#' inferring it from wording.
#'
#' @param msg the error message text (`repro_install_deps()`'s own `message`
#'   column for one failed row)
#'
#' A package that Docker's second pass found in none of the places it looks
#' (Bioconductor, the CRAN Archive, the list of known places, R-universe) is
#' `"source_unknown"`: the code loads it without saying where it comes from.
#'
#' @returns a single category string: `"source_unknown"`, `"cran_unavailable"`,
#'   `"compile_failure"`, `"network"`, `"transitive_dependency_missing"`, or
#'   `"other"`
#' @keywords internal
.repro_classify_install_message <- function(msg) {
  if (is.null(msg) || is.na(msg) || !nzchar(msg)) return("other")

  # Our own time limit for installing in Docker ran out, which says nothing
  # about the network, so it must not reach the "timed out" pattern below.
  if (grepl("^docker install timed out after", msg)) return("other")

  # Docker's second pass looked everywhere it knows and found the package in
  # none of those places (see .repro_docker_elsewhere_lines()): the code does not
  # say where it comes from (#389). Before "not available", which it also says.
  if (grepl("found nowhere else: ", msg, fixed = TRUE)) return("source_unknown")

  # A source build that failed partway through compiling/configuring — most
  # often a missing system library or compiler toolchain, not an R-level
  # problem at all. Checked before the broader patterns below: a compile
  # failure's own text ("non-zero exit status") never overlaps with them, but
  # checking it first keeps this category's meaning narrow and unambiguous.
  # NOTE: a bare "error: " substring was deliberately tried and dropped here —
  # R wraps almost any condition's own text in "Error: ..." regardless of
  # cause (confirmed: it false-matched a GitHub 404 "not found" message that
  # has nothing to do with compiling), so it is not a usable compile signal on
  # its own; every pattern below is specific to an actual build-tool message.
  if (grepl("configure(:| failed| error)|non-zero exit status|compilation failed|make(:| error)\\b|\\bgcc\\b|g\\+\\+|gfortran",
           msg, ignore.case = TRUE, perl = TRUE))
    return("compile_failure")

  # Could not even reach the repository/host — a transient condition, worth
  # retrying, not evidence the package is unavailable.
  if (grepl("could not resolve host|couldn.t connect to server|timed? ?out|connection (refused|reset|timed out)|network is unreachable|ssl (connect|certificate) error|could not reach",
           msg, ignore.case = TRUE, perl = TRUE))
    return("network")

  # The package ITSELF is not being installed here — a NAMED dependency of it
  # is what's missing/unavailable ("there is no package called 'x'", "dependency
  # 'x' is not available for package 'y'"). Checked before the generic
  # cran_unavailable pattern below, since "dependency 'x' is not available..."
  # would otherwise also match that broader "is not available" wording and be
  # misclassified as the package's own CRAN availability, not its dependency's.
  if (grepl("there is no package called|depend(s|ency|encies) .*(is|are) not available|unable to (find|locate) (required )?package",
           msg, ignore.case = TRUE, perl = TRUE))
    return("transitive_dependency_missing")

  # CRAN's generic "not available" wording — covers both a fully archived
  # package and one simply incompatible with this R version; see roxygen above
  # for why those are not split further.
  if (grepl("is not available (for|as a package)|package .* is not available|was built (for|under) R version",
           msg, ignore.case = TRUE, perl = TRUE))
    return("cran_unavailable")

  "other"
}

#' Install a package's most recent CRAN Archive version
#'
#' Every package ever published to CRAN keeps its full version history under
#' the CRAN Archive (`https://cran.r-project.org/src/contrib/Archive/<pkg>/`),
#' even after the live repository stops serving it. This lists that directory,
#' takes the most recent version by file modification time (the directory
#' listing carries no other reliable ordering), and installs the source
#' tarball directly — the one fallback worth trying automatically when a
#' CRAN-source dependency is not on live CRAN, since it needs no extra
#' service or paper-specific date, just the package name.
#'
#' @param pkg the package name
#' @param install_lib the library to install into (the real library for
#'   `cran_to_main_lib`, else the throwaway `lib_dir`)
#' @param lib_dir the throwaway library (checked for loadability, same as the
#'   caller's main install path)
#'
#' @returns a list `ok` (logical), `msg` (character, error text on failure),
#'   `version` (the Archive version installed, or NA)
#' @keywords internal
.repro_cran_archive_install <- function(pkg, install_lib, lib_dir) {
  fail <- function(msg) list(ok = FALSE, msg = msg, version = NA_character_)
  archive_url <- paste0("https://cran.r-project.org/src/contrib/Archive/", pkg, "/")
  # The HTTP status is only in readLines()'s warning ("... HTTP status was
  # '404 Not Found'"); its error just says "cannot open the connection".
  # A 404 means the Archive has no folder for this package -- it was never
  # archived, or is not a CRAN package at all -- not that CRAN was
  # unreachable. Reporting it as "could not reach" made
  # .repro_classify_install_message() file every such package as a network
  # problem (issue #421).
  read_warnings <- character(0)
  listing <- withCallingHandlers(
    tryCatch(readLines(archive_url, warn = FALSE), error = function(e) NULL),
    warning = function(w) {
      read_warnings <<- c(read_warnings, conditionMessage(w))
      invokeRestart("muffleWarning")
    })
  if (is.null(listing)) {
    if (any(grepl("404", read_warnings, fixed = TRUE)))
      return(fail("package not found in the CRAN Archive"))
    return(fail("could not reach the CRAN Archive listing"))
  }

  # The Archive's directory listing is an HTML index: each row names a source
  # tarball "<pkg>_<version>.tar.gz" and its own modification date, which is
  # the only ordering signal available (versions do not always sort
  # lexically, e.g. "1.9" vs "1.10"). Take the row with the LATEST date.
  tarball_pat <- paste0(pkg, "_[0-9][^\"']*\\.tar\\.gz")
  hit_lines <- grep(tarball_pat, listing, value = TRUE)
  if (length(hit_lines) == 0)
    return(fail("package not found in the CRAN Archive"))

  date_pat <- "(\\d{4}-\\d{2}-\\d{2} \\d{2}:\\d{2})"
  # A line without a date sorts last: given the whole line, as.POSIXct()
  # stopped, and the whole install failed (U135)
  date_txt <- ifelse(grepl(date_pat, hit_lines, perl = TRUE),
                     sub(paste0(".*", date_pat, ".*"), "\\1", hit_lines), NA_character_)
  dates <- as.POSIXct(date_txt, format = "%Y-%m-%d %H:%M", tz = "UTC")
  files <- regmatches(hit_lines, regexpr(tarball_pat, hit_lines))
  ord <- if (all(is.na(dates))) order(files, decreasing = TRUE) else
    order(is.na(dates), -as.numeric(dates))
  latest_file <- files[ord][1]
  version <- sub(paste0("^", pkg, "_(.*)\\.tar\\.gz$"), "\\1", latest_file)
  tarball_url <- paste0(archive_url, latest_file)

  ok <- tryCatch({
    if (requireNamespace("remotes", quietly = TRUE)) {
      # remotes::install_url() resolves the tarball's OWN Imports/Depends from
      # live CRAN before installing it; a plain install.packages(repos = NULL,
      # type = "source") does not (repos = NULL disables all dependency
      # resolution, since it means "this is a direct file, not a repository
      # lookup") — confirmed as a real, general bug, not specific to one
      # package: an archived package with ANY unmet CRAN dependency fails
      # R CMD INSTALL with a non-zero exit status here, which read as "this
      # package cannot be installed on this R version" even though the
      # package itself installs fine once its dependencies are present
      # (confirmed against dynamic 1.1.0's own missing `simstandard` import).
      remotes::install_url(tarball_url, dependencies = NA, lib = install_lib,
                           upgrade = "never", quiet = FALSE)
    } else {
      # No remotes: fall back to the old dependency-blind path rather than
      # failing outright — still works for an archived package with no unmet
      # dependencies of its own.
      utils::install.packages(tarball_url, lib = install_lib,
                              repos = NULL, type = "source", quiet = FALSE)
    }
    if (!requireNamespace(pkg, quietly = TRUE, lib.loc = lib_dir) &&
        !requireNamespace(pkg, quietly = TRUE))
      stop(.repro_not_loadable_msg(pkg, character(0), .repro_load_error(pkg, lib_dir)))
    TRUE
  }, error = function(e) conditionMessage(e))
  if (isTRUE(ok)) list(ok = TRUE, msg = "", version = version)
  else fail(ok)
}

# A script's path in its repository (forward slashes, nothing in front), or its
# file name when it has no path.
.repro_script_path <- function(files) {
  path <- if ("file_path" %in% names(files)) as.character(files$file_path) else
    rep(NA_character_, nrow(files))
  none <- is.na(path) | !nzchar(path)
  path[none] <- files$file_name[none]
  sub("^(\\./|/)+", "", gsub("\\\\", "/", path))
}

# A name of its own for each script of a paper. Every step of the check knows a
# script by file_name, which holds no folder, so scripts that share a name (not
# byte-identical copies, which are removed first) shared one result, and only
# the first ran (#568). A name that occurs once stays as it is, so results stay
# comparable. A repeated name becomes the script's path, which still ends in
# the file name; with the repository in front when the path repeats too (the
# same path in a GitHub and a Zenodo copy), and as a last resort a number.
# `taken` holds names already used by the other language.
.repro_unique_script_names <- function(files, taken = character(0)) {
  out <- files$file_name
  key <- tolower(out)
  repeated <- key %in% c(key[duplicated(key)], tolower(taken))
  if (!any(repeated)) return(out)
  out[repeated] <- .repro_script_path(files)[repeated]
  clashes <- function() {
    k <- tolower(out)
    repeated & k %in% c(k[duplicated(k)], tolower(taken))
  }
  if ("repo_name" %in% names(files)) {
    repo <- as.character(files$repo_name)
    add <- clashes() & !is.na(repo) & nzchar(repo)
    out[add] <- paste0(repo[add], "/", out[add])
  }
  left <- clashes()
  out[left] <- sprintf("%d/%s", which(left), out[left])
  out
}

# The path to tell a script's folder from, for repro_run_order(): its unique
# name when that holds a folder (it may have the repository in front), else its
# path in the repository.
.repro_order_path <- function(files) {
  ifelse(grepl("/", files$file_name, fixed = TRUE), files$file_name, .repro_script_path(files))
}

# Where repro_write_scripts() puts each script (`script_paths`): its own path in
# the repository. Two scripts with one place (the same path in two repositories)
# go by their unique names, and no script is put above the sandbox.
.repro_script_places <- function(files) {
  fname <- files$file_name
  out <- .repro_script_path(files)
  k <- tolower(out)
  clash <- !is.na(k) & k %in% k[!is.na(k) & duplicated(k)]
  out[clash] <- fname[clash]
  gsub("(^|/)\\.\\.(?=/|$)", "\\1_", out, perl = TRUE)
}

# Where code file `idx` of `files` (code_check's table) came from, for the
# report: its link, else its path in the repository, else NA. Never its
# place on the computer that runs the check, which on a server is the
# server's job folder (Lakens/tue_datamanager_check#5, item 2).
.repro_shown_location <- function(files, idx) {
  col <- function(nm) if (nm %in% names(files)) as.character(files[[nm]][idx]) else rep(NA_character_, length(idx))
  url <- col("file_url")
  path <- col("file_path")
  ifelse(!is.na(url) & grepl("^https?://", url), url,
         ifelse(!is.na(path) & nzchar(path), path, NA_character_))
}

# The time for all of a paper's scripts together: the moment after which no
# script is started (NULL: no limit), and the seconds left until then
# (Lakens/tue_datamanager_check#4, item 6). A clock time (POSIXct) is the
# deadline itself, such as a server's job limit minus the time for the report.
.repro_deadline <- function(total_timeout) {
  if (is.null(total_timeout) || length(total_timeout) != 1 || is.na(total_timeout)) return(NULL)
  if (inherits(total_timeout, "POSIXct")) return(total_timeout)
  if (!is.finite(total_timeout)) return(NULL)
  Sys.time() + total_timeout
}
# The time limit as the report names it, for scripts not started because of it.
.repro_deadline_text <- function(total_timeout) {
  if (inherits(total_timeout, "POSIXct")) return("the time for all scripts together")
  sprintf("the time for all scripts together (%g minutes, `total_timeout`)", total_timeout / 60)
}
.repro_time_left <- function(deadline) {
  if (is.null(deadline)) return(Inf)
  as.numeric(difftime(deadline, Sys.time(), units = "secs"))
}
# A length of time as words, for the progress display: "12 sec", "4 min", "1 h 5 min".
.repro_duration <- function(secs) {
  secs <- max(0, round(secs))
  if (secs < 60) return(sprintf("%d sec", as.integer(secs)))
  mins <- as.integer(round(secs / 60))
  if (mins < 60) return(sprintf("%d min", mins))
  sprintf("%d h %d min", mins %/% 60L, mins %% 60L)
}
# What the install step is doing, with the time so far and an estimate of the
# time left from the packages finished so far. The estimate is rough: one package
# can take far longer to build than the others.
.repro_install_what <- function(i, n, pkg, version, started) {
  elapsed <- as.numeric(difftime(Sys.time(), started, units = "secs"))
  sprintf("Installing %s%s (%d of %d, %s so far%s)", pkg,
          if (is.na(version)) "" else paste0(" ", version), i, n, .repro_duration(elapsed),
          if (i > 1) sprintf(", about %s left", .repro_duration(elapsed / (i - 1) * (n - i + 1))) else "")
}
# The seconds for installing a paper's packages. Unless `install_timeout` says
# otherwise it is the per-script `timeout` as before, or at least an hour when
# recorded versions are installed, since those are built from source. With a
# clock time (such as a server's job limit), installing may take at most half of
# what is left, so that the scripts still get to run.
.repro_install_limit <- function(install_timeout, timeout, total_timeout, recorded = FALSE) {
  limit <- install_timeout %||% if (isTRUE(recorded)) max(timeout, 3600) else timeout
  if (inherits(total_timeout, "POSIXct"))
    limit <- min(limit, max(60, .repro_time_left(total_timeout) / 2))
  limit
}

#' Run the paper's scripts, in order, in isolated subprocesses
#'
#' Runs each script with [callr::r()] from `run_dir`, with `lib_dir` prepended to
#' the subprocess `.libPaths()` so the throwaway library is seen. Each run is
#' bounded by `timeout` seconds; its exit, error, stdout and stderr are captured.
#' A script whose inputs are known-missing (in `skip`) is not run — it is
#' recorded `skipped_missing_inputs`, since it cannot succeed and would only
#' produce a confusing error. callr isolates a crash to the subprocess; it does
#' not sandbox the filesystem or network, so this only runs on explicit opt-in.
#'
#' @param run_tbl `repro_write_scripts()` output (`file_name`, `script_path`,
#'   `run_dir`)
#' @param order a vector of `file_name`s in the order to run (unplaceable files
#'   are appended at the end in input order)
#' @param lib_dir throwaway library to expose to each subprocess (or NULL)
#' @param timeout per-script timeout in seconds
#' @param skip character vector of `file_name`s to record as
#'   `skipped_missing_inputs` instead of running
#' @param parses named logical (by file_name); a file that will not parse is
#'   recorded `not_parsed` and not run
#' @param failed_deps character vector of package names that
#'   [repro_install_deps()] could not install (CRAN Archive retry included) —
#'   used only to tell apart a script that failed because ITS OWN dependency is
#'   genuinely unavailable (`dependency_unavailable`) from a script that failed
#'   for any other reason (`errored`), so an unavailable package does not read
#'   as a bug in the paper's code
#'
#' @returns a data frame with `file_name`, `outcome` (one of `ran_ok`,
#'   `errored`, `timed_out`, `not_parsed`, `skipped_missing_inputs`,
#'   `dependency_unavailable`), `error` (message or ""), `error_type`
#'   (`undefined_variable`, `timeout`, `dependency_unavailable`, `runtime`, or
#'   NA), `undefined_var` (the missing variable name when
#'   `error_type == "undefined_variable"`, else NA), `stdout`, `stderr`,
#'   `elapsed` (seconds), and `script_lines` (list-column: the EXECUTED script's
#'   own text, one element per source line — the exact text `stdout`'s echoed
#'   statements were run from, so [read_r_output()] can match them back to a
#'   line number; empty character(0) for a script that did not run).
#' @param deadline the time ([Sys.time()] plus the time for all scripts
#'   together) after which no script is started, recorded as
#'   `skipped_time_limit`; a running script is stopped then. `NULL` (the
#'   default) sets no such limit.
#' @export
repro_run_scripts <- function(run_tbl, order, lib_dir = NULL, timeout = 600,
                              skip = character(0), parses = NULL,
                              failed_deps = character(0), deadline = NULL) {
  if (is.null(run_tbl) || nrow(run_tbl) == 0)
    return(data.frame(file_name = character(0), outcome = character(0),
                      error = character(0), error_type = character(0),
                      undefined_var = character(0), stdout = character(0),
                      stderr = character(0), elapsed = numeric(0)) |>
             dplyr::mutate(script_lines = list(), captures = list()))
  if (!requireNamespace("callr", quietly = TRUE))
    stop("the 'callr' package is required to execute code (execute = TRUE).",
         call. = FALSE)

  # Order the files: placeable ones by the given order, the rest appended.
  ordered_names <- c(order[order %in% run_tbl$file_name],
                     setdiff(run_tbl$file_name, order))
  libs <- if (!is.null(lib_dir)) c(lib_dir, .libPaths()) else .libPaths()

  pb_run <- pb(length(ordered_names), ":what [:bar] :current/:total")
  pb_run$tick(0, list(what = ""))
  on.exit(pb_run$terminate())
  run_started <- Sys.time()

  rows <- lapply(ordered_names, function(fn) {
    pb_run$tick(1, list(what = sprintf("Running %s (%s so far)", fn, .repro_duration(
      as.numeric(difftime(Sys.time(), run_started, units = "secs"))))))
    row <- run_tbl[run_tbl$file_name == fn, ][1, ]

    no_lines <- function(df) dplyr::mutate(df, script_lines = list(character(0)),
                                           captures = list(NULL))
    if (!is.null(parses) && fn %in% names(parses) && !isTRUE(parses[[fn]]))
      return(no_lines(data.frame(file_name = fn, outcome = "not_parsed", error = "",
                        error_type = NA_character_, undefined_var = NA_character_,
                        stdout = "", stderr = "", elapsed = 0)))
    if (fn %in% skip)
      return(no_lines(data.frame(file_name = fn, outcome = "skipped_missing_inputs",
                        error = "", error_type = NA_character_,
                        undefined_var = NA_character_,
                        stdout = "", stderr = "", elapsed = 0)))
    # Once the time for all scripts together has run out, a script is not
    # started; a script that is started stops when that time runs out
    # (Lakens/tue_datamanager_check#4, item 6)
    left <- .repro_time_left(deadline)
    if (left <= 0)
      return(no_lines(data.frame(file_name = fn, outcome = "skipped_time_limit",
                        error = "not started: the time for all scripts together had run out",
                        error_type = NA_character_, undefined_var = NA_character_,
                        stdout = "", stderr = "", elapsed = 0)))
    timeout <- min(timeout, ceiling(left))

    # The EXECUTED script's own text (path-rewritten, setwd()-commented — what
    # actually ran), one element per source line: this is what read_r_output()
    # matches echoed statements against to recover a line number, so it must be
    # read from script_path (not the pre-rewrite code_text_list) to stay aligned
    # with what the echo in stdout actually shows.
    exec_lines <- tryCatch(readLines(row$script_path, warn = FALSE),
                           error = function(e) character(0))

    # DEBUG (TEMPORARY — remove before release): announce each script as it starts
    # and finishes, so a hang is pinned to the exact script and its wall time.
    message("[repro]   -> running '", fn, "' (timeout ", timeout, "s) ...")

    # Capture the child's stdout/stderr to FILES, not OS pipes. With stdout = "|"
    # callr routes output through a pipe the parent only drains after the call
    # returns; a chatty script fills the pipe buffer and the child blocks on
    # write while the parent blocks on wait — a deadlock (this is the hang).
    # Redirecting to files removes the buffer entirely, so no script can wedge
    # the run, and the output is still captured (read back below).
    out_file <- tempfile(fileext = ".out")
    err_file <- tempfile(fileext = ".err")
    # Sidecar for CAPTURED RESULT OBJECTS (R/r-capture.R). A task callback in
    # the child records each top-level statistical object — the object itself,
    # not what it printed — so p-values, statistic identities and coefficient
    # matrices come back exact instead of re-parsed from console text. Written
    # by the child on exit; absent if the script died before writing, in which
    # case the stdout path alone is used.
    cap_file <- tempfile(fileext = ".rds")
    t0 <- Sys.time()
    out <- tryCatch(
      callr::r(
        # callr::r() has no working-directory argument, so set it INSIDE the
        # subprocess before sourcing, so the script's relative paths resolve
        # against the materialised layout root.
        .r_capture_runner(),
        args = list(script = row$script_path, wd = row$run_dir,
                    capture_file = cap_file,
                    helpers = .r_capture_helpers()),
        libpath = libs,
        timeout = timeout, error = "error",
        stdout = out_file, stderr = err_file),
      error = function(e) e)
    elapsed <- as.numeric(difftime(Sys.time(), t0, units = "secs"))
    captures <- if (file.exists(cap_file))
      tryCatch(readRDS(cap_file), error = function(e) NULL) else NULL
    unlink(cap_file)

    # Read whatever the child wrote before it finished / errored / timed out.
    read_cap <- function(f) if (file.exists(f))
      paste(readLines(f, warn = FALSE), collapse = "\n") else ""
    so <- read_cap(out_file); se <- read_cap(err_file)
    unlink(c(out_file, err_file))

    message("[repro]   <- '", fn, "' done in ", round(elapsed, 1), "s (",
            if (inherits(out, "condition")) "condition/error" else "ok",
            "; stdout ", nchar(so), " chars, stderr ", nchar(se), " chars)")

    # callr signals a timeout with a classed condition; a script error arrives as
    # a callr "error in processx"/"callr_error" carrying the child's message.
    if (inherits(out, "condition")) {
      is_timeout <- inherits(out, "callr_timeout_error") ||
        grepl("timed? ?out", conditionMessage(out), ignore.case = TRUE)
      msg <- conditionMessage(out)
      # Classify the error. An "object 'X' not found" is special: it usually
      # means the script expected a variable an earlier (unrun / separately-run)
      # script defined — the classic symptom of a snippet meant to be source()d
      # into a larger session, not run standalone. We capture the variable name;
      # a later phase uses it as a run-order signal (the file defining X should
      # precede this one). Check both the callr message and the captured stderr,
      # since the child's own error text lands in stderr.
      #
      # R phrases a missing FUNCTION differently from a missing variable
      # ("could not find function \"foo\"" vs "object 'x' not found") — both are
      # the same underlying problem (an unresolved top-level symbol, usually
      # because the script that defines it was never run/sourced first), so
      # both patterns are checked and feed the same undefined_var extraction.
      undef_pat <- "object ['\"]([^'\"]+)['\"] not found"
      fn_pat    <- "could not find function ['\"]([^'\"]+)['\"]"
      undef_src <- if (grepl(undef_pat, msg)) msg else
        if (grepl(undef_pat, se)) se else
        if (grepl(fn_pat, msg)) msg else
        if (grepl(fn_pat, se)) se else NA_character_
      undef_var <- if (!is.na(undef_src) && grepl(undef_pat, undef_src))
        sub(paste0(".*", undef_pat, ".*"), "\\1",
            regmatches(undef_src, regexpr(undef_pat, undef_src)))
        else if (!is.na(undef_src) && grepl(fn_pat, undef_src))
        sub(paste0(".*", fn_pat, ".*"), "\\1",
            regmatches(undef_src, regexpr(fn_pat, undef_src)))
        else NA_character_

      # A script that library()/require()s a package we already know FAILED to
      # install (even after the CRAN Archive retry — see repro_install_deps())
      # is not a bug in the paper's code: it never got the chance to run its
      # real analysis. R's own error for this is "there is no package called
      # 'x'" (from library()) or the same text via require()'s stop-on-failure
      # form; match it against failed_deps rather than trusting the message's
      # wording alone, so a script that merely REFERENCES an unavailable
      # package's name elsewhere is not misclassified.
      # curly quotes too: in a UTF-8 locale R writes them around the name,
      # which the ASCII-only quotes missed, so the script was "errored", not
      # dependency_unavailable (U132)
      nopkg_pat <- "there is no package called ['\"\u2018]([^'\"\u2019]+)['\"\u2019]"
      nopkg_src <- if (grepl(nopkg_pat, msg)) msg else
        if (grepl(nopkg_pat, se)) se else NA_character_
      nopkg_var <- if (!is.na(nopkg_src))
        sub(paste0(".*", nopkg_pat, ".*"), "\\1",
            regmatches(nopkg_src, regexpr(nopkg_pat, nopkg_src))) else NA_character_
      dep_unavailable <- !is.na(nopkg_var) && nopkg_var %in% failed_deps

      etype <- if (is_timeout) "timeout"
               else if (dep_unavailable) "dependency_unavailable"
               else if (!is.na(undef_var)) "undefined_variable"
               else "runtime"
      outc <- if (is_timeout) "timed_out"
              else if (dep_unavailable) "dependency_unavailable"
              else "errored"
      data.frame(file_name = fn, outcome = outc,
                 error = msg, error_type = etype, undefined_var = undef_var,
                 stdout = so, stderr = se, elapsed = elapsed) |>
        dplyr::mutate(script_lines = list(exec_lines),
                      captures = list(captures))
    } else {
      data.frame(file_name = fn, outcome = "ran_ok", error = "",
                 error_type = NA_character_, undefined_var = NA_character_,
                 stdout = so, stderr = se, elapsed = elapsed) |>
        dplyr::mutate(script_lines = list(exec_lines),
                      captures = list(captures))
    }
  })
  dplyr::bind_rows(rows)
}
