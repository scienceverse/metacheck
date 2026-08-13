# Match statistics reported ONLY in a results table's cells (paper$table$contents,
# populated from Grobid's <table> parsing — see .tei_table_contents() in
# R/import-grobid.R) against the extracted analysis output, the table-side
# counterpart to match-reported.R's text-based matching.
#
# A table row cannot be matched the way a sentence is: extract_eq()/extract_tests()
# rely on TEXT (an anchor word, a repeat, a sentence boundary) to decide which
# numbers belong to the same reported test, and a table row has none of that — it
# is already a delimited unit, but WHAT each column means is only as reliable as
# Grobid's own header extraction, which routinely produces empty, duplicated, or
# colspan-merged headers (confirmed against a real corpus paper: a header cell
# spanning two data columns, e.g. "Range" covering two different variables' actual
# range, gives both underlying values the SAME label). Four fallbacks are tried,
# in order, each only for what the previous one left untyped:
#   1. HEADER typing: .stat_family() on the column's own header text (the same
#      classifier the output side already uses) — precise when the header is a
#      real, unambiguous statistic name, which is the common case.
#   2. CAPTION typing: when the header is unrecognised/ambiguous, the table's own
#      caption (paper$text row for the table's section_id) is checked for a
#      recognised statistic keyword ("correlation", "regression", ...) and used
#      as a per-table fallback family — weaker than a real header (it says
#      nothing about WHICH column), so used only to fill gaps 1 left, never to
#      override a header that already resolved.
#   3. VALUE-SHAPE typing: a cell shaped like a bracketed interval ("[.16, .29]")
#      is a CI regardless of what its header says — the one shape unambiguous
#      enough to type from the value alone.
#   4. ROW-AS-TEST: every cell typed by 1-3 in one data row is bundled into ONE
#      test and matched against the output the same way match_reported_output()
#      already matches a sentence's components — requiring several to co-occur
#      at one site, never a lone number (the same coincidence guard prose testing
#      already relies on).
#   5. REPEATED-MATRIX fallback: a table structurally shaped like a symmetric
#      matrix (numbered row AND column headers, e.g. a correlation matrix) has
#      cells that are each independently meaningful — row 3's value under column
#      5 is "the correlation between variable 3 and variable 5", not part of a
#      multi-component test with its row's OTHER cells. Each such cell is instead
#      checked as its own single-value test against the output.

# Build the (family, label) the CAPTION suggests for an ambiguous column, from
# the table's own text row (paper$text, matched by section_id). NA when the
# caption names no recognised statistic keyword. Deliberately narrow: only a
# handful of table TYPES are common enough in practice to name unambiguously
# from a caption alone.
.table_caption_family <- function(caption) {
  cap <- tolower(caption %||% "")
  if (!nzchar(cap)) return(NA_character_)
  dplyr::case_when(
    grepl("correlat", cap)                       ~ "r",
    grepl("regression|coefficient|fixed.effect",  cap) ~ "b",
    grepl("reliabilit|cronbach|internal consist", cap) ~ "alpha",
    grepl("descriptive|means? and standard",      cap) ~ "mean",
    TRUE ~ NA_character_
  )
}

# The caption text for a table, looked up via paper$text (Grobid's own figDesc
# for a table's <figure> ends up there, tagged with the table's section_id — see
# .tei_text() in R/import-grobid.R) — table.contents/html carry no caption
# column of their own, so this is the only place it survives to.
.table_caption <- function(paper, section_id) {
  if (is.na(section_id) || is.null(paper$text) || !nrow(paper$text)) return(NA_character_)
  hit <- paper$text$section_id == section_id & !is.na(paper$text$section_id)
  if (!any(hit)) return(NA_character_)
  paste(paper$text$text[hit], collapse = " ")
}

# Type every cell in a table's rows (tiers 1-3): header family first, caption
# family for cells the header left untyped, then value-shape (CI) for anything
# still untyped. Returns a list of per-data-row lists of
# list(col, family, name, value, dec, censored) — one element per cell that
# resolved to SOME family; untyped cells (a genuine label column, e.g.
# "Condition") are dropped, same as .recompose_eq() drops a junk component.
.table_typed_cells <- function(content, caption) {
  if (is.null(content) || length(content) < 2) return(list())
  header <- content[[1]]
  data_rows <- content[-1]
  w <- length(header)

  # A folded multi-row header (.tei_table_contents()'s own " / "-joined
  # super-header + sub-header + column-name rows) reads like a PROSE PHRASE
  # ("Association with controllability-reappraisal slope / β11 (SE)"), and
  # .stat_family()'s unanchored catch-all patterns (e.g. bare "ci" for a
  # confidence interval) can match a substring INSIDE that prose rather than
  # the real column name — confirmed against a real corpus paper: "Asso-CI-
  # ation with..." matched the bare "ci" pattern and mistyped a β/SE cell as
  # a confidence interval. Typed on the LAST segment only (the specific
  # column name closest to the data, e.g. "β11 (SE)"), which is what a
  # single-row header would have been in the first place; earlier segments
  # (super-header context) are kept in `header` for display/caption purposes
  # only, never handed to the family classifier.
  hdr_last <- vapply(strsplit(header, " / ", fixed = TRUE),
                     function(p) if (length(p)) p[[length(p)]] else "",
                     character(1))
  hdr_fam <- vapply(hdr_last, .stat_family, character(1), USE.NAMES = FALSE)
  ambiguous <- vapply(hdr_last, .table_header_ambiguous, logical(1), USE.NAMES = FALSE)
  cap_fam <- .table_caption_family(caption)

  lapply(data_rows, function(cells) {
    n <- min(length(cells), w)
    if (n == 0) return(list())
    out <- lapply(seq_len(n), function(ci) {
      val <- trimws(cells[[ci]])
      if (!nzchar(val)) return(NULL)

      # Tier 3 (value-shape) is tried FIRST, not last, for one deliberate
      # reason: a bracketed CI is unambiguous regardless of what the header
      # says, but a header can be WRONG in a way that still resolves to SOME
      # family (e.g. a merged "Range" header sitting over a column that is
      # really a CI bound) — value-shape evidence should win over a
      # confidently-but-wrongly-typed header for this one shape, since
      # nothing else here can produce a false CI shape by accident (plain
      # numbers never contain brackets).
      ivl <- .norm_interval(val)
      if (!is.null(ivl)) {
        return(list(
          list(col = ci, family = "ci_lower", name = "ci_lower",
               value = ivl$lo$num, dec = ivl$lo$dec, censored = ""),
          list(col = ci, family = "ci_upper", name = "ci_upper",
               value = ivl$hi$num, dec = ivl$hi$dec, censored = "")))
      }

      # A header that is a real, recognisable label ("Well-being measure",
      # "Condition") is NOT ambiguous by .table_header_ambiguous()'s test
      # (blank/bare-numeric placeholder) — it is simply not a STATISTIC name,
      # which .stat_family() correctly returns NA for. The caption fallback
      # must apply ONLY to a truly ambiguous header (Grobid gave us nothing
      # to go on at all), never to a genuine, meaningful-but-non-statistical
      # label column: applying it there mistyped an entire row-label column
      # ("1. Depression", "4. Neuroticism") as the caption's guessed family
      # ("r", from a "Correlations" caption) — confirmed against a real
      # corpus paper's Table 2, whose OWN row-label column was then
      # (wrongly) scored as correlation values.
      if (!is.na(hdr_fam[ci])) fam <- hdr_fam[ci]
      else if (ambiguous[ci]) fam <- cap_fam
      else return(NULL)
      if (is.na(fam)) return(NULL)

      # A cell must look like an actual VALUE, not merely start with a digit,
      # before being typed at all — .norm_value() strips everything after the
      # first non-numeric character, so "4. Neuroticism" silently parses as
      # the number 4 without this guard (the same real bug the header check
      # above closes off for a full label COLUMN; this closes it for a stray
      # label-like cell in an otherwise-numeric column, e.g. a "-" or "n/a"
      # placeholder cell that happens to start with a digit elsewhere).
      if (!grepl("^[<>]?\\s*[-+]?[0-9.]", val)) return(NULL)

      nv <- .norm_value(val)
      if (is.na(nv$num)) return(NULL)
      # Reject a value whose STRIPPED numeric prefix is shorter than a
      # meaningful trailing remainder — the same "4. Neuroticism" shape, now
      # caught even when the surrounding column WAS typed by a real header
      # (a header can be right about the column's general type while one
      # cell is still a non-numeric placeholder or footnote marker).
      stripped <- sub("^[<>]?\\s*", "", val)
      consumed <- regmatches(stripped, regexpr("^[-+]?[0-9.,]+", stripped))
      remainder <- substr(stripped, nchar(consumed) + 1, nchar(stripped))
      if (nzchar(trimws(remainder)) &&
          grepl("[a-zA-Z]{2,}", remainder)) return(NULL)

      list(list(col = ci, family = fam,
                name = if (!ambiguous[ci]) header[[ci]] else fam,
                value = nv$num, dec = nv$dec, censored = nv$censored))
    })
    unlist(Filter(Negate(is.null), out), recursive = FALSE)
  })
}

# Detect a table shaped like a symmetric matrix (a correlation matrix being the
# common real case): the header carries a run of bare numeric column labels
# ("1.", "2.", ...) and the data rows carry the SAME numbers as a row-label
# prefix ("1. Depression", "2. Anxiety", ...). When true, the matrix cells (the
# numeric-column-labelled ones) are each independently meaningful (row i's value
# under column j is "the statistic between i and j"), not part of one
# multi-component test alongside their row's OTHER cells — tier 5 matches them
# as individual single-value tests instead. Returns the column indices (into
# `header`) that make up the matrix, or integer(0) when not matrix-shaped.
.table_matrix_cols <- function(content) {
  if (is.null(content) || length(content) < 2) return(integer(0))
  header <- content[[1]]
  # Matched on the LAST " / "-joined segment, same as the family lookup in
  # .table_typed_cells() and for the same reason: a folded multi-row header
  # can prefix the bare column-index label with super-header text ("Range /
  # Actual" for a non-matrix column, but also "Correlations / 3." for a
  # matrix one, whenever Grobid's own colspan happened to cover that
  # particular column) — matching the WHOLE folded string against
  # "^[0-9]+\\.?$" misses any matrix column a super-header happened to span,
  # confirmed against a real corpus paper's correlation matrix: its "3."
  # column (folded to "Correlations / 3.") was the only one of six silently
  # excluded from the matrix, so its row's correlations were wrongly bundled
  # into a multi-component test with that row's alpha/mean/range instead.
  last_seg <- vapply(strsplit(trimws(header %||% ""), " / ", fixed = TRUE),
                     function(p) if (length(p)) p[[length(p)]] else "",
                     character(1))
  num_cols <- which(grepl("^[0-9]+\\.?$", last_seg))
  # A real matrix needs at least 2 numbered columns to be a "grid" at all — a
  # single numbered column is more likely an ordinary row-index, not a matrix.
  if (length(num_cols) < 2) return(integer(0))
  num_cols
}

# Build match_reported_output()'s internal "test" shape (see .recompose_eq()'s
# own comment for the exact shape) from ONE table's contents: tier 4 (row-as-
# test) plus tier 5 (repeated-matrix, split out as independent single-value
# tests). `text_id` is synthesised as a NEGATIVE number unique to this
# (table_id, row) — see match_reported_output()'s own comment on why: it must
# never collide with a real paper$text row id, and result rows display it, so a
# reader can tell a table-derived row apart from a text-derived one at a glance.
.table_tests_one <- function(table_id, content, caption) {
  if (is.null(content) || length(content) < 2) return(list())
  matrix_cols <- .table_matrix_cols(content)
  typed <- .table_typed_cells(content, caption)

  tests <- list()
  for (ri in seq_along(typed)) {
    comps <- typed[[ri]]
    if (!length(comps)) next
    tid <- -(table_id * 1000000L + ri)

    in_matrix <- vapply(comps, function(c) c$col %in% matrix_cols, logical(1))
    row_comps <- comps[!in_matrix]
    matrix_comps <- comps[in_matrix]

    # Tier 4: this row's non-matrix cells, bundled as one multi-component test
    # (a descriptives row: alpha, mean/SD, range — see .table_typed_cells()).
    if (length(row_comps) > 0) {
      row_comps <- lapply(row_comps, function(c) { c$col <- NULL; c })
      tests[[length(tests) + 1L]] <- list(text_id = tid, grp_id = 1L,
                                          components = row_comps)
    }
    # Tier 5: this row's matrix cells (if any), each its OWN single-value test —
    # a correlation matrix's row 3 / column 5 cell is not evidence about the
    # SAME test as row 3's alpha or mean, so bundling them together would risk
    # a spurious co-occurrence match neither cell actually supports.
    for (mi in seq_along(matrix_comps)) {
      c <- matrix_comps[[mi]]; c$col <- NULL
      tests[[length(tests) + 1L]] <- list(
        text_id = -(table_id * 1000000L + ri * 1000L + mi), grp_id = 1L,
        components = list(c))
    }
  }
  tests
}

#' Build reported-test elements from a paper's table contents
#'
#' The table-side counterpart to [extract_tests()]/`.recompose_eq()`: for every
#' table with parsed `contents` (see `.tei_table_contents()` in
#' R/import-grobid.R), types each cell via header, caption, and value-shape (see
#' this file's header comment for the full 5-tier fallback), then bundles a
#' row's typed cells into one test — or, for a matrix-shaped table (a
#' correlation matrix), matches each matrix cell as its own independent
#' single-value test. Used by [match_reported_output()] when
#' `include_tables = TRUE`; not exported because its output shape (a plain list
#' of `list(text_id, grp_id, components)`) is match-reported.R's own internal
#' convention, not a user-facing table.
#'
#' @param paper a paper object
#'
#' @returns a list of test elements, `match_reported_output()`'s internal shape
#' @keywords internal
.table_tests <- function(paper) {
  if (is.null(paper$table) || nrow(paper$table) == 0 ||
      !"contents" %in% names(paper$table)) return(list())

  out <- lapply(seq_len(nrow(paper$table)), function(i) {
    content <- paper$table$contents[[i]]
    if (is.null(content)) return(list())
    caption <- .table_caption(paper, paper$table$section_id[[i]])
    .table_tests_one(paper$table$table_id[[i]], content, caption)
  })
  unlist(out, recursive = FALSE)
}
