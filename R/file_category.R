#' Categorise files
#'
#' @param contents a table with columns name, path such as from `osf_contents()`
#'
#' @returns the table with new column file_category
#' @export
#' @examples
#' contents <- c("script.R", "data.csv", "README", "codebook.csv")
#' file_category(contents)
file_category <- function(contents) {
  if (is.atomic(contents)) {
    contents <- data.frame(
      name = contents
    )
  }

  # a name that is not valid UTF-8 (a zip's old Windows name) matched nothing
  # (U76); match on a valid copy and keep contents$name as given
  name_utf8 <- .utf8_lines(as.character(contents$name))

  if (is.null(contents$filetype)) {
    file_types <- metacheck::file_types

    # Each extension is compared once with all names, as the end of the name in
    # lower case: a regular expression per extension and per name took most of
    # data_check's half hour for a repository with thousands of files
    # (10.1111/2041-210X.14428; metacheck#568). Same result: ".ext$", case ignored.
    lnm <- tolower(name_utf8)
    ends <- paste0(".", tolower(file_types$ext))
    hits <- matrix(FALSE, nrow = length(lnm), ncol = length(ends))
    for (j in seq_along(ends)) hits[, j] <- endsWith(lnm, ends[j]) %in% TRUE
    contents$filetype <- lapply(seq_along(lnm), \(i) {
      file_types$type[hits[i, ]] |>
        unique() |>
        paste(collapse = ";")
    })
  }

  nm <- name_utf8
  cat <- contents$category %||% NA
  ft <- contents$filetype

  # category is from OSF, so can be: analysis, communication, data, hypothesis, instrumentation, methods and measures, procedure, project, software, other, but mostly uncategorized (NA)

  # hard rules. Most files carry a single type, matched exactly as before. Two
  # compound cases are handled here. A statistics-package file that bundles
  # BOTH a dataset and its analyses (a .jasp / .por is typed "data;stats"):
  # classify it as DATA, since the dataset is the primary artifact and any
  # bundled analyses are recovered separately as the code file. A gzip-
  # compressed scientific data file (e.g. "sample.fasta.gz", "sample.fq.gz")
  # matches BOTH its compound data extension (file_types' "fasta.gz"/"fq.gz"
  # entries) and the bare "gz" extension (typed "archive" in the community
  # extension list this table is built from), so its own filetype is
  # "data;archive" -- classify it as DATA for the same reason as above: the
  # research data is the primary artifact, and gzip compression is a storage
  # detail, not a different kind of content. Other compound types (code;web,
  # code;exec, code;data) keep their previous behaviour (fall through to NA).
  ft_has <- function(t) grepl(paste0("\\b", t, "\\b"), ft)
  sure_class <- dplyr::case_when(
    ft == "stats" ~ "code",
    ft == "data" ~ "data",
    ft == "code" ~ "code",
    ft_has("data") & ft_has("stats") ~ "data",   # .jasp / .por: data + analyses
    ft_has("data") & ft_has("archive") ~ "data", # sample.fasta.gz / .fq.gz
  )

  is_readme <- .is_readme_name(name_utf8)

  # data
  is_data <- dplyr::case_when(
    cat == "data" ~ TRUE,
    ft == "data" ~ TRUE,
    grepl("\\bdata\\b", ft) ~ TRUE,
    grepl("data", nm, ignore.case = TRUE) ~ TRUE,
    .default = FALSE
  )

  # code
  is_code <- dplyr::case_when(
    cat == "code" ~ TRUE,
    ft == "code" ~ TRUE,
    grepl("code|script", nm, ignore.case = TRUE) ~ TRUE,
    grepl("\\bcode\\b", ft) ~ TRUE,
    grepl("\\bstats\\b", ft) ~ TRUE,
    .default = FALSE
  )

  # codebook
  # Separators are `[ _.-]?` throughout: authors write "code book", "code_book",
  # "code-book" and, in dotted export names, "Race_IAT.public.2025.codebook.csv".
  # Every branch requires a TWO-WORD compound. That is deliberate: checked against
  # 27k real repository filenames, the single generic words are all substrings of
  # ordinary data files — `meta_data.csv` and `encoding.csv` in this corpus are
  # participant and trial-level DATA, so matching bare "metadata"/"codes"/"key"
  # would steal real datasets away from data_check, which is worse than missing a
  # codebook. The three families below were each verified to hold documentation.
  is_codebook <- dplyr::case_when(
    cat == "codebook" ~ TRUE,
    grepl("code[ _.-]?book", nm, ignore.case = TRUE) ~ TRUE,
    grepl("data[ _.-]?dict", nm, ignore.case = TRUE) ~ TRUE,
    # "all40_variable key.xlsx" (136 variables), "Variable_Key.pdf".
    # NOT "label": that word describes a PROPERTY of a dataset rather than a kind
    # of document. An SPSS-style export named "...WITH.variable.labels.dat" is
    # real data CARRYING labels, and because this branch outranks the
    # format-based `sure_class` below, matching it stole a 382-column x 6344-row
    # dataset out of data_check's tabular path (data_check.R selects on
    # `data_type == "data"`) while yielding nothing as a codebook — parse_codebook
    # returned 6345 lines of raw participant rows, which then blew past
    # codebook_max_calls. Worst case is a `.sav`, our single best label source.
    grepl("var(iable)?[ _.-]?(key|list|descript)", nm, ignore.case = TRUE) ~ TRUE,
    # "repetition_texting_tm_data-legend.csv" (19 variables).
    grepl("data[ _.-]?legend", nm, ignore.case = TRUE) ~ TRUE,
    # Content-analysis coder rulebooks. These document how HUMANS coded rather
    # than what columns mean, so they rarely yield a structured table and are
    # mostly useful via the LLM text tier; kept because a content-analysis paper
    # often has no other documentation.
    grepl("coding[ _.-]?(manual|scheme|sheet)", nm, ignore.case = TRUE) ~ TRUE,
    .default = FALSE
  )

  contents$file_category <- dplyr::case_when(
    is_readme ~ "readme",
    is_codebook ~ "codebook",
    !is.na(sure_class) ~ sure_class,
    # is_code ~ "code",
    # is_data ~ "data",
    .default = NA_character_
  )

  return(contents)
}

# Whether a file name names a README. "README", "Read_me", "READ ME" and
# "DataReadMe" match anywhere in the name; the dotted form "read.me.txt"
# (#487) only as a whole word, so "spread.means.csv" is not a README. The one
# rule file_category(), repo_check and code_check all use.
.is_readme_name <- function(x) {
  grepl("read[ _-]?me", x, ignore.case = TRUE) |
    grepl("(^|[^A-Za-z])read\\.me([^A-Za-z]|$)", x, ignore.case = TRUE, perl = TRUE)
}

#' Get file Type from Extension
#'
#' @param filename the file name
#'
#' @returns a named vector of file types
#' @export
#'
#' @examples
#' filetype("script.R")
filetype <- function(filename) {
  .data <- NULL
  # strsplit() gives no extension for a name that is not valid UTF-8 (U76);
  # split a valid copy, and name the result by the names as given
  given <- filename
  filename <- .utf8_lines(as.character(filename))

  ext <- data.frame(
    id = seq_along(filename),
    ext = strsplit(filename, "\\.") |>
      sapply(\(x) x[[length(x)]]) |>
      tolower()
  )

  add_types <- ext |>
    dplyr::left_join(metacheck::file_types, by = "ext") |>
    dplyr::summarise(
      type = paste(.data$type, collapse = ";"),
      .by = c("id", "ext")
    )

  filetype <- add_types$type
  names(filetype) <- given

  return(filetype)
}
