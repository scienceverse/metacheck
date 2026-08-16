# Read Inquisit (.iqx) experiment scripts ------------------------------------
#
# An .iqx file is the Inquisit (Millisecond) experiment DEFINITION — the
# scripting-language source that specifies the stimuli, item wording, trial and
# block structure, and response coding. It is the instrument-definition companion
# of the .iqdat DATA files (which metacheck reads into Behaverse paradata): the
# .iqx says WHAT the task is, the .iqdat records the responses.
#
# This is a LEVEL-1 reader: it extracts the human-meaningful DESCRIPTION and the
# ITEM / STIMULUS wording, which are stored as regular `<item name> / N = "text"`
# blocks. It deliberately does NOT interpret the trial/block flow, expressions or
# conditional logic — that is a full scripting language (an Inquisit interpreter),
# fragile and low-value here. The extracted description + items are used two ways:
#   * as naming CONTEXT for the LLM instrument/scale identification (so an opaque
#     Inquisit task like "generalization_1" can be named for OSD), and
#   * to populate the Behaverse Instrument.name / Instrument.description of the
#     paired paradata file.
#
# The .iqx pairs with its .iqdat by FILENAME STEM: the .iqx basename (consent.iqx)
# is the prefix of the output data files (consent_10_2019-...iqdat). This holds
# when the researcher used script-named output (the common case); when the output
# was renamed at runtime it does not, so callers fall back to repo-level context.

#' Read an Inquisit (.iqx) experiment script
#'
#' Extracts the human-readable description and the item / stimulus wording from an
#' Inquisit `.iqx` script (its instrument definition). A level-1 reader: it does
#' not interpret the trial/block flow or expressions. Intended to give the LLM
#' scale/task identifier real content to name an otherwise-opaque Inquisit task,
#' and to fill in a paired Behaverse instrument's name/description.
#'
#' @param path path to a `.iqx` file
#'
#' @returns a list with `title` (from the script's `title:` header or leading
#'   comment, or `NA`), `description` (the leading comment block, condensed),
#'   `items` (a character vector of stimulus / item wording), `data_file` (the
#'   `.iqdat` filename declared in the `<data>` block, or `NA`), and `stem` (the
#'   `.iqx` basename without extension, the pairing key to the `.iqdat`).
#' @export
#' @keywords internal
read_iqx <- function(path) {
  if (!file.exists(path)) stop("File not found: ", path)
  txt <- .iqx_read_text(path)
  if (!nzchar(txt))
    stop("Could not read '", basename(path), "' as text.")

  lines <- strsplit(txt, "\r\n|\n|\r")[[1]]

  # Title: a "title:" line if present, else the first substantive comment line
  # (Inquisit comments are lines of / leading * or bare prose above the elements).
  title <- NA_character_
  tl <- grep("^\\s*title\\s*:", lines, ignore.case = TRUE, value = TRUE)
  if (length(tl))
    title <- trimws(sub("^\\s*title\\s*:\\s*", "", tl[[1]], ignore.case = TRUE))

  # Description: the leading comment block before the first <element>. Strip the
  # decorative asterisk rule lines and blanks; keep real prose.
  first_el <- utils::head(grep("^\\s*<[a-z]", lines, ignore.case = TRUE), 1)
  head_lines <- if (length(first_el) && first_el > 1) lines[seq_len(first_el - 1)] else character(0)
  desc <- trimws(head_lines)
  desc <- desc[nzchar(desc) & !grepl("^[*_=-]+$", desc) & !grepl("^\\*+\\s*$", desc)]
  desc <- gsub("^\\*+\\s*|\\s*\\*+$", "", desc)        # trim asterisk borders
  desc <- desc[nzchar(desc)]
  desc <- desc[!grepl("^\\s*title\\s*:", desc, ignore.case = TRUE)]  # title is separate
  if (is.na(title) && length(desc)) title <- desc[[1]]
  description <- if (length(desc)) paste(utils::head(desc, 8), collapse = " ") else NA_character_

  list(
    title       = title,
    description = description,
    items       = .iqx_items(txt),
    data_file   = .iqx_data_file(txt),
    stem        = tools::file_path_sans_ext(basename(path)))
}

# Read a .iqx as text, tolerating the UTF-16 and BOM encodings Inquisit writes.
# Reuses the same tolerant path as the E-Prime / text sniffers (text_peek reads
# the whole file when n = Inf).
.iqx_read_text <- function(path) {
  paste(text_peek(path, n = Inf), collapse = "\n")
}

# Extract stimulus / item wording from the `<item ...>` blocks. Each item block
# is `<item name> / N = "text" ... </item>`; we take the quoted values. Skips
# entries that are just references to other elements (a bare identifier with no
# spaces/punctuation) and Inquisit template tokens (<% ... %>). Returns unique,
# trimmed wording strings.
.iqx_items <- function(txt) {
  # (?s) = dotall so an item block spans its lines; [^>]* not \b after <item
  # (perl \b behaved inconsistently here). (?i) for case-insensitive tags.
  blocks <- regmatches(txt, gregexpr("(?si)<item[^>]*>.*?</item>", txt,
                                     perl = TRUE))[[1]]
  if (!length(blocks)) return(character(0))
  vals <- unlist(lapply(blocks, function(b)
    regmatches(b, gregexpr('=\\s*"([^"]*)"', b, perl = TRUE))[[1]]))
  vals <- sub('^=\\s*"', "", vals); vals <- sub('"$', "", vals)
  vals <- trimws(vals)
  # <item> block values are genuine stimuli (the element-reference tokens live in
  # <text> blocks, which we do not parse). Keep them all, dropping only empties
  # and pure Inquisit template tokens (<% ... %> with nothing else).
  keep <- nzchar(vals) & !grepl("^<%[^>]*%>$", vals)
  unique(vals[keep])
}

# The .iqdat filename declared in the `<data>` block (`/ file = "x.iqdat"`), or NA
# when the script does not name it. Note this is often unset in practice; the
# filename-stem pairing (the .iqx basename) is the more reliable link.
.iqx_data_file <- function(txt) {
  m <- regmatches(txt, regexpr('/\\s*file\\s*=\\s*"[^"]+\\.iqdat"', txt,
                               ignore.case = TRUE, perl = TRUE))
  if (!length(m)) return(NA_character_)
  fn <- sub('.*"([^"]+)".*', "\\1", m[[1]])
  basename(fn)
}
