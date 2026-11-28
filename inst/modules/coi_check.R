#' COI Check
#'
#' @description
#' Identify and extract Conflicts of Interest (COI) statements.
#'
#' @details
#' The COI Check module uses regular expressions to check sentences for words related to conflict of interest statements. It will return the sentences in which the conflict of interest statement was found.
#'
#' The function incorporates code from [rtransparent](https://github.com/serghiou/rtransparent), which is no longer maintained. For their validation, see [the paper](https://doi.org/10.1371/journal.pbio.3001107).
#'
#' @references
#' Serghiou, S., Contopoulos-Ioannidis, D. G., Boyack, K. W., Riedel, N., Wallach, J. D., & Ioannidis, J. P. (2021). Assessment of transparency indicators across the biomedical literature: How open is open?. PLoS biology, 19(3), e3001107. doi: 10.1371/journal.pbio.3001107
#'
#' Serghiou S (2025). _rtransparent: Identifies indicators of transparency_. R package version 0.2.5, commit d0d5dfe4b6c4e519d54e341436d4263fb05d84be, <https://github.com/serghiou/rtransparent>.
#'
#' @keywords general
#'
#' @author Daniel Lakens (\email{d.lakens@tue.nl})
#'
#' @param paper a paper object or paperlist object
#'
#' @returns a list
coi_check <- function(paper) {
  # nothing to check in an empty paper or list: "na", not red or green
  if (.paper_is_empty(paper)) return(.empty_input_result(paper))

  # table ----

  # get text and send to rtransparent function (defined below)
  table <- paper |>
    text_search() |>
    dplyr::select(paper_id, text) |>
    dplyr::summarise(text = rtransparent_coi(text), .by = paper_id) |>
    dplyr::filter(!is.na(text) & nzchar(text))
  # A section whose heading names a conflict of interest is the statement, with
  # its heading (see .coi_section_statement()): the words "conflict of
  # interest" are often only in the heading, above "None." or "All three
  # authors worked at Code Ocean", which the sentence search above cannot see
  # (#695, point 19: 7 Meta-Psychology papers; in mp20202601 it took a sentence
  # about "selective disclosure" from the body instead). A paper without such a
  # section keeps the sentence search's statement.
  from_section <- .coi_section_statement(paper)
  if (nrow(from_section)) {
    table <- dplyr::bind_rows(table[!table$paper_id %in% from_section$paper_id, , drop = FALSE],
                              from_section)
  }

  # The statement the article declares (a paper read from a publisher's XML
  # has it in `info$coi_statement`) wins over a search that found nothing:
  # it is the statement. `coi_source` says where the statement is from:
  # "text", "metadata" or "both".
  declared <- .coi_declared(paper)
  from_text <- unique(table$paper_id)
  only_meta <- declared[!declared$paper_id %in% from_text, , drop = FALSE]
  if (nrow(only_meta)) table <- dplyr::bind_rows(table, only_meta)

  # summary_table ----
  summary_table <- table |>
    dplyr::summarise(coi_found = TRUE, .by = paper_id)
  summary_table$coi_source <- ifelse(
    !summary_table$paper_id %in% from_text, "metadata",
    ifelse(summary_table$paper_id %in% declared$paper_id, "both", "text"))

  # traffic light ----
  tl <- ifelse(nrow(table), "green", "red")

  # report ----
  # summary_text ----
  if (tl == "green") {
    report <- c(
      "The following conflict of interest statement was detected.",
      scroll_table(.coi_short_text(table$text))
    )
    summary_text <- "A conflict of interest statement was detected."
  } else if (tl == "red") {
    report <- "No conflict of interest statement was detected. Consider adding one."
    summary_text <- "No conflict of interest statement was detected."
  }
  # a notice about another article (a correction, a retraction) reports no
  # study: without a statement it is "na", not red (see .paper_notice())
  notice <- .paper_notice(paper)
  if (nrow(summary_table)) summary_table$notice <- unname(notice[summary_table$paper_id])
  if (tl == "red" && length(notice) == 1 && !is.na(notice)) {
    tl <- "na"
    summary_text <- sprintf("No conflict of interest statement was detected; this is %s, where none is expected.", notice)
    report <- summary_text
  }

  # return list ----
  list(
    table = table,
    summary_table = summary_table,
    na_replace = list(coi_found = FALSE),
    traffic_light = tl,
    summary_text = summary_text,
    report = report
  )
}


# The statement of each paper with a section whose heading names a conflict of
# interest ("Conflict of Interest", "Competing interests", "Declaration of
# conflicting interests", "Duality of interest", "Disclosure"): the heading and
# the section's text, per paper (paper_id, text); no rows when there is none.
# The heading is compared without its spaces, as the PDF reading often splits
# its letters ("co n fli c t o f i nte r e s t"). The reading's own section type
# "conflict" is not used: it also marks "Peer review", "Open practices",
# "Acknowledgements" and "Author contributions" (#695, point 19).
.coi_section_statement <- function(paper) {
  none <- data.frame(paper_id = character(0), text = character(0))
  sec <- tryCatch(paper_table(paper, "section"), error = function(e) NULL)
  if (is.null(sec) || !nrow(sec) || !all(c("header", "section_id") %in% names(sec))) return(none)
  head_flat <- gsub("[^a-z]", "", tolower(sec$header))
  names_coi <- grepl(paste0("conflicts?of(?:competing)?interests?|competinginterests?|",
                            "declarationsof(?:conflicting)?interests?|declarationof(?:conflicting)?interests?|",
                            "dualit(?:y|ies)ofinterests?|^disclosures?(?:statement)?$"), head_flat, perl = TRUE) &
    !grepl("financialdisclosure", head_flat, fixed = TRUE)
  sec <- sec[names_coi, , drop = FALSE]
  if (!nrow(sec)) return(none)
  tx <- paper_table(paper, "text")
  out <- lapply(seq_len(nrow(sec)), function(i) {
    rows <- tx$paper_id == sec$paper_id[i] & tx$section_id %in% sec$section_id[i]
    # the section's first paragraph only: the reading often lets the section run
    # on into a publisher's notice or an appendix's tables (up to 8,592
    # characters in Ecology and Evolution)
    if ("paragraph_id" %in% names(tx) && any(rows)) {
      first_par <- min(tx$paragraph_id[rows], na.rm = TRUE)
      if (is.finite(first_par)) rows <- rows & tx$paragraph_id %in% first_par
    }
    body <- tx$text[rows]
    head <- trimws(sec$header[i] %||% "")
    # a heading of only "Disclosure(s)" holds other statements too ("Statement
    # of Inclusion", "Benefit Sharing" in Ecology and Evolution): it counts only
    # when its paragraph is about interests
    if (grepl("^disclosures?(?:statement)?$", gsub("[^a-z]", "", tolower(head)), perl = TRUE) &&
        !grepl("conflict|competing|interest|financial|funding|funded|honorari|consult|employ|shares|stock",
               paste(body, collapse = " "), ignore.case = TRUE))
      return(NULL)
    txt <- trimws(paste(c(if (nzchar(head)) head, body[!is.na(body)]), collapse = " "))
    data.frame(paper_id = sec$paper_id[i], text = txt)
  })
  if (!length(Filter(Negate(is.null), out))) return(none)
  out <- do.call(rbind, out)
  out <- out[nzchar(out$text), , drop = FALSE]
  if (!nrow(out)) return(none)
  stats::aggregate(text ~ paper_id, data = out, FUN = function(x) paste(x, collapse = " "))
}

# A statement as the report shows it: its first `max_sentences` sentences, and
# at most `max_chars` characters, with an ellipsis where text was left out.
# The table keeps the whole statement. The reading can let a statement run on
# into an appendix's tables (8,592 characters in Ecology and Evolution, one
# "sentence" of 5,977), while 96% of statements are at most three sentences
# (#695, point 19).
.coi_short_text <- function(x, max_sentences = 5, max_chars = 1000) {
  vapply(x, function(s) {
    if (is.na(s)) return(s)
    parts <- strsplit(trimws(s), "(?<=[.!?])\\s+(?=\\S)", perl = TRUE)[[1]]
    cut <- length(parts) > max_sentences
    out <- paste(utils::head(parts, max_sentences), collapse = " ")
    if (nchar(out) > max_chars) {
      out <- substr(out, 1, max_chars - 1)
      cut <- TRUE
    }
    if (cut) out <- paste0(trimws(out), "\u2026")
    out
  }, character(1), USE.NAMES = FALSE)
}

# function adapted from rtransparent
rtransparent_coi <- function(splitted) {
  conflict <- "conflict of interest"
  conflicts <- "conflicts of interest"
  competing <- "competing interest"
  disclosure <- "disclosure"
  finance <- "competing financial interest"
  declare <- "declaration of interest"
  dual <- "duality of interest"

  is_conflict <- agrep(conflict, splitted, ignore.case = T)
  is_conflicts <- agrep(conflicts, splitted, ignore.case = T)
  is_competing <- agrep(competing, splitted, ignore.case = T)
  is_disclosure <- agrep(disclosure, splitted, ignore.case = T)
  is_finance <- agrep(finance, splitted, ignore.case = T)
  is_declare <- agrep(declare, splitted, ignore.case = T)
  is_dual <- agrep(dual, splitted, ignore.case = T)

  # Exclude financial disclosures
  if (length(is_disclosure) > 0) {
    a <- grep("financial disclosure", splitted[is_disclosure], ignore.case = T, invert = T)
    is_disclosure <- is_disclosure[a]
  }

  # Exclude mentions of COI that are not COI
  # Assume if no capital C, no mention of "no", no punctuation and no reported
  # then, this is NOT a mention of COI.
  the_conflicts <- unique(c(is_conflict, is_conflicts, is_competing))

  if (length(the_conflicts) > 0) {
    for (j in seq_along(the_conflicts)) {
      # No capital C
      is_capital_1 <- agrepl("Conflict", splitted[the_conflicts[j]])
      is_capital_2 <- agrepl("CONFLICT", splitted[the_conflicts[j]])
      is_capital_3 <- agrepl("Compet", splitted[the_conflicts[j]])
      is_capital_4 <- agrepl("COMPET", splitted[the_conflicts[j]])
      is_capital <- any(c(is_capital_1, is_capital_2, is_capital_3, is_capital_4))
      # No mention of "no"
      is_no <- agrepl("no.{0,20}conflict", splitted[the_conflicts[j]], ignore.case = T)
      # No mention of authors (needed for 0245)
      is_author <- agrepl("author", splitted[the_conflicts[j]], ignore.case = T)
      # No punctuation after interests
      is_punctuation <-
        agrepl("interest.{0,1}[.:;,]", splitted[the_conflicts[j]], ignore.case = T)
      # No mention of reporting after interests
      is_report <- grepl("disclosed|reported|mentioned|declared|communicated|revealed|divulged|aired|voiced|expressed", splitted[the_conflicts[j]], ignore.case = T)

      if (!is_capital & !is_no & !is_punctuation & !is_report & !is_author) {
        the_conflicts[j] <- NA
      }
    }
    the_conflicts <- the_conflicts[!is.na(the_conflicts)]
  }

  index <- unique(c(the_conflicts, is_disclosure, is_finance, is_declare, is_dual)) |> sort()

  # Try to protect from mentions of words in other sections of text
  # If a hit ends with interest or starts with Disclosure, more likely to be COI
  vals <- c("interest.{0,1}:|Disclosure.{0,1}:")
  if (length(index) > 1) {
    a <- grep(vals, splitted[index], ignore.case = T)

    if (length(a) > 0) { # only do this if a hit was found
      index <- index[index >= index[min(a)]]
    }
  }

  # If in point form, sections are missed when they do not include the keywords.
  if (length(index) > 1 & any(diff(index) > 1)) {
    if (max(index) - min(index) < 10) { # Safeguard
      index <- seq(min(index), max(index), 1)
    }
  }

  coi_text <- splitted[index] |>
    unlist() |>
    paste(collapse = " ")

  # Identify text that may have been  missed because it was in a new line
  if (length(index) == 1) {
    no_stop_words <- gsub(" of ", " ", coi_text, ignore.case = T)
    if (length(strsplit(no_stop_words, " ")[[1]]) < 4) {
      # Only lines that exist are read: a heading as the last sentence made
      # nchar(NA) the condition, which stopped the module, and a read past
      # the end pasted "NA" into the statement (U97). The heading is then
      # used as found.
      n_lines <- length(splitted)
      if (!grepl("no", splitted[index], ignore.case = T) && index < n_lines) {
        if (nchar(splitted[index + 1]) == 0 && index + 2 <= n_lines) {
          index <- c(index, index + 2)
        } else {
          index <- c(index, index + 1)
        }
        new_str <- gsub("^.+(None.*$)", "\\1", splitted[index[2]])
        if (grepl("^.*\\.$", new_str) || index[2] >= n_lines) {
          # make sure this is a whole sentence
          coi_text <- paste(coi_text, new_str)
        } else {
          coi_text <- paste(coi_text, new_str, splitted[index[2] + 1])
        }
      }
    }
  }

  # Exclude other mentions of disclosure that are not disclosures of interest
  # e.g. Patient information disclosure
  # If no capital D and no mention of competing/conflicts, or None
  # then false positive
  # I am doing this after the above b/c things like Disclosure/n None. would not be captured otherwise
  if (length(is_disclosure) > 0 & length(the_conflicts) == 0) {
    # Capital D
    is_capital <- grepl("Disclos|DISCLOS", coi_text)
    # Mention of conflict/competing
    # (this uses only mentions of conflict/competing interest deemed relevant)
    is_compconf <- length(the_conflicts) > 0
    # Mention negation
    is_no_1 <- grepl("None|Nothing|No|Nil", coi_text)
    is_no_2 <- grepl("NONE|NOTHING|NO|NIL", coi_text)
    is_no <- any(c(is_no_1, is_no_2))
    # Punctuation
    # (disclose. needed for 0245)
    is_punctuation <- grepl("disclosure.{0,1}[.:;,]", coi_text, ignore.case = T)
    # Mention of author
    is_author <- grepl("Author.{0,2} [dD]isclo", coi_text)

    if (!is_capital & !is_compconf & !is_no & !is_punctuation & !is_author) {
      index <- c()
      coi_text <- ""
    }
  }

  # Adjudicate presence of COI
  is_coi_pred <- length(index) > 0

  # Remove preceding text that is not relevant
  coi_text <-
    gsub( # lazy match to stop at first occurrence of a word
      "^.*?(Disclosure.*conflict.*$)|^.*?(Disclosure.*compet.*$)|^.*?(Declaration of Conflict.*$)|^.*?(Declaration of Interest.*$)|^.*?(Potential conflict.{0,1} of interest.*$)|^.*?(Conflict.*$)|^.*?(Competing.*$)", # disclosure may refer to financial disclosures
      "\\1\\2\\3\\4\\5\\6\\7",
      coi_text,
      fixed = F
    )

  # Capture disclosures that do not contain "conflict" but are relevant
  if (grepl("disclosure", coi_text, ignore.case = T)) {
    val <- "^.*conflict.*disclosure.*$|^.*compet.*disclosure.*$"
    if (!grepl(val, coi_text, ignore.case = T)) {
      coi_text <- gsub("^.*?(Disclosure.*$)", "\\1", coi_text, fixed = F)
    }
  }

  # If only None/No/Nothing appear, stop after the fullstop. One group for
  # all the endings: the replacement kept only groups 1-4 of 11, so "Nil.",
  # "No.", "None mentioned." and "Nothing to declare." became "" and the
  # statement was dropped (U95).
  coi_text <-
    gsub(
      "^(.+(None|None disclosed|None reported|None declared|None mentioned|None aired|None communicated|None revealed|Nothing to declare|No|Nil)\\.).*$",
      "\\1",
      coi_text,
      fixed = F, ignore.case = T
    )

  # Correct statements with repeating sentences (e.g. 0036, 0405)
  # 0405 not fixed b/c it contains 3 slightly different versions - hard to fix
  new <- strsplit(coi_text, "\\. {0,1}")[[1]]
  if (length(new) > 1) {
    if (all(duplicated(new)[2:length(new)])) {
      coi_text <- paste0(new[1], ".")
    }
  }

  # If 'The authors declare no competing financial interests' appears, extract.
  # (solves e.g. 0077, 0098)
  # e.g. "no competing interests exist."
  val1 <- "^.*(The author.+no.*competing.+interest.*$)"
  val2 <- "^.*(The author.+no.*conflict.+interest.*$)"
  val3 <- "^.*(None of the author.+no.*competing.+interest.*$)"
  val4 <- "^.*(None of the author.+no.*conflict.+interest.*$)"
  val5 <- "^.*(No conflict.*$)"
  val6 <- "^.*(No competing.*$)"
  vals <- paste(val1, val2, val3, val4, sep = "|")
  if (grepl(vals, coi_text)) {
    # Make sure that we are not missing the title
    new_n <- stringr::str_count(coi_text, "[cC]ompeting|[cC]onflict")
    # Identify all statements that are not preceded by a title
    # and extract everything from "The author" onwards
    if (new_n == 1 & length(c(is_disclosure, is_declare, is_dual)) == 0) {
      coi_text <- gsub(vals, "\\1\\2\\3\\4\\5\\6", coi_text)
    }

    # Do not extract info after the last such sentence, if such info exists.
    # Only do this if "author" is mentioned once.
    # (e.g. 0089, 0097, 0137, 0231)
    if (stringr::str_count(coi_text, "author") < 2) {
      val1 <- "(^.*The author.+no.*competing.+interest.*?\\.) [A-Z].*$"
      val2 <- "(^.*The author.+no.*conflict.+interest.*?\\.) [A-Z].*$"
      val3 <- "(^.*All authors.+no.*conflict.+interest.*?\\.) [A-Z].*$"
      val4 <- "(^.*Both authors.+no.*conflict.+interest.*?\\.) [A-Z].*$"
      val5 <- "(^.*No conflicts of interest.{0,12}?\\.) [A-Z].*$"
      val6 <- "(^.*No conflicting.{0,12} interest.{0,12}?\\.) [A-Z].*$"
      val7 <- "(^.*No competing.{0,12} interest.{0,12}?\\.) [A-Z].*$"
      vals <- paste(val1, val2, val3, val4, val5, val6, val7, sep = "|")
      coi_text <- gsub(vals, "\\1\\2\\3\\4\\5\\6\\7", coi_text)
    }
  }

  trimws(coi_text)
}

#' The conflict-of-interest statement an article declares
#'
#' @param paper a paper object or paperlist object
#' @returns a data frame: `paper_id` and `text`, a row per paper whose info
#'   table has a declared statement (a paper read from a publisher's XML)
#' @noRd
.coi_declared <- function(paper) {
  empty <- data.frame(paper_id = character(0), text = character(0))
  info <- tryCatch(paper_table(paper, "info"), error = function(e) NULL)
  if (!is.data.frame(info) || !all(c("paper_id", "coi_statement") %in% names(info))) return(empty)
  has <- !is.na(info$coi_statement) & nzchar(info$coi_statement)
  data.frame(paper_id = as.character(info$paper_id[has]), text = as.character(info$coi_statement[has]))
}
