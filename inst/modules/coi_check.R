#' COI Check
#'
#' @description
#' Identify and extract Conflicts of Interest (COI) statements.
#'
#' @details
#' The COI Check module uses regular expressions to check sentences for words related to conflict of interest statements. It will return the sentence in which the conflict of interest statement was found. The function was part of [rtransparent](https://github.com/serghiou/rtransparent), which is no longer maintained.
#'
#' The regular expressions can miss information, or falsely detect sentences as a COI statement. For the validation, see [the paper](https://doi.org/10.1371/journal.pbio.3001107).
#'
#' @keywords general
#'
#' @author Stylianos Serghiou
#'
#' @param paper a paper object or paperlist object
#'
#' @returns a list
#'
coi_check <- function(paper) {

  splitted <- paper$full_text$text
  id <- paper$id
  doi <- paper$info$doi

  conflict <- "conflict of interest"
  conflicts <- "conflicts of interest"
  competing <- "competing interest"
  disclosure <- "disclosure"
  finance <- "competing financial interest"
  declare <- "declaration of interest"
  dual <- "duality of interest"

  is_conflict = agrep(conflict, splitted, ignore.case = T)
  is_conflicts = agrep(conflicts, splitted, ignore.case = T)
  is_competing = agrep(competing, splitted, ignore.case = T)
  is_disclosure = agrep(disclosure, splitted, ignore.case = T)
  is_finance = agrep(finance, splitted, ignore.case = T)
  is_declare = agrep(declare, splitted, ignore.case = T)
  is_dual = agrep(dual, splitted, ignore.case = T)

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

  index <- unique(c(the_conflicts, is_disclosure, is_finance, is_declare, is_dual)) %>% sort()

  # Try to protect from mentions of words in other sections of text
  # If a hit ends with interest or starts with Disclosure, more likely to be COI
  vals <- c("interest.{0,1}:|Disclosure.{0,1}:")
  if (length(index) > 1) {
    a <- grep(vals, splitted[index], ignore.case = T)

    if (length(a) > 0) {  # only do this if a hit was found
      index <- index[index >= index[min(a)]]
    }
  }

  # If in point form, sections are missed when they do not include the keywords.
  if (length(index) > 1 & any(diff(index) > 1)) {
    if (max(index) - min(index) < 10) {  # Safeguard
      index <- seq(min(index), max(index), 1)
    }
  }

  coi_text <- splitted[index] %>% unlist %>% paste(., collapse = " ")

  # Identify text that may have been  missed because it was in a new line
  if (length(index) == 1) {
    no_stop_words <- gsub(" of ", " ", coi_text,  ignore.case = T)
    if (length(strsplit(no_stop_words, " ")[[1]]) < 4) {
      if (!grepl("no", splitted[index], ignore.case = T)) {
        if (nchar(splitted[index + 1]) == 0) {
          index <- c(index, index + 2)
        } else {
          index <- c(index, index + 1)
        }
        new_str <- gsub("^.+(None.*$)", "\\1", splitted[index[2]])
        if (grepl("^.*\\.$", new_str)) {
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
    gsub(  # lazy match to stop at first occurrence of a word
      "^.*?(Disclosure.*conflict.*$)|^.*?(Disclosure.*compet.*$)|^.*?(Declaration of Conflict.*$)|^.*?(Declaration of Interest.*$)|^.*?(Potential conflict.{0,1} of interest.*$)|^.*?(Conflict.*$)|^.*?(Competing.*$)",  # disclosure may refer to financial disclosures
      "\\1\\2\\3\\4\\5\\6\\7",
      coi_text,
      fixed = F
    )

  # Capture disclosures that do not contain "conflict" but are relevant
  if (grepl("disclosure", coi_text, ignore.case = T)) {
    val <- "^.*conflict.*disclosure.*$|^.*compet.*disclosure.*$"
    if (!grepl(val, coi_text, ignore.case = T)) {
      coi_text <- gsub( "^.*?(Disclosure.*$)", "\\1", coi_text, fixed = F)
    }
  }

  # If only None/No/Nothing appear, stop after the fullstop.
  coi_text <-
    gsub(
      "(^.+None\\.).*$|(^.+None disclosed\\.).*$|(^.+None reported\\.).*$|(^.+None declared\\.).*$|(^.+None mentioned\\.).*$|(^.+None aired\\.).*$|(^.+None communicated\\.).*$|(^.+None revealed\\.).*$|(^.+Nothing to declare\\.).*$|(^.+No\\.).*$|(^.+Nil\\.).*$",
      "\\1\\2\\3\\4",
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

  coi_text <- trimws(coi_text)

  table <- data.frame(id, doi, is_coi_pred, coi_text, stringsAsFactors = F)
  summary_table <- table
  # traffic light ----
  tl <- ifelse(is_coi_pred, "green", "red")

  # report ----
  if (tl == "green") {
    report <- sprintf("The following conflict of interest statement was detected: \n%s",
                      coi_text)
    summary_text <- "A conflict of interest statement was detected"
  } else if (tl == "red") {
    report <- "No conflict of interest statement was detected. Consider adding one."
    summary_text <- "No conflict of interest statement was detected"
  }

  # return list ----
  list(
    table = table,
    summary_table = summary_table,
    na_replace = 0,
    traffic_light = tl,
    summary_text = summary_text,
    report = report
  )
}


