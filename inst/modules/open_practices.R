#' Open Practices Check
#'
#' @description
#' This module searches for open data, code, materials, and registration statements.
#'
#' @details
#' It is much faster than the previous ODDPub version of this module, and has a lower false negative rate, but also a higher false positive rate.
#'
#' For a paper read from a publisher's XML, the summary table also gives the open-science badges the journal awarded (`badge_data`, `badge_materials`, `badge_prereg`); they are NA for a paper read from a PDF.
#'
#' @keywords general
#'
#' @author Lisa DeBruine (\email{lisa.debruine@glasgow.ac.uk})
#'
#' @import dplyr
#'
#' @param paper a paper object or paperlist object
#'
#' @returns a list
open_practices <- function(paper) {
  # nothing to check in an empty paper or list: "na", not red or green
  if (.paper_is_empty(paper)) return(.empty_input_result(paper))

  # search for words associated with shared data, code, materials or reg

  ## patterns ----
  on_request <- "(on|by)\\s+(reasonable\\s+)?request"
  availability <- c("\\bavailab",
                    "\\bsupplement",
                    "\\barchive",
                    "\\baccess",
                    "\\bshare",
                    "\\bonline\\b",
                    "\\bfound\\b",
                    "\\bfind\\b",
                    "\\bdetailed\\b",
                    "\\bsee\\b")
  repo_words <- c("http", # url
                  "repositor",
                  "archive",
                  #"\\b\\d{4}\\b", # year in a citations
                  "\\bosf\\b",
                  "open science framework",
                  "researchbox",
                  "zenodo",
                  "github",
                  "figshare",
                  "datadryad",
                  "kaggle",
                  "mendeley",
                  "ldbase",
                  "ukbiobank",
                  "ukdataservice",
                  "dataverse",
                  "clinicalstudydatarequest",
                  "ourworldindata",
                  on_request)

  data_words <- c("data")
  # a capital R only (searched separately, below): ignoring case, the
  # statistic "r(68) = .58" counted as code (U107)
  code_words <- c("\\bcode\\b",
                  "\\bsoftware\\b",
                  "\\bscript",
                  "\\banaly",
                  "\\bpython\\b")
  materials_words <- c("\\bmaterials?\\b",
                       "\\bquestionnaires?\\b",
                       "\\binstruments?\\b",
                       "\\bfigures?\\b",
                       "\\bplots?\\b")
  # omit "supplemental material"?
  prereg_words <- c("pre-?regist",
                    "aspredicted")
  data <- paper |>
    text_search(data_words) |>
    text_search(repo_words) |>
    text_search(availability)
  data$data <- TRUE

  code <- dplyr::bind_rows(text_search(paper, code_words),
                           text_search(paper, "\\bR\\b", ignore.case = FALSE)) |>
    dplyr::distinct() |>
    text_search(repo_words) |>
    text_search(availability)
  code$code <- TRUE

  materials <- paper |>
    text_search(materials_words) |>
    text_search(repo_words) |>
    text_search(availability)
  materials$materials <- TRUE

  prereg <- paper |>
    text_search(prereg_words) |>
    text_search("non-?pre-?regist", exclude = TRUE) |>
    text_search("not\\s+pre-?regist", exclude = TRUE) |>
    text_search(repo_words)
  prereg$prereg <- TRUE

  # table ----
  # put in a sensible naming scheme and order
  by <- setdiff(names(code), "code")
  table <- dplyr::full_join(data, code, by = by) |>
    dplyr::full_join(materials, by = by) |>
    dplyr::full_join(prereg, by = by)
  # replace NAs with FALSE
  table$data[is.na(table$data)] <- FALSE
  table$code[is.na(table$code)] <- FALSE
  table$materials[is.na(table$materials)] <- FALSE
  table$prereg[is.na(table$prereg)] <- FALSE

  # flag on_request
  # ignoring case, as the repository search does: "ON REQUEST" passed that
  # search but was not flagged (U107)
  table$on_request <- grepl(on_request, table$text, ignore.case = TRUE)

  # re-order by paper_id (same as paper) and text_id (inc)
  paper_ids <- paper_id(paper)
  table$paper_id <- factor(table$paper_id, paper_ids)
  table <- dplyr::arrange(table, paper_id, text_id)
  table$paper_id <- as.character(table$paper_id)

  # summary_table ----
  # sentences that only matched repo_words because of "on request"
  # are flagged in the table, but do not count as open sharing
  in_repo <- setdiff(repo_words, on_request) |>
    paste(collapse = "|") |>
    grepl(table$text, ignore.case = TRUE)

  summary_table <- table |>
    dplyr::mutate(.in_repo = in_repo) |>
    summarise(data_open = any(data & .in_repo),
              code_open = any(code & .in_repo),
              materials_open = any(materials & .in_repo),
              prereg_open = any(prereg),
              on_request = any(on_request),
              data_statements = list(unique(text[data])),
              code_statements = list(unique(text[code])),
              materials_statements = list(unique(text[materials])),
              prereg_statements = list(unique(text[prereg])),
              .by = paper_id)
  # one row per paper: a paper without a flagged sentence had none, so a
  # list with one such paper was reported as a single paper, and those
  # papers did not count as "neither" (U106)
  summary_table <- dplyr::left_join(data.frame(paper_id = paper_ids),
                                    summary_table, by = "paper_id")
  for (col in c("data_open", "code_open", "materials_open", "prereg_open", "on_request"))
    summary_table[[col]][is.na(summary_table[[col]])] <- FALSE
  for (col in c("data_statements", "code_statements", "materials_statements", "prereg_statements"))
    summary_table[[col]][vapply(summary_table[[col]], is.null, logical(1))] <- list(character(0))
  data_na <- sapply(summary_table$data_statements, length) == 0
  summary_table$data_statements[data_na] <- NA_character_
  code_na <- sapply(summary_table$code_statements, length) == 0
  summary_table$code_statements[code_na] <- NA_character_
  materials_na <- sapply(summary_table$materials_statements, length) == 0
  summary_table$materials_statements[materials_na] <- NA_character_
  prereg_na <- sapply(summary_table$prereg_statements, length) == 0
  summary_table$prereg_statements[prereg_na] <- NA_character_

  # the badges the publisher awarded ----
  # A publisher's XML declares the open-science badges of an article (Open
  # Data, Open Materials, Preregistered; Psychological Science and other
  # journals that award them): what the journal itself checked, next to what
  # the search above found in the text. TRUE or FALSE for a paper read from a
  # publisher's XML, NA for one read from a PDF, which does not say.
  badges <- .open_practices_badges(paper)
  k <- match(summary_table$paper_id, badges$paper_id)
  summary_table$badge_data <- badges$badge_data[k]
  summary_table$badge_materials <- badges$badge_materials[k]
  summary_table$badge_prereg <- badges$badge_prereg[k]
  # What the publisher declares wins over a search that found nothing: a
  # badge counts as shared data, materials or a preregistration also when no
  # sentence was found. The reverse stays as it is: a sentence without a
  # badge is the search's answer alone. `*_source` says where each answer is
  # from: "text", "metadata" (the badge) or "both"; NA when neither found it.
  source_of <- function(text, meta) {
    ifelse(text & meta %in% TRUE, "both",
           ifelse(meta %in% TRUE, "metadata", ifelse(text, "text", NA_character_)))
  }
  summary_table$data_source <- source_of(summary_table$data_open, summary_table$badge_data)
  summary_table$materials_source <- source_of(summary_table$materials_open, summary_table$badge_materials)
  summary_table$prereg_source <- source_of(summary_table$prereg_open, summary_table$badge_prereg)
  summary_table$data_open <- summary_table$data_open | summary_table$badge_data %in% TRUE
  summary_table$materials_open <- summary_table$materials_open | summary_table$badge_materials %in% TRUE
  summary_table$prereg_open <- summary_table$prereg_open | summary_table$badge_prereg %in% TRUE

  # traffic_light ----
  # summary_text ----
  if (nrow(summary_table) > 1) {
    tl <- "info"
    # summary for a paperlist
    summary_text <- sprintf(
      "%d papers shared both data and code, %d only data, %d only code, and %d neither.",
      sum(summary_table$data_open & summary_table$code_open),
      sum(summary_table$data_open & !summary_table$code_open),
      sum(!summary_table$data_open & summary_table$code_open),
      sum(!summary_table$data_open & !summary_table$code_open)
    )
  } else if (nrow(summary_table) == 0) {
      summary_text <- "Neither shared data nor code detected."
      # yellow, not red, until a validation shows people agree with most of its
      # red findings: red for 70 of 149 Meta-Psychology papers (#695, point 2)
      tl <- "yellow"
  } else {
    # summary for a single paper
    if (summary_table$data_open == TRUE &
        summary_table$code_open == TRUE) {
      summary_text <- "Shared data and code detected."
      tl <- "green"
    } else if (summary_table$data_open == TRUE &
               summary_table$code_open == FALSE) {
      summary_text <- "Shared data detected."
      tl <- "yellow"
    } else if (summary_table$data_open == FALSE &
               summary_table$code_open == TRUE) {
      summary_text <- "Shared code detected."
      tl <- "yellow"
    } else {
      summary_text <- "Neither shared data nor code detected."
      tl <- "yellow"   # see above (#695, point 2)
    }

    if (summary_table$on_request) {
      summary_text <- gsub("\\.$", "; some sharing is only on request.", summary_text)
      tl <- "yellow"   # see above (#695, point 2)
    }
  }

  # report ----
  data_report <- NULL
  code_report <- NULL
  on_request_report <- NULL

  if (nrow(summary_table) == 1) {
    # data_report
    if (!summary_table$data_open) {
      data_report <- "We did not detect open sharing of data, which could be because there is no data related to this article, or the repository is not recognized by our code. If there is data, please consider sharing it in a repository."
    } else if (summary_table$data_source %in% "metadata") {
      data_report <- "Data was openly shared for this article, according to the Open Data badge the journal awarded; we did not find a sentence that says where."
    } else {
      data_report <- sprintf(
        "Data was openly shared for this article, based on the following text:\n\n> %s",
        paste(table$text[table$data & in_repo], collapse = "\n\n> ")
      )
    }

    # code_report
    if (!summary_table$code_open) {
      code_report <- "We did not detect open sharing of code, which could be because there is no code related to this article, or the repository is not recognized by our code. If there is code, please consider sharing it in a repository."
    } else {
      code_report <- sprintf(
        "Code was openly shared for this article, based on the following text:\n\n> %s",
        paste(table$text[table$code & in_repo], collapse = "\n\n> ")
      )
    }

    # on_request
    if (summary_table$on_request) {
      on_request_report <- sprintf(
        "Some materials are shared on request, which is not an acceptable method of sharing. This is based on the following text:\n\n> %s",
        # each sentence once, in one paragraph: sprintf() over the sentences
        # repeated the whole paragraph for each one (U107)
        paste(gsub("\n\n", "\n\n> ", unique(table$text[table$on_request])),
              collapse = "\n\n> ")
      )
    }
  }

  # badges
  badge_report <- NULL
  if (nrow(summary_table) == 1) {
    got <- c("Open Data", "Open Materials", "Preregistered")[
      c(summary_table$badge_data, summary_table$badge_materials, summary_table$badge_prereg) %in% TRUE]
    if (length(got)) {
      badge_report <- sprintf("The journal awarded this article the open-science badge%s %s.",
                              plural(length(got)), paste(got, collapse = ", "))
    }
  }

  report <- c(
    data_report,
    code_report,
    on_request_report,
    badge_report
  )

  # return a list ----
  list(
    table = table,
    summary_table = summary_table,
    na_replace = list(data_open = FALSE, code_open = FALSE),
    traffic_light = tl,
    summary_text = summary_text,
    report = report
  )
}

#' The open-science badges a publisher's XML declares
#'
#' From the paper's subject table (bibr export schema 12.6), where the terms
#' of the scheme "open-science-badge" are the badges the journal awarded.
#'
#' @param paper a paper object or paperlist object
#' @returns a data frame with a row per paper: `paper_id`, `badge_data`,
#'   `badge_materials` and `badge_prereg` (TRUE or FALSE for a paper with a
#'   subject table, NA for a paper without one)
#' @noRd
.open_practices_badges <- function(paper) {
  ids <- paper_id(paper)
  out <- data.frame(paper_id = ids, badge_data = NA, badge_materials = NA, badge_prereg = NA)
  papers <- if (.is_paper_list(paper)) paper else list(paper)
  for (i in seq_along(papers)) {
    s <- papers[[i]]$subject
    if (!is.data.frame(s) || !all(c("scheme", "term") %in% names(s))) next
    # the badge's code ("open-data") or its name ("Open Data")
    b <- tolower(paste(s$term, s$code %||% "")[s$scheme %in% "open-science-badge"])
    out$badge_data[i] <- any(grepl("data", b))
    out$badge_materials[i] <- any(grepl("material", b))
    out$badge_prereg[i] <- any(grepl("prereg", b))
  }
  out
}
