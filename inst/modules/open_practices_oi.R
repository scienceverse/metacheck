#' Open Practices Check (Overinclusive)
#'
#' @description
#' This module searches for open data, code, materials, and registration statements.
#'
#' @details
#' New module under construction. It is much faster than the ODDPub version of this module, and has a lower false negative rate, but also a higher false positive rate.
#'
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
  # search for words associated with dhared data, code, materials or reg

  ## patterns ----
  availability <- c("\\bavailab",
                    "\\bsupplement",
                    "\\barchive",
                    "\\bfound\\b",
                    "\\bfind\\b",
                    "\\bdetailed\\b",
                    "\\bsee\\b")
  repo_words <- c("http", # url
                  "\\b\\d{4}\\b", # year in a citations
                  "osf\\.io", # OSF
                  "researchbox",
                  "repository",
                  "archive",
                  "zenodo",
                  "github",
                  "figshare")
  data_words <- c("data")
  code_words <- c("\\bcode\\b",
                  "\\bsoftware\\b",
                  "\\bscript",
                  "\\banaly",
                  "\\bR\\b",
                  "\\bpython\\b")
  materials_words <- c("\\bmaterials?\\b",
                       "\\bquestionnaires?\\b",
                       "\\binstruments?\\b")
  # omit "supplemental material"?
  prereg_words <- c("pre-?regist",
                    "aspredicted")
  data <- paper |>
    search_text(data_words) |>
    search_text(repo_words) |>
    search_text(availability)
  data$data <- TRUE

  code <- paper |>
    search_text(code_words) |>
    search_text(repo_words) |>
    search_text(availability)
  code$code <- TRUE

  materials <- paper |>
    search_text(materials_words) |>
    search_text(repo_words) |>
    search_text(availability)
  materials$materials <- TRUE

  prereg <- paper |>
    search_text(prereg_words) |>
    search_text("non-?pre-?regist", exclude = TRUE) |>
    search_text("not\\s+pre-?regist", exclude = TRUE)
  prereg$prereg <- TRUE

  # table ----
  # put in a sensible naming scheme and order
  by <- setdiff(names(code), "code")
  table <- dplyr::full_join(data, code, by = by) |>
    dplyr::left_join(materials, by = by) |>
    dplyr::left_join(prereg, by = by)
  # replace NAs with FALSE
  table$data[is.na(table$data)] <- FALSE
  table$code[is.na(table$code)] <- FALSE
  table$materials[is.na(table$materials)] <- FALSE
  table$prereg[is.na(table$prereg)] <- FALSE

  # summary_table ----
  summary_table <- table |>
    summarise(data_open = any(data),
              code_open = any(code),
              materials_open = any(materials),
              prereg_open = any(prereg),
              data_statements = list(unique(text[data])),
              code_statements = list(unique(text[code])),
              materials_statements = list(unique(text[materials])),
              prereg_statements = list(unique(text[prereg])),
              .by = paper_id)
  data_na <- sapply(summary_table$data_statements, length) == 0
  summary_table$data_statements[data_na] <- NA_character_
  code_na <- sapply(summary_table$code_statements, length) == 0
  summary_table$code_statements[code_na] <- NA_character_
  materials_na <- sapply(summary_table$materials_statements, length) == 0
  summary_table$materials_statements[materials_na] <- NA_character_
  prereg_na <- sapply(summary_table$prereg_statements, length) == 0
  summary_table$prereg_statements[prereg_na] <- NA_character_

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
      tl <- "red"
    }
  }

  # report ----
  data_report <- NULL
  code_report <- NULL

  if (nrow(summary_table) == 1) {
    # data_report
    if (!summary_table$data_open) {
      data_report <- "We did not detect open sharing of data, which could be because there is no data related to this article, or the repository is not recognized by our code. If there is data, please consider sharing it in a repository."
    } else {
      data_report <- sprintf(
        "Data was openly shared for this article, based on the following text:\n\n> %s",
        gsub("\n\n", "\n\n> ", table$text[table$data])
      )
    }

    # code_report
    if (!summary_table$code_open) {
      code_report <- "We did not detect open sharing of code, which could be because there is no code related to this article, or the repository is not reconized by our code. If there is code, please consider sharing it in a repository."
    } else {
      code_report <- sprintf(
        "Code was openly shared for this article, based on the following text:\n\n> %s",
        gsub("\n\n", "\n\n> ", table$text[table$code])
      )
    }
  }

  report <- c(
    data_report,
    code_report
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
