#' Funding Check (Overinclusive)
#'
#' @description
#' Identify and extract funding statements.
#'
#' @details
#' The Funding Check module uses regular expressions to check sentences for words related to funding statements. It will return the sentences in which the conflict of interest statement was found.
#'
#' The function is based on code from [rtransparent](https://github.com/serghiou/rtransparent), which is no longer maintained. For their validation, see [the paper](https://doi.org/10.1371/journal.pbio.3001107).
#'
#' This version is over-inclusive, so will have more false positives, but is less likely to miss something.
#'
#' @references
#' Serghiou, S., Contopoulos-Ioannidis, D. G., Boyack, K. W., Riedel, N., Wallach, J. D., & Ioannidis, J. P. (2021). Assessment of transparency indicators across the biomedical literature: How open is open?. PLoS biology, 19(3), e3001107. doi: 10.1371/journal.pbio.3001107
#'
#' Serghiou S (2025). _rtransparent: Identifies indicators of transparency_. R package version 0.2.5, commit d0d5dfe4b6c4e519d54e341436d4263fb05d84be, <https://github.com/serghiou/rtransparent>.
#'
#' @keywords general
#'
#' @import dplyr
#'
#' @author Daniel Lakens (\email{d.lakens@tue.nl})
#' @author Lisa DeBruine (\email{lisa.debruine@glasgow.ac.uk})
#'
#' @param paper a paper object or paperlist object
#'
#' @returns a list
#'
funding_check <- function(paper) {
  # table ----

  # get all sentences with both funding and study terms
  pattern_fund <- c(
    "funder",
    "funded",
    "funding",
    "financed",
    "support"
  )
  pattern_study <- c(
    "work",
    "study",
    "studies",
    "research",
    "manuscript",
    "collaboration",
    "paper",
    "article",
    "project",
    "program",
    "grant",
    "award",
    "fellowship",
    "scholarship",
    "stipend",
    # other common words
    "none",
    "author",
    "declare",
    "thank",
    "acknowledge"
  )

  # if more than 1 per ID, favour those in specific sections
  likely_section <- c("funding", "annex", "acknowledgement")

  table <- paper |>
    search_text(pattern_fund) |>
    search_text(pattern_study) |>
    # merge the text by section/id
    # search_text(return = "section") |>
    dplyr::filter(
      !any(section %in% likely_section) |
        section %in% likely_section,
      .by = id
    )


  # summary_table ----
  summary_table <- dplyr::summarise(table, funding_found = TRUE, .by = id)

  # traffic light ----
  tl <- ifelse(nrow(table), "green", "red")

  # report ----
  # summary_text ----
  if (tl == "green") {
    report <- c(
      "The following funding statement was detected:",
      scroll_table(table$text)
    )
    summary_text <- "A funding statement was detected."
  } else if (tl == "red") {
    report <- "No funding statement was detected. Consider adding one."
    summary_text <- "No funding statement was detected."
  }

  # return list ----
  list(
    table = table,
    summary_table = summary_table,
    na_replace = FALSE,
    traffic_light = tl,
    summary_text = summary_text,
    report = report
  )
}
