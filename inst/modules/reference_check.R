#' Reference Check
#'
#' @description
#' This module checks references. It warns for missing DOI's, citations in the RetractionWatch database, citations that have comments on pubpeer (excluding Statcheck comments), and citations of original studies for which replication studies exist in the Replication Database.
#'
#' @author Lisa DeBruine (\email{lisa.debruin@glasgow.ac.uk}) and Daniel Lakens (\email{D.Lakens@tue.nl})
#'
#' @import dplyr httr jsonlite
#'
#' @param paper a paper object or paperlist object
#'
#' @returns report text
#'
#' @examples
#' module_run(psychsci[[129]], "reference_check")
reference_check <- function(paper) {
  # for testing: paper <- psychsci[[109]]
  refs <- paper$bib

  # If there are no rows, return immediately
  if (nrow(refs) == 0) {
    return(refs)
  }
  # Add new columns to refs
  refs$total_comments <- 0
  refs$pubpeer_url <- NA_character_
  refs$users <- NA_character_
  refs$doi_from_crossref <- 0
  refs$retraction_watch <- 0
  refs$replication_exists <- 0

  # How many references of the type 'Article' have a DOI?
  articles <- subset(refs, bibtype == "Article")
  articles <- dplyr::mutate(articles, has_doi = !is.na(doi))

  articles_with_doi <- subset(articles, !is.na(doi))
  articles_without_doi <- subset(articles, is.na(doi))

  print(paste0("Retrieving information for ", nrow(articles), " references."))
  for (i in seq_len(nrow(articles))) {
    # Ensure DOI is present
    if (is.na(articles$doi[i])) {
      doi <- get_doi(articles$ref[i])   # run only if doi is NA
      articles$doi_from_crossref[i] <- 1
      articles$doi[i] <- doi            # replace NA with the new value
    } else {
      doi <- articles$doi[i]
      articles$doi_from_crossref[i] <- 0
    }

    # Create index mapping from refs$xref_id to rows in articles_with_pubpeer
    idx <- match(refs$xref_id, articles$xref_id)
    # Assign matched totals; set non-matches to 0
    refs$doi_from_crossref <- ifelse(
      is.na(idx),
      0L,
      articles$doi_from_crossref[idx]
    )

    pubpeer_info <- get_pubpeer_comment(doi)

    # Add pubpeer info to columns
    if (!is.null(pubpeer_info)) {
      if (length(pubpeer_info$users) > 0 & any(pubpeer_info$users != "Statcheck ")){
        articles$total_comments[i] <- pubpeer_info$total_comments
        articles$pubpeer_url[i] <- pubpeer_info$url
        articles$users[i] <- paste(pubpeer_info$users, collapse = ", ")
      }
    }
  }
  articles_with_pubpeer <- subset(articles, !is.na(pubpeer_url))

  articles_without_doi <- subset(articles, doi_from_crossref == 1)

  # Keep the original DOI code available to use as label
  raw_doi <- articles_without_doi[["doi"]]

  # Build URL only for non-missing DOIs
  doi_url <- ifelse(
    !is.na(raw_doi) & raw_doi != "",
    paste0("https://doi.org/", raw_doi),
    NA_character_
  )

  # Build clickable HTML <a> tag (label = DOI), or NA if no URL
  articles_without_doi[["doi"]] <- ifelse(
    !is.na(doi_url),
    paste0('<a href="', doi_url, '" target="_blank">', raw_doi, '</a>'),
    NA_character_
  )

  # Create a reference with DOI in green (only when from Crossref)
  articles_without_doi[["ref_doi"]] <- ifelse(
    articles_without_doi[["doi_from_crossref"]] == 1 & !is.na(articles_without_doi[["doi"]]),
    paste(
      articles_without_doi[["ref"]],
      sprintf('<span style="color:green;">%s</span>', articles_without_doi[["doi"]])
    ),
    NA_character_
  )

  # Create the report string for missing doi
  if (nrow(articles_without_doi) == 0) {
    # summary_text ----
    summary_doi <- "This module only checks references classified as articles. No references to articles with a missing DOI were found."
    summary_text_doi <- "No references to articles with a missing DOI were found."
  } else {
    summary_doi <- sprintf("This module only checks references classified as articles. Out of %d references to articles in the reference list, %d have a DOI. Articles with a missing DOI were (our best guess for the correct DOI is below in green):",
      nrow(articles),
      nrow(articles_with_doi))
    summary_text_doi <- sprintf("Out of %d references to articles %d have a DOI.",
                           nrow(articles),
                           nrow(articles_with_doi))
  }
  report_table_doi <- scroll_table(articles_without_doi$ref_doi)
  guidance_doi <- c("DOI's are retrieved from crossref. Only missing DOI's with a match score > 50 are returned to have high enough accuracy.")
  report_doi <- c(summary_doi, report_table_doi, collapse_section(guidance_doi))

  # PubPeer report
  if (nrow(articles_with_pubpeer) == 0) {
    summary_pubpeer <- "No Pubpeer comments were found."
    summary_text_pubpeer <- "No Pubpeer comments were found."
    report_table_pubpeer <- ""
  } else {
    # Text with green bold count
    summary_pubpeer <- sprintf(
      "We found %d references with comments on Pubpeer. You can check out the comments by visiting the URLs below:",
      nrow(articles_with_pubpeer))
    summary_text_pubpeer <- sprintf(
      "We found %d references with comments on Pubpeer.",
      nrow(articles_with_pubpeer))
    report_table_pubpeer <- paste0(
      '<a href="', articles_with_pubpeer$pubpeer_url, '" target="_blank">',
      articles_with_pubpeer$pubpeer_url,
      '</a>'
    )
    # Create index mapping from refs$xref_id to rows in articles_with_pubpeer
    idx <- match(refs$xref_id, articles_with_pubpeer$xref_id)
    # Assign matched totals; set non-matches to 0
    refs$total_comments <- ifelse(
      is.na(idx),
      0L,
      articles_with_pubpeer$total_comments[idx]
    )
  }

  # Guidance text
  explan_pubpeer <- c(
    "Pubpeer is a platform for post-publication peer review. We have filtered out Pubpeer comments by 'Statcheck'."
  )
  # report text
  report_pubpeer <- c(
    summary_pubpeer,
    scroll_table(report_table_pubpeer),
    collapse_section(explan_pubpeer)
  ) |> paste(collapse = "\n\n")


  # Check citations to references
  FReD_data <- FReD()
  cited_replications <- FReD_data[FReD_data$doi_original %in% articles_with_doi$doi, ]

  # FReD report
  if (nrow(cited_replications) == 0) {
    summary_FReD <- "No citations to studies in the FReD replication database were found."
    summary_text_FReD <- "No citations to studies in the FReD replication database were found."
  } else {
    summary_FReD <- sprintf(
      "You have cited %d articles for which replication studies exist, and are listed in the FORRT Replication Database. Check if you are aware of the replication studies, and cite them where appropriate. The articles you cited that have been replicated:",
      nrow(cited_replications)
    )
    summary_text_FReD <- sprintf(
      "You have cited %d articles for which replication studies exist.",
      nrow(cited_replications)
    )
    # Set to 1 where refs$xref_id is in cited_retractions$xref_id
    refs$replication_exists[refs$xref_id %in% cited_retractions$xref_id] <- 1
  }

  report_table_FReD <- data.frame(
    Original = cited_replications$ref_original,
    Replication = cited_replications$ref_replication,
    stringsAsFactors = FALSE
  )
  explan_FReD <- "The FRED replication database aims to keep track of replication studies."
    report_FReD <- c(
      summary_FReD,
      scroll_table(report_table_FReD),
      collapse_section(explan_FReD)
    ) |> paste(collapse = "\n\n")

  # Check citations to retractions
  rw_data <- retractionwatch()
  cited_retractions <- articles[articles$doi %in% rw_data$doi, ]

  # rw report
  if (nrow(cited_retractions) == 0) {
    summary_rw <- "No citations to studies in the retraction watch database were found."
    summary_text_rw <- "No citations to studies in the retraction watch database were found."
  } else {
    summary_rw <- sprintf(
      "You have cited %d articles in the retraction watch database. Check if you are aware of the retracted studies. The articles you cited that are in the retraction watch database:",
      nrow(cited_retractions)
    )
    summary_text_rw <- sprintf(
      "You have cited %d articles in the retraction watch database.",
      nrow(cited_retractions)
    )
    # Set to 1 where refs$xref_id is in cited_retractions$xref_id
    refs$retraction_watch[refs$xref_id %in% cited_retractions$xref_id] <- 1
  }

    report_table_rw <- cited_retractions$ref
    explan_rw <- "The rw database aims to keep track of retractions, corrections, and expressions of concern."
    report_rw <- c(
      summary_rw,
      scroll_table(report_table_rw),
      collapse_section(explan_rw)
    ) |> paste(collapse = "\n\n")

  # traffic_light ----
  tl <- if (length(report_table_doi)|length(report_table_pubpeer)|length(report_table_FReD)|length(report_table_rw)) "info" else "na"

  report <- c(report_doi, report_pubpeer, report_FReD, report_rw)

  summary_table <- data.frame(
    id = paper$id,
    retraction_watch = sum(refs$retraction_watch == 1, na.rm = TRUE),
    replication_exists = sum(refs$replication_exists == 1, na.rm = TRUE),
    doi_missing = sum(refs$doi_from_crossref == 1, na.rm = TRUE),
    pubpeer_comments = sum(refs$total_comments)
  )

  # return a list ----
  list(
    table = refs,
    summary_table = summary_table,
    na_replace = 0,
    traffic_light = tl,
    report = report,
    summary_text = paste(summary_text_doi, summary_text_pubpeer, summary_text_FReD, summary_text_rw)
  )
}


#' Get Pubpeer Comments
#'
#' @description
#' Function that takes a DOI, and retrieves information from pubpeer related to post-publication peer review comments.
#'
#' @author  Daniel Lakens (\email{D.Lakens@tue.nl})
#'
#' @import httr jsonlite
#'
#' @param doi a doi of a paper
#'
#' @returns a dataframe with information from pubpeer
#'
#' @examples
#' get_pubpeer_comment("10.1177/0146167211398138")

# Function that takes a DOI, and retrieves information from pubpeer related to post-publication peer review comments.
get_pubpeer_comment <- function(doi) {
  url <- "https://pubpeer.com/v3/publications?devkey=PubPeerZotero"
  body <- list(
    dois = list(tolower(doi))
  )
  response <- httr::POST(
    url,
    body = jsonlite::toJSON(body, auto_unbox = TRUE),
    encode = "raw",
    httr::add_headers(`Content-Type` = "application/json;charset=UTF-8")
  )
  if (status_code(response) == 200) {
    data <- httr::content(response, as = "parsed", type = "application/json")
    if (length(data[["feedbacks"]]) > 0) {
      return(list(
        total_comments = data[["feedbacks"]][[1]][["total_comments"]],
        url = data[["feedbacks"]][[1]][["url"]],
        users = strsplit(data[["feedbacks"]][[1]][["users"]], ",\\s*")[[1]]
      ))
    } else {
      return(NULL)  # No feedback found
    }
  } else {
    return(NULL)  # Request failed
  }
}
