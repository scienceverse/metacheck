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
  refs$total_comments <- NA_integer_
  refs$pubpeer_url <- NA_character_
  refs$users <- NA_character_
  refs$doi_from_crossref <- NA_character_
  refs$retraction_watch <- NA_character_

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

  # Create a reference with DOI in green
  articles_without_doi$doi <- paste0("<https://doi.org/",articles_without_doi$doi,">")
  articles_without_doi$ref_doi <- ifelse(
    articles_without_doi$doi_from_crossref == 1 & !is.na(articles_without_doi$doi),
    paste(articles_without_doi$ref, sprintf('<span style="color:green;">%s</span>',
                                articles_without_doi$doi)),
    NA_character_
  )

  # Create the report string for missing doi
  if (nrow(articles_without_doi) == 0) {
    report_doi <- "This module only checks references classified as articles. No references to articles with a missing DOI were found."
  } else {
    doi_report <- sprintf(
      paste0(
        "This module only checks references classified as articles. Out of <strong><span style='color:#006400;'>%d</span></strong> references to articles in the reference list, ",
        "<strong><span style='color:#006400;'>%d</span></strong> have a DOI. "),
      nrow(articles),
      nrow(articles_with_doi)
    )
    missing_heading <- "<div><strong>Articles with a missing DOI were (our best guess for the correct DOI is below in green):</strong></div>"

    issues_doi_found <- paste(
      sprintf(
        "<li style='border-bottom:1px solid #ddd; padding-bottom:6px; margin-bottom:6px;'>%s</li>",
        articles_without_doi$ref_doi),
      collapse = "\n"
    )

    refs_block <- paste0(
      "<div style='border:1px solid #ccc; padding:10px; ",
      "max-height:250px; overflow-y:auto; background-color:#f9f9f9; ",
      "margin-top:5px; margin-bottom:15px;'>",
      "<ul style='list-style-type: circle; padding-left:20px; margin:0;'>",
      issues_doi_found,
      "</ul>",
      "</div>"
    )

    report_doi <- sprintf(
      "%s\n\n%s\n\n%s",
      doi_report,
      missing_heading,
      refs_block
    )
  }

  # PubPeer report (scrollable layout) ----
  if (nrow(articles_with_pubpeer) == 0) {
    # Even if there is nothing to show, I keep a collapsible section -> UI stays sonsistent across papers
    report_pubpeer <- paste0(
      "<strong><span style='font-size:20px; color:#006400;'>Pubpeer</span></strong>",
      "</summary>",
      "<div style='margin-top:10px;'>",
      "No Pubpeer comments were found.",
      "</div>",
      "</details>"
    )
  } else {
    # Text with green bold count
    pubpeer_report <- sprintf(
      paste0(
        "We found <strong><span style='color:#006400;'>%d</span></strong> references with comments on Pubpeer. ",
        "You can check out the comments by visiting the URLs below:"
      ),
      nrow(articles_with_pubpeer)
    )

    # I show each Pubpeer URL as a bullet with a separator line
    issues_pubpeer_found <- paste(
      sprintf(
        "<li style='border-bottom:1px solid #ddd; padding-bottom:6px; margin-bottom:6px;'>
           <a href='%s' target='_blank'>%s</a>
         </li>",
        articles_with_pubpeer$pubpeer_url,
        articles_with_pubpeer$pubpeer_url
      ),
      collapse = "\n"
    )

    # I put the URL list in a scrollable box with bullets
    pubpeer_box <- paste0(
      "<div style='border:1px solid #ccc; padding:10px; ",
      "max-height:250px; overflow-y:auto; background-color:#f9f9f9; ",
      "margin-top:5px; margin-bottom:15px;'>",

      "<ul style='list-style-type: circle; padding-left:20px; margin:0;'>",
      issues_pubpeer_found,
      "</ul>",

      "</div>"
    )

    # I wrapped here the PubPeer content in a collapsible block -> users can expand it when they want to view its content
    report_pubpeer <- paste0(
      "<strong><span style='font-size:20px; color:#006400;'>Pubpeer</span></strong>",
      "</summary>",
      "<div style='margin-top:10px;'>",
      pubpeer_report, "<br><br>",
      pubpeer_box,
      "</div>",
      "</details>"
    )
  }

  # Check citations to references
  FReD_data <- FReD()
  # for testing: paper <- psychsci[[109]]
  cited_replications <- FReD_data[FReD_data$doi_original %in% articles_with_doi$doi, ]

  # Citations to Replicated Studies layout ---
  # Similar steps and layout as PubPeer
  if (nrow(cited_replications) == 0) {
    report_FReD <- paste0(
      "<strong><span style='font-size:20px; color:#006400;'>Citations to Replicated Studies</span></strong>",
      "</summary>",
      "<div style='margin-top:10px;'>",
      "No citations to studies in the FReD replication database were found.",
      "</div>",
      "</details>"
    )
  } else {
    FRED_report <- sprintf(
      paste0(
        "You have cited <strong><span style='color:#006400;'>%d</span></strong> articles for which replication studies exist, ",
        "and are listed in the FORRT Replication Database. Check if you are aware of the replication studies, ",
        "and cite them where appropriate."
      ),
      nrow(cited_replications)
    )

    # Original articles cited ----
    replicated_items <- paste(
      sprintf(
        "<li style='border-bottom:1px solid #ddd; padding-bottom:6px; margin-bottom:6px;'>%s</li>",
        cited_replications$ref_original
      ),
      collapse = "\n"
    )

    replicated_box <- paste0(
      "<div style='border:1px solid #ccc; padding:10px; ",
      "max-height:250px; overflow-y:auto; background-color:#f9f9f9; ",
      "margin-top:5px; margin-bottom:15px;'>",
      "<ul style='list-style-type: circle; padding-left:20px; margin:0;'>",
      replicated_items,
      "</ul>",
      "</div>"
    )

    # Replication studies ----
    replication_items <- paste(
      sprintf(
        "<li style='border-bottom:1px solid #ddd; padding-bottom:6px; margin-bottom:6px;'>%s</li>",
        cited_replications$ref_replication
      ),
      collapse = "\n"
    )

    replication_box <- paste0(
      "<div style='border:1px solid #ccc; padding:10px; ",
      "max-height:250px; overflow-y:auto; background-color:#f9f9f9; ",
      "margin-top:5px; margin-bottom:15px;'>",
      "<ul style='list-style-type: circle; padding-left:20px; margin:0;'>",
      replication_items,
      "</ul>",
      "</div>"
    )

    report_FReD <- paste0(
      "<strong><span style='font-size:20px; color:#006400;'>Citations to Replicated Studies</span></strong>",
      "</summary>",
      "<div style='margin-top:10px;'>",
      FRED_report,
      "<br><br><div><strong>The articles you cited that has been replicated:</strong></div>",
      replicated_box,
      "<div><strong>The reference to the replication study:</strong></div>",
      replication_box,
      "</div>",
      "</details>"
    )
  }


  # Check citations to retractions
  rw_data <- retractionwatch()
  # for testing: paper <- psychsci[[109]]
  cited_retractions <- articles[articles$doi %in% rw_data$doi, ]

  # Citations to Retracted Articles (Retractionwatch) layout ---
  # Same layout as before as well!
  if (nrow(cited_retractions) == 0) {
    report_rw <- paste0(
      "<strong><span style='font-size:20px; color:#006400;'>Citations to Retracted Articles</span></strong>",
      "</summary>",
      "<div style='margin-top:10px;'>",
      "No citations to articles in the RetractionWatch database were found.",
      "</div>",
      "</details>"
    )
  } else {
    rw_report <- sprintf(
      paste0(
        "You have cited <strong><span style='color:#006400;'>%d</span></strong> articles in the RetractionWatch Database. ",
        "Check if you are aware of the retraction."
      ),
      nrow(cited_retractions)
    )

    retractions_rw_items <- paste(
      sprintf(
        "<li style='border-bottom:1px solid #ddd; padding-bottom:6px; margin-bottom:6px;'>%s</li>",
        cited_retractions$ref
      ),
      collapse = "\n"
    )

    retractions_box <- paste0(
      "<div style='border:1px solid #ccc; padding:10px; ",
      "max-height:250px; overflow-y:auto; background-color:#f9f9f9; ",
      "margin-top:5px; margin-bottom:15px;'>",
      "<ul style='list-style-type: circle; padding-left:20px; margin:0;'>",
      retractions_rw_items,
      "</ul>",
      "</div>"
    )

    report_rw <- paste0(
      "<strong><span style='font-size:20px; color:#006400;'>Citations to Retracted Articles</span></strong>",
      "</summary>",
      "<div style='margin-top:10px;'>",
      rw_report,
      "<br><br><div><strong>The retracted articles were cited:</strong></div>",
      retractions_box,
      "</div>",
      "</details>"
    )
  }

  report <- paste(report_doi, report_pubpeer, report_FReD, report_rw)

  tl <- "yellow"

  # return a list ----
  list(
    traffic_light = tl,
    report = report,
    table = refs,
    summary_table = refs
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
