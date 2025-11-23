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
  articles <- articles %>%
    mutate(has_doi = !is.na(doi))
  
  articles_with_doi <- subset(articles, !is.na(doi))
  articles_without_doi <- subset(articles, is.na(doi))
  
  # Create the report string
  if (nrow(articles_without_doi) == 0) {
    report_doi <- "No references to articles with a missing DOI were found."
  } else {
  doi_report <- sprintf(
    "Out of %d references to articles in the reference list, %d have a DOI. Note that we do not check the DOI for references not classified as an article. Articles with missing references were:",
    nrow(articles),
    nrow(articles_with_doi)
  )
  issues_doi_found <- paste(sprintf("**%s**", articles_without_doi$ref), collapse = "\n\n")
  
  report_doi <- sprintf(
    "%s\n\n%s",
    doi_report, issues_doi_found)
  }
  
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
      if (length(pubpeer_info$users) > 0 && any(pubpeer_info$users != "Statcheck ")){
        articles$total_comments[i] <- pubpeer_info$total_comments
        articles$pubpeer_url[i] <- pubpeer_info$url
        articles$users[i] <- paste(pubpeer_info$users, collapse = ", ")
      }
    }
  }
  articles_with_pubpeer <- subset(articles, !is.na(pubpeer_url))
  
  # Create the report string
  if (nrow(articles_with_pubpeer) == 0) {
    report_pubpeer <- "\n\n#### Pubpeer\n\n No Pubpeer comments were found.\n\n"
  } else {
  pubpeer_report <- sprintf(
    "\n\n#### Pubpeer\n\nWe found %d references with comments on Pubpeer. You can check out the comments by visiting the URLs below:",
    nrow(articles_with_pubpeer)  )
  issues_pubpeer_found <- paste(sprintf("**%s**", articles_with_pubpeer$pubpeer_url), collapse = "\n\n")
  report_pubpeer <- sprintf(
    "%s\n\n%s\n\n",
    pubpeer_report, issues_pubpeer_found)
  }
  
  # Check citations to references
  FReD_data <- FReD()
  # for testing: paper <- psychsci[[109]]
  cited_replications <- FReD_data[FReD_data$doi_original %in% articles_with_doi$doi, ]
  # Create the report string
  if (nrow(cited_replications) == 0) {
    report_FReD <- "\n\n#### Citations to Replicated Studies\n\n No citations to studies in the FReD replication database were found."
  } else {
    FRED_report <- sprintf(
      "\n\n#### Citations to Replicated Studies\n\nYou have cited  %d articles for which replication studies exist, and are listed in the FORRT Replication Database. Check if you are aware of the replications studies, and cite them where appropriate.",
      nrow(cited_replications)
    )
    replicated_FReD_found <- paste(sprintf("**%s**", cited_replications$ref_original), collapse = "\n\n")
    replication_FReD_found <- paste(sprintf("**%s**", cited_replications$ref_replication), collapse = "\n\n")
    
    report_FReD <- sprintf(
      "%s\n\n#### The replicated articles were cited:\n\n%s\n\n#### These studies have replicated studies in papers you have cited:\n\n%s",
      FRED_report, replicated_FReD_found, replication_FReD_found)
  }
  
  # Check citations to retractions
  rw_data <- retractionwatch()
  # for testing: paper <- psychsci[[109]]
  cited_retractions <- rw_data[rw_data$doi %in% articles_with_doi$doi, ]
  # Create the report string
  if (nrow(cited_retractions) == 0) {
    report_rw <- "\n\n#### Citations to Retracted Articles\n\n No citations to articles in the RetractionWatch database were found."
  } else {
    rw_report <- sprintf(
      "\n\n#### Citations to Retracted Articles\n\nYou have cited  %d articles in the RetractionWatch Database. Check if you are aware of the retractions.",
      nrow(cited_retractions)
    )
    retractions_rw_found <- paste(sprintf("**%s**", cited_retractions$retractionwatch), collapse = "\n\n")

    report_rw <- sprintf(
      "%s\n\n#### The retracted articles were cited:\n\n%s\n\n",
      rw_report, retractions_rw_found)
  }

  report <- paste(report_doi, report_pubpeer, report_FReD, report_rw)
  
  tl <- "yellow"
    
  # return a list ----
  list(
    traffic_light = tl,
    report = report
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
  response <- POST(
    url,
    body = toJSON(body, auto_unbox = TRUE),
    encode = "raw",
    add_headers(`Content-Type` = "application/json;charset=UTF-8")
  )
  if (status_code(response) == 200) {
    data <- content(response, as = "parsed", type = "application/json")
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
