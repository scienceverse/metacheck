#' Search for biblio info
#'
#' @param title The title of the work
#' @param source The source (journal or book)
#' @param authors The authors
#' @param strict Whether to return NULL or the best match if there isn't a single match
#'
#' @returns A data frame with citation info
#' @export
#'
#' @examples
#' \dontrun{
#'   bibsearch("Sample Size Justification", "Collabra Psychology")
#' }
bibsearch <- function(title, source = NA, authors = NA, strict = TRUE) {
  if (is.null(email())) {
    stop("You need to set an email with email('your@address.org') to use OpenAlex")
  }

  fields <- c(
    "id",
    "doi",
    "relevance_score",
    "display_name",
    "publication_year",
    "primary_location",
    "authorships",
    "type",
    "biblio"
  ) |>
    paste0(collapse = ",")

  url <- paste0(
    "https://api.openalex.org/works?filter=title.search:",
    utils::URLencode(gsub(",", "", title)),
    "&mailto=", email(),
    "&select=", fields
  )

  j <- tryCatch(jsonlite::read_json(url),
                error = \(e) { "error" },
                warning = \(w) {
                  if (grepl("Couldn't resolve host name",
                            w$message)) return("offline")
                })

  if (is.character(j)) {
    if (j == "offline") {
      message("OpenAlex is offline")
      return(NULL)
    } else if (j == "error") {
      message("Error querying OpenAlex")
      return(NULL)
    }
  }

  if (is.null(j$results) || length(j$results) == 0) {
    if (grepl(":", title)) {
      # try partial title match
      maintitle <- strsplit(title, ":", TRUE)[[1]][[1]]
      bib <- bibsearch(maintitle, source, authors, strict)
      return(bib)
    } else {
      message("No results from OpenAlex")
      return(NULL)
    }
  }

  info <- lapply(j$results, \(res) {
    res$source <- res$primary_location$source$display_name
    res$primary_location <- NULL
    res$authors <- res$authorships |>
      sapply(\(a) a$raw_author_name) |>
      paste(collapse = "; ")
    res$authorships <- NULL
    res <- unlist(res)
  }) |> do.call(dplyr::bind_rows, args = _) |>
    dplyr::arrange(dplyr::desc(relevance_score))

  required_cols <- c("display_name", "source")
  for (rq in required_cols) {
    if (!rq %in% names(info)) info[[rq]] <- ""
  }

  info$title_match <- tolower(info$display_name) == tolower(title)
  info$source_match <- tolower(info$source) == tolower(source)
  # TODO: fuzzy match authors

  matches <- dplyr::filter(info, title_match, source_match)

  if (nrow(matches) == 1) {
    message("1 title/source match")
    return(matches)
  } else if (nrow(matches) > 1) {
    message("multiple title/source matches")
    if (strict) return(NULL)
    return(matches[1, ])
  }

  matches <- dplyr::filter(info, title_match)
  if (nrow(matches) == 1) {
    message("matches title, not source")
    if (strict) return(NULL)
    return(matches[1, ])
  } else if (nrow(matches) > 1) {
    message("multiple title matches, no source match")
    if (strict) return(NULL)
    return(matches[1, ])
  }

  message("no title/journal exact matches")
  if (strict) return(NULL)
  return(info[1, ])
}


#' Add DOIs to a bib file
#'
#' Uses OpenAlex to search for items that match the title and journal of bibtex entries that don't have a DOI and adds them in.
#'
#' @param bibfile The file path to the .bib file
#' @param save_to The file to save the results to; if NULL, saves to bibfile name with _doi appended
#' @param strict Should there be a single exact match for title and journal, if FALSE, gives the best match
#'
#' @returns a bib table in the bib2df format
#' @export
bibtex_add_dois <- function(bibfile,
                         save_to = NULL,
                         strict = TRUE) {
  if (is.null(save_to)) {
    save_to <- sub("\\.bib$", "_doi.bib", bibfile)
  }

  df <- suppressMessages(
    suppressWarnings(
      bib2df::bib2df(bibfile)
    )
  )

  if (!"DOI" %in% names(df)) df$DOI <- NA_character_

  old_doi <- df$DOI

  msgs <- character()

  df$DOI <- sapply(seq_along(df$TITLE), \(i) {
    # message(i, ": ", substr(df$TITLE[[i]], 1, 60), "...")

    if (!is.na(df$DOI[i])) {
      msgs[[i]] <<- "DOI exists"
      return(df$DOI[[i]])
    }
    if (df$CATEGORY[i] != "ARTICLE") {
      msgs[[i]] <<- "not an article"
      return(df$DOI[[i]])
    }

    withCallingHandlers(
      expr = {
        info <- bibsearch(title = df$TITLE[[i]],
                          source = df$JOURNAL[[i]],
                          authors = df$AUTHOR[[i]],
                          strict = strict)
      },
      message = function(m) {
        msgs[[i]] <<- conditionMessage(m) |>
          sub("^.{5}", "", x = _) |>
          sub(".{6}$", "", x = _)
        invokeRestart("muffleMessage") # prevents immediate printing
      }
    )

    if (is.null(info)) return(NA_character_)

    shortdoi <- sub("https://", "", info$doi, fixed = TRUE) |>
      sub("doi.org/", "", x = _, fixed = TRUE)

    return(shortdoi)
  })

  attr(df, "msgs") <- msgs

  new_doi <- df$DOI

  dois_added <- sum(!is.na(new_doi)) - sum(!is.na(old_doi))

  message(dois_added, " new DOIs added")

  bib2df::df2bib(df, save_to, )

  invisible(df)
}


#' Add DOIs to bibliography
#'
#' @param bib the bib table
#' @param strict Should there be a single exact match for title and journal, if FALSE, gives the best match
#'
#' @returns the bib table with updated DOIs
#' @export
bib_add_dois <- function(bib, strict = TRUE) {
  old_doi <- bib$doi

  msgs <- character()

  ## set up progress bar ----
  if (verbose()) {
    pb <- progress::progress_bar$new(
      total = length(bib$doi), clear = FALSE,
      format = "Processing bibentry [:bar] :current/:total :elapsedfull"
    )
    pb$tick(0)
    Sys.sleep(0.2)
    pb$tick(0)
  }

  bib$doi <- sapply(seq_along(bib$doi), \(i) {
    # message(i, ": ", substr(df$TITLE[[i]], 1, 60), "...")
    if (verbose()) pb$tick()

    if (!is.na(bib$doi[i])) {
      msgs[[i]] <<- "DOI exists"
      return(bib$doi[[i]])
    }

    withCallingHandlers(
      expr = {
        info <- bibsearch(title = bib$title[[i]],
                          source = bib$journal[[i]],
                          authors = bib$authors[[i]],
                          strict = strict)
      },
      message = function(m) {
        msgs[[i]] <<- conditionMessage(m) |>
          sub("^.{5}", "", x = _) |>
          sub(".{6}$", "", x = _)
        invokeRestart("muffleMessage") # prevents immediate printing
      }
    )

    if (is.null(info) || !"doi" %in% names(info)) return(NA_character_)

    shortdoi <- sub("https://", "", info$doi, fixed = TRUE) |>
      sub("doi.org/", "", x = _, fixed = TRUE)

    return(shortdoi)
  })

  new_doi <- bib$doi

  dois_added <- sum(!is.na(new_doi)) - sum(!is.na(old_doi))

  message(dois_added, " new DOIs added")

  return(bib)
}



