#' Find ResearchBox Links in Papers
#'
#' @param paper a paper object or paperlist object
#'
#' @returns a table with the ResearchBox url in the first (text) column
#' @export
#'
#' @examples
#' researchbox_links(psychsci)
researchbox_links <- function(paper) {
  found <- search_text(paper, "researchbox")

  # match up to ">"
  match_ap <- search_text(found, "/researchbox\\.org[^\\>]+", return = "match")

  # clean up the text
  match_ap$text <- match_ap$text |>
    paste0("https:/", x = _)

  unique_matches <- match_ap |>
    unique()

  return(unique_matches)
}

#' Retrieve info from ResearchBox by URL
#'
#' @param ap_url an ResearchBox URL, or a table containing them (e.g., as created by `researchbox_links()`)
#' @param id_col the index or name of the column that contains ResearchBox URLs, if id is a table
#'
#' @returns a data frame of information
#' @export
researchbox_retrieve <- function(rb_url, id_col = 1) {
  if (is.null(curl::nslookup("researchbox.org", error = FALSE))) {
    stop("ResearchBox.org seems to be offline")
  }

  # handle list of links
  if (is.data.frame(rb_url)) {
    table <- rb_url
    id_col_name <- colnames(table[id_col])
    raw_urls <- table[[id_col]]
  } else {
    id_col_name <- "ap_url"
    raw_urls <- unique(rb_url) |> stats::na.omit()
    table <- data.frame(rb_url = raw_urls)
  }

  # remove blank, missing, duplicate, or invalid IDs
  ids <- data.frame(
    rb_url = raw_urls
  )
  ids <- ids[!is.na(ids$rb_url), , drop = FALSE] |> unique()
  valid_ids <- unique(ids$rb_url)

  if (length(valid_ids) == 0) {
    message("No valid AsPredicted links")
    return(table)
  }

  # iterate over valid IDs
  message("Starting ResearchBox retrieval for ",
          length(valid_ids), " file",
          ifelse(length(valid_ids) == 1, "", "s"),"...")

  id_info <- vector("list", length(valid_ids))
  i = 0
  error <- FALSE
  while (!error & i < length(valid_ids)) {
    i = i + 1
    info <- researchbox_info(valid_ids[[i]])
    if ("error" %in% names(info)) error <- TRUE
    id_info[[i]] <- info
  }

  info <- id_info |>
    do.call(dplyr::bind_rows, args = _) |>
    dplyr::left_join(ids, by = "rb_url")

  # reduplicate and add original table info
  by <- stats::setNames("rb_url", id_col_name)
  data <- dplyr::left_join(table, info, by = by,
                           suffix = c("", ".rb"))

  message("...ResearchBox retrieval complete!")

  return(data)
}

#' Retrieve info from ResearchBox by URL
#'
#' @param rb_url a ResearchBox URL
#'
#' @returns a data frame of information
#' @export
#' @keywords internal
researchbox_info <- function(rb_url) {
  message("* Retrieving info from ", rb_url, "...")

  # set up return table
  obj <- data.frame(
    rb_url = rb_url
  )
  # get website
  res <- httr::GET(rb_url)

  # check if redirect
  pattern <- "(?<=window\\.location\\.replace\\(')https://researchbox.org/\\d+(?='\\))"
  if (grepl(pattern, res, perl = TRUE)) {
    matches <- gregexpr(pattern, res, perl = TRUE)
    redirect_url <- regmatches(as.character(res), matches)

    res <- httr::GET(redirect_url[[1]])
  }

  # handle missing file
  if (res$status_code != 200) {
    warning(rb_url, " could not be found", call. = FALSE)
    obj$error <- "unfound"
    return(obj)
  }

  # Read the content with specified encoding
  html <- httr::content(res, "text", encoding = "UTF-8") |>
    xml2::read_html()

  # get file list
  file_names <- xml2::xml_find_all(html, "//p [@class='file_name']") |>
    xml2::xml_text()
  filedesc <- xml2::xml_find_all(html, "//p [@class='preview_link']") |>
    xml2::xml_text()
  filedesc <- filedesc[filedesc!=""]
  file_list <- data.frame(
    name = file_names,
    description = filedesc
  )
  obj$files <- list(file_list)

  # get info from bottom table
  body <- xml2::xml_find_all(html, "//body") |>
    rvest::html_text2() #xml2::xml_text()

  # section borders
  sections <- c(
    RB_target = "SUPPLEMENTARY FILES FOR",
    RB_license = "LICENSE FOR USE",
    RB_public = "BOX PUBLIC SINCE",
    RB_authors = "BOX CREATORS",
    RB_abstract = "ABSTRACT",
    done = "$('.file_number')"
  )

  for (i in 1:5) {
    obj[[names(sections)[i]]] <- tryCatch({
      after <- strsplit(body, sections[[i]], fixed = TRUE)[[1]][[2]]
      answer <- strsplit(after, sections[[i + 1]], fixed = TRUE)[[1]][[1]]
      trimws(answer)
    }, error = \(e) return(NA_character_))
  }

  return(obj)
}
