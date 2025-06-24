#' Find AsPredicted Links in Papers
#'
#' @param paper a paper object or paperlist object
#'
#' @returns a table with the AsPredicted url in the first (text) column
#' @export
#'
#' @examples
#' aspredicted_links(psychsci)
aspredicted_links <- function(paper) {
  # Step 1: Search for "aspredicted"
  RGX_ASPREDICTED <- "/aspredicted>?\\s*\\.org"
  found_ap <- search_text(paper, RGX_ASPREDICTED)

  # fix blind.php? with x=abcdef in the next sentence
  blind <- grep("blind\\.php\\?$", found_ap$text)
  expanded <- expand_text(found_ap[blind, ], paper, plus = 1)
  found_ap$text[blind] <- expanded$expanded

  # fix space stuff
  found_ap$text <- gsub(RGX_ASPREDICTED, "/aspredicted\\.org",
                        x = found_ap$text)
  found_ap$text <- gsub("blind\\.php\\s*\\?\\s*x\\s*=\\s*",
                        "blind\\.php\\?x=", x = found_ap$text)

  # match up to ">"
  match_ap <- search_text(found_ap, "/aspredicted\\.org[^\\>]+", return = "match")

  # clean up the text
  match_ap$text <- match_ap$text |>
    gsub("\\s", "", x = _) |>
    gsub("\\.pdf.*", "\\.pdf", x = _) |> # some end in ".pdf)."
    paste0("https:/", x = _)

  # remove trailing blind links
  unique_matches <- match_ap |>
    dplyr::filter(text != "https://aspredicted.org/blind.php?") |>
    unique()

  return(unique_matches)
}

#' Retrieve info from AsPredicted by URL
#'
#' @param ap_url an AsPredicted URL, or a table containing them (e.g., as created by `aspredicted_links()`)
#' @param id_col the index or name of the column that contains AsPredicted URLs, if id is a table
#'
#' @returns a data frame of information
#' @export
aspredicted_retrieve <- function(ap_url, id_col = 1) {
  if (is.null(curl::nslookup("aspredicted.org", error = FALSE))) {
    stop("AsPredicted.org seems to be offline")
  }

  # handle list of links
  if (is.data.frame(ap_url)) {
    table <- ap_url
    id_col_name <- colnames(table[id_col])
    raw_urls <- table[[id_col]]
  } else {
    id_col_name <- "ap_url"
    raw_urls <- unique(ap_url) |> stats::na.omit()
    table <- data.frame(ap_url = raw_urls)
  }

  # remove blank, missing, duplicate, or invalid IDs
  ids <- data.frame(
    ap_url = raw_urls
  )
  ids <- ids[!is.na(ids$ap_url), , drop = FALSE] |> unique()
  valid_ids <- unique(ids$ap_url)

  if (length(valid_ids) == 0) {
    message("No valid AsPredicted links")
    return(table)
  }

  # iterate over valid IDs
  message("Starting AsPredicted retrieval for ",
          length(valid_ids), " file",
          ifelse(length(valid_ids) == 1, "", "s"),"...")

  id_info <- vector("list", length(valid_ids))
  i = 0
  captcha <- FALSE
  while (!captcha & i < length(valid_ids)) {
    i = i + 1
    ap <- aspredicted_info(valid_ids[[i]])
    if (identical(ap$error, "captcha")) captcha <- TRUE
    id_info[[i]] <- ap
  }

  info <- id_info |>
    do.call(dplyr::bind_rows, args = _) |>
    dplyr::left_join(ids, by = "ap_url")

  # reduplicate and add original table info
  by <- stats::setNames("ap_url", id_col_name)
  data <- dplyr::left_join(table, info, by = by,
                           suffix = c("", ".ap"))

  message("...AsPredicted retrieval complete!")

  return(data)
}

#' Retrieve info from AsPredicted by URL
#'
#' @param ap_url an AsPredicted URL
#'
#' @returns a data frame of information
#' @export
#' @keywords internal
aspredicted_info <- function(ap_url) {
  message("* Retrieving info from ", ap_url, "...")

  # set up return table
  obj <- data.frame(
    ap_url = ap_url
  )
  # get website
  res <- httr::GET(ap_url)

  # handle missing file
  if (res$status_code != 200) {
    warning(ap_url, " could not be found", call. = FALSE)
    obj$error <- "unfound"
    return(obj)
  }

  # Read the content with specified encoding
  html <- httr::content(res, "text", encoding = "UTF-8") |>
    xml2::read_html()

  body <- xml2::xml_find_all(html, "//body") |>
    rvest::html_text2() #xml2::xml_text()

  if (grepl("CLICK after solving captcha", body, fixed = TRUE)) {
    warning("Log in to AsPredicted to bypass the CAPTCHA", call. = FALSE)
    obj$error <- "captcha"
    return(obj)
  }

  obj$AP_title <- xml2::xml_find_first(html, "//h3 //b //i") |>
    xml2::xml_text()

  # section borders
  sections <- c(
    AP_authors = "Author(s)",
    AP_created = "Pre-registered on",
    AP_data = "1) Have any data been collected for this study already?",
    AP_hypotheses = "2) What's the main question being asked or hypothesis being tested in this study?",
    AP_key_dv = "3) Describe the key dependent variable(s) specifying how they will be measured.",
    AP_conditions = "4) How many and which conditions will participants be assigned to?",
    AP_analyses = "5) Specify exactly which analyses you will conduct to examine the main question/hypothesis.",
    AP_outliers = "6) Describe exactly how outliers will be defined and handled, and your precise rule(s) for excluding observations.",
    AP_sample_size = "7) How many observations will be collected or what will determine sample size?\nNo need to justify decision, but be precise about exactly how the number will be determined.",
    AP_anything_else = "8) Anything else you would like to pre-register?\n(e.g., secondary analyses, variables collected for exploratory purposes, unusual analyses planned?)",
    AP_version = "Version of AsPredicted Questions: ",
    bug = "Report a bug"
  )


  for (i in 1:11) {
    obj[[names(sections)[i]]] <- tryCatch({
      after <- strsplit(body, sections[[i]], fixed = TRUE)[[1]][[2]]
      answer <- strsplit(after, sections[[i + 1]], fixed = TRUE)[[1]][[1]]
      trimws(answer)
    }, error = \(e) return(NA_character_))
  }

  return(obj)
}

