#' Search text
#'
#' Search the text of a paper or list of paper objects. Also works on the table results of a `search_text()` call.
#'
#' @param paper a paper object or a list of paper objects
#' @param pattern the regex pattern to search for
#' @param section the section(s) to search in
#' @param return the kind of text to return, the full sentence, paragraph, div, or section that the text is in, or just the (regex) match, or all body text for a paper (id)
#' @param ignore.case whether to ignore case when text searching
#' @param fixed logical. If TRUE, pattern is a string to be matched as is. Overrides all conflicting arguments.
#' @param perl logical. Should Perl-compatible regexps be used?
#'
#' @return a data frame of matches
#' @export
#'
#' @examples
#' filename <- demoxml()
#' paper <- read_grobid(filename)
#'
#' search_text(paper, "p\\s*(=|<)\\s*[0-9\\.]+", return = "match")
search_text <- function(paper, pattern = ".*", section = NULL,
                        return = c("sentence", "paragraph", "div",  "section", "match", "id"),
                        ignore.case = TRUE,
                        fixed = FALSE, perl = FALSE) {
  return <- match.arg(return)
  text <- NULL # hack to stop cmdcheck warning :(

  # test pattern for errors (TODO: deal with warnings + errors)
  test_pattern <- tryCatch(
    grep(pattern, "test", ignore.case = ignore.case,
         perl = perl, fixed = fixed),
    error = function(e) {
      stop("Check the pattern argument:\n", e$message, call. = FALSE)
    })

  if (is.data.frame(paper)) {
    full_text <- paper
  } else if (inherits(paper, "scivrs_paper")) {
    full_text <- paper$full_text
  } else if (is_paper_list(paper)) {
    full_text <- concat_tables(paper, "full_text")
  } else if (is.vector(paper) && is.character(paper)) {
    full_text <- data.frame(text = paper)
  } else {
    stop("The paper argument doesn't seem to be a scivrs_paper object or a list of paper objects")
  }
  # make sure
  required_cols <- c("text", "section", "header", "div", "p", "s", "id")
  missing_cols <- setdiff(required_cols, names(full_text))
  full_text[missing_cols] <- NA

  # filter full text----
  section_filter <- seq_along(full_text$section)
  if (!is.null(section))
    section_filter <- full_text$section %in% section
  ft <- full_text[section_filter, ]

  # get all rows with a match----
  match_rows <- tryCatch(
    grep(pattern, ft$text, ignore.case = ignore.case,
         perl = perl, fixed = fixed),
    error = function(e) { stop(e) },
    warning = function(w) {}
  )
  ft_match <- ft[match_rows, ]

  # add back the other parts----
  paragraph_marker <- "<~p~>"

  if (return == "sentence") {
    ft_match_all <- ft_match
  } else if (return == "match") {
    ft_match_all <- ft_match
    matches <- gregexpr(pattern, ft_match$text,
                        ignore.case = ignore.case,
                        perl = perl, fixed = fixed)
    ft_match_all$text <- regmatches(ft_match$text, matches)
    text_lens <- sapply(ft_match_all$text, length)
    rowrep <- rep(seq_along(text_lens), text_lens)
    longtext <- unlist(ft_match_all$text)
    ft_match_all <- ft_match_all[rowrep, ]
    if (is.null(longtext)) longtext <- character(0)
    ft_match_all$text <- longtext
  } else {
    # recombine paragraphs first
    pgroups <- c("section", "header", "div", "p", "id")
    ft_p <- dplyr::summarise(ft, text = paste(text, collapse = " "),
                             .by = dplyr::all_of(pgroups))

    if (return == "paragraph") {
      groups <- c("section", "header", "div", "p", "id")
    } else if (return == "div") {
      groups <- c("section", "header", "div", "id")
    } else if (return == "section") {
      groups <- c("section", "id")
    } else if (return == "id") {
      groups <- c("id")
    }

    ft_match_all <- dplyr::semi_join(ft_p, ft_match, by = groups) |>
      dplyr::summarise(text = paste(text, collapse = paragraph_marker),
                       .by = dplyr::all_of(groups))
  }

  all_cols <- names(ft)

  if (return == "match") {
    ft_match_all <- ft_match_all[, all_cols]
  } else if (nrow(ft_match_all) > 0) {
    # add back sentence and paragraph markers
    ft_match_all$text <- gsub("\\s+", " ", ft_match_all$text)
    ft_match_all$text <- gsub(" , ", ", ", ft_match_all$text)
    ft_match_all$text <- gsub(paragraph_marker, "\n\n", ft_match_all$text)
    missing_cols <- setdiff(all_cols, names(ft_match_all))
    for (mc in missing_cols) {
      ft_match_all[[mc]] <- NA
      #ft_match_all[[mc]] <- methods::as(ft_match_all[[mc]], typeof(ft[[mc]]))
    }
    ft_match_all <- ft_match_all[, all_cols]
  } else {
    # empty df with same structure
    ft_match_all <- ft[c(), ]
  }

  if (return == "match") {
    ft_match_unique <- dplyr::tibble(ft_match_all)
  } else {
    ft_match_unique <- unique(ft_match_all) |> dplyr::tibble()
  }

  return(ft_match_unique)
}
