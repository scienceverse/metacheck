#' Search text
#'
#' Search the text of a paper or list of paper objects. Also works on the table results of a `search_text()` call.
#'
#' @details
#' The section argument can take a vector of section names, or a PERL regular expression (use ".*" to match all sections). Possible section types are abstract, intro, method, results, discussion, references, acknowledgment, funding, endnote, footnote, table, figure, and unknown. The default includes all sections except references, tables and figures.
#'
#' @param paper a paper object or a list of paper objects
#' @param pattern the regex pattern to search for, if a vector with length > 1, the patterns will be searched separately and combined
#' @param return the kind of text to return, the full sentence, paragraph, header, or section that the text is in, or just the (regex) match, or all body text for a paper (paper_id)
#' @param ignore.case whether to ignore case when text searching
#' @param fixed logical. If TRUE, pattern is a string to be matched as is. Overrides all conflicting arguments.
#' @param perl logical. Should Perl-compatible regexps be used?
#' @param exclude should matches be included or excluded
#' @param search_header also search the header
#' @param include_refs whether to include the reference section in the search
#'
#' @return a data frame of matches
#' @export
#'
#' @examples
#' paper <- demopaper()
#' all_text <- search_text(paper)
#' study <- search_text(paper, "study")
#' equations <- search_text(paper, "\\b\\S+\\s*(=|<)\\s*[0-9\\.]+", return = "match")
#' no_numbers <- search_text(paper, "\\d", exclude = TRUE)
search_text <- function(paper, pattern = ".*",
                        return = c("sentence", "paragraph", "section", "header", "match", "paper_id"),
                        ignore.case = TRUE,
                        fixed = FALSE,
                        perl = FALSE,
                        exclude = FALSE,
                        search_header = FALSE,
                        include_refs = FALSE) {
  return <- match.arg(return)
  text <- NULL # hack to stop cmdcheck warning :(

  # iterate and combine if multiple patterns ----
  if (length(pattern) > 1) {
    matches <- lapply(pattern, \(p) {
      search_text(paper, p, return,
                  ignore.case, fixed, perl,
                  exclude, search_header, include_refs)
    })

    if (exclude) {
      # identify entries in all returned tables
      tbl <- do.call(dplyr::intersect, args = matches) |> unique()
    } else {
      # combine all returned tables and exclude duplicates
      tbl <- do.call(dplyr::bind_rows, args = matches) |> unique()
    }

    return(tbl)
  }

  # test pattern for errors (TODO: deal with warnings + errors)
  test_pattern <- tryCatch(
    grep(pattern, "test",
      ignore.case = ignore.case,
      perl = perl, fixed = fixed
    ),
    error = function(e) {
      stop("Check the pattern argument in '", pattern, "':\n",
        e$message,
        call. = FALSE
      )
    }
  )

  if (is.data.frame(paper)) {
    text <- paper
  } else if (is_paper(paper) || is_paper_list(paper)) {
    text <- paper_table(paper, "text")

    # add headers and section types
    sections <- paper_table(paper, "section")
    cols <- c("section_id", "paper_id", "header", "section_type")
    if (all(cols %in% names(sections))) {
      text <- dplyr::left_join(
        text, sections[, cols],
        by = cols[1:2],
        # TODO: figure out why this is needed
        relationship = "many-to-many" # quiet some warnings?
      )
    }
  } else if (is.vector(paper) && is.character(paper)) {
    text <- data.frame(text = paper)
  } else {
    stop("The paper argument doesn't seem to be a scivrs_paper object or a list of paper objects")
  }

  # make sure all columns exist
  required_cols <- c("text", "text_id", "section_id", "paragraph_id",
                     "paper_id", "header", "section_type")
  missing_cols <- setdiff(required_cols, names(text))
  for (m in missing_cols) {
    text[[m]] <- rep(NA, nrow(text))
  }

  if ("text" %in% missing_cols) text$text <- text[[1]]

  # filter reference section ----
  if (include_refs) {
    section_filter <- rep(TRUE, nrow(text))
  } else {
    section_filter <- !text$section_type %in% "references"
  }
  ft <- text[section_filter, ]

  # get all rows with a text match ----
  match_rows <- tryCatch(
    grepl(pattern, ft$text,
      ignore.case = ignore.case,
      perl = perl, fixed = fixed
    ),
    error = function(e) {
      stop(e)
    },
    warning = function(w) {}
  )

  # add all rows with a header match ----
  if (search_header) {
    header_match_rows <- tryCatch(
      grepl(pattern, ft$header,
        ignore.case = ignore.case,
        perl = perl, fixed = fixed
      ),
      error = function(e) {
        stop(e)
      },
      warning = function(w) {}
    )
    match_rows <- match_rows | header_match_rows
  }

  if (exclude) match_rows <- !match_rows
  ft_match <- ft[match_rows, ]

  # add back the other parts----
  paragraph_marker <- "<~p~>"

  if (return == "sentence") {
    ft_match_all <- ft_match
  } else if (return == "match") {
    ft_match_all <- ft_match
    matches <- gregexpr(pattern, ft_match$text,
      ignore.case = ignore.case,
      perl = perl, fixed = fixed
    )
    ft_match_all$text <- regmatches(ft_match$text, matches)
    text_lens <- sapply(ft_match_all$text, length)
    rowrep <- rep(seq_along(text_lens), text_lens)
    longtext <- unlist(ft_match_all$text)
    ft_match_all <- ft_match_all[rowrep, ]
    if (is.null(longtext)) longtext <- character(0)
    ft_match_all$text <- longtext
  } else {
    # recombine paragraphs first
    pgroups <- c("section_type", "header", "section_id", "paragraph_id", "paper_id")
    ft_p <- dplyr::summarise(ft,
      text = paste(text, collapse = " "),
      .by = dplyr::all_of(pgroups)
    )

    if (return == "paragraph") {
      groups <- c("section_type", "header", "section_id", "paragraph_id", "paper_id")
    } else if (return == "header") {
      groups <- c("section_type", "header", "section_id", "paper_id")
    } else if (return == "section") {
      groups <- c("section_type", "section_id", "paper_id")
    } else if (return == "paper_id") {
      groups <- c("paper_id")
    }

    ft_match_all <- dplyr::semi_join(ft_p, ft_match, by = groups) |>
      dplyr::summarise(
        text = paste(text, collapse = paragraph_marker),
        .by = dplyr::all_of(groups)
      )
  }

  all_cols <- names(ft)

  if (return == "match") {
    ft_match_all <- ft_match_all[, all_cols]
  } else if (nrow(ft_match_all) > 0) {
    # add back sentence and paragraph markers
    ft_match_all$text <- gsub("\\s+", " ", ft_match_all$text)
    ft_match_all$text <- gsub(" , ", ", ", ft_match_all$text)
    ft_match_all$text <- gsub(paragraph_marker, "\n\n", ft_match_all$text)
    missing_match_cols <- setdiff(all_cols, names(ft_match_all))
    for (mc in missing_match_cols) {
      ft_match_all[[mc]] <- NA
      # ft_match_all[[mc]] <- methods::as(ft_match_all[[mc]], typeof(ft[[mc]]))
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

  # remove all-NA columns
  # for (n in names(ft_match_unique)) {
  #   if (all(is.na(ft_match_unique[[n]]))) {
  #     ft_match_unique[[n]] <- NULL
  #   }
  # }

  # remove text if read from first column
  if ("text" %in% missing_cols) ft_match_unique$text <- NULL

  # return vector if input is vector
  if (is.atomic(paper)) ft_match_unique <- ft_match_unique$text

  return(ft_match_unique)
}
