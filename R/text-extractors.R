#' Extract URLs
#'
#' Get a table of URLs from a paper or paperlist. Matches urls that start with http or doi:
#'
#' @param paper a paper object or paperlist object
#'
#' @returns a table
#' @export
#'
#' @examples
#' paper <- demopaper()
#' urls <- extract_urls(paper)
extract_urls <- function(paper) {
  pattern <- "\\b((?:doi:)?(?:https?://)?(?:(?:www\\.)?(?:[\\da-z\\.-]+)\\.(?:[a-z]{2,6})|(?:(?:25[0-5]|2[0-4][0-9]|[01]?[0-9][0-9]?)\\.){3}(?:25[0-5]|2[0-4][0-9]|[01]?[0-9][0-9]?)|(?:(?:[0-9a-fA-F]{1,4}:){7,7}[0-9a-fA-F]{1,4}|(?:[0-9a-fA-F]{1,4}:){1,7}:|(?:[0-9a-fA-F]{1,4}:){1,6}:[0-9a-fA-F]{1,4}|(?:[0-9a-fA-F]{1,4}:){1,5}(?::[0-9a-fA-F]{1,4}){1,2}|(?:[0-9a-fA-F]{1,4}:){1,4}(?::[0-9a-fA-F]{1,4}){1,3}|(?:[0-9a-fA-F]{1,4}:){1,3}(?::[0-9a-fA-F]{1,4}){1,4}|(?:[0-9a-fA-F]{1,4}:){1,2}(?::[0-9a-fA-F]{1,4}){1,5}|[0-9a-fA-F]{1,4}:(?:(?::[0-9a-fA-F]{1,4}){1,6})|:(?:(?::[0-9a-fA-F]{1,4}){1,7}|:)|fe80:(?::[0-9a-fA-F]{0,4}){0,4}%[0-9a-zA-Z]{1,}|::(?:ffff(?::0{1,4}){0,1}:){0,1}(?:(?:25[0-5]|(?:2[0-4]|1{0,1}[0-9]){0,1}[0-9])\\.){3,3}(?:25[0-5]|(?:2[0-4]|1{0,1}[0-9]){0,1}[0-9])|(?:[0-9a-fA-F]{1,4}:){1,4}:(?:(?:25[0-5]|(?:2[0-4]|1{0,1}[0-9]){0,1}[0-9])\\.){3,3}(?:25[0-5]|(?:2[0-4]|1{0,1}[0-9]){0,1}[0-9])))(?::[0-9]{1,4}|[1-5][0-9]{4}|6[0-4][0-9]{3}|65[0-4][0-9]{2}|655[0-2][0-9]|6553[0-5])?(?:/[\\w\\.-]*)*/?)\\b"
  # simplified version without strict IPv4 validation or IPv6 support
  # these are very unlikely to be in papers
  pattern <- "\\b((doi:)?(https?://)?(([\\w.-]+\\.[a-z]{2,})|(\\d{1,3}(\\.\\d{1,3}){3}))(:\\d+)?(/[^\\s]*)?)\\b"
  table <- search_text(paper, pattern, return = "match", "perl" = TRUE)

  return(table)
}


#' Extract P-Values
#'
#' List all p-values in the text, returning the matched text (e.g., 'p = 0.04') and document location in a table.
#'
#' @details
#' Note that this will not catch p-values reported like "the p-value is 0.03" because that results in a ton of false positives when papers discuss p-value thresholds. If you need to detect text like that, use the `search_text()` function and a custom pattern.
#'
#' This will catch most comparators like =<>~ and most versions of scientific notation like 5.0 x 10^-2 or 5.0e-2. If you find any formats that are not correctly handled by this function, please contact the author.
#'
#' @param paper a paper object or paperlist object
#'
#' @returns a table
#' @export
#'
#' @examples
#' paper <- demopaper()
#' p_values <- extract_p_values(paper)
extract_p_values <- function(paper) {
  # set up pattern
  operators <- c(
    "=", "<", ">", "~",
    "\u2248", # ~~
    "\u2260", # !=
    "\u2264", # <=
    "\u2265", # >=
    "\u226A", # <<
    "\u226B" # >>
  ) |> paste(collapse = "")

  pattern <- paste0(
    "\\bp-?(value)?\\s*", # ways to write p
    "[", operators, "]{1,2}\\s*", # 1-2 operators
    "(n\\.?s\\.?|\\d?\\.\\d+)", # ns or valid numbers
    "\\s*(e\\s*-\\d+)?", # also match scientific notation
    "(\\s*[x\\*]\\s*10\\s*\\^\\s*-\\d+)?"
  )

  p <- search_text(paper, pattern,
    return = "match",
    perl = TRUE, ignore.case = FALSE
  )

  # get operator
  pattern <- paste0("[", operators, "]{1,2}")
  matches <- gregexpr(pattern, p$text, perl = TRUE)
  p$p_comp <- regmatches(p$text, matches) |> sapply(`[[`, 1)

  # get value
  s <- strsplit(p$text, paste0("\\s*[", operators, "]{1,2}\\s*"))
  pvals <- sapply(s, \(x) x[[2]]) |>
    gsub("\\s", "", x = _) |>
    gsub("[x*]10\\^", "e", x = _)
  p$p_value <- suppressWarnings(as.numeric(pvals))

  return(p)
}


#' Extract P-Values
#'
#' List all equations in the text, returning the matched text (e.g., 'p = 0.04') and document location in a table.
#'
#' @details
#' Note that this will not catch p-values reported like "the p-value is 0.03" because that results in a ton of false positives when papers discuss p-value thresholds. If you need to detect text like that, use the `search_text()` function and a custom pattern.
#'
#' This will catch most comparators like =<>~and most versions of scientific notation like 5.0 x 10^-2 or 5.0e-2. If you find any formats that are not correctly handled by this function, please contact the author.
#'
#' @param paper a paper object or paperlist object
#'
#' @returns a table
#' @export
#'
#' @examples
#' paper <- demopaper()
#' p_values <- extract_equations(paper)
extract_equations <- function(paper) {
  # set up pattern
  operators <- c(
    "=", "<", ">", "~",
    "\u2248", # ~~
    "\u2260", # !=
    "\u2264", # <=
    "\u2265", # >=
    "\u226A", # <<
    "\u226B" # >>
  )
  greek <- c(
    "\u03B2", # beta
    "\u03B7", # eta
    "\u03B1" # alpha
  )

  op <- operators |> paste(collapse = "")
  gr <- greek |> paste(collapse = "")
  pattern <- paste0(
    "(?:(Cronbach..|Cohen..|\\d{1,2}%)\\s+)?", # common prefix
    "[", gr, "a-zA-Z-_\\.0-9\\{\\}\\^\\\\]+\\s*", # statistic name
    "(?:\\([^)]*\\))?\\s*", # optional parentheses
    "[", op , "]{1,3}\\s*", # 1-3 operators
    "([0-9\\.,+-]*[0-9]|\\[[^\\]]+\\])", # valid numbers or anything in []
    "\\s*(e\\s*-\\d+)?", # also match scientific notation
    "(\\s*[x\\*]\\s*10\\s*\\^\\s*-\\d+)?"
  )

  eq <- paper |>
    search_text(operators) |>
    search_text(pattern,
                return = "match",
                perl = TRUE,
                ignore.case = TRUE)

  if (nrow(eq) == 0) {
    return(data.frame())
  }

  # get operator
  pattern <- paste0("[", op, "]{1,2}")
  matches <- gregexpr(pattern, eq$text, perl = TRUE)
  eq$comp <- regmatches(eq$text, matches) |> sapply(`[[`, 1)

  # get lhs & rhs
  s <- strsplit(eq$text, paste0("\\s*[", op, "]{1,2}\\s*"))
  eq$lhs <- sapply(s, \(x) x[[1]]) |> trimws()

  eq$rhs <- sapply(s, \(x) x[[2]]) |> trimws()
  #   gsub("\\s", "", x = _) |>
  #   gsub("[x*]10\\^", "e", x = _)
  # eq$rhs <- suppressWarnings(as.numeric(rhs))

  # set group equal to sentence for now
  eq$grp_id <- 1
  for (i in seq_along(eq$text_id)) {
    if (i == 1 || eq$paper_id[[i]] != eq$paper_id[[i-1]]) {
      eq$grp_id[[i]] <- 1
    } else if (eq$text_id[[i]] == eq$text_id[[i-1]]) {
      eq$grp_id[[i]] <- eq$grp_id[[i-1]]
    } else {
      eq$grp_id[[i]] <- eq$grp_id[[i-1]] + 1
    }
  }

  cols <- c("text_id", "grp_id", "lhs", "comp", "rhs", "paper_id")

  return(eq[, cols])
}
