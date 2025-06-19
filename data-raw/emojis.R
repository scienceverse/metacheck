show_unicode_escape <- function(char) {
  # Get the integer code points of the character (UTF-32)
  codes <- utf8ToInt(char)

  # Format each code point as a Unicode escape sequence
  escapes <- sapply(codes, function(code) {
    if (code <= 0xFFFF) {
      # Use \u for code points <= FFFF
      sprintf("\\u%04X", code)
    } else {
      # Use \U for code points > FFFF
      sprintf("\\U%08X", code)
    }
  })

  paste(escapes, collapse = "")
}

utf <- list(
  check     = "\u2705",
  star      = "\u2B50",
  warning   = "\u26A0\uFE0F",
  stop      = "\U0001F6D1",
  x         = "\u274C",
  no        = "\U0001F6AB",
  thumbs_up = "\U0001F44D",
  thumbs_down = "\U0001F44E",
  info      = "\u2139\uFE0F",
  question  = "\u2753",

  tl_green  = "\ud83d\udfe2",
  tl_yellow = "\ud83d\udfe1",
  tl_red    = "\ud83d\udd34",
  tl_info   = "\ud83d\udd35",
  tl_na     = "\u26aa\ufe0f",
  tl_fail   = "\u26ab\ufe0f",

  red    = "\u2764\uFE0F",
  orange = "\U0001F9E1",
  yellow = "\U0001F49B",
  green  = "\U0001F49A",
  blue   = "\U0001F499",
  purple = "\U0001F49C",
  brown  = "\U0001F90E",
  black  = "\U0001F5A4",
  white  = "\U0001F90D",
  pink   = "\U0001F496"
)

# emojis <- lapply(utf, show_unicode_escape) |>
#   lapply(\(x) { class(x) <- "emoji"; x })

emojis <- utf

usethis::use_data(emojis, overwrite = TRUE)
usethis::use_r("emojis")


#' Print Emojis
#'
#' @param x The escaped emoji characters (from papercheck::emoji)
#' @param ... Additional parameters for print
#'
#' @export
#' @keywords internal
#'
print.emoji <- function(x, ...) {
  parsed <- parse(text = paste0('"', x, '"'))
  utf <- eval(parsed)
  cat(utf)
}
