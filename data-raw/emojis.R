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

  tl_green  = "\u2705\uFE0F",
  tl_yellow = "\uD83D\uDD0D",
  tl_red    = "\u26A0\uFE0F",
  tl_info   = "\u2139\uFE0F",
  tl_na     = "\u26AA\uFE0F",
  tl_fail   = "\u2620\uFE0F",

  dot_green  = "\ud83d\udfe2",
  dot_yellow = "\ud83d\udfe1",
  dot_red    = "\ud83d\udd34",
  dot_info   = "\ud83d\udd35",
  dot_na     = "\u26aa\ufe0f",
  dot_fail   = "\u26ab\ufe0f",

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
#' @param x The escaped emoji characters (from metacheck::emoji)
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
