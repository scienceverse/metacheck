# Interactive cap prompts. When a resource cap (download size, file_limit,
# codebook LLM calls) would refuse a unit of work, `cap_prompt()` — by default,
# in an interactive session — shows the offending files/counts and lets the user
# accept the default (skip) or raise the limit for that unit. Under `auto(TRUE)`
# or a non-interactive session (e.g. Rscript overnight) it never blocks: it emits
# the message inline and takes the default action (skip).

#' Automatic (non-interactive) cap handling
#'
#' Controls whether metacheck **asks** before skipping a repository or codebook
#' tier that exceeds a resource cap. When `auto` is `FALSE` (and the session is
#' interactive), a cap that would refuse a unit of work pauses and prompts,
#' listing the offending files and their sizes so the choice is informed; the
#' user can accept the skip or raise the limit for that unit. When `auto` is
#' `TRUE`, no prompt is shown — the unit is skipped and the reason is reported
#' inline, which is the right behaviour for an unattended run.
#'
#' The default is `!interactive()`, so a script run with `Rscript` (or any
#' non-interactive session) never blocks, while an interactive session asks.
#'
#' @param auto `TRUE`/`FALSE` to set, or `NULL` (the default) to get the current
#'   value. When unset, the value is `!interactive()`.
#'
#' @returns the current `auto` setting (invisibly when setting)
#' @export
#'
#' @examples
#' \dontrun{
#' auto(TRUE)   # never prompt; skip over-cap units and report inline
#' auto()       # query the current setting
#' }
auto <- function(auto = NULL) {
  if (is.null(auto)) {
    return(getOption("metacheck.auto", default = !interactive()))
  }
  if (!is.logical(auto) || length(auto) != 1 || is.na(auto))
    stop("Set auto with TRUE or FALSE")
  options(metacheck.auto = auto)
  invisible(auto)
}

# Format a byte size as a short human string for a prompt line (e.g. "5.4 GB").
.cap_size_str <- function(bytes) {
  if (is.na(bytes)) return("unknown size")
  units <- c("B", "KB", "MB", "GB", "TB")
  i <- if (bytes <= 0) 1L else min(length(units),
                                   1L + floor(log(bytes, 1024)))
  sprintf("%.1f %s", bytes / 1024^(i - 1), units[[i]])
}

# Ask the user how to handle a cap, or (auto / non-interactive) take the default.
#
# `message`   : the cap_gate_* sentence (already names the param + value to lift).
# `param`     : the parameter that lifts the cap ("max_file_size", "file_limit",
#               "codebook_max_calls", ...).
# `needed`    : the value that would let the whole unit proceed.
# `current`   : the current cap value.
# `items`     : optional data.frame(name, size) of the offending files (sizes in
#               bytes) shown to inform the choice; NULL for count-only caps.
#
# Returns a list(action = "skip" | "raise", value = <new cap or NA>). "raise"
# means proceed with `value` as the effective cap for this unit only.
# `custom`: whether to offer an "enter a custom value" option. Off for the size
# gate, where two caps are raised together and a single custom number is
# ambiguous; on for single-parameter count caps (file_limit, codebook_max_calls).
cap_prompt <- function(message, param, needed, current, items = NULL,
                       custom = TRUE) {
  # Always show the situation inline, immediately (not batched at end of a loop).
  base::message(message)

  # `skip` also raises a warning() so the refusal is captured by tryCatch() /
  # warnings() (the inline message() handles live visibility; the warning is the
  # terse, programmatically-capturable marker).
  skip <- function() {
    warning(message, call. = FALSE)
    list(action = "skip", value = NA)
  }

  # Non-interactive or auto: take the default (skip) without blocking.
  if (isTRUE(auto()) || !interactive()) return(skip())

  # Show the offending files + sizes so the decision is informed.
  if (!is.null(items) && nrow(items) > 0) {
    ord <- order(items$size, decreasing = TRUE, na.last = TRUE)
    items <- items[ord, , drop = FALSE]
    show <- utils::head(items, 10)
    base::message("  Files over the limit:")
    for (i in seq_len(nrow(show)))
      base::message(sprintf("    - %s  (%s)", show$name[i],
                            .cap_size_str(show$size[i])))
    if (nrow(items) > 10)
      base::message(sprintf("    ... and %d more", nrow(items) - 10))
  }

  choices <- c(
    "Skip this repository/tier (keep the current limit)",
    sprintf("Raise `%s` to %s and proceed", param,
            format(needed, scientific = FALSE)))
  if (custom)
    choices <- c(choices, sprintf("Enter a custom value for `%s`", param))

  choice <- utils::menu(
    choices = choices,
    title = sprintf("Cap reached for `%s` (currently %s). What would you like to do?",
                    param, format(current, scientific = FALSE)))

  if (choice == 2) return(list(action = "raise", value = needed))
  if (custom && choice == 3) {
    ans <- readline(sprintf("New value for `%s`: ", param))
    val <- suppressWarnings(as.numeric(ans))
    if (is.na(val)) {
      base::message("  Not a number; skipping.")
      return(skip())
    }
    return(list(action = "raise", value = val))
  }
  # choice == 0 (cancel) or 1 (skip)
  skip()
}
