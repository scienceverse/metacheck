#' Check Stats
#'
#' @param text the search table (or list of paper objects)
#' @param ... arguments to pass to statcheck()
#'
#' @return a table of statistics
#' @export
#'
#' @examples
#' filename <- demoxml()
#' papers <- read(filename)
#' stats(papers)
stats <- function(text, ...) {
  # lines with stats must have at least one number
  text <- search_text(text, "[0-9]")


  n <- nrow(text)
  if (n == 0) return(data.frame())

  # set up progress bar ----
  if (verbose()) {
    pb <- progress::progress_bar$new(
      total = n, clear = FALSE,
      format = "Checking stats [:bar] :current/:total :elapsedfull"
    )
    pb$tick(0)
    Sys.sleep(0.2)
    pb$tick(0)
  }

  subchecks <- lapply(seq_along(text$text), \(i) {
    subtext <- text$text[[i]]

    # statcheck uses cat() to output messages :(
    sink_output <- utils::capture.output(
      sc <- tryCatch(statcheck::statcheck(subtext, messages = FALSE, ...),
                     error = function(e) {
                       return(NULL)
                     }, warning = function(w) {})
    )
    if (verbose()) pb$tick()
    if (is.null(sc)) return(data.frame())
    sc$source <- i

    return(sc)
  })
  checks <- do.call(rbind, subchecks)

  if (nrow(checks) == 0) return(checks)

  text$source = seq_along(text$text)

  stat_table <- dplyr::left_join(checks, text, by = "source")
  rownames(stat_table) <- NULL

  stat_table[, 2:ncol(stat_table)]
}
