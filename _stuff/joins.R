
#' Fix join column data types
#'
#' Dplyr's join functions fail if the by columns have different data types. This fixes them.
#'
#' @param x the left data frame
#' @param y the right data frame
#' @param by the join specification
#'
#' @returns a list of x and y data frames
#' @export
#'
#' @examples
#' x <- data.frame(id = 1:3, a = 1:3)
#' y <- data.frame(id = as.character(1:3), a = as.character(1:3))
#' xy <- pre_join(x, y, by = "id")
pre_join <- function(x, y, by = NULL) {
  # deal with dplyr by styles
  common_cols <- by %||% intersect(names(x), names(y))
  x_cols <- names(common_cols) %||% common_cols
  blank <- which(x_cols == "")
  x_cols[blank] <- common_cols[blank]
  y_cols <- unname(common_cols)

  #
  for (i in seq_along(x_cols)) {
    xcol <- x_cols[[i]]
    ycol <- y_cols[[i]]
    xc <- x[[xcol]]
    yc <- y[[ycol]]
    if (typeof(xc) != typeof(yc)) {
      tryCatch({
        vec <- c(xc, yc)
        x[[xcol]] <- c(xc, vec[which(F)])
        y[[ycol]] <- c(yc, vec[which(F)])
      }, error = \(e) { })
    }
  }

  list(x = x, y = y)
}

left_join <- function(x, y, by = NULL, ...) {
  fix <- pre_join(x, y, by)
  dplyr::left_join(fix$x, fix$y, by, ...)
}

right_join <- function(x, y, by = NULL, ...) {
  fix <- pre_join(x, y, by)
  dplyr::right_join(fix$x, fix$y, by, ...)
}

inner_join <- function(x, y, by = NULL, ...) {
  fix <- pre_join(x, y, by)
  dplyr::inner_join(fix$x, fix$y, by, ...)
}

full_join <- function(x, y, by = NULL, ...) {
  fix <- pre_join(x, y, by)
  dplyr::full_join(fix$x, fix$y, by, ...)
}
