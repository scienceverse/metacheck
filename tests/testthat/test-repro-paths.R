# How reproducibility_check rewrites the paths in the authors' scripts before a
# run, on the tiny deposit in fixtures/repro_paths/ (see its README.md): a line
# marked KEEP must stay as written, a line marked RESOLVES must lead to its file
# from the folder the script runs in (#648).

# One row per marker: id, kind, the line as rewritten, and whether it passes.
repro_path_marks <- function(pv) {
  rows <- lapply(seq_len(nrow(pv)), function(i) {
    orig <- strsplit(pv$original[i], "\n", fixed = TRUE)[[1]]
    new <- strsplit(pv$rewritten[i], "\n", fixed = TRUE)[[1]]
    m <- regmatches(orig, regexec("#\\s*(KEEP|RESOLVES|NAMES)\\s+(\\S+)\\s*(\\S*)\\s*$", orig))
    m <- m[lengths(m) > 0]
    lapply(m, function(x) {
      marker <- trimws(x[1])
      orig_line <- orig[grepl(marker, orig, fixed = TRUE)][1]
      new_line <- new[grepl(marker, new, fixed = TRUE)][1]
      ok <- if (is.na(new_line)) FALSE else if (x[2] == "KEEP") {
        identical(trimws(new_line), trimws(orig_line))
      } else if (x[2] == "NAMES") {
        # the file named as in the deposit, and the rest of the line as written
        grepl(paste0("[\"']", x[4], "[\"']"), new_line) &&
          identical(tolower(trimws(new_line)), tolower(trimws(orig_line)))
      } else {
        quoted <- regmatches(new_line, gregexpr("[\"']([^\"']+)[\"']", new_line))[[1]]
        quoted <- gsub("^[\"']|[\"']$", "", quoted)
        quoted <- quoted[basename(quoted) == basename(x[4])]
        run_dir <- pv$run_dir[i]
        length(quoted) > 0 && any(vapply(quoted, function(q)
          identical(.repro_norm_rel(if (run_dir %in% c(".", "")) q else file.path(run_dir, q)), x[4]),
          logical(1)))
      }
      data.frame(file = pv$file_name[i], id = x[3], kind = x[2], ok = ok,
                 original = trimws(orig_line), rewritten = trimws(new_line %||% NA_character_))
    })
  })
  do.call(rbind, unlist(rows, recursive = FALSE))
}

test_that("paths in the fixture deposit are kept or pointed to their file (#648)", {
  skip_if_not_installed("knitr")
  pv <- .repro_rewrite_preview(test_path("fixtures", "repro_paths"))
  marks <- repro_path_marks(pv)
  expect_gt(nrow(marks), 50)
  bad <- marks[!marks$ok, , drop = FALSE]
  expect(nrow(bad) == 0, paste0(nrow(bad), " path line(s) handled wrongly:\n",
                                paste(sprintf("%s %s (%s): %s  ->  %s", bad$file, bad$id, bad$kind,
                                              bad$original, bad$rewritten), collapse = "\n")))
})
