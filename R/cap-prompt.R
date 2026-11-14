# Cap reporting. When a resource cap (download size, codebook LLM calls) refuses
# a unit of work, the tool does not prompt — it reports what was skipped (inline
# and as a warning, so it is captured by warnings()/tryCatch and recorded in the
# manifest) and continues. Caps are plain parameters; the message names the
# parameter and the value needed to include the skipped unit on a future run.

# Emit a cap-refusal message: shown inline immediately (so a looped run reports
# each refusal with the right item, not batched at the end) and raised as a
# warning for programmatic capture. `message` is a cap_gate_* sentence that
# already names the parameter and the value to lift the cap.
.cap_report <- function(message) {
  base::message(message)
  warning(message, call. = FALSE)
  invisible(NULL)
}

# Warn, once per repository, that a repository whose files are about to be
# downloaded was listed only in part: repo_check's listing of an OSF project
# stopped at `osf_max_requests` (its `partial_repos`), so files in folders not
# yet opened are not downloaded either. `partial` is that table, `repo_urls`
# the repositories this download uses. Reported like a size cap
# (.cap_report()), so a missing file is never missing without a word.
.cap_report_partial <- function(partial, repo_urls) {
  if (!is.data.frame(partial) || !nrow(partial) || !"repo_url" %in% names(partial))
    return(invisible(NULL))
  partial <- partial[partial$repo_url %in% repo_urls, , drop = FALSE]
  partial <- partial[!duplicated(partial$repo_url), , drop = FALSE]
  for (i in seq_len(nrow(partial))) {
    n <- partial$files_listed[i] %||% NA
    .cap_report(sprintf(
      paste0("Repository %s was listed only in part%s: the OSF listing stopped ",
             "at the request limit, so files in folders not yet opened are not ",
             "downloaded. Run again with `cache = TRUE` to list the next part, ",
             "or raise `osf_max_requests`."),
      partial$repo_url[i],
      if (is.na(n)) "" else sprintf(" (%d file%s listed)", n, plural(n))))
  }
  invisible(NULL)
}

# Format a byte size as a short human string (e.g. "5.4 GB").
.cap_size_str <- function(bytes) {
  if (is.na(bytes)) return("unknown size")
  units <- c("B", "KB", "MB", "GB", "TB")
  i <- if (bytes <= 0) 1L else min(length(units),
                                   1L + floor(log(bytes, 1024)))
  sprintf("%.1f %s", bytes / 1024^(i - 1), units[[i]])
}
