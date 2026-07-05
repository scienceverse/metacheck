# Cap reporting. When a resource cap (download size, codebook LLM calls) refuses
# a unit of work, the tool does not prompt — it reports what was skipped (inline
# and as a warning, so it is captured by warnings()/tryCatch and recorded in the
# manifest) and continues. Caps are plain parameters; the message names the
# parameter and the value needed to include the skipped unit on a future run.

# Emit a cap-refusal message: shown inline immediately (so a looped run reports
# each refusal with the right item, not batched at the end) and raised as a
# warning for programmatic capture. `message` is a cap_gate_* sentence that
# already names the parameter and the value to lift the cap.
cap_report <- function(message) {
  base::message(message)
  warning(message, call. = FALSE)
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
