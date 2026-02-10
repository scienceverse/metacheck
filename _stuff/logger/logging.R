# I based this very simple thing on my previous internship work in Python. its a 
# claude based quick and dirty R implementation, but does serve the purpose of actually
# calling functions with error logging, preserving the root cause and the arguments
# aka the papers!! in the funciton used when used poperly
# ------------------------------------------------------------
# Helper: describe callable (function with arguments)
# ------------------------------------------------------------
#' Describe a callable (function) with its arguments in a safe manner
#' This function generates a string representation of a callable (function)
#' along with its arguments, handling potential errors in argument representation
#' and truncating long representations for readability.
#' @param func A function to describe
#' @param func_name Optional name of the function (if NULL, will be inferred)
#' @param args A named list of arguments to the function
#' @param max_len Maximum length for argument representation (default: 200)
#' @param indent_spaces Number of spaces for indentation in multi-line output (default: 4)
#' @return A string representation of the callable with its arguments
#' @export
#' 
describe_callable <- function(func, func_name = NULL, args = NULL, max_len = 200, indent_spaces = 4) {
  # Get function name
  if (is.null(func_name)) {
    func_name <- deparse(substitute(func))
    if (length(func_name) > 1 || nchar(func_name) > 50) {
      func_name <- "<function>"
    }
  }
  
  # Safe representation of arguments
  safe_repr <- function(obj) {
    tryCatch({
      r <- capture.output(str(obj, max.level = 1, give.attr = FALSE))
      r <- paste(r, collapse = " ")
      if (nchar(r) > max_len) {
        r <- paste0(substr(r, 1, max_len), "...<truncated>")
      }
      r
    }, error = function(e) {
      paste0("<unrepr-able ", class(obj)[1], ">")
    })
  }
  
  if (is.null(args) || length(args) == 0) {
    return(paste0(func_name, "()"))
  }
  
  arg_names <- names(args)
  if (is.null(arg_names)) arg_names <- rep("", length(args))
  
  all_items <- character(length(args))
  for (i in seq_along(args)) {
    val_repr <- safe_repr(args[[i]])
    if (arg_names[i] != "") {
      all_items[i] <- paste0(arg_names[i], " = ", val_repr)
    } else {
      all_items[i] <- val_repr
    }
  }
  
  # Wrap long arguments
  wrap_line <- function(text, width = 80, indent = "") {
    if (nchar(text) <= width) return(text)
    pieces <- strwrap(text, width = width - nchar(indent))
    paste(c(pieces[1], paste0(indent, pieces[-1])), collapse = "\n")
  }
  
  indent <- paste(rep(" ", indent_spaces), collapse = "")
  
  if (length(all_items) == 1) {
    return(paste0(func_name, "(", wrap_line(all_items[1], width = 80, indent = indent), ")"))
  } else {
    first <- wrap_line(all_items[1], width = 80, indent = indent)
    rest <- paste(sapply(all_items[-1], wrap_line, width = 80, indent = indent), collapse = ",\n")
    return(paste0(func_name, "(", first, ",\n", indent, rest, ")"))
  }
}


# ------------------------------------------------------------
# helper wrap_text ; wrapping text
# ------------------------------------------------------------

wrap_text <- function(x, width = 80, indent = 0) {
  lines <- unlist(strsplit(x, "\n", fixed = TRUE))

  wrapped <- unlist(lapply(lines, function(line) {
    if (length(line) == 0 || nchar(line) == 0) {
      return("")
    }
    strwrap(line, width = width, indent = indent, exdent = indent)
  }))

  wrapped
}




# ------------------------------------------------------------
# safe_run: Execute function safely with error logging
# ------------------------------------------------------------

#' Execute a function safely with comprehensive error logging
#'
#' Wraps function execution in a try-catch block and logs any errors
#' to a specified log file with full traceback information.
#' @param label A label to identify the context of the function call
#' @param func The function to execute
#' @param args A named list of arguments to pass to the function
#' @param log_path Path to the log file (default: "logs/error/undefined.log")
#' @param max_arg_len Maximum length for argument representation in logs (default:
#'   200)
#' @return The result of the function call, or NULL if an error occurred
#' @export
#' I implemented this in my previous internship and made a quick and dirty R implementation here using CLAUDE.


safe_run <- function(label, func, args = list(), log_path = NULL, max_arg_len = 200) {
  # Extract current timestamp
  timestamp <- format( as.POSIXct(Sys.time(), tz = "UTC"), "%Y-%m-%d_%H-%M-%OS3")
  timestamp <- gsub("\\.", "-", timestamp)

  # in case log path is null, generate one
  if (is.null(log_path)) {
    log_path <- file.path("logs", "error", "undefined.log")
  }
  # Add the timestamp to the log
  ext <- tools::file_ext(log_path)
  if (nzchar(ext)) {
    base <- sub(paste0("\\.", ext, "$"), "", log_path)
    log_path <- paste0(base, "_", timestamp, ".", ext)
  } else {
    log_path <- paste0(log_path, "_", timestamp, ".log")
  }
  # Create the log dir if it does not exist yet

  dir.create(dirname(log_path), recursive = TRUE, showWarnings = FALSE)

  # Describe the command that was called with the actual args in plaintext for proper reproducition
  call_desc <- tryCatch({
    describe_callable(func, args = args, max_len = max_arg_len)
  }, error = function(e) {
    "<could not describe call>"
  })

  # Generate the log message
  SEP <- paste(rep("=", 80), collapse = "")
  SUBSEP <- paste(rep("-", 80), collapse = "")

  log_condition <- function(cond, type = "ERROR") {
    exc_type <- class(cond)[1]
    exc_msg  <- conditionMessage(cond)

    tb <- paste(capture.output(cond), collapse = "\n")
    tb <- wrap_text(
      paste(
        c(
          "Condition:",
          capture.output(cond),
          "",
          "Call stack:",
          capture.output(sys.calls())
        ),
        collapse = "\n"
      ),
      width = 80,
      indent = 4
    )

    log_conn <- file(log_path, open = "at")
    tryCatch({
      writeLines(c(
        "",
        SEP,
        sprintf("[%s] %s", timestamp, type),
        sprintf("Timestamp  : %s", timestamp),
        sprintf("Label      : %s", label),
        sprintf("Call       : %s", call_desc),
        sprintf("Condition  : %s", exc_type),
        sprintf("Message    : %s", exc_msg),
        SUBSEP,
        "       Traceback (failing call only):",
        SUBSEP,
        tb,
        SEP
      ), log_conn)
    }, finally = {
      close(log_conn)
    })
  }

  result <- withCallingHandlers(
    tryCatch(
      {
        do.call(func, args)
      },
      error = function(e) {
        log_condition(e, "ERROR")
        return(sprintf("'%s', traceback at %s", e, log_path))
      }
    ),
    warning = function(w) {
      log_condition(w, "WARNING")
      stop(w)                     # promote warning → error
    }
  )

  result
}
