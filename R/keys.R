# Keys in text and files (#607): passwords for paid services, such as API keys
# and access tokens. A key in a paper's repository lets anyone use the service
# at the authors' cost, so data_check warns about one (.key_scan_files()), and
# module_run() removes any key from a module's result (.key_remove()), so that
# metacheck never copies one into its own output.
#
# The rules are betterleaks' rules of high and medium confidence
# (inst/databases/key_patterns.Rds, made by data-raw/key_patterns.R; MIT
# licence, see inst/COPYRIGHTS). Each has keywords, of which one must be in the
# text before the rule is tried, as in betterleaks, and is tried only near
# them (.key_windows()): a backtracking search can be very slow on a long text.
# Of a rule's filter, its minimum entropy (how random the key must be) is
# applied, and the general filter's placeholders (such as ${API_KEY}).
#
# A key is never tested with its provider: that would use someone else's key.

.key_env <- new.env(parent = emptyenv())

# the rules, read once: list(patterns = data frame, ignore = list(patterns,
# texts), source = list(...), keywords = every rule's keywords)
.key_rules <- function() {
  if (is.null(.key_env$rules)) {
    x <- readRDS(system.file("databases/key_patterns.Rds", package = "metacheck"))
    x$keywords <- unique(unlist(x$patterns$keywords))
    .key_env$rules <- x
  }
  .key_env$rules
}

# The keywords that may be in `texts` (in lower case). The texts are split at
# every character that is in no keyword, and each keyword is looked for once in
# their unique pieces, joined: a keyword can only be inside one piece, and text
# repeats a lot, so this is much faster than looking in the whole text (on 18.6
# million characters of a data table, 6 seconds). A keyword with such a
# character in it (a space: "<add key=", "waka time") counts when each of its
# own pieces is there, which it must be if the keyword is: no keyword is
# missed, and the rules are then tried on the texts themselves.
.key_split <- "[^a-z0-9$./:_~+<=-]+"
.key_present <- function(texts) {
  # strsplit() with a pattern takes time in the square of a text's length (105
  # seconds for 2 MB); replacing the characters first and splitting at one
  # fixed character takes a fraction of a second
  pieces <- strsplit(gsub(.key_split, " ", texts, perl = TRUE), " ", fixed = TRUE)
  pieces <- unique(unlist(pieces, use.names = FALSE))
  if (!length(pieces)) return(character(0))
  joined <- paste(pieces, collapse = " ")
  kw <- .key_rules()$keywords
  if (is.null(.key_env$kw_parts)) {
    .key_env$kw_parts <- lapply(kw, function(k) {
      p <- strsplit(k, .key_split, perl = TRUE)[[1]]
      p[nzchar(p)]
    })
  }
  kw[vapply(.key_env$kw_parts, function(p) all(vapply(p, grepl, logical(1), x = joined, fixed = TRUE)),
            logical(1))]
}

# Shannon entropy in bits per character, as betterleaks computes it
.key_entropy <- function(s) {
  ch <- strsplit(s, "", fixed = TRUE)[[1]]
  if (!length(ch)) return(0)
  p <- as.numeric(table(ch)) / length(ch)
  -sum(p * log2(p))
}

# TRUE when betterleaks' filters drop this find: too little entropy for the
# rule, a pattern of the rule it must not match (such as ".+EXAMPLE$") or must
# match, a text it must not contain, or a placeholder of the general filter
.key_not_a_key <- function(secret, rule) {
  e <- rule$entropy
  if (!is.na(e)) {
    h <- .key_entropy(secret)
    if (if (isTRUE(rule$entropy_strict)) h <= e else h < e) return(TRUE)
  }
  drop <- rule$drop_match[[1]]
  if (length(drop) && any(vapply(drop, function(rx) grepl(rx, secret, perl = TRUE), logical(1)))) return(TRUE)
  need <- rule$need_match[[1]]
  if (length(need) && !any(vapply(need, function(rx) grepl(rx, secret, perl = TRUE), logical(1)))) return(TRUE)
  txt <- rule$drop_contains[[1]]
  if (length(txt) && any(vapply(txt, function(t) grepl(t, secret, fixed = TRUE), logical(1)))) return(TRUE)
  ign <- .key_rules()$ignore
  any(vapply(ign$patterns, function(rx) grepl(rx, secret, perl = TRUE), logical(1))) ||
    any(vapply(ign$texts, function(t) grepl(t, secret, fixed = TRUE), logical(1)))
}

# characters searched on either side of a keyword
.key_window <- 5000L

# The stretches of `low` (the text in lower case) within .key_window characters
# of one of `keywords`, joined where they overlap: a matrix of start and end.
# TruffleHog likewise tries a detector's patterns only on the part of a text
# around one of its keywords.
.key_windows <- function(low, keywords) {
  n <- nchar(low)
  hits <- unlist(lapply(keywords, function(k) {
    m <- gregexpr(k, low, fixed = TRUE)[[1]]
    if (m[[1]] == -1) integer(0) else as.integer(m)
  }))
  if (!length(hits)) return(matrix(integer(0), ncol = 2))
  if (n <= 2L * .key_window) return(matrix(c(1L, n), ncol = 2))
  a <- pmax(1L, sort(hits) - .key_window)
  b <- pmin(n, sort(hits) + .key_window + max(nchar(keywords)))
  out <- matrix(c(a[[1]], b[[1]]), ncol = 2)
  for (i in seq_along(a)[-1]) {
    last <- nrow(out)
    if (a[[i]] <= out[last, 2]) out[last, 2] <- max(out[last, 2], b[[i]])
    else out <- rbind(out, c(a[[i]], b[[i]]))
  }
  out
}

# Encoded data, not text: an unbroken run of this many base64 characters or
# more, such as an image in a knitted HTML page or a notebook. It is masked
# before searching: its random letters matched rules such as the Brave Search
# key's ("BSA" and 24 to 40 letters) four times in one HTML page of a corpus
# repository, and "[key removed]" in it would damage the image. No rule's key
# is near this long (the longest about 160 characters), and a private key's
# base64 is in lines of 64 characters.
.key_encoded_run <- 1000L

# `text` with each run of encoded data replaced by spaces of the same length,
# so that positions and line numbers stay as they are
.key_mask_encoded <- function(text) {
  m <- gregexpr(sprintf("[A-Za-z0-9+/=]{%d,}", .key_encoded_run), text, perl = TRUE)
  if (m[[1]][[1]] == -1) return(text)
  regmatches(text, m) <- list(strrep(" ", attr(m[[1]], "match.length")))
  text
}

# The keys in one text: a data frame with the `start` and `end` (character
# positions) of each, the `rule` that found it and the `key` itself. `rules`
# can be given to try only some (see .key_remove()); with `checked = TRUE`
# they are known to have a keyword in the text. `path`, the file the text is
# from, decides the rules made for some files only (such as a Terraform
# password, for .tf files); without a path these are not tried.
.key_find <- function(text, rules = NULL, checked = FALSE, path = NULL) {
  none <- data.frame(start = integer(0), end = integer(0), rule = character(0),
                     key = character(0), stringsAsFactors = FALSE)
  if (length(text) != 1 || is.na(text) || !nzchar(text)) return(none)
  if (!validUTF8(text)) text <- iconv(text, "UTF-8", "UTF-8", sub = "?")
  text <- .key_mask_encoded(text)
  pats <- rules %||% .key_rules()$patterns
  for_path <- if (is.null(path)) is.na(pats$path) else
    is.na(pats$path) | vapply(pats$path, function(rx) is.na(rx) || grepl(rx, path, perl = TRUE), logical(1))
  pats <- pats[for_path, , drop = FALSE]
  if (!nrow(pats)) return(none)
  low <- tolower(text)
  # only the rules with a keyword in the text
  if (!checked) {
    present <- .key_present(low)
    has_kw <- vapply(pats$keywords, function(k) !length(k) || any(k %in% present), logical(1))
    pats <- pats[has_kw, , drop = FALSE]
  }
  if (!nrow(pats)) return(none)
  out <- list()
  for (r in seq_len(nrow(pats))) {
    rule <- pats[r, ]
    kw <- rule$keywords[[1]]
    win <- if (length(kw)) .key_windows(low, kw) else matrix(c(1L, nchar(text)), ncol = 2)
    for (w in seq_len(nrow(win))) {
      part <- substr(text, win[w, 1], win[w, 2])
      m <- tryCatch(gregexpr(rule$regex, part, perl = TRUE)[[1]], error = function(e) -1L,
                    warning = function(w) -1L)
      if (m[[1]] == -1) next
      cs <- attr(m, "capture.start")
      cl <- attr(m, "capture.length")
      for (j in seq_along(m)) {
        # the rule's group, else its first group that matched, else the match
        g <- rule$group
        if (is.na(g) && !is.null(cs) && ncol(cs) > 0) {
          used <- which(cs[j, ] > 0 & cl[j, ] > 0)
          g <- if (length(used)) used[[1]] else NA_integer_
        }
        if (!is.na(g) && !is.null(cs) && g <= ncol(cs) && cs[j, g] > 0) {
          s <- cs[j, g]; len <- cl[j, g]
        } else {
          s <- m[[j]]; len <- attr(m, "match.length")[[j]]
        }
        if (len <= 0) next
        key <- substr(part, s, s + len - 1L)
        if (.key_not_a_key(key, rule)) next
        start <- win[w, 1] + s - 1L
        out[[length(out) + 1L]] <- data.frame(start = start, end = start + len - 1L,
                                              rule = rule$name, key = key,
                                              stringsAsFactors = FALSE)
      }
    }
  }
  if (!length(out)) return(none)
  out <- do.call(rbind, out)
  # one row per place: two windows or two rules can find the same key
  out <- out[order(out$start, -out$end), , drop = FALSE]
  out[!duplicated(out[c("start", "end")]), , drop = FALSE]
}

# `text` with each key replaced by `with`; keys that overlap are taken as one
.key_replace <- function(text, found, with = "[key removed]") {
  if (!nrow(found)) return(text)
  found <- found[order(found$start), , drop = FALSE]
  pieces <- character(0)
  pos <- 1L
  for (i in seq_len(nrow(found))) {
    if (found$end[[i]] < pos) next             # inside a key already replaced
    if (found$start[[i]] >= pos) pieces <- c(pieces, substr(text, pos, found$start[[i]] - 1L), with)
    pos <- found$end[[i]] + 1L
  }
  paste0(c(pieces, substr(text, pos, nchar(text))), collapse = "")
}

# The keys removed from `x` (a module's result: lists, data frames, text),
# each replaced by "[key removed]", in names too. Only the rules with a keyword
# somewhere in x are tried, and only on the texts holding one of them, so a
# result without any keyword passes almost at once. The number of keys removed
# is the attribute "keys_removed".
.key_remove <- function(x) {
  texts <- list()
  collect <- function(v) {
    if (is.character(v)) texts[[length(texts) + 1L]] <<- unique(v[!is.na(v)])
    else if (is.factor(v)) texts[[length(texts) + 1L]] <<- levels(v)
    else if (is.list(v)) for (e in v) collect(e)
    if (!is.null(names(v))) texts[[length(texts) + 1L]] <<- names(v)
  }
  collect(x)
  texts <- unique(unlist(texts, use.names = FALSE))
  if (!length(texts)) return(x)
  present <- .key_present(tolower(texts))
  if (!length(present)) return(x)
  pats <- .key_rules()$patterns
  pats <- pats[vapply(pats$keywords, function(k) !length(k) || any(k %in% present), logical(1)), , drop = FALSE]
  n_removed <- 0L
  # each text once (a table repeats its values), with only the rules whose
  # keywords are in it
  seen <- character(0)       # texts already looked at that hold a key ...
  cleaned <- character(0)    # ... and the same with the keys replaced
  looked <- character(0)     # texts already looked at
  clean <- function(s) {
    if (!length(s)) return(s)
    todo <- unique(s[!is.na(s) & nzchar(s)])
    todo <- todo[is.na(match(todo, looked))]
    if (length(todo)) {
      looked <<- c(looked, todo)
      lt <- tolower(todo)
      hits <- vapply(present, function(k) grepl(k, lt, fixed = TRUE), logical(length(todo)))
      if (!is.matrix(hits)) hits <- matrix(hits, nrow = length(todo))
      for (j in which(rowSums(hits) > 0)) {
        kws <- present[hits[j, ]]
        rules <- pats[vapply(pats$keywords, function(k) any(k %in% kws), logical(1)), , drop = FALSE]
        f <- .key_find(todo[[j]], rules, checked = TRUE)
        if (nrow(f)) {
          n_removed <<- n_removed + nrow(f)
          seen <<- c(seen, todo[[j]])
          cleaned <<- c(cleaned, .key_replace(todo[[j]], f))
        }
      }
    }
    i <- match(s, seen)
    s[!is.na(i)] <- cleaned[i[!is.na(i)]]
    s
  }
  walk <- function(v) {
    if (is.character(v)) v[] <- clean(v)
    else if (is.factor(v)) levels(v) <- clean(levels(v))
    else if (is.list(v)) {
      for (i in seq_along(v)) if (!is.null(v[[i]])) v[[i]] <- walk(v[[i]])
    }
    if (!is.null(names(v))) names(v) <- clean(names(v))
    v
  }
  x <- walk(x)
  attr(x, "keys_removed") <- n_removed
  x
}

# The beginning of a key, to show without giving it away: up to a sixth of it,
# at least 4 and at most 12 characters, then "..."
.key_shown <- function(key) {
  n <- pmin(12L, pmax(4L, nchar(key) %/% 6L))
  paste0(substr(key, 1L, n), "...")
}

# largest file searched, in bytes
.key_max_bytes <- 25 * 1024^2

# A number for each distinct key, kept in `seen` (an environment shared by the
# searches of one module run, so that the same key in a survey file and in a
# data file has one number); the keys are never returned
.key_number <- function(keys, seen) {
  seen$keys <- c(seen$keys, setdiff(unique(keys), seen$keys))
  match(keys, seen$keys)
}

# The keys in files: a data frame with `file` (the name given in `names`),
# `line`, `key` (a number per distinct key: the same key in two places has the
# same number), `rule` (betterleaks' name for the type of key) and `shown` (the
# beginning of the key, see .key_shown()); never the key itself. A file that
# holds a null byte in its first 8 KB is binary and not searched; one over
# .key_max_bytes is not searched either. The paths not searched are the
# attribute "not_searched" (file and reason).
.key_scan_files <- function(paths, names = paths, seen = new.env()) {
  none <- data.frame(file = character(0), line = integer(0), key = integer(0), rule = character(0),
                     shown = character(0), stringsAsFactors = FALSE)
  skipped <- data.frame(file = character(0), reason = character(0), stringsAsFactors = FALSE)
  out <- list(none)
  for (i in seq_along(paths)) {
    p <- paths[[i]]
    if (is.na(p) || !file.exists(p) || dir.exists(p)) next
    size <- file.size(p)
    if (is.na(size) || size == 0) next
    if (size > .key_max_bytes) {
      skipped <- rbind(skipped, data.frame(file = names[[i]], reason = "too large"))
      next
    }
    raw <- tryCatch(readBin(p, "raw", n = size), error = function(e) NULL)
    if (is.null(raw)) next
    if (any(raw[seq_len(min(8192L, length(raw)))] == as.raw(0))) next   # binary
    raw[raw == as.raw(0)] <- as.raw(32)
    txt <- rawToChar(raw)
    Encoding(txt) <- "UTF-8"
    if (!validUTF8(txt)) txt <- iconv(txt, "UTF-8", "UTF-8", sub = "?")
    if (is.na(txt)) next
    f <- tryCatch(.key_find(txt, path = p), error = function(e) NULL)
    if (is.null(f) || !nrow(f)) next
    nl <- gregexpr("\n", txt, fixed = TRUE)[[1]]
    nl <- if (nl[[1]] == -1) integer(0) else as.integer(nl)
    out[[length(out) + 1L]] <- data.frame(
      file = names[[i]], line = findInterval(f$start, nl) + 1L, key = .key_number(f$key, seen),
      rule = f$rule, shown = .key_shown(f$key), stringsAsFactors = FALSE)
  }
  out <- do.call(rbind, out)
  rownames(out) <- NULL
  attr(out, "not_searched") <- skipped
  out
}

# Rows searched at the top of a data table. A data file's values come from
# participants or instruments, so a key in them is rare, and searching a large
# data file whole is slow. But a key the authors set for a whole survey is in
# every response: in 6qf9p_v3 (#607) a Qualtrics survey flow held an
# OpenRouter key as embedded data, which the export wrote into the column
# "OpenRouterAPIKey" of every response. The table as data_check read it (with
# Qualtrics' extra header rows taken out) is used, not the file's first lines:
# Qualtrics' question texts hold line breaks, so the first response can be
# many lines down.
.key_table_rows <- 3L

# The keys in data tables (data_check's previews: a named list of data
# frames): their column names and first .key_table_rows rows. A data frame
# with `table` (the list's name), `row` (0 for the column names), `key`,
# `rule` and `shown`, as .key_scan_files().
.key_scan_tables <- function(tables, seen = new.env()) {
  out <- list(data.frame(table = character(0), row = integer(0), key = integer(0),
                         rule = character(0), shown = character(0), stringsAsFactors = FALSE))
  for (nm in names(tables)) {
    df <- tables[[nm]]
    if (!is.data.frame(df)) next
    top <- utils::head(df, .key_table_rows)
    txt <- c(names(df), unlist(lapply(top, function(v) if (is.factor(v)) as.character(v) else
      if (is.character(v)) v else NULL), use.names = FALSE))
    row <- c(rep(0L, ncol(df)), unlist(lapply(top, function(v)
      if (is.character(v) || is.factor(v)) seq_along(v) else NULL), use.names = FALSE))
    keep <- !is.na(txt) & nzchar(txt)
    txt <- txt[keep]; row <- row[keep]
    if (!length(txt)) next
    lt <- tolower(txt)
    present <- .key_present(lt)
    if (!length(present)) next
    # each value with only the rules whose keywords are in it
    pats <- .key_rules()$patterns
    hits <- vapply(present, function(k) grepl(k, lt, fixed = TRUE), logical(length(txt)))
    if (!is.matrix(hits)) hits <- matrix(hits, nrow = length(txt))
    for (j in which(rowSums(hits) > 0)) {
      kws <- present[hits[j, ]]
      rules <- pats[vapply(pats$keywords, function(k) any(k %in% kws), logical(1)), , drop = FALSE]
      f <- .key_find(txt[[j]], rules, checked = TRUE)
      if (!nrow(f)) next
      out[[length(out) + 1L]] <- data.frame(table = nm, row = row[[j]], key = .key_number(f$key, seen),
                                            rule = f$rule, shown = .key_shown(f$key),
                                            stringsAsFactors = FALSE)
    }
  }
  out <- do.call(rbind, out)
  rownames(out) <- NULL
  out
}

# The keys in a module's file table (data_check's all_files: paper_id,
# repo_url, file_name, file_path, file_location, data_type): .key_scan_files()
# on each file on disk, with paper_id and repo_url. A tabular data file
# (data_type "data") is left out: see .key_scan_tables(). A file without a
# file_location of its own is looked for where download_repo_files() keeps it
# for the same `cache` setting, so a file code_check downloaded earlier in the
# chain is searched too, without downloading anything.
.key_scan_repo_files <- function(files, cache = FALSE, seen = new.env()) {
  none <- data.frame(paper_id = character(0), repo_url = character(0), file = character(0),
                     line = integer(0), key = integer(0), rule = character(0), shown = character(0),
                     stringsAsFactors = FALSE)
  attr(none, "not_searched") <- data.frame(file = character(0), reason = character(0))
  if (is.null(files) || !nrow(files)) return(none)
  n <- nrow(files)
  col <- function(nm) if (nm %in% names(files)) as.character(files[[nm]]) else rep(NA_character_, n)
  loc <- col("file_location")
  shown_name <- col("file_path")
  shown_name[is.na(shown_name) | !nzchar(shown_name)] <- col("file_name")[is.na(shown_name) | !nzchar(shown_name)]
  repo <- col("repo_url")
  on_disk <- !is.na(loc) & nzchar(loc) & file.exists(loc)
  look <- which(!on_disk & !is.na(repo) & nzchar(repo) & !is.na(shown_name) & nzchar(shown_name))
  for (i in look) {
    p <- .repo_cache_path(repo[[i]], shown_name[[i]], cache = cache)
    if (file.exists(p) && !dir.exists(p)) loc[[i]] <- p
  }
  pid <- col("paper_id")
  is_data <- col("data_type") %in% "data"
  # A local folder's hidden files, which local_files() leaves out (except
  # junk such as .DS_Store): keys are usually kept in one (.env). Of .git/
  # only its config, which can hold a token in a remote's address; the rest is
  # compressed.
  for (dir in unique(repo[!is.na(repo) & dir.exists(repo)])) {
    rel <- list.files(dir, recursive = TRUE, all.files = TRUE, no.. = TRUE)
    rel <- setdiff(rel, list.files(dir, recursive = TRUE))
    rel <- rel[!grepl("(^|/)\\.git/", rel) | grepl("(^|/)\\.git/config$", rel)]
    if (!length(rel)) next
    first <- match(dir, repo)
    loc <- c(loc, file.path(dir, rel))
    shown_name <- c(shown_name, rel)
    repo <- c(repo, rep(dir, length(rel)))
    pid <- c(pid, rep(pid[[first]], length(rel)))
    is_data <- c(is_data, rep(FALSE, length(rel)))
  }
  use <- which(!is_data & !is.na(loc) & nzchar(loc) & file.exists(loc) & !dir.exists(loc))
  use <- use[!duplicated(normalizePath(loc[use], winslash = "/", mustWork = FALSE))]
  if (!length(use)) return(none)
  found <- .key_scan_files(loc[use], names = as.character(use), seen = seen)
  skipped <- attr(found, "not_searched")
  if (nrow(skipped)) skipped$file <- shown_name[as.integer(skipped$file)]
  if (!nrow(found)) {
    attr(none, "not_searched") <- skipped
    return(none)
  }
  row <- as.integer(found$file)
  out <- data.frame(paper_id = pid[row], repo_url = repo[row],
                    file = shown_name[row], line = found$line, key = found$key, rule = found$rule,
                    shown = found$shown, stringsAsFactors = FALSE)
  attr(out, "not_searched") <- skipped
  out
}
