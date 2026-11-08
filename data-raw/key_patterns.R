# Collects the rules that the secret scanner betterleaks uses to find keys
# (passwords for paid services, such as API keys) into
# inst/databases/key_patterns.Rds, which .key_rules() reads (R/keys.R). Run it
# by hand to take up newer rules; needs the RcppTOML package.
#
# The rules are the work of betterleaks, https://github.com/betterleaks/betterleaks
# (MIT licence, (c) the betterleaks authors; see inst/COPYRIGHTS), made by the
# authors of gitleaks. The commit that was read is kept with the rules.
#
# Why betterleaks (#607): each rule has a confidence level and filters against
# false finds, and the rules cover OpenRouter, whose key GitHub found in a
# result. TruffleHog's patterns are written to be followed by a test of the key
# with its provider, so on their own they find ordinary words; gitleaks' rules
# are in betterleaks too.
#
# Taken: the rules with confidence "high" or "medium" that report a find on
# their own (not the parts of composite rules, skipReport). Left out: the
# "low" rules (generic-api-key, generic-password, ...), which take any value
# after a word such as "key" or "password".
#
# A rule's filter drops a find when any of its conditions holds (they are only
# joined by "||" in the rules taken). Kept of them: the minimum entropy (how
# random the key must be), the patterns a key must not match
# (matchesAny(finding["secret"], ...), such as ".+EXAMPLE$" for AWS) or must
# match (!matchesAny), and the texts it must not contain (containsAny). Other
# conditions (such as tokenRatio, which needs betterleaks' word list) are left
# out, so a little more is found than betterleaks would report. The general
# filter's placeholders (such as ${VAR}) are kept as `ignore`.
#
# betterleaks is written in Go, whose regular expressions (RE2) R reads with
# perl = TRUE almost unchanged: flags such as (?i) in the middle of a pattern,
# \z, [[:alnum:]] and \p{L} all mean the same there. A pattern R cannot read is
# left out, and named.

repo <- "betterleaks/betterleaks"
sha <- jsonlite::fromJSON(sprintf("https://api.github.com/repos/%s/commits/HEAD", repo))$sha
toml_file <- tempfile(fileext = ".toml")
utils::download.file(sprintf("https://raw.githubusercontent.com/%s/%s/config/betterleaks.toml", repo, sha),
                     toml_file, quiet = TRUE, mode = "wb")
conf <- RcppTOML::parseTOML(toml_file)

# RcppTOML gives a literal string ('''...''', which betterleaks uses for its
# patterns and filters) escaped once more: a backslash doubled, a quote and a
# line break written as \" and \n. Undone here, one escape at a time, from the
# left. Checked on 8 November 2026 against Python's tomllib for every rule's
# regex, filter and keywords: identical.
unescape <- function(s) {
  if (is.null(s)) return(s)
  m <- gregexpr("\\\\(.)", s, perl = TRUE)
  regmatches(s, m) <- lapply(regmatches(s, m), function(x) {
    ch <- substr(x, 2, 2)
    ifelse(ch == "n", "\n", ifelse(ch == "t", "\t", ifelse(ch == "r", "\r", ch)))
  })
  s
}
conf$rules <- lapply(conf$rules, function(r) {
  r$regex <- unescape(r$regex)
  r$path <- unescape(r$path)
  r$filter <- unescape(r$filter)
  r$keywords <- unescape(unlist(r$keywords))
  r
})
conf$filter <- unescape(conf$filter)

reads <- function(rx) {
  !inherits(tryCatch(grepl(rx, "", perl = TRUE), error = function(e) e, warning = function(w) w),
            c("error", "warning"))
}

# the entropy a key must reach by the rule's filter, which drops a find with
# entropy(finding["secret"]) < N (or <= N)
min_entropy <- function(flt) {
  m <- regmatches(flt %||% "", regexec('entropy\\(finding\\["secret"\\]\\)\\s*(<=?)\\s*([0-9.]+)', flt %||% ""))[[1]]
  if (length(m) < 3) return(c(NA_real_, NA_real_))
  c(as.numeric(m[[3]]), as.numeric(m[[2]] == "<="))
}

# The items of each list given to `fun` on finding["secret"] in a filter, as
# list(negated, items): fun(finding["secret"], [`a`, "b"]). An item is a Go raw
# string (`...`) or a quoted one ("...", with \\ and \" as escapes); the list
# ends at the first "]" outside an item, as a pattern can hold "]" itself.
secret_lists <- function(flt, fun) {
  if (is.null(flt) || !nzchar(flt)) return(list())
  starts <- gregexpr(paste0("(!?)\\s*", fun, '\\(finding\\["secret"\\],\\s*\\['), flt, perl = TRUE)[[1]]
  if (starts[[1]] == -1) return(list())
  lens <- attr(starts, "match.length")
  lapply(seq_along(starts), function(i) {
    neg <- startsWith(substr(flt, starts[[i]], starts[[i]] + lens[[i]] - 1L), "!")
    rest <- substr(flt, starts[[i]] + lens[[i]], nchar(flt))
    items <- character(0)
    repeat {
      rest <- sub("^[\\s,]+", "", rest, perl = TRUE)
      if (!nzchar(rest) || startsWith(rest, "]")) break
      if (startsWith(rest, "`")) {
        end <- regexpr("`", substr(rest, 2, nchar(rest)), fixed = TRUE)
        items <- c(items, substr(rest, 2, end))
        rest <- substr(rest, end + 2L, nchar(rest))
      } else if (startsWith(rest, '"')) {
        m <- regexpr('^"(?:[^"\\\\]|\\\\.)*"', rest, perl = TRUE)
        item <- substr(rest, 2, attr(m, "match.length") - 1L)
        items <- c(items, gsub('\\\\(["\\\\])', "\\1", item, perl = TRUE))
        rest <- substr(rest, attr(m, "match.length") + 1L, nchar(rest))
      } else break
    }
    list(negated = neg, items = items)
  })
}
# the items of the lists of one kind (negated or not)
list_items <- function(lists, negated) {
  unique(unlist(lapply(lists, function(l) if (identical(l$negated, negated)) l$items)))
}

rules <- conf$rules
take <-vapply(rules, function(r) !is.null(r$regex) && !isTRUE(r$skipReport) &&
                 isTRUE((r$confidence %||% "") %in% c("high", "medium")), logical(1))
left_out <- vapply(rules[!take], `[[`, "", "id")
rules <- rules[take]
ok <- vapply(rules, function(r) reads(r$regex), logical(1))
unreadable <- vapply(rules[!ok], `[[`, "", "id")
rules <- rules[ok]

ent <- t(vapply(rules, function(r) min_entropy(r$filter), numeric(2)))
patterns <- data.frame(
  name = vapply(rules, `[[`, "", "id"),
  description = vapply(rules, function(r) r$description %||% NA_character_, ""),
  confidence = vapply(rules, `[[`, "", "confidence"),
  regex = vapply(rules, `[[`, "", "regex"),
  group = vapply(rules, function(r) as.integer(r$valueGroup %||% r$secretGroup %||% NA_integer_), 1L),
  # the files a rule is for, such as "(?i)\.(?:tf|hcl)$" for a Terraform
  # password; NA for all files
  path = vapply(rules, function(r) r$path %||% NA_character_, ""),
  entropy = ent[, 1],
  entropy_strict = as.logical(ent[, 2]),
  stringsAsFactors = FALSE
)
patterns$keywords <- lapply(rules, function(r) tolower(unlist(r$keywords) %||% character(0)))
readable <- function(x) x[vapply(x, reads, logical(1))]
patterns$drop_match <- lapply(rules, function(r) readable(list_items(secret_lists(r$filter, "matchesAny"), FALSE)))
patterns$need_match <- lapply(rules, function(r) readable(list_items(secret_lists(r$filter, "matchesAny"), TRUE)))
patterns$drop_contains <- lapply(rules, function(r) list_items(secret_lists(r$filter, "containsAny"), FALSE))

# the general filter: patterns and texts that a find must not be
flt <- conf$filter %||% ""
ignore_patterns <- readable(list_items(secret_lists(flt, "matchesAny"), FALSE))
ignore_texts <- list_items(secret_lists(flt, "containsAny"), FALSE)

key_patterns <- list(
  source = list(url = sprintf("https://github.com/%s", repo), file = "config/betterleaks.toml",
                commit = sha, licence = "MIT", read = format(Sys.Date())),
  ignore = list(patterns = unname(ignore_patterns), texts = unname(ignore_texts)),
  patterns = patterns
)
saveRDS(key_patterns, "inst/databases/key_patterns.Rds", compress = "xz")
message(sprintf("betterleaks %s: %d rules taken; %d left out (low confidence, or part of another rule); %d R cannot read: %s",
                substr(sha, 1, 7), nrow(patterns), length(left_out), length(unreadable),
                paste(unreadable, collapse = ", ")))
message(sprintf("ignore: %d patterns, %d texts", length(ignore_patterns), length(ignore_texts)))
message(sprintf("rule filters: %d rules with patterns a key must not match, %d with one it must match, %d with texts",
                sum(lengths(patterns$drop_match) > 0), sum(lengths(patterns$need_match) > 0),
                sum(lengths(patterns$drop_contains) > 0)))
