# Build `tasks`: a dictionary of behavioural/cognitive tasks (Stroop, IAT,
# n-back, Raven's), harvested from the Cognitive Atlas task ontology.
#
# This is the task counterpart of `scales` (data-raw/scales.R). A task is not a
# different kind of object from a scale: it is expressed in the same OSD format
# (see inst/osd/tasks-in-osd.md), with trials as items and conditions as
# dimensions. There is no separate "task format".
#
# Source: https://www.cognitiveatlas.org/api/v-alpha/task (public, no auth).
# The list endpoint returns names only; conditions/contrasts/indicators need one
# request per task, so all 857 are fetched once and cached on disk.
#
# ── What the Atlas can and cannot fill (full harvest, n = 857) ────────────────
#   name, description        857 (100%)   -> scale_info
#   atlas id                 857 (100%)   -> scale_info$url + tasks$atlas_id
#   contrasts (named)        529  (62%)   -> dimensions / scoring
#   citation (+pmid)         470  (55%)   -> scale_info$citation
#   indicators               317  (37%)   -> dimensions (RT / accuracy)
#   conditions               272  (32%)   -> items
#   alias                    259  (30%)   -> scale_info$abbreviation
#   COMPUTABLE contrasts       7   (1%)   -> scoring with real weights
#   parameters                 0   (0%)   -> omitted; authored by hand
#   per-task licence           0   (0%)
#
# ── Decisions (recorded in full in inst/osd/tasks-in-osd.md section 7) ────────
# 1. Scope: ALL 857 tasks, no domain gate. Unlike PhenX (whose topic-label names
#    match ordinary prose), an Atlas name like "motor fMRI task paradigm" is
#    distinctive enough that it simply never matches, so it costs one row.
# 2. Collisions: the OpenScales definition WINS. The Atlas lists questionnaires
#    (BIS, CES-D, ERQ, MMPI, PSQI) as "tasks" because it catalogues things
#    participants do. An OpenScales record has items, subscales, weights and
#    alpha; the Atlas record has prose. Dropping the Atlas duplicate avoids a
#    second entry the scale matcher would then have to disambiguate.
# 3. Batteries: ignored. 113 tasks record battery membership; OSD has no
#    containment concept and we do not add one.
# 4. Parameters: omitted. Regex extraction from prose was tried and rejected at
#    a 26% error rate (see section 7.4). Authored by hand, per task, on request.
#
# ── Licensing ────────────────────────────────────────────────────────────────
# The Atlas has NO per-task licence field. Its own content is CC-BY, but tasks
# it merely NAMES (WAIS, Conners) are proprietary. `license` here describes the
# DEFINITION RECORD, never a right to administer the task.
#
# Columns of `tasks` (one row per task):
#   code          slug used as the OSD scale_info$code (uppercase/digits/hyphen)
#   name          canonical task name
#   acronym       from Atlas `alias`, when it is a real acronym; "" otherwise
#   atlas_id      Cognitive Atlas id (trm_*/tsk_*); the join key, 100% populated
#   description   Atlas definition_text
#   citation      first citation's description ("" when none); see note below
#   pmid          first citation's PubMed id ("" when none)
#   url           canonical Atlas URL for the task
#   n_conditions  conditions recorded upstream
#   n_contrasts   contrasts recorded upstream (mostly NAMES, not computable)
#   n_computable  contrasts carrying real condition weights
#   indicators    comma-separated normalised indicators (response_time, accuracy)
#   text_ok       TRUE when the name is safe to regex against manuscript prose
#
# Rebuild with:  source("data-raw/tasks.R")

`%||%` <- function(a, b) if (is.null(a)) b else a

api  <- "https://www.cognitiveatlas.org/api/v-alpha/task"
# Cache the per-task fetch: 857 requests is slow and rude to repeat.
cache_dir <- file.path("data-raw", "cache")
dir.create(cache_dir, showWarnings = FALSE, recursive = TRUE)
cache <- file.path(cache_dir, "cognitive_atlas_tasks.rds")

# ── Fetch ─────────────────────────────────────────────────────────────────────
if (file.exists(cache)) {
  full <- readRDS(cache)
  message(sprintf("tasks: %d cached Atlas records (delete %s to refetch)",
                  length(full), cache))
} else {
  lst <- jsonlite::fromJSON(api, simplifyVector = FALSE)
  ids <- vapply(lst, function(x) as.character(x$id %||% ""), character(1))
  ids <- unique(ids[nzchar(ids)])
  message(sprintf("tasks: fetching %d full records from the Cognitive Atlas", length(ids)))
  pb <- if (requireNamespace("utils", quietly = TRUE))
    utils::txtProgressBar(min = 0, max = length(ids), style = 3) else NULL
  full <- vector("list", length(ids))
  for (i in seq_along(ids)) {
    r <- tryCatch(jsonlite::fromJSON(paste0(api, "?id=", ids[i]),
                                     simplifyVector = FALSE),
                  error = function(e) NULL)
    if (!is.null(r)) full[[i]] <- if (is.null(names(r))) r[[1]] else r
    if (!is.null(pb)) utils::setTxtProgressBar(pb, i)
  }
  if (!is.null(pb)) close(pb)
  full <- Filter(Negate(is.null), full)
  saveRDS(full, cache)
}

# ── Cleaning helpers ──────────────────────────────────────────────────────────
# The API returns HTML entities (&#39; occurs 272x) and stray whitespace.
unescape_html <- function(x) {
  x <- gsub("&#39;", "'", x, fixed = TRUE)
  x <- gsub("&quot;", '"', x, fixed = TRUE)
  x <- gsub("&amp;", "&", x, fixed = TRUE)
  x <- gsub("&lt;", "<", x, fixed = TRUE)
  x <- gsub("&gt;", ">", x, fixed = TRUE)
  x <- gsub("&#34;", '"', x, fixed = TRUE)
  x
}
clean <- function(x) {
  x <- unescape_html(as.character(x %||% ""))
  trimws(gsub("\\s+", " ", x))
}
# `definition_text` is sometimes the literal string "None".
clean_desc <- function(x) {
  v <- clean(x)
  if (tolower(v) %in% c("none", "null", "n/a")) "" else v
}

# An OSD code: uppercase letters, digits and hyphens only (spec section 2).
osd_code <- function(name, max_chars = 40L) {
  x <- toupper(gsub("[^A-Za-z0-9]+", "-", clean(name)))
  x <- gsub("^-+|-+$", "", x)
  if (!nzchar(x)) return("TASK")
  if (nchar(x) > max_chars) {
    trunc <- substr(x, 1, max_chars)
    at <- regexpr("-[^-]*$", trunc)
    if (at > 1) trunc <- substr(trunc, 1, at - 1L)
    x <- gsub("-+$", "", trunc)
  }
  if (!nzchar(x)) "TASK" else x
}

# Atlas `alias` is a comma-separated list of SYNONYMS, not abbreviations. Keep
# the first token only when it looks like an acronym (all-caps, 2-10 chars);
# "two-stage decision task" is a synonym and must not become an acronym.
atlas_acronym <- function(alias) {
  a <- clean(alias)
  if (!nzchar(a)) return("")
  first <- trimws(strsplit(a, ",", fixed = TRUE)[[1]][1])
  if (nchar(first) >= 2 && nchar(first) <= 10 &&
      identical(first, toupper(first)) &&
      grepl("^[A-Z0-9-]+$", first)) first else ""
}

# The Atlas `indicators.type` field is free text: 225 distinct values, 187 of
# them appearing once ("response time" vs "reaction time" vs "rt"). Normalise
# to a small vocabulary; keep nothing when it does not map.
norm_indicator <- function(x) {
  s <- tolower(clean(x))
  if (!nzchar(s)) return(NA_character_)
  if (grepl("(response|reaction) time|^rt$", s)) return("response_time")
  if (grepl("accura|correct|error", s))          return("accuracy")
  if (grepl("\\bscore\\b|\\biq\\b", s))          return("score")
  if (grepl("activation|bold|erp|eeg|fmri", s))  return("neural")
  if (grepl("rating|subjective", s))             return("rating")
  NA_character_
}

# Same generic-token logic as data-raw/scales.R, so the two dictionaries agree.
generic_tok <- c("scale","scales","questionnaire","inventory","test","index",
  "survey","measure","checklist","rating","self","short","form","revised",
  "brief","version","the","of","for","and","a","an","in","to","assessment",
  "schedule","screen","screening","task","tasks","paradigm")
content_tokens <- function(s) {
  toks <- unlist(strsplit(tolower(clean(s)), "[^a-z0-9]+"))
  unique(toks[nzchar(toks) & !(toks %in% generic_tok)])
}

# ── Flatten one Atlas record ──────────────────────────────────────────────────
n_weighted_contrasts <- function(t) {
  cs <- t$contrasts %||% list()
  sum(vapply(cs, function(c) {
    conds <- c$conditions %||% list()
    flat <- list()
    for (e in conds) flat <- c(flat, if (is.null(names(e))) e else list(e))
    any(vapply(flat, function(e) is.list(e) && !is.null(e$weight),
               logical(1)))
  }, logical(1)))
}

row_of <- function(t) {
  nm <- clean(t$name)
  if (!nzchar(nm)) return(NULL)          # one Atlas entry has an empty name
  cites <- t$citation %||% list()
  ind <- unique(stats::na.omit(vapply(t$indicators %||% list(),
                                      function(i) norm_indicator(i$type),
                                      character(1))))
  data.frame(
    code         = osd_code(nm),
    name         = nm,
    acronym      = atlas_acronym(t$alias),
    atlas_id     = clean(t$id),
    description  = clean_desc(t$definition_text),
    # NOTE: Atlas citations are UNORDERED. The IAT's first citation is not
    # Greenwald 1998. This is "a" citation, never "the" canonical one.
    citation     = if (length(cites)) clean(cites[[1]]$citation_desc) else "",
    pmid         = if (length(cites)) clean(cites[[1]]$citation_pmid) else "",
    url          = paste0("https://www.cognitiveatlas.org/task/id/", clean(t$id), "/"),
    n_conditions = length(t$conditions %||% list()),
    n_contrasts  = length(t$contrasts %||% list()),
    n_computable = n_weighted_contrasts(t),
    indicators   = paste(sort(ind), collapse = ","),
    text_ok      = length(content_tokens(nm)) >= 1L
  )
}

tasks <- do.call(rbind, Filter(Negate(is.null), lapply(full, row_of)))
tasks <- tasks[!duplicated(tasks$atlas_id), , drop = FALSE]

# ── Decision 2: drop tasks that OpenScales already defines properly ───────────
# Prefix-normalised, NOT string equality: "Conners Comprehensive Behavior Rating
# Scales" vs "...Scale", "Kessler Psychological Distress Scale (K6+)" vs
# "Kessler Psychological Distress Scale". The 8-character floor stops short
# names producing spurious prefix hits; empty names are already excluded above.
if (!exists("scales")) {
  f <- file.path("data", "scales.rda")
  if (file.exists(f)) load(f) else
    stop("scales not found. Run data-raw/scales.R first.")
}
norm_name <- function(x) gsub("[^a-z0-9]", "", tolower(x))
dict_norm <- norm_name(scales$name)
dict_norm <- dict_norm[nchar(dict_norm) > 8L]

in_openscales <- vapply(norm_name(tasks$name), function(tn) {
  if (nchar(tn) <= 8L) return(any(dict_norm == tn))
  any(startsWith(tn, dict_norm) | startsWith(dict_norm, tn))
}, logical(1))

dropped <- tasks[in_openscales, , drop = FALSE]
tasks   <- tasks[!in_openscales, , drop = FALSE]
rownames(tasks) <- NULL

message(sprintf("tasks: %d harvested | %d dropped as OpenScales duplicates",
                nrow(tasks), nrow(dropped)))
message(sprintf("       %d text-matchable | %d with conditions | %d with contrasts | %d computable",
                sum(tasks$text_ok), sum(tasks$n_conditions > 0),
                sum(tasks$n_contrasts > 0), sum(tasks$n_computable > 0)))
if (nrow(dropped))
  message("       dropped: ", paste(utils::head(dropped$name, 6), collapse = "; "),
          if (nrow(dropped) > 6) sprintf(" ... (+%d more)", nrow(dropped) - 6) else "")

usethis::use_data(tasks, overwrite = TRUE, compress = "xz")
