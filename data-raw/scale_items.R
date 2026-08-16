# Build `scale_items` and `scale_scoring`: the ITEM-LEVEL companion to `scales`.
#
# `scales` (data-raw/scales.R) answers "is this column block the PANAS?" from a
# name / acronym. It is built from the OpenScales *manifest*, which is metadata
# only (204 records, no items). The repository itself ships ~1,095 `.osd` files
# carrying the parts the manifest omits: item text, subscale structure,
# per-item scoring weights (+1 / -1, i.e. REVERSE CODING) and Cronbach's alpha.
# This script harvests those, so codebook_check / data_validate can check a
# shared dataset against the real instrument rather than just its name.
#
# The whole repo is fetched ONCE as a tarball (~11 MB) rather than 1,095 raw
# requests, which would hit GitHub's unauthenticated rate limit.
#
# ── Collections (n = 1,095) ───────────────────────────────────────────────────
#   ipip        356  Public Domain               IPIP personality pools
#   phenx       420  PhenX Toolkit protocol      NIH-standard measures
#   openscales  262  mostly CC BY 4.0            community-contributed scales
#   miss         35  mostly CC BY 4.0            ZIS/GESIS short scales
#   restricted   22  CC BY-NC-*, CC BY-ND-*      EXCLUDED — see below
#
# ── Two deliberate exclusions ─────────────────────────────────────────────────
# 1. `restricted/` is dropped entirely. Every file in it carries a NonCommercial
#    or NoDerivatives licence (CC BY-NC-SA, CC BY-NC-ND, "free for research
#    use"). metacheck is GPL-3 on CRAN, so NC/ND item text cannot ship with it.
#    The scale NAMES stay reachable via `scales`; only the items are withheld.
# 2. `text_ok` (see below) gates which rows may be regex-scanned against
#    manuscript prose. PhenX names its measures after TOPICS, not instruments
#    ("Insomnia", "Body Image", "Birthplace", "Immunizations"). Scanning those
#    across paper text would fire on ordinary prose. They are still ingested —
#    their items are good — but flagged as unsafe for text matching.
#
# Columns of `scale_items` (one row per item):
#   code        OpenScales code, joins to `scales$code`
#   item_id     item id within the scale
#   text        English item wording, resolved via translations[["en"]][text_key]
#   dimension   subscale the item belongs to ("" when unassigned)
#   type        item type (likert, multi, text, ...)
#   reverse     TRUE when the item is reverse-keyed (weight -1) in ANY subscale
#   position    1-based order within the scale
#
# Columns of `scale_scoring` (one row per scale x subscale):
#   code, dimension, method, n_items, n_reverse, alpha, description
#
# Rebuild with:  source("data-raw/scale_items.R")

`%||%` <- function(a, b) if (is.null(a)) b else a

# ── Fetch the repo once ───────────────────────────────────────────────────────
tar_url  <- "https://github.com/stmueller/OpenScales/archive/refs/heads/main.tar.gz"
tmp      <- tempfile(fileext = ".tar.gz")
exdir    <- tempfile("openscales")
utils::download.file(tar_url, tmp, mode = "wb", quiet = TRUE)
utils::untar(tmp, exdir = exdir)

osd_files <- list.files(exdir, pattern = "\\.osd$", recursive = TRUE,
                        full.names = TRUE)
# `runner/examples/*.osd` are demo files, not instruments.
osd_files <- osd_files[grepl("/scales/", gsub("\\\\", "/", osd_files), fixed = TRUE)]

collection_of <- function(path) {
  p <- strsplit(gsub("\\\\", "/", path), "/scales/", fixed = TRUE)[[1]]
  if (length(p) < 2) return("")
  strsplit(p[2], "/", fixed = TRUE)[[1]][1]
}

# NC / ND licences cannot ship in a GPL-3 CRAN package.
EXCLUDE_COLLECTIONS <- "restricted"

# ── Name quality: may this row be matched against manuscript prose? ────────────
# Mirrors data-raw/scales.R's generic-token logic. A name of <= 2 CONTENT tokens
# ("Insomnia", "Body Image") is a topic label, not an instrument name.
generic_tok <- c("scale","scales","questionnaire","inventory","test","index",
  "survey","measure","checklist","rating","self","short","form","revised",
  "brief","version","the","of","for","and","a","an","in","to","assessment",
  "schedule","screen","screening")
content_tokens <- function(s) {
  toks <- unlist(strsplit(tolower(s %||% ""), "[^a-z0-9]+"))
  unique(toks[nzchar(toks) & !(toks %in% generic_tok)])
}
# `PX010201`, `MISS10991`: catalogue IDs masquerading as abbreviations.
is_id_like <- function(a) grepl("^(PX|MISS)[0-9]+$", a %||% "")

# Is this name safe to regex against running prose?
#
# The question is NOT "is this name long?" but "is this the name of an
# INSTRUMENT, or of a TOPIC?". Those come apart by collection, not by length:
#
#   * PhenX names measures after the construct measured — "Insomnia", "Body
#     Image", "General Well-being", "Sleep Apnea - Adult", "Coping with
#     COVID-19". A paper writing "we measured general well-being" must never be
#     recorded as having administered PhenX PX720301. So PhenX is excluded
#     WHOLESALE. (A token-count rule does not catch these: "Sleep Apnea -
#     Adult" has 3 content tokens and would pass.)
#   * Every other collection names actual instruments. Short names there are
#     still instrument names, not prose: "Grit Scale", "COPE Inventory",
#     "CAGE Questionnaire", "Self-Compassion Scale", "Rosenberg Self-Esteem
#     Scale". Requiring >= 3 content tokens wrongly excluded 117 of them —
#     including the Rosenberg, the BDI and the BFI, since `content_tokens`
#     strips "self", "scale" and "inventory" as generic. Length is simply the
#     wrong axis.
#
# Their ITEMS are ingested either way and stay joinable by `code`; `text_ok`
# governs only whether the NAME may be matched against a manuscript.
name_text_ok <- function(name, collection) {
  if (identical(collection, "phenx")) return(FALSE)
  length(content_tokens(name)) >= 1L
}

# Cronbach's alpha as recorded in the free-text `description`. Upstream has at
# least one typo (`Cronbach's alpha = 12` in IPIP-MPQ / IPIP-MPQ-WB), so the
# parsed value is range-checked: alpha is a correlation-like coefficient and
# anything outside [0, 1] is dropped to NA rather than shipped as data.
parse_alpha <- function(desc) {
  if (!grepl("alpha", desc, ignore.case = TRUE)) return(NA_real_)
  a <- suppressWarnings(as.numeric(
    sub(".*alpha\\s*=\\s*([0-9.]+).*", "\\1", desc)))
  if (length(a) != 1 || is.na(a) || a < 0 || a > 1) return(NA_real_)
  a
}

# ── Resolve one .osd ──────────────────────────────────────────────────────────
# `scoring` has three shapes in the wild, all of which must be handled:
#   items = named list  -> weights, e.g. {"i1": 1, "i2": -1}   (reverse coding)
#   items = plain array -> unweighted, all +1
#   scores = array      -> second-order factor over other subscales, no items
# A `_note` key can also hold a bare string instead of a spec object.
parse_osd <- function(path) {
  j <- tryCatch(jsonlite::read_json(path, simplifyVector = FALSE),
                error = function(e) NULL)
  if (is.null(j)) return(NULL)
  defn <- j$definition %||% list()
  si   <- defn$scale_info %||% list()
  code <- as.character(si$code %||% "")
  if (!nzchar(code)) return(NULL)

  items <- defn$items %||% list()
  if (!length(items)) return(NULL)
  en <- (j$translations %||% list())$en %||% list()

  # -- scoring: collect per-item weights and per-dimension summaries -----------
  weights <- list()   # item_id -> any weight seen (-1 wins)
  sc_rows <- list()
  scoring <- defn$scoring %||% list()
  for (dim in names(scoring)) {
    spec <- scoring[[dim]]
    if (!is.list(spec)) next            # `_note`: a bare string
    it <- spec$items
    w  <- character(0)
    n_rev <- 0L
    if (is.list(it) && length(it)) {
      if (!is.null(names(it)) && any(nzchar(names(it)))) {
        # named -> weights
        for (id in names(it)) {
          v <- suppressWarnings(as.numeric(it[[id]]))
          if (length(v) != 1 || is.na(v)) next
          if (identical(v, -1)) n_rev <- n_rev + 1L
          prev <- weights[[id]] %||% 1
          weights[[id]] <- if (identical(v, -1) || identical(prev, -1)) -1 else prev
        }
        w <- names(it)
      } else {
        # unnamed -> plain item ids, unweighted (all +1)
        w <- unlist(it, use.names = FALSE)
        for (id in w) if (is.null(weights[[id]])) weights[[id]] <- 1
      }
    }
    # Cronbach's alpha is only ever recorded in free text ("Cronbach's alpha = 0.78").
    desc  <- as.character(spec$description %||% "")
    alpha <- parse_alpha(desc)

    sc_rows[[length(sc_rows) + 1L]] <- data.frame(
      code        = code,
      dimension   = dim,
      method      = as.character(spec$method %||% ""),
      n_items     = length(w),
      n_reverse   = n_rev,
      alpha       = alpha,
      description = desc
    )
  }

  # -- items ------------------------------------------------------------------
  it_rows <- lapply(seq_along(items), function(k) {
    it <- items[[k]]
    if (!is.list(it)) return(NULL)
    id <- as.character(it$id %||% "")
    tk <- as.character(it$text_key %||% "")
    txt <- as.character(en[[tk]] %||% "")
    data.frame(
      code      = code,
      item_id   = id,
      text      = txt,
      dimension = as.character(it$dimension %||% ""),
      type      = as.character(it$type %||% ""),
      reverse   = identical(weights[[id]] %||% 1, -1),
      position  = k
    )
  })
  it_df <- do.call(rbind, it_rows[!vapply(it_rows, is.null, logical(1))])
  if (is.null(it_df) || !nrow(it_df)) return(NULL)

  nm   <- as.character(si$name %||% "")
  abbr <- as.character(si$abbreviation %||% "")
  coll <- collection_of(path)
  meta <- data.frame(
    code        = code,
    name        = nm,
    acronym     = if (is_id_like(abbr)) "" else abbr,
    collection  = coll,
    license     = as.character(si$license %||% ""),
    citation    = as.character(si$citation %||% ""),
    url         = as.character(si$url %||% ""),
    domain      = as.character(si$domain %||% ""),
    n_items     = nrow(it_df),
    n_reverse   = sum(it_df$reverse),
    languages   = paste(names(j$translations %||% list()), collapse = ","),
    text_ok     = name_text_ok(nm, coll)
  )
  list(meta = meta,
       items = it_df,
       scoring = if (length(sc_rows)) do.call(rbind, sc_rows) else NULL)
}

parsed <- lapply(osd_files, function(f) {
  if (collection_of(f) %in% EXCLUDE_COLLECTIONS) return(NULL)
  parse_osd(f)
})
parsed <- parsed[!vapply(parsed, is.null, logical(1))]

scale_meta    <- do.call(rbind, lapply(parsed, `[[`, "meta"))
scale_items   <- do.call(rbind, lapply(parsed, `[[`, "items"))
scale_scoring <- do.call(rbind, lapply(parsed, function(p) p$scoring))

# Drop items whose English wording did not resolve — an item with no text is
# useless for matching and would only dilute the table.
#
# This costs 12 of the 1,073 eligible files, all for defensible reasons:
#   * 2 have no `code` at all (openscales/EBQ, openscales/LCS)
#   * 3 have no `items` block (miss/MISS11061, MISS11077, MISS13651)
#   * 7 are single-language editions with no English translation — Swahili,
#     German, Portuguese, Hungarian (e.g. miss/MISS11207 [sw], openscales/EmSE
#     [de], openscales/HSPS20H [hu]).
# The last group is a genuine limitation, not a parse failure: item text is
# resolved through translations[["en"]], so a scale published only in German is
# out of scope. Its NAME is still reachable through `scales`. If non-English
# item matching is ever wanted, this is the place to widen (`languages` on
# scale_meta already records what each scale offers).
scale_items <- scale_items[nzchar(scale_items$text), , drop = FALSE]
scale_meta  <- scale_meta[scale_meta$code %in% scale_items$code, , drop = FALSE]

rownames(scale_meta) <- rownames(scale_items) <- NULL
if (!is.null(scale_scoring)) rownames(scale_scoring) <- NULL

message(sprintf(
  "scale_meta:    %d instruments (%d text-matchable) across %d collections",
  nrow(scale_meta), sum(scale_meta$text_ok),
  length(unique(scale_meta$collection))))
message(sprintf(
  "scale_items:   %d items (%d reverse-keyed, %.0f%% of scales have >=1)",
  nrow(scale_items), sum(scale_items$reverse),
  100 * mean(tapply(scale_items$reverse, scale_items$code, any))))
message(sprintf(
  "scale_scoring: %d subscales (%d with a Cronbach's alpha)",
  nrow(scale_scoring), sum(!is.na(scale_scoring$alpha))))

usethis::use_data(scale_meta, scale_items, scale_scoring,
                  overwrite = TRUE, compress = "xz")
