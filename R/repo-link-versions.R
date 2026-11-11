# Repository links that a space or a hyphen split -----------------------------
#
# Reading a paper joins the parts of a split link only where the joined form is
# certain (.repair_links(), import-read.R). Other splits need the repository
# itself to say which form exists: a Dryad DOI cut in the middle
# ("10.5061/dryad.n2z34 tn2d" for dryad.n2z34tn2d; dryad.n2z34 is also the
# shape of a real, older Dryad DOI), a cut after a deeper "/"
# ("figshare.com/articles/Figure_data/ 8079713", while "github.com/J0vid/
# Color" is an account and a word), a cut inside a name ("github.com/kun-ecolo
# gy/SER"), and a hyphen a line break added ("github.com/Betha-nyL" for
# BethanyL). In 17 corpora (13,706 papers), 192 of the links repo_check
# gathers have another form in the text; 49 of them exist only when joined
# (18 Dryad DOIs, 7 Figshare links, and 24 GitHub links, nearly all an
# account with its repository's name after a space), with 280 requests in
# all (checked 2026-11-11). 3 GitHub links exist only without a hyphen. In an
# earlier sample, 5 links cut after a deeper "/" existed only as written;
# they are kept, because a link that exists as written is never changed.
#
# Only links that may be split are checked, with at most one request for the
# link as written and one per other form. No key is used: GitHub and GitLab
# answer on their websites, Dryad and Zenodo through their DOIs at doi.org,
# Figshare through its public API. OSF is not checked: its website answers
# for any address, and its API allows 100 requests an hour without a key.

# Whether a repository link names an item that exists: TRUE; FALSE when it
# does not, or names no item at all (an account, or a site's own page); NA
# when the host is not one checked here, or the check itself failed.
.repo_link_exists <- function(url, repo_type) {
  url <- sub("[].,;:)]+$", "", url)
  tryCatch(switch(repo_type,
    github = {
      if (!is.null(.github_account_name(url)) || is.null(.github_repo_name(url))) FALSE
      else !is.null(github_repo(url))
    },
    gitlab = {
      if (is.null(.gitlab_repo_path(url))) FALSE else !is.null(gitlab_repo(url))
    },
    dryad = {
      doi <- .dryad_doi(url)
      if (is.na(doi)) FALSE else doi_resolves(doi)
    },
    zenodo = {
      id <- .zenodo_id(url)
      if (is.na(id)) FALSE else doi_resolves(paste0("10.5281/zenodo.", id))
    },
    figshare = {
      art <- .figshare_id(url)
      proj <- .figshare_project_id(url)
      if (!is.na(art)) .figshare_item_exists("articles", art)
      else if (!is.na(proj)) .figshare_item_exists("projects", proj)
      else FALSE
    },
    NA), error = \(e) NA)
}

# Whether Figshare's public API knows an article or project: TRUE (200),
# FALSE (404), NA otherwise.
.figshare_item_exists <- function(kind, id) {
  resp <- tryCatch(
    .mc_request(sprintf("https://api.figshare.com/v2/%s/%s", kind, id)) |>
      httr2::req_error(is_error = \(r) FALSE) |>
      httr2::req_perform(),
    error = \(e) NULL)
  if (is.null(resp)) return(NA)
  status <- httr2::resp_status(resp)
  if (status == 200) TRUE else if (status == 404) FALSE else NA
}

# The forms of a link without one of its hyphens, one per hyphen, in the
# order they occur. Only the owner and repository part of a GitHub or GitLab
# link is changed ("github.com/Betha-nyL/DeepKoopman" ->
# "github.com/BethanyL/DeepKoopman"), never the host.
.repo_link_without_hyphen <- function(url) {
  host_end <- regexpr("(?i)git(?:hub|lab)\\.com/", url, perl = TRUE)
  if (host_end < 0) return(character(0))
  from <- host_end + attr(host_end, "match.length")
  pos <- gregexpr("-", url, fixed = TRUE)[[1]]
  pos <- pos[pos >= from]
  vapply(pos, \(p) paste0(substr(url, 1, p - 1), substr(url, p + 1, nchar(url))),
         character(1))
}

# The link's other forms, from the paper's own text: the link joined with the
# word that follows it after a space in the same sentence, when that word has
# the shape of a link's part (a "/", ".", "_", "=", "-" or digit, or a capital
# letter after its first letter); after a link that ends in a hyphen, the
# form without the hyphen is tried first, as a line break added it. An
# ordinary word ("Further", "The") has none of these, so a sentence that goes
# on after a link gives no other form. A link found without the "/" that
# ends it in the text ("github.com/zhoupc" for "github.com/zhoupc/
# OASIS_matlab") is joined with the "/".
.repo_link_joined <- function(url, sentences) {
  pat <- paste0("\\Q", url, "\\E(/?)\\s+([A-Za-z0-9][A-Za-z0-9._/~=?&#-]*)")
  m <- regmatches(sentences, regexec(pat, sentences, perl = TRUE, ignore.case = TRUE))
  m <- Filter(length, m)
  slash <- vapply(m, `[[`, character(1), 2L)
  nxt <- sub("[].,;:)]+$", "", vapply(m, `[[`, character(1), 3L))
  ok <- grepl("[/._=-]|\\d|.[A-Z]", nxt, perl = TRUE)
  out <- character(0)
  for (j in which(ok)) {
    if (grepl("-$", url) && !nzchar(slash[[j]]))
      out <- c(out, paste0(sub("-$", "", url), nxt[[j]]))
    out <- c(out, paste0(url, slash[[j]], nxt[[j]]))
  }
  unique(out)
}

# repo_check()'s table of links (paper_id, repo_url, repo_type), with each
# link that does not exist as written, but exists joined with the rest the
# text gives it, replaced by that joined form (see the top of this file).
# `paper` is the paper or paper list the links came from.
.repo_links_joined <- function(repos, paper) {
  types <- c("github", "gitlab", "dryad", "zenodo", "figshare")
  rows <- which(repos$repo_type %in% types & !is.na(repos$repo_url))
  if (!length(rows)) return(repos)
  tx <- paper_table(paper, "text", cols = "text")
  by_paper <- split(tx$text, tx$paper_id)
  for (i in rows) {
    url <- repos$repo_url[[i]]
    s <- by_paper[[as.character(repos$paper_id[[i]])]] %||% character(0)
    s <- s[!is.na(s) & grepl(tolower(url), tolower(s), fixed = TRUE)]
    alt <- .repo_link_joined(url, s)
    if (!length(alt)) next
    if (!isFALSE(.repo_link_exists(url, repos$repo_type[[i]]))) next
    for (a in alt) {
      if (isTRUE(.repo_link_exists(a, repos$repo_type[[i]]))) {
        repos$repo_url[[i]] <- a
        break
      }
    }
  }
  dplyr::distinct(repos)
}

# For a GitHub or GitLab link that the listing could not find: the first form
# without one of its hyphens that `exists` (github_repo() or gitlab_repo())
# finds, or NULL. The website is asked, not the API, so a link that is not
# found costs none of the API's hourly requests.
.repo_link_unhyphenated <- function(url, exists) {
  for (a in .repo_link_without_hyphen(url)) {
    ok <- tryCatch(!is.null(exists(a)), error = \(e) FALSE)
    if (ok) return(a)
  }
  NULL
}
