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
# when the host is not one checked here, or the check itself failed. A link
# of another repository that holds a DOI is checked through doi.org's handle
# service, which answers for every registration agency (#634).
.repo_link_exists <- function(url, repo_type) {
  url <- sub("[].,;:)]+$", "", url)
  doi <- regmatches(url, regexpr("10\\.\\d{4,9}/\\S+", url, perl = TRUE))
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
      else if (length(doi)) doi_resolves(doi)
      else FALSE
    },
    if (length(doi)) doi_resolves(doi) else NA), error = \(e) NA)
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

# The link's other forms, from the paper's own text: the link joined with up
# to three words that follow it after a space in the same sentence. A word is
# joined when it has the shape of a link's part (a "/", ".", "_", "=", "-" or
# digit, or a capital letter after its first letter); an ordinary word
# ("Further", "The") has none of these, so a sentence that goes on after a
# link gives no other form. While the link so far is a GitHub or GitLab
# account, with no repository yet, any word is joined: a space can fall
# anywhere in an account's name ("github.com/crs h/papaja",
# "github.com/jgabr y/ bayes_R2"). After a link that ends in a hyphen, the
# form without the hyphen is tried first, as a line break added it. A link
# found without the "/" that ends it in the text ("github.com/zhoupc" for
# "github.com/zhoupc/ OASIS_matlab") is joined with the "/". The longest form
# comes first, so the caller keeps the longest form that exists
# ("github.com/keras -team/keras -tuner" is keras-team/keras-tuner, not
# keras-team/keras).
.repo_link_joined <- function(url, sentences, max_words = 3) {
  pat <- paste0("\\Q", url, "\\E(/?)((?:\\s+\\S+){1,", max_words, "})")
  m <- regmatches(sentences, regexec(pat, sentences, perl = TRUE, ignore.case = TRUE))
  m <- Filter(length, m)
  shaped <- function(w) grepl("[/._=-]|\\d|.[A-Z]", w, perl = TRUE)
  account <- function(u) grepl("(?i)git(?:hub|lab)\\.com/[^/]+/?$", u, perl = TRUE)
  out <- character(0)
  n_words <- integer(0)
  for (x in m) {
    words <- strsplit(trimws(x[[3]]), "\\s+")[[1]]
    form <- paste0(url, x[[2]])
    for (k in seq_along(words)) {
      w <- sub("[].,;:)]+$", "", words[[k]])
      if (!grepl("^[A-Za-z0-9._/~=?&#-]+$", w) || nchar(w) > 60) break
      if (!shaped(w) && !account(form)) break
      if (k == 1 && grepl("-$", url) && !nzchar(x[[2]])) {
        out <- c(out, paste0(sub("-$", "", url), w))
        n_words <- c(n_words, k)
      }
      form <- paste0(form, w)
      out <- c(out, form)
      n_words <- c(n_words, k)
      if (w != words[[k]]) break
    }
  }
  # most words first; with as many words, in the order made (order() keeps it)
  unique(out[order(-n_words)])
}

# Tidy repo_check()'s gathered links (paper_id, repo_url, repo_type) before
# they are checked, for damage that reading the PDF did to them (#634):
# - a link inside an e-mail security wrapper (Outlook "safelinks",
#   Proofpoint "urldefense", Check Point "protect") is replaced by the link it
#   wraps: 237 sentences in 14 papers of 17 corpora;
# - a link ends where another one begins inside it: a DOI written straight
#   after it ("...collabra.48.pr10.1525/..." , 233 sentences in 65 papers), a
#   DOI link whose "https://doi" was lost ("...6a47.org/10.1186/...", 621
#   sentences in 16 papers, nearly all reference lists), or "https" run into
#   it ("PhylogeneticFactorAnalysis.https");
# - a website's home page alone ("https://zenodo.org/", "http://osf.io"), or
#   a DOI's prefix alone ("doi.org/10.5281"), is not a repository and is left
#   out (home pages: 174 sentences in 146 papers);
# - a link that the paper also gives longer is left out when it cannot be
#   whole: an account beside one of its repositories ("github.com/XiaoyangSu"
#   beside "github.com/XiaoyangSu/AccuCor"), a link ending in "-", "_" or ".",
#   an OSF identifier of fewer than 5 characters. Other shorter links
#   ("10.6084/m9" beside "10.6084/m9.figshare.27262470") are left to
#   .repo_links_joined(), which asks which form exists, as names often share
#   a beginning;
# - an OSF identifier of 10 characters is two run together; the first is kept.
# Counts from the text of the 17 corpora (13,706 papers), 2026-11-12.
.repo_links_tidy <- function(repos, paper) {
  if (!nrow(repos)) return(repos)
  u <- vapply(repos$repo_url, .repo_link_unwrapped, character(1), USE.NAMES = FALSE)
  # a link ends where another begins inside it
  u <- sub("(?<!doi)(?<!DOI)\\.org/10\\.\\d{4,9}/.*$", "", u, perl = TRUE)
  first <- regexpr("10\\.\\d{4,9}/", u, perl = TRUE)
  glued <- gregexpr("(?<=[A-Za-z0-9])10\\.\\d{4,9}/", u, perl = TRUE)
  for (i in seq_along(u)) {
    g <- glued[[i]]
    g <- g[g > 0 & (first[[i]] < 0 | g > first[[i]])]
    if (length(g)) u[[i]] <- substr(u[[i]], 1, g[[1]] - 1)
  }
  u <- sub("(?<=[A-Za-z0-9.])https?://.*$", "", u, perl = TRUE)
  u <- sub("[.]https?$", "", u, ignore.case = TRUE)
  # what is left of the space or punctuation between the two links
  u <- sub("(?:%20|[;,\\s])+$", "", u, perl = TRUE)
  u <- sub("[.,;:]+$", "", u)
  # an OSF identifier has five characters: ten are two run together
  # ("osf.io/3bh673bh67", "osf.io/3bh67c9pag"); the second is found on its own
  u <- sub("(?i)(osf\\.io/[a-z0-9]{5})[a-z0-9]{5}(?=$|[/?#])", "\\1", u, perl = TRUE)
  # a DOI written right after the link in the text, where the link's pattern
  # stopped at the DOI's first ".": "dryad.w0vt4b8sh10.1007/s00018-..."
  tx <- paper_table(paper, "text", cols = "text")
  by_paper <- split(tx$text, tx$paper_id)
  for (i in which(grepl("10$", u))) {
    s <- by_paper[[as.character(repos$paper_id[[i]])]] %||% character(0)
    if (any(grepl(paste0("\\Q", u[[i]], "\\E\\.\\d{4,9}/"), s, perl = TRUE, ignore.case = TRUE)))
      u[[i]] <- sub("10$", "", u[[i]])
  }
  repos$repo_url <- u
  # a website's home page alone, or a DOI's prefix alone ("doi.org/10.5281");
  # a name without "/" that is not a website's ("knb.341.1", a DataONE
  # identifier) stays
  bare <- sub("/+$", "", sub("^(?:https?://)?(?:www\\.|dx\\.)?", "", tolower(u), perl = TRUE))
  home <- grepl("^(?:[a-z0-9-]+\\.)+[a-z]{2,}$", bare, perl = TRUE) |
    grepl("^(?:doi\\.org/)?10\\.\\d{4,9}$", bare, perl = TRUE)
  repos <- repos[!home, , drop = FALSE]
  repos <- dplyr::distinct(repos)
  # a link cut inside a name, beside its longer form; an account beside one
  # of its repositories
  key <- sub("[/.]+$", "", sub("^(?:https?://)?(?:www\\.|dx\\.)?(?:doi\\.org/)?", "",
                               tolower(repos$repo_url), perl = TRUE))
  drop <- vapply(seq_along(key), \(i) {
    others <- key[repos$paper_id == repos$paper_id[[i]] & key != key[[i]]]
    longer <- others[startsWith(others, key[[i]])]
    # a longer form that only repeats the link's last part is the reader
    # writing it twice ("...Policy" beside "...PolicyPolicy"), not more of it
    rest <- substring(longer, nchar(key[[i]]) + 1)
    longer <- longer[!(nchar(rest) >= 3 & endsWith(key[[i]], rest))]
    if (!length(longer)) return(FALSE)
    nxt <- substr(longer, nchar(key[[i]]) + 1, nchar(key[[i]]) + 1)
    # Only a link that cannot be whole is left out here: an account beside one
    # of its repositories, a link ending in "-", "_" or ".", an OSF identifier
    # of fewer than 5 characters. Names often share a beginning
    # ("mastodon-sc/mastodon" and ".../mastodon-averoflab" are two
    # repositories): of 120 shorter links left out by a rule without these
    # limits in 17 corpora, 27 existed (checked 2026-11-12). The others are
    # decided by .repo_links_joined(), which asks which form exists.
    (grepl("^git(?:hub|lab)\\.com/[^/]+$", key[[i]]) && any(nxt == "/")) ||
      grepl("[-_.]$", key[[i]]) ||
      grepl("^(?:10\\.17605/)?osf\\.io/[a-z0-9]{1,4}$", key[[i]])
  }, logical(1))
  repos[!drop, , drop = FALSE]
}

# Left out by .repo_links_joined(): a link that does not exist, when the paper
# also gives a longer form of it that does ("10.6084/m9" beside
# "10.6084/m9.figshare.27262470"). TRUE for the rows to drop.
.repo_links_cut_short <- function(repos) {
  key <- sub("[/.]+$", "", sub("^(?:https?://)?(?:www\\.|dx\\.)?(?:doi\\.org/)?", "",
                               tolower(repos$repo_url), perl = TRUE))
  vapply(seq_len(nrow(repos)), \(i) {
    others <- which(repos$paper_id == repos$paper_id[[i]] & key != key[[i]] &
                      startsWith(key, key[[i]]))
    if (!length(others)) return(FALSE)
    if (!isFALSE(.repo_link_exists(repos$repo_url[[i]], repos$repo_type[[i]]))) return(FALSE)
    any(vapply(others, \(j) isTRUE(.repo_link_exists(repos$repo_url[[j]], repos$repo_type[[j]])),
               logical(1)))
  }, logical(1))
}

# The link inside an e-mail security wrapper, or `url` itself
.repo_link_unwrapped <- function(url) {
  if (is.na(url)) return(url)
  dec <- function(x) tryCatch(utils::URLdecode(x), error = \(e) x)
  if (grepl("safelinks\\.protection\\.outlook\\.com/.*[?&]url=", url, ignore.case = TRUE))
    return(dec(sub("(?i)^.*[?&]url=([^&]+).*$", "\\1", url, perl = TRUE)))
  if (grepl("urldefense\\.proofpoint\\.com/v2/url\\?u=", url, ignore.case = TRUE)) {
    x <- sub("(?i)^.*\\?u=([^&]+).*$", "\\1", url, perl = TRUE)
    return(dec(gsub("-([0-9A-F]{2})", "%\\1", gsub("_", "/", x, fixed = TRUE))))
  }
  # (this wrapper writes "https:/" with one slash)
  if (grepl("urldefense\\.com/v3/__", url, ignore.case = TRUE))
    return(sub("^(https?:)/(?!/)", "\\1//",
               sub("(?i)^.*/v3/__(.+?)__.*$", "\\1", url, perl = TRUE), perl = TRUE))
  if (grepl("protect\\.checkpoint\\.com/v2/___", url, ignore.case = TRUE))
    return(sub("(?i)^.*/v2/___(.+?)___.*$", "\\1", url, perl = TRUE))
  url
}

# repo_check()'s table of links (paper_id, repo_url, repo_type), with each
# link that does not exist as written, but exists joined with the rest the
# text gives it, replaced by that joined form (see the top of this file).
# `paper` is the paper or paper list the links came from.
.repo_links_joined <- function(repos, paper) {
  # GitHub and GitLab on their websites; the others through their DOI
  types <- c("github", "gitlab", "dryad", "zenodo", "figshare")
  rows <- which(!is.na(repos$repo_url) & (repos$repo_type %in% types |
                  grepl("10\\.\\d{4,9}/", repos$repo_url, perl = TRUE)))
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
  repos <- dplyr::distinct(repos)
  # a link cut short, beside its longer form in the same paper (#634)
  checkable <- repos$repo_type %in% types | grepl("10\\.\\d{4,9}/", repos$repo_url, perl = TRUE)
  if (any(checkable)) {
    cut <- rep(FALSE, nrow(repos))
    cut[checkable] <- .repo_links_cut_short(repos[checkable, , drop = FALSE])
    repos <- repos[!cut, , drop = FALSE]
  }
  repos
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
