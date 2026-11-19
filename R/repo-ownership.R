# Whose deposit is it? (#634)
#
# repo_check() finds repository links in a paper. Some hold the authors' own
# data, code or materials, made for this paper or earlier; others hold other
# people's data or code that the paper reuses; and some are not deposits at
# all (a preprint or other publication). .repo_ownership() tells these apart
# without a language model, from four kinds of information:
#
# - the sentence or reference that cites the link (the paper's own text);
# - the deposit's own record, from the request that lists its files: who made
#   it (names, ORCID iDs), its title, description, type, the papers it names,
#   and its dates;
# - the paper's own record: its authors' names, ORCID iDs and affiliations,
#   its title and DOI;
# - the deposit's files: a software package's layout, installation
#   instructions in its README, file names that share words with the paper.
#
# The rules were written as general principles on eight rounds of 200 links
# (100 from the main text, 100 from reference lists) drawn from 13,706
# papers in 17 corpora, each round tested before the next rule was written.
# On the rounds a rule had not seen, the three classes (the authors' own,
# others', not a deposit) were right for about 97% of main-text links and
# 92% of reference-list links. Each link gets one of six labels:
#
#   own_this          made by the authors for this paper
#   own_earlier_data  the authors' own data from earlier work
#   own_earlier_code  the authors' own code or tool from earlier work
#   others_data       data or materials made by other people
#   others_code       code or a tool made by other people
#   not_data          not a deposit (a preprint, an article, a manuscript)
#
# and the step that decided it. The split between this paper and earlier
# work is the least certain part; the three groups are reliable.

# ---- the paper -------------------------------------------------------------

# The facts about the paper the rules compare a deposit with: its title, DOI
# and abstract, its authors' family names, full names, ORCID iDs and
# affiliations, and its year (the newest year in its reference list, since a
# paper read from a PDF carries no date of its own).
.own_paper_facts <- function(paper) {
  au <- .FAIR_repo_authors(paper)
  raw <- paper$author
  fam <- if (is.data.frame(raw) && "family" %in% names(raw)) as.character(raw$family) else character(0)
  # without Grobid's noise ("Hecker -Orcid"; #682)
  fam <- .own_fam_clean(fam[!is.na(fam)])
  fam <- fam[nzchar(fam)]
  orc <- au$orcid %||% character(0)
  orc <- unlist(regmatches(orc, gregexpr("\\d{4}-\\d{4}-\\d{4}-\\d{3}[0-9X]", orc)))
  aff <- unlist(strsplit(au$affiliation[!is.na(au$affiliation)], "[;\n\t]+"))
  aff <- unique(trimws(aff)); aff <- aff[nchar(aff) > 2]
  b <- paper$bib
  yr <- if (is.data.frame(b) && "year" %in% names(b)) suppressWarnings(as.numeric(b$year)) else numeric(0)
  this_year <- as.numeric(format(Sys.Date(), "%Y"))
  yr <- yr[!is.na(yr) & yr >= 1900 & yr <= this_year]
  year <- if (length(yr)) max(yr) else NA_real_
  tx <- paper$text
  se <- paper$section
  abstract <- ""
  if (is.data.frame(tx) && is.data.frame(se) && all(c("section_id", "section_type") %in% names(se))) {
    st <- se$section_type[match(tx$section_id, se$section_id)]
    abstract <- paste(tx$text[st %in% "abstract"], collapse = " ")
  }
  # a paper without a title or DOI (an empty info table, or no such column)
  # gets NA, not a value of length zero
  one <- function(v) {
    v <- as.character(unlist(v))
    if (length(v) && !is.na(v[[1]]) && nzchar(trimws(v[[1]]))) v[[1]] else NA_character_
  }
  full <- au$name[nzchar(au$name)]
  # a paper whose author table is empty (53 of 13,706 papers in 17 corpora)
  # still names its authors in the text, in 25 of them: the byline after the
  # title, or "Correspondence ... should be addressed to <name>"
  if (!length(fam)) {
    ta <- .own_text_authors(paper, one(paper$info$title))
    fam <- ta$family; full <- c(full, ta$full)
  }
  list(
    title = one(paper$info$title),
    doi = tolower(one(paper$info$doi)),
    family = fam,
    full = full,
    orcids = unique(orc),
    affiliations = aff,
    year = year,
    date = if (is.na(year)) as.Date(NA) else as.Date(paste0(year, "-07-01")),
    abstract = abstract
  )
}

# The authors a paper names in its text, for a paper whose author table is
# empty: the byline that follows the title in the first sentence ("Alexandre
# René 1,2,3 & André Longtin 3, 4, 5", each name followed by its affiliation
# numbers), and the person correspondence is addressed to. Returns full names
# and family names (the last word of each name).
.own_text_authors <- function(paper, title = NA_character_) {
  none <- list(full = character(0), family = character(0))
  tx <- paper$text
  if (!is.data.frame(tx) || !nrow(tx) || !"text" %in% names(tx)) return(none)
  s <- as.character(tx$text); s[is.na(s)] <- ""
  nm <- "\\p{Lu}[\\p{L}'.-]+(?:\\s+\\p{Lu}[\\p{L}'.-]*){1,3}"
  found <- character(0)
  # the byline: names, each followed by affiliation numbers, after the title
  if (!is.na(title) && nchar(title) >= 20) {
    k <- which(grepl(substr(title, 1, 40), s, fixed = TRUE))[1]
    if (!is.na(k)) {
      rest <- substring(s[[k]], regexpr(substr(title, 1, 40), s[[k]], fixed = TRUE) + nchar(title))
      by <- regmatches(rest, regexpr(paste0("^\\s*(?:", nm, "\\s*[0-9][0-9,*\\s]*(?:&|and|,)?\\s*)+"),
                                     rest, perl = TRUE))
      if (length(by)) found <- c(found, regmatches(by, gregexpr(nm, by, perl = TRUE))[[1]])
    }
  }
  # "Correspondence and requests for materials should be addressed to <name>"
  m <- regmatches(s, regexpr(paste0("(?i:correspondence|requests for materials)[^.]{0,80}?",
                                    "(?i:addressed|sent) to:?\\s+(?:", nm, ")(?:\\s*(?:,|or|and|&)\\s*", nm, ")*"),
                             s, perl = TRUE))
  for (x in m) {
    x <- sub("^.*?(?i:addressed|sent) to:?\\s+", "", x, perl = TRUE)
    found <- c(found, regmatches(x, gregexpr(nm, x, perl = TRUE))[[1]])
  }
  found <- unique(sub("[.]+$", "", trimws(found)))
  found <- found[nchar(found) >= 4]
  if (!length(found)) return(none)
  family <- vapply(strsplit(found, "\\s+"), \(w) w[[length(w)]], character(1))
  list(full = found, family = unique(family[nchar(family) >= 2]))
}

# ---- the paper's text around each link -------------------------------------

# The part of a link that identifies it, for finding it in a sentence
.own_core <- function(h) tolower(gsub("[[:space:]]", "", sub("^https?://(dx\\.)?(www\\.)?", "", sub("/+$", "", h))))

# One key for every form of one deposit's link, for telling links apart and
# matching them to what a lookup learnt: a DOI as written in a reference
# ("10.6084/m9.figshare.29094266") and as a link ("https://doi.org/10.6084/
# m9.figshare.29094266"), an OSF project and its other forms (see
# .repo_link_same_key()). With .own_core() alone a reference's DOI kept both
# forms, and the lookup, which joins them, matched its record to one: the
# other was classified without it, so one deposit was the paper's own and a
# publication at once (#644).
.own_key <- function(h) .own_core(.repo_link_same_key(h))

# What to look for in a sentence to find a link: the link's own words, or,
# for a sequence or expression archive's record (#660), its accession number,
# as papers name these deposits by number ("GSE149523") more than by link
.own_needle <- function(h) {
  acc <- .seq_acc_of(h)
  archive <- !is.na(acc) & grepl("ncbi\\.nlm\\.nih\\.gov/geo/|ebi\\.ac\\.uk/(ena|biostudies)", h)
  # an address found by resolving a Handle: the Handle the paper gave
  alias <- .repo_link_alias_of(h)
  ifelse(archive, tolower(acc), ifelse(!is.na(alias), .own_core(alias), .own_core(h)))
}

# For main-text links: the sentences holding the link (at most two), their
# sections ("type:header"), and whether those sentences cite other people's
# work or the authors' own (a reference sharing a family name with them).
.own_text_context <- function(paper, urls) {
  n <- length(urls)
  out <- data.frame(repo_url = urls, context = rep(NA_character_, n), sections = rep(NA_character_, n),
                    cites_others = rep(NA, n), cites_own = rep(NA, n))
  tx <- paper$text
  if (!n || !is.data.frame(tx) || !nrow(tx)) return(out)
  se <- paper$section
  sec <- if (is.data.frame(se)) se[match(tx$section_id, se$section_id), , drop = FALSE] else
    data.frame(section_type = rep(NA_character_, nrow(tx)), header = NA_character_)
  squeezed <- tolower(gsub("[[:space:]]", "", tx$text))
  refs <- sec$section_type %in% "references"
  fam <- .own_paper_facts(paper)$family
  b <- paper$bib; x <- paper$xref
  for (i in seq_len(n)) {
    hit <- which(!is.na(tx$text) & grepl(.own_needle(urls[[i]]), squeezed, fixed = TRUE) & !refs)
    if (!length(hit)) next
    out$context[[i]] <- paste(unique(tx$text[hit])[seq_len(min(2, length(unique(tx$text[hit]))))], collapse = " || ")
    out$sections[[i]] <- paste(unique(paste0(sec$section_type[hit], ":", sec$header[hit])), collapse = " | ")
    cited <- if (is.data.frame(x) && nrow(x) && all(c("xref_id", "text_id", "xref_type") %in% names(x)))
      unique(x$xref_id[x$text_id %in% tx$text_id[hit] & x$xref_type %in% "bibr"]) else character(0)
    if (length(cited) && is.data.frame(b) && all(c("bib_id", "authors") %in% names(b))) {
      au <- b$authors[match(cited, b$bib_id)]
      ov <- vapply(au, \(a) isTRUE(.own_people_author(as.character(a %||% NA), fam)), logical(1))
      out$cites_others[[i]] <- any(!ov); out$cites_own[[i]] <- any(ov)
    } else {
      out$cites_others[[i]] <- FALSE; out$cites_own[[i]] <- FALSE
    }
  }
  out
}

# Deposits cited in the reference list and not linked in the main text: each
# reference becomes one sentence (its text and its DOI as a link), and the
# same link finders as repo_check() read them. dspace7_links() asks doi.org
# where every DOI leads, which a reference list full of DOIs makes slow, so
# only its host-name part is used here. Returns one row per deposit with the
# reference's text, authors, year and title, and the sentences citing it.
.own_reference_links <- function(paper, exclude = character(0)) {
  empty <- data.frame(repo_url = character(0), repo_type = character(0), bib_id = character(0),
                      ref_text = character(0), ref_authors = character(0), ref_year = character(0),
                      ref_title = character(0), citing = character(0), citing_sections = character(0))
  b <- paper$bib; tx <- paper$text
  if (!is.data.frame(b) || !nrow(b) || !"text_id" %in% names(b) || !is.data.frame(tx)) return(empty)
  id <- paper_id(paper)[[1]] %||% "paper"
  s <- tx$text[match(b$text_id, tx$text_id)]
  s[is.na(s)] <- ""
  d <- ifelse(!is.na(b$doi) & nzchar(b$doi), paste0(" https://doi.org/", b$doi, " "), "")
  q <- paper(id)
  q$text <- data.frame(text = paste0(s, d), text_id = seq_len(nrow(b)), paragraph_id = seq_len(nrow(b)),
                       section_id = 1L, paper_id = id)
  q$section <- data.frame(section_id = 1L, header = "refs", section_type = "body")
  # repo_check's own finders, the general ones included (EPrints, Invenio,
  # CKAN, #636, #637). dspace7_links() looks up only DOIs under a DSpace 7
  # prefix (78 lookups in the reference lists of 17 corpora), so a DSpace DOI
  # in a reference list is found too, not only a DSpace address
  # and, each naming its links' type itself, the sequence archives (#660)
  # and Handles that lead to one of these repositories
  finders <- c(.repo_link_finders(), list(seq = seq_archive_links, handle = handle_links, icpsr = icpsr_links))
  found <- dplyr::bind_rows(lapply(names(finders), \(k) {
    x <- tryCatch(suppressMessages(suppressWarnings(finders[[k]](q))), error = \(e) NULL)
    if (!is.data.frame(x) || !nrow(x) || !"href" %in% names(x)) return(NULL)
    # the sequence archives' finder names each link's archive itself (#660)
    data.frame(href = sub("[.,;:]+$", "", x$href), text_id = x$text_id %||% NA_integer_,
               repo_type = x$repo_type %||% k)
  }))
  if (is.null(found) || !nrow(found)) return(empty)
  # a GitHub repository written by its name alone ("GitHub repository,
  # bouralab/Prop3D") gets its address, so it is looked up and matched to the
  # same repository linked in the text
  gh_bare <- found$repo_type %in% "github" & !grepl("github", found$href, ignore.case = TRUE) &
    grepl("^[A-Za-z0-9_.-]+/[A-Za-z0-9_.-]+$", found$href)
  found$href[gh_bare] <- paste0("https://github.com/", found$href[gh_bare])
  # a link whose ending reading the PDF wrote twice ("macroeco_phylophylo")
  found$href <- .repo_link_undoubled(found$href, .paper_link_hrefs(paper))
  # one row per deposit, however often and in whatever form the reference
  # writes it; the full address is kept, which a report can link to
  found <- found[order(!grepl("^https?://", found$href, ignore.case = TRUE)), , drop = FALSE]
  found <- found[!duplicated(.own_key(found$href)), , drop = FALSE]
  found <- found[!.own_key(found$href) %in% .own_key(exclude), , drop = FALSE]
  if (!nrow(found)) return(empty)
  se <- paper$section
  sec <- if (is.data.frame(se)) se[match(tx$section_id, se$section_id), , drop = FALSE] else NULL
  x <- paper$xref
  rows <- lapply(seq_len(nrow(found)), \(i) {
    j <- found$text_id[[i]]
    if (is.na(j)) j <- which(grepl(.own_needle(found$href[[i]]), tolower(gsub("[[:space:]]", "", q$text$text)), fixed = TRUE))[1]
    if (is.na(j)) return(NULL)
    cit <- if (is.data.frame(x) && nrow(x)) unique(x$text_id[x$xref_id %in% b$bib_id[[j]] & x$xref_type %in% "bibr"]) else integer(0)
    cs <- if (!is.null(sec)) sec[match(cit, tx$text_id), , drop = FALSE] else NULL
    data.frame(repo_url = found$href[[i]], repo_type = found$repo_type[[i]], bib_id = as.character(b$bib_id[[j]]),
               ref_text = s[[j]], ref_authors = as.character(b$authors[[j]] %||% NA),
               ref_year = as.character(b$year[[j]] %||% NA), ref_title = as.character(b$title[[j]] %||% NA),
               citing = paste(tx$text[match(cit, tx$text_id)][seq_len(min(3, length(cit)))], collapse = " || "),
               citing_sections = if (is.null(cs)) NA_character_ else
                 paste(unique(paste0(cs$section_type, ":", cs$header)), collapse = " | "))
  })
  out <- dplyr::bind_rows(rows)
  if (!nrow(out)) empty else out
}

# Is a reference-list link a preprint server's record? Those are publications,
# decided without asking any host (PsyArXiv, MetaArXiv, OSF Preprints,
# SocArXiv, bioRxiv, arXiv and others; an OSF id whose reference gives a
# preprint server's DOI for it).
.own_preprint_rx <- "10\\.(31234|31222|31219|31235|31730|31233|31236|31237|35542|21203|1101|48550)/"
.own_preprint_link <- function(href, ref_text = NA_character_) {
  osf_id <- ifelse(grepl("osf\\.io/[a-z0-9]{5}", href, ignore.case = TRUE),
                   tolower(sub("^.*osf\\.io/([a-z0-9]{5}).*$", "\\1", href, ignore.case = TRUE)), NA)
  grepl(.own_preprint_rx, href, ignore.case = TRUE) |
    (!is.na(osf_id) & mapply(\(id, r) !is.na(id) && !is.na(r) &&
      grepl(paste0(sub("/$", "", .own_preprint_rx), "/osf\\.io/", id), tolower(r)), osf_id, ref_text))
}

# A paper whose text is one link per sentence, for looking up deposits cited
# in a reference list with repo_check()'s own machinery
.own_reference_paper <- function(id, urls) {
  q <- paper(id)
  q$text <- data.frame(text = urls, text_id = seq_along(urls), paragraph_id = seq_along(urls),
                       section_id = 1L, paper_id = id)
  q$section <- data.frame(section_id = 1L, header = "refs", section_type = "body")
  q
}

# A README's text, from its plain download address (one request; at most
# 300 kB read, 20,000 characters kept). NA when it cannot be read.
.repo_readme_text <- function(url, n = 20000) {
  # a space or other character an address may not hold, encoded (#667);
  # a file on disk is left as it is
  web <- !is.na(url) & grepl("^https?://", url)
  url[web] <- utils::URLencode(url[web])
  if (length(url) != 1 || is.na(url) || !nzchar(url)) return(NA_character_)
  r <- tryCatch(.mc_request(url) |> httr2::req_error(is_error = \(e) FALSE) |>
                  httr2::req_timeout(30) |> httr2::req_perform(), error = \(e) NULL)
  if (is.null(r) || httr2::resp_status(r) != 200) return(NA_character_)
  b <- httr2::resp_body_raw(r)
  if (length(b) > 3e5) b <- b[seq_len(3e5)]
  s <- tryCatch(rawToChar(b[b != as.raw(0)]), error = \(e) NA_character_)
  if (is.na(s)) return(NA_character_)
  Encoding(s) <- "UTF-8"
  substr(s, 1, n)
}

# The README at a code repository's top level, from its file listing (columns
# `path` and `download_url`, as github_tree_files() and gitlab_tree_files()
# give them): README.md first, then README, .rst, .txt or .markdown. One plain
# file request, not the host's rate-limited API.
.repo_readme_from_listing <- function(files, n = 20000) {
  if (!is.data.frame(files) || !nrow(files) || !all(c("path", "download_url") %in% names(files)))
    return(NA_character_)
  k <- which(grepl("^readme([.](md|rst|txt|markdown))?$", files$path, ignore.case = TRUE))
  if (!length(k)) return(NA_character_)
  k <- k[order(!grepl("[.]md$", files$path[k], ignore.case = TRUE))][1]
  .repo_readme_text(files$download_url[[k]], n)
}

# ---- the deposit's own record ----------------------------------------------

# What repo_check() knows about each linked deposit, as the columns
# .repo_ownership() reads: its record (repo_metadata, and the OSF
# descriptions of every OSF link, registrations included) and its files
# (the listing, with data_classify_files()'s data_type). `links` has
# paper_id, repo_url and repo_type.
.own_facts <- function(paper, links, meta, files, osf_desc = NULL) {
  n <- nrow(links)
  x <- data.frame(paper_id = links$paper_id, repo_url = links$repo_url, repo_type = links$repo_type)
  if (!n) return(x)
  col <- function(df, nm, k) if (is.data.frame(df) && nm %in% names(df)) as.character(df[[nm]])[k] else rep(NA_character_, length(k))
  meta <- if (is.data.frame(meta) && nrow(meta) && "repo_url" %in% names(meta)) meta[!duplicated(meta$repo_url), , drop = FALSE] else NULL
  k <- if (is.null(meta)) rep(NA_integer_, n) else match(links$repo_url, meta$repo_url)
  od <- if (is.data.frame(osf_desc) && nrow(osf_desc)) osf_desc[!duplicated(osf_desc$repo_url), , drop = FALSE] else NULL
  ko <- if (is.null(od)) rep(NA_integer_, n) else match(links$repo_url, od$repo_url)
  pick <- function(nm) dplyr::coalesce(col(meta, nm, k), col(od, nm, ko))
  code_host <- links$repo_type %in% c("github", "gitlab")
  x$dep_title <- pick("title")
  x$dep_description <- pick("description")
  x$dep_creators <- ifelse(code_host, NA_character_, pick("creators"))
  x$dep_orcids <- pick("creator_orcids")
  # the OSF's own family names of its contributors, and how many it has: it
  # names only the first 10 in its answer (#682)
  x$dep_creator_families <- col(od, "creator_families", ko)
  x$dep_n_creators <- col(od, "n_creators", ko)
  x$dep_type <- .own_dep_type(links$repo_type, col(meta, "resource_type", k), col(meta, "resource_subtype", k))
  # the institute that submitted it, where the record names no people (#660)
  x$dep_org <- col(meta, "organisation", k)
  x$dep_related <- col(meta, "related", k)
  x$dep_date <- substr(pick("published"), 1, 10)
  x$dep_year <- substr(x$dep_date, 1, 4)
  x$osf_type <- ifelse(links$repo_type %in% "osf", col(od, "osf_type", ko), NA_character_)
  x$osf_category <- ifelse(links$repo_type %in% "osf", col(od, "category", ko), NA_character_)
  x$osf_contributors <- ifelse(links$repo_type %in% "osf", pick("creators"), NA_character_)
  x$readme <- col(meta, "readme", k)
  x$gh_owner_name <- col(meta, "owner_name", k)
  x$gh_stars <- col(meta, "stars", k)
  x$gh_forks <- col(meta, "forks", k)
  # the files
  f <- if (is.data.frame(files) && nrow(files) && "repo_url" %in% names(files)) files else NULL
  # a repository two papers share has its files once per paper in `files`
  by <- lapply(links$repo_url, \(u) {
    if (is.null(f)) return(NULL)
    d <- f[f$repo_url %in% u, , drop = FALSE]
    if (!nrow(d)) return(d)
    d[!duplicated(paste(d$file_url %||% "", d$file_path %||% "", d$file_name)), , drop = FALSE]
  })
  x$n_files <- vapply(by, \(d) if (is.null(d)) 0L else nrow(d), 1L)
  x$listed <- x$n_files > 0
  dt <- \(d) if ("data_type" %in% names(d)) d$data_type else d$file_type
  x$has_data <- vapply(by, \(d) !is.null(d) && nrow(d) > 0 && any(dt(d) %in% "data"), logical(1))
  x$has_code <- vapply(by, \(d) !is.null(d) && nrow(d) > 0 && any(dt(d) %in% "code"), logical(1))
  x$pkg_layout <- vapply(by, \(d) {
    if (is.null(d) || !nrow(d)) return(FALSE)
    fn <- as.character(d$file_name); fp <- as.character(d$file_path %||% d$file_name); fp[is.na(fp)] <- fn[is.na(fp)]
    any(grepl(.own_pkg_files, fn)) || (any(grepl("(^|/)src/", fp)) && any(grepl("(^|/)tests?/", fp)))
  }, logical(1))
  # file names that share words with the paper's title or abstract
  pfs <- list()
  x$file_paper_words <- vapply(seq_len(n), \(i) {
    d <- by[[i]]
    if (is.null(d) || !nrow(d)) return(0L)
    id <- links$paper_id[[i]]
    if (is.null(pfs[[id]])) {
      p <- if (.is_paper_list(paper)) paper[[id]] else paper
      pf <- .own_paper_facts(p)
      pfs[[id]] <<- unique(c(.own_cwords(pf$title), .own_cwords(pf$abstract)))
    }
    fp <- as.character(d$file_path %||% d$file_name); fp[is.na(fp)] <- as.character(d$file_name)[is.na(fp)]
    sum(.own_cwords(paste(gsub("[_.-]", " ", fp), collapse = " ")) %in% pfs[[id]])
  }, 1L)
  x
}

# The deposits a paper links in its main text that are not its own for this
# paper: other people's, or the authors' earlier work (#648). repo_check lists
# them but does not check them, so the other modules, which read its file
# table, do not check them either. A registration's files are kept under the
# project it was registered from (`reg_parent`: registration address ->
# project address), so that project is set aside with it. `ownership` is
# repo_check's table; returns paper_id and repo_url.
.own_not_this <- function(ownership, reg_parent = character(0)) {
  none <- data.frame(paper_id = character(0), repo_url = character(0))
  if (!is.data.frame(ownership) || !nrow(ownership)) return(none)
  k <- ownership$source %in% "text" & grepl("^(others|own_earlier)", ownership$ownership)
  out <- ownership[k, c("paper_id", "repo_url"), drop = FALSE]
  if (!nrow(out)) return(none)
  p <- intersect(out$repo_url, names(reg_parent))
  if (length(p)) {
    j <- match(p, out$repo_url)
    out <- rbind(out, data.frame(paper_id = out$paper_id[j], repo_url = unname(reg_parent[p])))
  }
  out <- unique(out)
  rownames(out) <- NULL
  out
}

# Classify links of one or more papers: `x` holds .own_facts() columns plus
# `source` and the citing text. Each paper's links are compared with that
# paper's own facts.
.own_classify <- function(paper, x) {
  if (!nrow(x)) return(.repo_ownership(x, list()))
  out <- lapply(split(seq_len(nrow(x)), x$paper_id), \(ii) {
    id <- x$paper_id[[ii[1]]]
    p <- if (.is_paper_list(paper)) paper[[id]] else paper
    .repo_ownership(x[ii, , drop = FALSE], .own_paper_facts(p))
  })
  dplyr::bind_rows(out)
}

# A host's record type as a DataCite resource type ("Dataset", "Software",
# "Preprint", "JournalArticle", "Text", ...), so every host is read alike
# A record repo_check ignores as a publication (a paper or thesis in a
# repository, its own type says so, without data or code files; #636, #637)
# is not a deposit
.own_pub_reason <- "a publication in a repository (its record says so), without data or code files"

# Mark the rows of `out` whose link repo_check ignored as a publication
.own_mark_publications <- function(out, urls) {
  if (!is.data.frame(out) || !nrow(out) || !length(urls)) return(out)
  k <- .own_key(out$repo_url) %in% .own_key(urls)
  out$ownership[k] <- "not_data"
  out$ownership_group[k] <- "not_data"
  out$ownership_reason[k] <- .own_pub_reason
  out
}

.own_dep_type <- function(repo_type, type, subtype = NA_character_) {
  type <- tolower(trimws(as.character(type))); subtype <- tolower(trimws(as.character(subtype)))
  # InvenioRDM (other Invenio installations, #636) gives type and subtype in
  # one: "publication-article", "image-figure"
  rdm <- !is.na(type) & grepl("-", type, fixed = TRUE)
  subtype[rdm & is.na(subtype)] <- sub("^[^-]+-", "", type[rdm & is.na(subtype)])
  type[rdm] <- sub("-.*$", "", type[rdm])
  out <- rep(NA_character_, length(repo_type))
  z <- repo_type %in% "zenodo"
  zen <- c(dataset = "Dataset", software = "Software", poster = "Text", presentation = "Text", image = "Image",
           video = "Audiovisual", lesson = "Text", workflow = "Workflow", model = "Model", other = "Other",
           physicalobject = "PhysicalObject")
  pub <- c(preprint = "Preprint", article = "JournalArticle", conferencepaper = "ConferencePaper", book = "Book",
           section = "BookChapter", report = "Report", thesis = "Dissertation", proposal = "Text",
           deliverable = "Report", milestone = "Report", workingpaper = "Preprint", other = "Text")
  out[z] <- ifelse(type[z] %in% "publication", unname(pub[subtype[z]] %||% NA), unname(zen[type[z]]))
  out[z & type %in% "publication" & is.na(out)] <- "Text"
  f <- repo_type %in% "figshare"
  fig <- c(dataset = "Dataset", software = "Software", code = "Software", "journal contribution" = "JournalArticle",
           preprint = "Preprint", thesis = "Dissertation", "conference contribution" = "ConferencePaper",
           book = "Book", chapter = "BookChapter", report = "Report", presentation = "Text", poster = "Text",
           figure = "Image", media = "Audiovisual", workflow = "Workflow", "online resource" = "Other",
           "educational resource" = "Text", "data management plan" = "Text", monograph = "Book", model = "Model")
  out[f] <- unname(fig[type[f]])
  out[f & !is.na(type) & is.na(out)] <- "Other"
  out[repo_type %in% c("dryad", "dataverse", "mendeley", "reshare", "researchdata4tu", "dataone", "ldbase",
                       "geo", "ena", "arrayexpress", "icpsr")] <- "Dataset"
  out
}

# ---- name and text matching ------------------------------------------------

.own_ascii <- function(x) tolower(iconv(x, "UTF-8", "ASCII//TRANSLIT", sub = ""))

# Words of three letters or more, as family names are matched
.own_fam_words <- function(x) {
  if (length(x) != 1 || is.na(x)) return(character(0))
  w <- unlist(strsplit(.own_ascii(x), "[^a-z]+"))
  unique(w[nchar(w) >= 3])
}

# Name particles: words that begin a family name ("van", "von", "der",
# "dos", "al") and that many unrelated people share. On its own such a word
# says nothing about who a person is: "Van Lissa" matched every list of
# people with a "van" in it, such as "Marcel A.L.M. van Assen" (#682). Words
# of one or two letters ("de", "da", "le", "'t") are never matched, so only
# longer ones are listed.
.own_particles <- c("van", "von", "der", "den", "des", "dei", "del", "della", "delle", "dello", "degli",
  "dalla", "das", "dos", "las", "les", "los", "ter", "ten", "het", "auf", "vom", "zum", "zur", "bin", "ibn",
  "abu", "mac")

# A family name as Grobid can write it, without its noise: a word broken at a
# line end is joined ("Przybyl- Ski"), and what follows a hyphen with a space
# before it is no part of the name ("Hecker -Orcid", "Ruge - Orcid").
.own_fam_clean <- function(x) {
  x <- gsub("([[:alpha:]])-\\s+([[:alpha:]])", "\\1\\2", x, perl = TRUE)
  trimws(sub("\\s+-.*$", "", x, perl = TRUE))
}

# The words by which the paper's family names are matched: each name's words
# of three letters or more, without its particles ("Van Lissa" -> "lissa").
# Grobid's family names can hold a given name as well ("Moritz Heene"), so
# each word is a key, and the whole name is not required. A name of
# particles only ("Van Der", a name Grobid cut short) keeps the whole name,
# written together ("vander"), which matches no particle on its own. With
# `joined`, each name written together ("vanaert") is added, as a name runs
# together in an account name or a README ("rbmvanaert").
.own_fam_keys <- function(family, joined = FALSE) {
  family <- .own_fam_clean(as.character(family[!is.na(family)]))
  unique(unlist(lapply(family, \(f) {
    w <- unlist(strsplit(.own_ascii(f), "[^a-z]+"))
    w <- w[nchar(w) >= 3]
    k <- w[!w %in% .own_particles]
    whole <- gsub("[^a-z]", "", .own_ascii(f))
    if (!length(k) && nchar(whole) >= 3) k <- whole
    if (joined && nchar(whole) >= 3) k <- c(k, whole)
    k
  })))
}

# TRUE when a family name of the paper's authors is written in `x`: an
# account name, a README, or the name an account gives itself (a person's,
# or a lab's such as "Bourne & Mura Computational Biosciences Lab"). A key of
# .own_fam_keys() must be a whole word of `x`. With `inside`, for an account
# name or a README, where names run together ("carabrook", "cjvanlissa"), a
# key of five letters or more, or a name written together, may also be part
# of a word. NA when either side has no names. Lists of people are compared
# with .own_people_author().
.own_names_author <- function(x, family, inside = FALSE) {
  if (length(x) != 1 || is.na(x) || !length(family)) return(NA)
  wx <- .own_fam_words(x)
  wa <- .own_fam_keys(family)
  if (!length(wx) || !length(wa)) return(NA)
  if (any(wa %in% wx)) return(TRUE)
  if (!inside) return(FALSE)
  long <- .own_fam_keys(family, joined = TRUE)
  any(vapply(long[nchar(long) >= 5], \(w) any(grepl(w, wx, fixed = TRUE)), logical(1)))
}

# The family part of each person in a list of people ("Name; Name"): the part
# before the comma of "Family, Given"; otherwise every word after the first
# ("Given Family", with any middle names), the first word when every other
# word is an initial ("Smith AB"), or the name itself when it is one word
.own_person_family <- function(people) {
  p <- trimws(unlist(strsplit(as.character(people), ";")))
  p <- p[!is.na(p) & nzchar(p)]
  vapply(p, \(q) {
    if (grepl(",", q, fixed = TRUE)) return(trimws(sub(",.*$", "", q)))
    w <- strsplit(q, "\\s+")[[1]]
    if (length(w) == 1) return(q)
    if (all(grepl("^([[:upper:]][.]?){1,3}$", w[-1]))) return(w[[1]])
    paste(w[-1], collapse = " ")
  }, character(1), USE.NAMES = FALSE)
}

# TRUE when a family name of the paper's authors is the family name of a
# person in `people` (a deposit's creators or contributors, "Name; Name", or
# a reference's authors, "Family, G; ..."): a key of .own_fam_keys() is a
# whole word of a person's family part. A given name ("Lissa" in "Melissa
# Lewis"), a particle ("van" in "van Assen") or part of a word is no match
# (#682). `families` gives each person's family name where the source says
# it (the OSF does, as "Family; Family"); otherwise it is read from the
# names. NA when either side has no names.
.own_people_author <- function(people, family, families = NA_character_) {
  if (length(people) != 1 || is.na(people) || !length(family)) return(NA)
  fp <- if (length(families) == 1 && !is.na(families) && nzchar(trimws(families)))
    trimws(unlist(strsplit(families, ";"))) else .own_person_family(people)
  wp <- unique(unlist(lapply(fp, .own_fam_words)))
  # a particle-only key ("vander") is a family part written together
  wp <- c(wp, gsub("[^a-z]", "", .own_ascii(fp)))
  wa <- .own_fam_keys(family)
  if (!length(wp) || !any(nchar(wp) >= 3) || !length(wa)) return(NA)
  any(wa %in% wp)
}

# A paper author's full name (first and last word) among a list of people
.own_full_match <- function(people, full) {
  if (length(people) != 1 || is.na(people) || !length(full)) return(FALSE)
  norm <- \(x) gsub("[^a-z ]", "", .own_ascii(x))
  fn <- trimws(norm(full)); fn <- fn[nchar(fn) > 3]
  pp <- trimws(norm(unlist(strsplit(people, ";"))))
  any(vapply(fn, \(f) {
    w <- strsplit(f, " +")[[1]]
    any(vapply(pp, \(q) all(w[c(1, length(w))] %in% strsplit(q, " +")[[1]]), logical(1)))
  }, logical(1)))
}

.own_stop <- c("this", "that", "with", "from", "were", "have", "their", "which", "these", "between", "using",
  "study", "data", "results", "based", "effect", "effects", "during", "after", "about", "there", "other",
  "while", "both", "into", "than", "also", "more", "such", "when", "they", "only", "analysis", "model",
  "models", "files", "file", "code", "script", "scripts", "readme", "final", "version", "figure", "table")
.own_cwords <- function(x, n = 5) {
  if (length(x) != 1 || is.na(x) || !nzchar(x)) return(character(0))
  w <- unlist(strsplit(.own_ascii(x), "[^a-z]+"))
  unique(w[nchar(w) >= n & !w %in% .own_stop])
}
# words of four letters or more, for "names the paper's title"
.own_tw <- function(x) {
  if (length(x) != 1 || is.na(x)) return(character(0))
  w <- unlist(strsplit(.own_ascii(x), "[^a-z0-9]+")); unique(w[nchar(w) >= 4])
}
# nearly all (80%) of the title's words (at least four) in `x`
.own_title_in <- function(t, x) {
  tw <- .own_tw(t)
  if (length(tw) < 4 || length(x) != 1 || is.na(x)) return(FALSE)
  mean(tw %in% .own_tw(x)) >= 0.8
}
.own_doi_in <- function(d, x) length(d) == 1 && !is.na(d) && length(x) == 1 && !is.na(x) &&
  grepl(tolower(d), tolower(x), fixed = TRUE)

# The DOIs of the related works whose relation is one of `types`
.own_rel_dois <- function(x, types) {
  if (length(x) != 1 || is.na(x)) return(character(0))
  parts <- trimws(strsplit(x, "\\|")[[1]])
  parts <- parts[grepl(paste0("^(", types, "):"), parts, ignore.case = TRUE)]
  d <- unlist(regmatches(parts, gregexpr("10\\.\\d{4,9}/[^\\s|]+", parts, perl = TRUE)))
  unique(tolower(sub("[.,;)]+$", "", d)))
}

# Is a file list a software package, does it hold study files, how many of
# its names' words are the paper's
.own_pkg_files <- "^(DESCRIPTION|NAMESPACE|setup[.]py|setup[.]cfg|pyproject[.]toml|package[.]json|CMakeLists[.]txt|Cargo[.]toml|environment[.]ya?ml|Makefile|configure)$"

# Whether an institute's name is one of the paper's affiliations: its own
# words (leaving out "University", "Institute", "of" and the like) are mostly
# in one affiliation, or its short form ("FISABIO", "NIEHS") is a word or the
# initials of one. NA when the name has no such words (ENA gives a submission
# number, "SUB14660361", for some records) or the paper gives no
# affiliations; FALSE when it matches none.
.own_org_stop <- c("university", "universidad", "universitat", "universite", "universita", "universidade",
  "universiteit", "institute", "institut", "instituto", "istituto", "inst", "department", "dept", "center",
  "centre", "centro", "research", "national", "of", "the", "and", "for", "de", "la", "del", "der", "fur",
  "school", "college", "laboratory", "laboratories", "lab", "hospital", "medical", "medicine", "sciences",
  "science", "faculty", "academy", "and", "et", "di", "da", "do", "y", "e", "at", "in", "for", "group", "unit",
  "foundation", "health", "biology", "biological", "technology")
.own_org_match <- function(org, affiliations) {
  if (is.na(org) || !nzchar(trimws(org)) || !length(affiliations)) return(NA)
  # a submission number or other code is no name
  org <- gsub("\\b[A-Za-z]*[0-9][A-Za-z0-9]*\\b", " ", org, perl = TRUE)
  words <- function(s) { w <- unlist(strsplit(.own_ascii(s), "[^a-z]+")); w[nzchar(w)] }
  ow <- words(org); sig <- setdiff(ow[nchar(ow) >= 3], .own_org_stop)
  short <- unique(tolower(unlist(regmatches(org, gregexpr("\\b[A-Z]{3,10}\\b", org)))))
  if (!length(sig) && !length(short)) return(NA)
  hit <- vapply(affiliations, \(a) {
    aw <- words(a)
    ini <- tolower(paste(substr(unlist(strsplit(iconv(a, "UTF-8", "ASCII//TRANSLIT", sub = ""), "[^A-Za-z]+")), 1, 1),
                         collapse = ""))
    (length(sig) >= 2 && mean(sig %in% aw) >= 0.6) ||
      (length(sig) == 1 && nchar(sig) >= 4 && sig %in% aw) ||
      (length(short) > 0 && any(short %in% aw | vapply(short, \(s) grepl(s, ini, fixed = TRUE), NA)))
  }, NA)
  any(hit)
}

# ---- the classification ----------------------------------------------------

# Columns .repo_ownership() reads; any it is not given are NA
.own_columns <- c("source", "repo_url", "repo_type", "context", "sections", "ref_text", "ref_authors",
  "ref_year", "ref_title", "citing", "citing_sections", "cites_others", "cites_own", "dep_title",
  "dep_description", "dep_creators", "dep_orcids", "dep_type", "dep_related", "dep_year", "dep_date",
  "listed", "n_files", "has_data", "has_code", "pkg_layout", "file_paper_words", "readme", "osf_type",
  "osf_category", "osf_contributors", "gh_owner_name", "gh_stars", "gh_forks", "cff_dois", "dep_org",
  "dep_creator_families", "dep_n_creators")

#' Whose deposit is each repository link?
#'
#' @param x one row per link: `source` ("text" or "reference"), `repo_url`,
#'   `repo_type`, the citing text (`context`, `sections` for the main text;
#'   `ref_text`, `ref_authors`, `ref_year`, `ref_title`, `citing`,
#'   `citing_sections` for the reference list), and the deposit's record and
#'   files (see `.own_columns`); missing columns count as unknown
#' @param pf the paper's facts, from .own_paper_facts()
#' @returns `x` with `ownership` (one of six labels), `ownership_group`
#'   ("own", "others" or "not_data") and `ownership_reason`
#' @keywords internal
#' @noRd
.repo_ownership <- function(x, pf) {
  x <- as.data.frame(x)
  n <- nrow(x)
  for (cc in setdiff(.own_columns, names(x))) x[[cc]] <- rep(NA, n)
  if (n == 0) {
    x$ownership <- character(0); x$ownership_group <- character(0); x$ownership_reason <- character(0)
    return(x)
  }
  T_ <- function(v) v %in% TRUE; F_ <- function(v) v %in% FALSE
  chr <- function(v) { v <- as.character(v); v }
  num <- function(v) suppressWarnings(as.numeric(v))
  ref <- x$source == "reference"
  href <- chr(x$repo_url)
  said <- chr(ifelse(x$source == "text", x$context, x$citing)); said[is.na(said)] <- ""
  rt <- chr(x$ref_text); rt[is.na(rt) | !ref] <- ""
  rd <- chr(x$readme)
  fam <- pf$family
  pdoi <- pf$doi
  dep_type <- chr(x$dep_type)
  dc_kind <- ifelse(is.na(dep_type), NA,
    ifelse(dep_type %in% c("Text", "Preprint", "JournalArticle", "ConferencePaper", "Report", "Book", "BookChapter",
                           "Dissertation"), "text",
           ifelse(dep_type %in% c("Software", "Workflow", "ComputationalNotebook"), "software", "data")))
  dep_date <- as.Date(substr(chr(x$dep_date), 1, 10), optional = TRUE)
  gap_years <- as.numeric(dep_date - pf$date) / 365.25
  dc_year_gap <- num(x$dep_year) - pf$year
  ref_year_gap <- num(x$ref_year) - pf$year
  creators <- chr(x$dep_creators)
  families <- chr(x$dep_creator_families)
  # how many creators the deposit has. The OSF names only the first 10 of a
  # project's contributors in its answer and says how many there are in all
  # (270 for the Reproducibility Project: Psychology, #682); a record stored
  # before that number was kept, naming exactly 10, may have been cut too.
  # A list cut short shows who is among its creators, but not who is not.
  n_listed <- vapply(strsplit(ifelse(is.na(creators), "", creators), ";"), length, 1L)
  n_total <- num(x$dep_n_creators)
  creators_cut <- !is.na(creators) &
    ifelse(is.na(n_total), x$repo_type %in% "osf" & n_listed == 10, n_listed < n_total)
  n_creators <- ifelse(!is.na(n_total), n_total, ifelse(creators_cut, NA, n_listed))
  dc_creator_overlap <- vapply(seq_len(n), \(i) .own_people_author(creators[[i]], fam, families[[i]]), NA)
  ref_author_overlap <- ifelse(ref, vapply(chr(x$ref_authors), .own_people_author, NA, family = fam,
                                           USE.NAMES = FALSE), NA)
  osf_people <- chr(x$osf_contributors)
  osf_author <- vapply(seq_len(n), \(i) .own_people_author(osf_people[[i]], fam, families[[i]]), NA)
  osf_author[creators_cut & F_(osf_author)] <- NA
  owner <- ifelse(x$repo_type %in% c("github", "gitlab"),
                  tolower(sub("^(https?://)?(www\\.)?(github|gitlab)[^/]*/([^/]+).*", "\\4", href, ignore.case = TRUE)), NA)
  # the account (its login, and the name its owner gives: a person's, or a
  # lab's such as "Bourne & Mura Computational Biosciences Lab"). A login
  # runs names together ("cjvanlissa"); the name an account gives itself
  # writes them as words (#682)
  login_author <- vapply(owner, .own_names_author, NA, family = fam, inside = TRUE, USE.NAMES = FALSE)
  owner_name_author <- vapply(chr(x$gh_owner_name), .own_names_author, NA, family = fam, USE.NAMES = FALSE)
  owner_author <- ifelse(T_(login_author) | T_(owner_name_author), TRUE,
                         ifelse(F_(login_author) | F_(owner_name_author), FALSE, NA))
  # names in the README's opening only (its title and author line): further
  # down, a family name of five letters or more is too often part of an
  # ordinary word ("Chang" in "changes"), and short ones are words ("Very")
  readme_author <- vapply(substr(rd, 1, 1500), .own_names_author, NA, family = fam, inside = TRUE,
                          USE.NAMES = FALSE)
  title_words <- .own_cwords(pf$title)
  readme_paper_words <- vapply(rd, \(r) if (is.na(r)) NA_integer_ else sum(.own_cwords(r) %in% title_words), 1L,
                               USE.NAMES = FALSE)
  install_rx <- "install[.]packages|pip install|devtools::install_github|remotes::install_github|conda install|library[(]|import [a-z]|usage:|## install"
  readme_install <- !is.na(rd) & grepl(install_rx, ifelse(is.na(rd), "", rd), ignore.case = TRUE)
  own_words <- "\\b(our|we|us)\\b|data (and|&) (code|materials?) (are|is)|(are|is) (openly |publicly |freely )?available (at|on|from|via|in)|have been (deposited|uploaded|made available)|can be (found|accessed|downloaded)|data availability|code availability|preregist|pre-regist|supplementa"
  own_wording <- grepl(own_words, said, ignore.case = TRUE, perl = TRUE)
  avail_section <- grepl("availab|supplement|accessib|open (data|science|practices)|data sharing|transparen|resource",
                         chr(x$sections), ignore.case = TRUE) |
    grepl("availab|supplement|accessib|open (data|science|practices)|data sharing|transparen",
          chr(x$citing_sections), ignore.case = TRUE)
  reuse <- "(obtained|downloaded|taken|retrieved|derived|adapted|sourced) from|publicly available (data|dataset)|using (the )?[A-Za-z0-9._-]+ (package|toolbox|software|pipeline|library|algorithm)|implemented in|(provided|developed|written) by|as described (in|by)|(package|toolbox|software|pipeline|library) \\("
  reuse_wording <- grepl(reuse, said, ignore.case = TRUE, perl = TRUE)

  # 1. not a deposit: an OSF preprint, a publication type with a few files at
  #    most, or a reference whose deposit could not be listed and is no data
  #    repository's record or OSF project
  osf_project <- x$repo_type %in% "osf" &
    (chr(x$osf_category) %in% c("project", "data", "analysis", "methods and measures", "procedure", "hypothesis",
                                "communication", "") | x$osf_type %in% "registrations")
  # any OSF project or registration is a deposit, not a publication, whatever
  # its category (osf.io/4g7fv, a task's materials, is "instrumentation"); an
  # OSF file or preprint is not one
  osf_node <- x$repo_type %in% "osf" & x$osf_type %in% c("nodes", "registrations")
  repo_doi <- grepl("10\\.(5061|5281|6084|17632|7910|5255|3886|4121|15146|25338|5068|15482|5522|26180)/", href,
                    ignore.case = TRUE) |
    x$repo_type %in% c("dryad", "zenodo", "figshare", "dataverse", "reshare", "mendeley", "researchdata4tu", "dataone")
  few_files <- !T_(x$listed) | T_(num(x$n_files) <= 3)
  # a reference written as a deposit: a data or software tag, a version, a
  # repository's name
  data_mark <- grepl("\\[(data ?set|computer software|software|code)\\]|\\bversion\\b|\\bv[0-9]+\\.[0-9]|\\b(zenodo|figshare|dryad|dataverse|github|gitlab|software heritage|swh:|osf\\b|open science framework|repository|data repository)\\b",
                     rt, ignore.case = TRUE, perl = TRUE)
  # a reference whose link gave nothing to read is a publication only when
  # its own words mark no deposit ("GitHub repository", "v0.0.1", "Software
  # Heritage"); with such a mark its authors decide whose deposit it is. Its
  # links are left out first: every OSF or GitHub link names its host
  rt_words <- gsub(paste0("https?://\\S+|\\bdoi:\\s*\\S+|\\b10\\.\\d{4,9}/\\S+",
                          "|\\b(?:www\\.)?[a-z0-9-]+(?:\\.[a-z0-9-]+)*\\.(?:io|org|com|net|edu)/\\S*"),
                   " ", rt, ignore.case = TRUE, perl = TRUE)
  deposit_words <- grepl("\\[(data ?set|computer software|software|code)\\]|\\bversion\\b|\\bv[0-9]+\\.[0-9]|\\b(zenodo|figshare|dryad|dataverse|github|gitlab|software heritage|swh:|osf\\b|open science framework|repository|data repository)\\b",
                         rt_words, ignore.case = TRUE, perl = TRUE)
  publication <- x$osf_type %in% "preprints" |
    (dep_type %in% c("Preprint", "JournalArticle", "ConferencePaper", "Book", "BookChapter", "Dissertation", "Report") &
       few_files) |
    (ref & !T_(x$listed) & !dc_kind %in% c("data", "software") & !osf_project & !osf_node & !repo_doi & !deposit_words)

  # 2. made for this paper: the README or the record names the paper (its DOI,
  #    or 80% of its title's words), relates to its DOI, or the reference's
  #    title begins "Data for", "Code for"...
  rel <- chr(x$dep_related)
  names_paper <- vapply(seq_len(n), \(i) {
    dc <- paste(x$dep_title[[i]], x$dep_description[[i]])
    .own_title_in(pf$title, rd[[i]]) || .own_doi_in(pdoi, rd[[i]]) ||
      .own_title_in(pf$title, dc) || .own_doi_in(pdoi, dc) ||
      (!is.na(pdoi) && !is.na(rel[[i]]) &&
         # ICPSR relates a study to every publication that used it
         # ("IsDocumentedBy", "IsCitedBy": 376 for MIDUS), so there only a
         # supplement counts (#660)
         (if (x$repo_type[[i]] %in% "icpsr") pdoi %in% .own_rel_dois(rel[[i]], "IsSupplementTo")
          else grepl(pdoi, tolower(rel[[i]]), fixed = TRUE)))
  }, logical(1)) |
    grepl("^\\s*(data|code|scripts?|supplementary (data|code|materials?|information)|replication (data|code|materials|package)|materials?|data and code|code and data)\\b.{0,20}\\b(for|from|of)\\b",
          ifelse(ref, chr(x$ref_title), NA), ignore.case = TRUE, perl = TRUE)
  # named after the paper: "Smith_et_al_2020", "Smith et al. (2020)", or a
  # README that says it accompanies this paper
  first_author <- gsub("[^a-z]", "", .own_ascii(fam[1] %||% ""))
  repo_name <- tolower(ifelse(x$repo_type %in% c("github", "gitlab"),
    sub("^(?:https?://)?(?:www\\.)?git(?:hub|lab)[^/]*/[^/]+/([^/?#]+).*$", "\\1", href, perl = TRUE), ""))
  yr <- if (is.na(pf$year)) NA_character_ else as.character(pf$year)
  named_after <- vapply(seq_len(n), \(i) {
    fa <- first_author
    r <- tolower(ifelse(is.na(rd[[i]]), "", rd[[i]]))
    accompanies <- grepl("\\b(accompan(y|ies|ying)|supporting|for|associated with|underlying|to reproduce)\\s+(the|this|our)\\s+(paper|manuscript|article|publication|study)\\b", r, perl = TRUE)
    if (is.na(fa) || nchar(fa) < 3) return(FALSE)
    rn <- repo_name[[i]]
    (nzchar(rn) && grepl(fa, gsub("[^a-z]", "", rn), fixed = TRUE) &&
       (grepl("et.?al", rn) || (!is.na(yr) && grepl(yr, rn, fixed = TRUE)))) ||
      (!is.na(yr) && grepl(paste0("\\b", fa, "\\s+et\\s+al\\.?,?\\s*\\(?(", yr, ")"), r, perl = TRUE)) ||
      accompanies
  }, logical(1))
  names_paper <- names_paper | named_after

  # authorship: a family name of the paper's authors among the reference's
  # authors, the deposit's creators (20 at most: community software lists
  # many), the OSF contributors, the account name, or the README
  author_raw <- T_(ref_author_overlap) | (T_(dc_creator_overlap) & T_(n_creators <= 20)) | T_(osf_author) |
    T_(owner_author) | T_(readme_author)
  # a text record of a few files by others is a document
  publication <- publication | (dep_type %in% "Text" & T_(num(x$n_files) <= 3) & !author_raw & !x$repo_type %in% "osf")
  popular <- T_(num(x$gh_stars) > 50) | T_(num(x$gh_forks) > 20)
  pkg_words <- ref & grepl("\\b(package|toolbox|library|plug-?in)\\b", chr(x$citing), ignore.case = TRUE, perl = TRUE)
  code_host <- x$repo_type %in% c("github", "gitlab") | dep_type %in% c("Software", "Workflow")
  tool <- T_(x$pkg_layout) | (readme_install & code_host) | pkg_words
  known_not_author <- (F_(dc_creator_overlap) & T_(n_creators <= 20) & !creators_cut) | F_(osf_author) |
    F_(owner_author)
  reused <- reuse_wording | T_(x$cites_others)
  old <- T_(gap_years < -3) & !osf_project
  recent <- T_(gap_years >= -1.5) | (is.na(gap_years) & (T_(dc_year_gap >= -1) | T_(ref_year_gap >= -1)))
  presented <- own_wording | avail_section
  shares_words <- T_(num(x$file_paper_words) > 0) | T_(readme_paper_words > 0)
  code <- x$repo_type %in% c("github", "gitlab") | dep_type %in% c("Software", "Workflow") |
    (T_(x$has_code) & !T_(x$has_data))
  kind <- ifelse(code, "code", "data")

  # the deposit's creators' ORCID iDs among the paper's settle authorship, as
  # does a paper author's full name among the OSF contributors or creators
  orcid_author <- vapply(chr(x$dep_orcids), \(o) {
    b <- unlist(regmatches(o, gregexpr("\\d{4}-\\d{4}-\\d{4}-\\d{3}[0-9X]", o)))
    length(b) > 0 && length(pf$orcids) > 0 && any(b %in% pf$orcids)
  }, logical(1), USE.NAMES = FALSE)
  name_author <- vapply(seq_len(n), \(i) .own_full_match(chr(x$gh_owner_name)[[i]], pf$full) ||
    .own_full_match(chr(x$osf_contributors)[[i]], pf$full) || .own_full_match(creators[[i]], pf$full), logical(1))
  # (H) a code-host account named after a paper author ("WonLab") or one of
  # the paper's institutes ("AllenNeuralDynamics")
  fam3 <- gsub("[^a-z]", "", .own_ascii(fam)); fam3 <- fam3[nchar(fam3) >= 3]
  tok <- function(o) { w <- unlist(strsplit(gsub("([a-z])([A-Z])", "\\1 \\2", o), "[^A-Za-z]+")); tolower(w[nzchar(w)]) }
  owner_raw <- ifelse(x$repo_type %in% c("github", "gitlab"),
    sub("^(?:https?://)?(?:www\\.)?git(?:hub|lab)[^/]*/([^/?#]+).*$", "\\1", href, perl = TRUE, ignore.case = TRUE), NA)
  host_author <- vapply(owner_raw, \(o) {
    if (is.na(o) || grepl("/", o)) return(FALSE)
    t <- tok(o); t2 <- sub("(lab|labs|group)$", "", t)
    if (length(fam3) && any(fam3 %in% c(t, t2[nchar(t2) >= 3]))) return(TRUE)
    if (!length(pf$affiliations)) return(FALSE)
    core <- setdiff(t, c("lab", "labs", "group", "the", "of", "and", "for"))
    any(vapply(pf$affiliations, \(a) {
      aw <- unlist(strsplit(.own_ascii(a), "[^a-z]+")); aw <- aw[nzchar(aw)]
      wc <- unlist(strsplit(gsub("([a-z])([A-Z])", "\\1 \\2", iconv(a, "UTF-8", "ASCII//TRANSLIT", sub = "")), "[^A-Za-z]+"))
      ini <- tolower(paste(substr(wc[grepl("^[A-Z]", wc)], 1, 1), collapse = ""))
      (length(core) >= 2 && all(core %in% aw)) ||
        (length(core) == 1 && nchar(core) >= 4 && (core %in% aw || grepl(core, ini, fixed = TRUE)))
    }, logical(1)))
  }, logical(1), USE.NAMES = FALSE)
  # (I) the institute that submitted a sequence or expression archive's
  # record, which names no people (#660): one of the paper's affiliations,
  # or, when the paper gives its affiliations, none of them
  # Only for a record without people: an institute is shared by many labs, so
  # where the record names its people, they decide (in one sample, a dataset
  # of another lab at the authors' university)
  org_match <- vapply(chr(x$dep_org), .own_org_match, NA, affiliations = pf$affiliations, USE.NAMES = FALSE)
  org_match[!is.na(creators)] <- NA
  author <- author_raw | orcid_author | name_author | host_author | T_(org_match)
  # authorship shown by the institute alone is weak: words of reuse in the
  # citing sentence outweigh it ("sequences derived from other publications")
  org_only <- T_(org_match) & !(author_raw | orcid_author | name_author | host_author)
  # (J) an R package on CRAN (#478, #660): CRAN requires a package to list
  # its authors, so the list settles whose it is, however long it is. The
  # paper's own when the paper presents it (the sentence says it is
  # available, hosted, or theirs, or a word of the paper's title is its
  # name); the
  # authors' earlier code when the paper only uses it.
  cran <- x$repo_type %in% "cran"
  cran_pkg <- tolower(.cran_pkg_of(href))
  cran_title <- vapply(cran_pkg, \(pk) {
    if (is.na(pk)) return(FALSE)
    tw <- unlist(strsplit(.own_ascii(pf$title %||% ""), "[^a-z0-9.]+"))
    any(tw == pk)
  }, logical(1), USE.NAMES = FALSE)
  cran_presented <- presented | cran_title |
    grepl("\\bavailable\\b|\\bhosted\\b|can be (downloaded|installed)|\\bour (open[- ]source )?(r )?package\\b|\\bwe (developed|wrote|present|introduce)\\b",
          said, ignore.case = TRUE, perl = TRUE)
  cran_others <- cran & !author & !is.na(creators)
  # (K) ICPSR and openICPSR (#660): a study's creators are its investigators,
  # so one without a paper author is others' data, also when the paper's
  # sentence presents it ("All data ... have been made publicly available via
  # ICPSR", the open-data badge text, for reused studies such as MIDUS)
  icpsr_others <- x$repo_type %in% "icpsr" & !author & !is.na(creators)
  known_not_author <- (known_not_author | (F_(org_match) & is.na(creators))) & !author

  # names another paper: "is supplement to" (or Dryad's primary article,
  # Figshare's article) a different article's DOI, preprint servers left out
  preprint_rx <- "^10\\.(1101|31234|31219|31235|31222|48550|20944|21203|2139|31730|17605/osf\\.io)/"
  supp <- "IsSupplementTo|IsReferencedBy|IsCitedBy|IsDocumentedBy|IsDescribedBy|primary_article|article|IsSupplementedBy"
  other_paper <- vapply(seq_len(n), \(i) {
    d <- .own_rel_dois(rel[[i]], supp)
    d <- d[!grepl(preprint_rx, d, perl = TRUE) & !grepl("^10\\.5281/|^10\\.6084/|^10\\.5061/", d)]
    length(d) > 0 && !is.na(pdoi) && !(pdoi %in% d)
  }, logical(1))
  this_paper <- vapply(seq_len(n), \(i) {
    if (is.na(pdoi)) return(FALSE)
    d <- c(.own_rel_dois(rel[[i]], "primary_article|article"),
           unlist(strsplit(ifelse(is.na(x$cff_dois[[i]]), "", chr(x$cff_dois[[i]])), " ")))
    # "accompanies the paper" in a GitHub or OSF description; a data
    # repository's description says it of the paper it belongs to, which
    # need not be this one (its related works name that paper)
    txt <- if (x$repo_type[[i]] %in% c("github", "gitlab", "osf"))
      tolower(paste(x$dep_title[[i]], x$dep_description[[i]])) else ""
    pdoi %in% d || grepl("\\b(accompan(y|ies|ying)|for|underlying|to reproduce)\\s+(the|this|our)\\s+(paper|manuscript|article|publication|study)\\b",
                         txt, perl = TRUE)
  }, logical(1))
  names_paper <- names_paper | this_paper
  old_osf_others <- x$repo_type %in% "osf" & T_(gap_years < -5) & !author
  # a plan or protocol is not a publication
  plan <- grepl("analysis plan|protocol|pre-?registration|registered report", paste(x$dep_title), ignore.case = TRUE)
  publication <- publication & !plan

  # the citing sentence: eLife's "copy archived at", made for this study,
  # from earlier work
  elife_copy <- grepl("copy archived at", said, ignore.case = TRUE)
  made_here <- grepl(paste0(
    "\\b(custom|our|in-house)[- ](written )?(code|scripts?|pipeline|software|analysis code)\\b",
    "|\\b(code|scripts?|data|materials)\\b[^.]{0,60}\\b(used|produced|generated|written|developed|collected)\\b[^.]{0,20}\\b(in|for|during) (this|the present|the current|our) (study|paper|work|article|analys[ie]s|manuscript)",
    "|\\bto reproduce (the|all|our|these) (results|analys[ie]s|figures|findings)",
    "|\\bwe (developed|wrote|built|created|implemented)\\b",
    "|\\b(all|the) (experimental )?(materials|data)[^.]{0,40}(reported|analy[sz]ed) here\\b",
    "|\\bdata and code (necessary|needed|required|used) to reproduce"), said, ignore.case = TRUE, perl = TRUE)
  from_earlier <- grepl(paste0(
    "\\b(original|previous|prior|earlier|first|parent) (study|studies|experiment|experiments|work|dataset|data collection|paper|publication)\\b",
    "|\\bas (noted|described|reported|detailed) (in|by) [A-Z][a-z]+ (et al|and)"), said, perl = TRUE)
  elife_copy <- elife_copy & !tool & !popular
  publication <- publication & !made_here

  # the reference itself: a publication's marks without a deposit's, a
  # sentence of the paper's own data statement, an availability statement
  # in the citing sentence, software marks
  pub_mark <-grepl(paste0("registered report|unpublished manuscript|\\(manuscript\\)|manuscript in preparation|\\bin press\\b|\\breport on\\b|\\bpreprint\\b",
                           "|\\b[0-9]{1,4}\\s*\\([0-9-]{1,5}\\)\\s*[,:]\\s*[0-9a-z]+|\\b(journal|proceedings|psychological science|nature|science|plos)\\b[^.]{0,40}\\b[0-9]{1,4}\\b"),
                    rt, ignore.case = TRUE, perl = TRUE)
  ref_publication <- ref & pub_mark & !data_mark
  statement_ref <- ref &
    grepl("\\b(can be found|are available|is available|available (at|on|from|via)|deposited|repository:|pository:|(is|are) (located|stored|archived|hosted) (in|at|on))", rt, ignore.case = TRUE, perl = TRUE) &
    !grepl("^\\s*[A-Z][^.,]{1,40},\\s*[A-Z]", rt, perl = TRUE) &
    !grepl("\\b(19|20)[0-9]{2}\\b", sub("https?://\\S+|10\\.[0-9]{4,9}/\\S+", "", rt), perl = TRUE)
  avail_cite <- ref & grepl(paste0(
    "\\b(code|codes|scripts?|data|materials)\\b[^.]{0,80}\\b(are|is) (freely |publicly |openly )?(available|deposited|accessible)",
    "|\\b(available|deposited) (online )?(on|at|in|via) (zenodo|github|figshare|dryad|osf|the open science framework)"),
    ifelse(is.na(x$citing), "", chr(x$citing)), ignore.case = TRUE, perl = TRUE)
  avail_text <- x$source == "text" & (avail_section | grepl(paste0(
    "\\b(code|scripts?|notebooks?|software)\\b[^.]{0,40}\\b(used|needed|written)\\b[^.]{0,30}\\b(to|for) (generate|reproduce|produce|run|perform|analy[sz]e)",
    "|\\b(source code|code) of [A-Za-z0-9-]+ is (freely |publicly |openly )?available"), said, ignore.case = TRUE, perl = TRUE))
  ref_tool <- ref & grepl("\\[computer software\\]|\\b(r|python|matlab|julia) package\\b", rt, ignore.case = TRUE, perl = TRUE)
  publication <- (publication | ref_publication) & !statement_ref & !avail_cite
  tool <- (tool & !(avail_text & !popular)) | ref_tool

  # a preprint server's DOI, or an OSF preprint, is a publication
  osf_id <- ifelse(x$repo_type %in% "osf", tolower(sub("^.*osf\\.io/([a-z0-9]{5}).*$", "\\1", href, ignore.case = TRUE)), NA)
  preprint_pub <- grepl("10\\.(31234|31222|31219|31235|31730|31233|31236|31237|35542|21203|1101|48550)/", href, ignore.case = TRUE) |
    x$osf_type %in% "preprints" |
    (!is.na(osf_id) & mapply(\(id, r) !is.na(id) && grepl(paste0("10\\.(31234|31222|31219|31235|31730|31233|31236|31237|35542)/osf\\.io/", id), tolower(r)),
                             osf_id, rt))
  # eLife's "copy archived at <name>" with a paper author's family name
  archived_by <- vapply(said, \(sd) {
    m <- regmatches(sd, gregexpr("copy archived at ([^,;)]+)", sd, ignore.case = TRUE))[[1]]
    if (!length(m)) return(FALSE)
    w <- gsub("[^a-z]", "", unlist(strsplit(.own_ascii(m), "\\s+")))
    length(fam3) > 0 && any(fam3 %in% w)
  }, logical(1), USE.NAMES = FALSE)
  # an availability sentence naming the reference's creator as the source,
  # when that creator is not a paper author, says the data are the creator's
  names_creator <- ref & vapply(seq_len(n), \(i) {
    c1 <- sub("^\\s*([^,.(]{3,80}).*$", "\\1", rt[[i]], perl = TRUE)
    w <- unlist(strsplit(.own_ascii(c1), "[^a-z]+")); w <- w[nchar(w) >= 4]
    if (!length(w) || !nzchar(said[[i]])) return(FALSE)
    m <- regmatches(said[[i]], gregexpr("\\b(from|by|of|at|through|via)\\s+(the\\s+)?[^.;]{0,60}", said[[i]],
                                         ignore.case = TRUE, perl = TRUE))[[1]]
    any(vapply(m, \(s) any(w %in% unlist(strsplit(.own_ascii(s), "[^a-z]+"))), logical(1)))
  }, logical(1))
  avail_cite <- avail_cite & !(names_creator & !author)
  publication <- publication | preprint_pub

  e <- paste0("own_earlier_", kind); o <- paste0("others_", kind)
  steps <- list(
    list(publication, "not_data", "a publication: a preprint, an article or a manuscript, not a deposit"),
    list(cran_others, "others_code", "an R package on CRAN by others"),
    list(icpsr_others, "others_data", "an ICPSR study whose investigators are not the paper's authors"),
    list(cran & author & cran_presented, "own_this", "an R package on CRAN by the paper's authors, presented in the paper"),
    list(cran & author, "own_earlier_code", "an R package on CRAN by the paper's authors, used in the paper"),
    list(statement_ref, "own_this", "the reference is the paper's own data statement"),
    list(names_paper, "own_this", "the deposit names this paper (its DOI, its title, or a link to it)"),
    list(archived_by, "own_this", "eLife's archived copy is credited to a paper author"),
    list(elife_copy, "own_this", "eLife archived it as this paper's code"),
    list(made_here & !other_paper, "own_this", "the paper says it was made for this study"),
    list(avail_cite & !tool & !popular & !other_paper, "own_this", "the citing sentence is an availability statement"),
    list(from_earlier & author, e, "the paper says it comes from the authors' earlier work"),
    list(from_earlier & known_not_author, o, "the paper says it comes from earlier work by others"),
    list(other_paper & author, e, "the deposit belongs to another paper by the authors"),
    list(other_paper, o, "the deposit belongs to another paper, by others"),
    list(old_osf_others, o, "an OSF project more than 5 years older than the paper, without a paper author"),
    list(popular & author, "own_earlier_code", "a widely used tool by the authors"),
    list(popular, "others_code", "a widely used tool by others"),
    list(tool & author, "own_earlier_code", "a tool by the authors"),
    list(tool, "others_code", "a tool by others"),
    list(old & author, e, "the authors' deposit is more than 3 years older than the paper"),
    list(old, o, "a deposit more than 3 years older than the paper, by others"),
    list(ref & author & recent, "own_this", "a reference by the authors, from about the paper's time"),
    list(ref & author, e, "an older reference by the authors"),
    list(ref, o, "a reference by others"),
    list(known_not_author & !presented & !shares_words, o, "made by others, and not presented as the paper's own"),
    list(known_not_author & reused, o, "made by others, and the sentence says it is used or taken from them"),
    list(org_only & reuse_wording, o, "the sentence says it is taken from elsewhere, and only the institute is shared"),
    list(rep(TRUE, n), "own_this", "a link in the main text, not shown to be anyone else's"))
  label <- rep(NA_character_, n); reason <- rep(NA_character_, n)
  for (st in steps) {
    k <- is.na(label) & (st[[1]] %in% TRUE)
    lab <- if (length(st[[2]]) == 1) rep(st[[2]], n) else st[[2]]
    label[k] <- lab[k]; reason[k] <- st[[3]]
  }
  x$ownership <- label
  x$ownership_group <- ifelse(grepl("^own", label), "own", ifelse(grepl("^others", label), "others", "not_data"))
  x$ownership_reason <- reason
  x
}
