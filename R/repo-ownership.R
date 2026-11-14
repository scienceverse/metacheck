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
  fam <- fam[!is.na(fam) & nzchar(trimws(fam))]
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
  list(
    title = one(paper$info$title),
    doi = tolower(one(paper$info$doi)),
    family = fam,
    full = au$name[nzchar(au$name)],
    orcids = unique(orc),
    affiliations = aff,
    year = year,
    date = if (is.na(year)) as.Date(NA) else as.Date(paste0(year, "-07-01")),
    abstract = abstract
  )
}

# ---- the paper's text around each link -------------------------------------

# The part of a link that identifies it, for finding it in a sentence
.own_core <- function(h) tolower(gsub("[[:space:]]", "", sub("^https?://(dx\\.)?(www\\.)?", "", sub("/+$", "", h))))

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
    hit <- which(!is.na(tx$text) & grepl(.own_core(urls[[i]]), squeezed, fixed = TRUE) & !refs)
    if (!length(hit)) next
    out$context[[i]] <- paste(unique(tx$text[hit])[seq_len(min(2, length(unique(tx$text[hit]))))], collapse = " || ")
    out$sections[[i]] <- paste(unique(paste0(sec$section_type[hit], ":", sec$header[hit])), collapse = " | ")
    cited <- if (is.data.frame(x) && nrow(x) && all(c("xref_id", "text_id", "xref_type") %in% names(x)))
      unique(x$xref_id[x$text_id %in% tx$text_id[hit] & x$xref_type %in% "bibr"]) else character(0)
    if (length(cited) && is.data.frame(b) && all(c("bib_id", "authors") %in% names(b))) {
      au <- b$authors[match(cited, b$bib_id)]
      ov <- vapply(au, \(a) isTRUE(.own_names_author(
        paste(sub(",.*", "", strsplit(as.character(a %||% ""), ";")[[1]]), collapse = " "), fam)), logical(1))
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
  finders <- list(osf = osf_links, github = github_links, gitlab = gitlab_links, researchbox = rbox_links,
                  dspace = dspace_links, dspace7 = dspace7_links, zenodo = invenio_links,
                  dataverse = dataverse_links, figshare = figshare_links, dryad = dryad_links,
                  reshare = eprints_links, researchdata4tu = researchdata4tu_links,
                  mendeley = mendeley_links, dataone = dataone_links, ldbase = ldbase_links,
                  ckan = ckan_links)
  found <- dplyr::bind_rows(lapply(names(finders), \(k) {
    x <- tryCatch(suppressMessages(suppressWarnings(finders[[k]](q))), error = \(e) NULL)
    if (!is.data.frame(x) || !nrow(x) || !"href" %in% names(x)) return(NULL)
    data.frame(href = sub("[.,;:]+$", "", x$href), text_id = x$text_id %||% NA_integer_, repo_type = k)
  }))
  if (is.null(found) || !nrow(found)) return(empty)
  found <- found[!duplicated(tolower(found$href)), , drop = FALSE]
  found <- found[!.own_core(found$href) %in% .own_core(exclude), , drop = FALSE]
  if (!nrow(found)) return(empty)
  se <- paper$section
  sec <- if (is.data.frame(se)) se[match(tx$section_id, se$section_id), , drop = FALSE] else NULL
  x <- paper$xref
  rows <- lapply(seq_len(nrow(found)), \(i) {
    j <- found$text_id[[i]]
    if (is.na(j)) j <- which(grepl(.own_core(found$href[[i]]), tolower(gsub("[[:space:]]", "", q$text$text)), fixed = TRUE))[1]
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
  x$dep_type <- .own_dep_type(links$repo_type, col(meta, "resource_type", k), col(meta, "resource_subtype", k))
  x$dep_related <- col(meta, "related", k)
  x$dep_date <- substr(pick("published"), 1, 10)
  x$dep_year <- substr(x$dep_date, 1, 4)
  x$osf_type <- ifelse(links$repo_type %in% "osf", col(od, "osf_type", ko), NA_character_)
  x$osf_category <- ifelse(links$repo_type %in% "osf", col(od, "category", ko), NA_character_)
  x$osf_contributors <- ifelse(links$repo_type %in% "osf", pick("creators"), NA_character_)
  x$readme <- col(meta, "readme", k)
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
  k <- .own_core(out$repo_url) %in% .own_core(urls)
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
  out[repo_type %in% c("dryad", "dataverse", "mendeley", "reshare", "researchdata4tu", "dataone", "ldbase")] <- "Dataset"
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

# TRUE when a family name of the paper's authors is a word of `x` (a list of
# people, an account name, a README), or, at five letters or more, part of a
# word ("carabrook"); NA when either side has no names.
.own_names_author <- function(x, family) {
  if (length(x) != 1 || is.na(x) || !length(family)) return(NA)
  wx <- .own_fam_words(x)
  wa <- .own_fam_words(paste(family, collapse = " "))
  if (!length(wx) || !length(wa)) return(NA)
  any(wa %in% wx) || any(vapply(wa[nchar(wa) >= 5], \(w) any(grepl(w, wx, fixed = TRUE)), logical(1)))
}

# The paper's family names among the family names of a "Family, G; ..." list
.own_overlap <- function(people, family, ref = FALSE) {
  if (length(people) != 1 || is.na(people)) return(NA)
  if (ref) people <- paste(sub(",.*", "", trimws(strsplit(people, ";")[[1]])), collapse = "; ")
  fam <- function(x) {
    x <- unlist(strsplit(.own_ascii(x), "[;,]"))
    x <- trimws(gsub("[^a-z -]", "", x))
    unique(x[nchar(x) >= 3])
  }
  words <- \(x) { w <- unlist(strsplit(fam(x), "[ -]+")); unique(w[nchar(w) >= 3]) }
  wa <- words(people); wb <- words(paste(family, collapse = "; "))
  if (!length(wa) || !length(wb)) return(NA)
  any(wb %in% wa) || any(vapply(wb[nchar(wb) >= 5], \(w) any(grepl(w, wa, fixed = TRUE)), logical(1)))
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

# ---- the classification ----------------------------------------------------

# Columns .repo_ownership() reads; any it is not given are NA
.own_columns <- c("source", "repo_url", "repo_type", "context", "sections", "ref_text", "ref_authors",
  "ref_year", "ref_title", "citing", "citing_sections", "cites_others", "cites_own", "dep_title",
  "dep_description", "dep_creators", "dep_orcids", "dep_type", "dep_related", "dep_year", "dep_date",
  "listed", "n_files", "has_data", "has_code", "pkg_layout", "file_paper_words", "readme", "osf_type",
  "osf_category", "osf_contributors", "gh_owner_name", "gh_stars", "gh_forks", "cff_dois")

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
  n_creators <- vapply(strsplit(ifelse(is.na(creators), "", creators), ";"), length, 1L)
  dc_creator_overlap <- vapply(creators, .own_overlap, NA, family = fam, USE.NAMES = FALSE)
  ref_author_overlap <- ifelse(ref, vapply(chr(x$ref_authors), .own_overlap, NA, family = fam, ref = TRUE,
                                           USE.NAMES = FALSE), NA)
  osf_author <- vapply(chr(x$osf_contributors), .own_names_author, NA, family = fam, USE.NAMES = FALSE)
  owner <- ifelse(x$repo_type %in% c("github", "gitlab"),
                  tolower(sub("^(https?://)?(www\\.)?(github|gitlab)[^/]*/([^/]+).*", "\\4", href, ignore.case = TRUE)), NA)
  owner_author <- vapply(owner, .own_names_author, NA, family = fam, USE.NAMES = FALSE)
  # names in the README's opening only (its title and author line): further
  # down, a family name of five letters or more is too often part of an
  # ordinary word ("Chang" in "changes"), and short ones are words ("Very")
  readme_author <- vapply(substr(rd, 1, 1500), .own_names_author, NA, family = fam, USE.NAMES = FALSE)
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
  repo_doi <- grepl("10\\.(5061|5281|6084|17632|7910|5255|3886|4121|15146|25338|5068|15482|5522|26180)/", href,
                    ignore.case = TRUE) |
    x$repo_type %in% c("dryad", "zenodo", "figshare", "dataverse", "reshare", "mendeley", "researchdata4tu", "dataone")
  few_files <- !T_(x$listed) | T_(num(x$n_files) <= 3)
  publication <- x$osf_type %in% "preprints" |
    (dep_type %in% c("Preprint", "JournalArticle", "ConferencePaper", "Book", "BookChapter", "Dissertation", "Report") &
       few_files) |
    (ref & !T_(x$listed) & !dc_kind %in% c("data", "software") & !osf_project & !repo_doi)

  # 2. made for this paper: the README or the record names the paper (its DOI,
  #    or 80% of its title's words), relates to its DOI, or the reference's
  #    title begins "Data for", "Code for"...
  rel <- chr(x$dep_related)
  names_paper <- vapply(seq_len(n), \(i) {
    dc <- paste(x$dep_title[[i]], x$dep_description[[i]])
    .own_title_in(pf$title, rd[[i]]) || .own_doi_in(pdoi, rd[[i]]) ||
      .own_title_in(pf$title, dc) || .own_doi_in(pdoi, dc) ||
      (!is.na(pdoi) && !is.na(rel[[i]]) && grepl(pdoi, tolower(rel[[i]]), fixed = TRUE))
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
  author_raw <- T_(ref_author_overlap) | (T_(dc_creator_overlap) & n_creators <= 20) | T_(osf_author) |
    T_(owner_author) | T_(readme_author)
  # a text record of a few files by others is a document
  publication <- publication | (dep_type %in% "Text" & T_(num(x$n_files) <= 3) & !author_raw & !x$repo_type %in% "osf")
  popular <- T_(num(x$gh_stars) > 50) | T_(num(x$gh_forks) > 20)
  pkg_words <- ref & grepl("\\b(package|toolbox|library|plug-?in)\\b", chr(x$citing), ignore.case = TRUE, perl = TRUE)
  code_host <- x$repo_type %in% c("github", "gitlab") | dep_type %in% c("Software", "Workflow")
  tool <- T_(x$pkg_layout) | (readme_install & code_host) | pkg_words
  known_not_author <- (F_(dc_creator_overlap) & n_creators <= 20) | F_(osf_author) | F_(owner_author)
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
  author <- author_raw | orcid_author | name_author | host_author
  known_not_author <- known_not_author & !author

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
  data_mark <- grepl("\\[(data ?set|computer software|software|code)\\]|\\bversion\\b|\\bv[0-9]+\\.[0-9]|\\b(zenodo|figshare|dryad|dataverse|github|gitlab|software heritage|swh:|osf\\b|open science framework|repository|data repository)\\b",
                     rt, ignore.case = TRUE, perl = TRUE)
  pub_mark <- grepl(paste0("registered report|unpublished manuscript|\\(manuscript\\)|manuscript in preparation|\\bin press\\b|\\breport on\\b|\\bpreprint\\b",
                           "|\\b[0-9]{1,4}\\s*\\([0-9-]{1,5}\\)\\s*[,:]\\s*[0-9a-z]+|\\b(journal|proceedings|psychological science|nature|science|plos)\\b[^.]{0,40}\\b[0-9]{1,4}\\b"),
                    rt, ignore.case = TRUE, perl = TRUE)
  ref_publication <- ref & pub_mark & !data_mark
  statement_ref <- ref &
    grepl("\\b(can be found|are available|is available|available (at|on|from|via)|deposited|repository:|pository:)", rt, ignore.case = TRUE, perl = TRUE) &
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
