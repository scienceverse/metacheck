# The metadata FAIR_repo() writes beside the reorganised files: a Psych-DS
# dataset_description.json per study, an RO-Crate ro-crate-metadata.json at
# the top, a requirements.txt, logs/, and README and CHANGES files. Ported
# from the convert_psychds branch, minus the OpenScales and Behaverse parts,
# which belong with the measures module. The paper's full text is not written
# for now (see .FAIR_repo_write_paper_text()).

# The text written into any field metacheck cannot extract, so a researcher
# can search the file for it and knows what to fill in by hand.
.FAIR_repo_unknown <- "unknown"

# Representation that receives a numeric statistics block in variableMeasured.
.FAIR_repo_numeric_reps <- c("numeric")
# Measurement levels that receive a valuePattern (their distinct values matter).
.FAIR_repo_categorical_levels <- c("nominal", "ordinal")

# One study of the copy: `dir` is its folder ("" when the copy holds one
# study, else "study-<group>"), `grp` its group code (NULL for one study).
.FAIR_repo_roots <- function(plan) {
  dirs <- unique(sub("^(study-[^/]+)/.*$", "\\1",
                     grep("^study-", stats::na.omit(plan$written), value = TRUE)))
  if (!length(dirs)) return(list(list(dir = "", grp = NULL)))
  lapply(sort(dirs), function(d) list(dir = d, grp = sub("^study-", "", d)))
}

# "-ex1" for a group's ids, "" for a copy with one study.
.FAIR_repo_id <- function(grp) if (is.null(grp)) "" else paste0("-", grp)
.FAIR_repo_label <- function(grp) if (is.null(grp)) "" else paste("Study", toupper(grp))

# ── dataset_description.json ─────────────────────────────────────────────────

# Build the variableMeasured list for one set of columns. `cols` is a subset of
# data_check's columns table; `labels` is codebook_check's labels table (or
# NULL). Returns a list of PropertyValue objects. Every column is described,
# because Psych-DS requires each CSV column to appear in variableMeasured; an
# empty (all-NA) column gets a short entry saying so, without statistics.
.FAIR_repo_variable_measured <- function(cols, labels = NULL, new_path = NULL) {
  if (is.null(cols) || !nrow(cols)) return(list())

  if (!is.null(labels) && nrow(labels) &&
      all(c("source_file", "column_name") %in% names(labels))) {
    keep <- c("source_file", "column_name", "label", "label_status",
              "label_source", "codebook_variable", "scale", "scale_confidence",
              "scale_source", "value_labels", "missing_values", "question")
    labels <- labels[, intersect(keep, names(labels)), drop = FALSE]
    labels <- labels[!duplicated(labels[, c("source_file", "column_name")]), ,
                     drop = FALSE]
    cols <- merge(cols, labels, by = c("source_file", "column_name"),
                  all.x = TRUE, sort = FALSE, suffixes = c("", ".lbl"))
  }
  field <- function(row, f) if (f %in% names(row)) row[[f]] %||% NA else NA
  has <- function(x) length(x) == 1 && !is.na(x) && nzchar(as.character(x))
  # the file a column is in, as it is named in the copy (`new_path`: data_check
  # source_file -> path written), so every variable names its file the same way
  src_of <- function(s) if (s %in% names(new_path)) unname(new_path[s]) else s

  lapply(seq_len(nrow(cols)), function(i) {
    row <- cols[i, , drop = FALSE]
    if (identical(tolower(field(row, "quality")), "empty")) {
      return(Filter(Negate(is.null), list(
        `@type` = "PropertyValue",
        name    = row$column_name,
        description = "Empty column (no observed values in the shared data).",
        `metacheck:quality` = "empty",
        `metacheck:sourceFile` = if (has(row$source_file)) src_of(row$source_file))))
    }

    rep_ <- field(row, "representation")
    lvl  <- field(row, "measurement_level")
    pv <- list(`@type` = "PropertyValue", name = row$column_name)

    # Variable description from the codebook, when documented.
    if (field(row, "label_status") %in% c("labelled", "llm") &&
        has(field(row, "label"))) {
      pv[["description"]] <- row$label
      if (has(field(row, "label_source")))
        pv[["metacheck:labelSource"]] <- row$label_source
      if (has(field(row, "codebook_variable")))
        pv[["metacheck:codebookVariable"]] <- row$codebook_variable
    }

    # The instrument a variable was measured with goes in schema.org's own
    # measurementTechnique; how it was identified in a metacheck extension.
    if (has(field(row, "scale"))) {
      pv[["measurementTechnique"]] <- row$scale
      pv[["metacheck:scale"]] <- Filter(Negate(is.null), list(
        name       = row$scale,
        source     = if (has(field(row, "scale_source"))) row$scale_source,
        confidence = if (has(field(row, "scale_confidence"))) row$scale_confidence))
    }

    # Measurement level and concept have no schema.org property (DDI
    # classificationLevel and Concept); unit is schema.org unitText.
    if (has(lvl)) pv[["metacheck:measurementLevel"]] <- lvl
    if (has(field(row, "concept"))) pv[["metacheck:concept"]] <- row$concept
    if (has(field(row, "unit"))) pv[["unitText"]] <- row$unit
    if (has(field(row, "role"))) pv[["metacheck:role"]] <- row$role
    if (has(field(row, "question"))) pv[["metacheck:question"]] <- row$question

    # Value labels (DDI CodeList), each code with its label.
    vl_json <- field(row, "value_labels")
    vl <- .decode_value_labels(if (has(vl_json)) vl_json else NULL)
    if (length(vl)) {
      pv[["metacheck:valueLabels"]] <- vl_json
      pv[["metacheck:codeList"]] <- unname(lapply(seq_along(vl), function(k)
        list(`@type` = "PropertyValue", value = names(vl)[k],
             name = unname(vl)[k])))
    }
    # Missing-value codes (DDI MissingValues).
    if (has(field(row, "missing_values")))
      pv[["metacheck:missingValues"]] <- row$missing_values

    if (identical(rep_, "numeric")) {
      if (has(field(row, "min"))) pv[["minValue"]] <- row$min
      if (has(field(row, "max"))) pv[["maxValue"]] <- row$max
      stat_fields <- c("n", "n_missing", "mean", "sd", "se", "median",
                       "p25", "p75", "iqr", "skewness", "kurtosis")
      stats_ <- lapply(stat_fields, function(f)
        if (has(field(row, f))) row[[f]] else NULL)
      names(stats_) <- stat_fields
      stats_ <- Filter(Negate(is.null), stats_)
      if (length(stats_)) pv[["metacheck:statistics"]] <- stats_
    }

    # The distinct values of a categorical text column.
    if (has(lvl) && lvl %in% .FAIR_repo_categorical_levels &&
        !identical(rep_, "numeric") && has(field(row, "sample_values"))) {
      vals <- unique(trimws(strsplit(row$sample_values, "\\|")[[1]]))
      vals <- vals[nzchar(vals)]
      if (length(vals)) pv[["valuePattern"]] <- paste(vals, collapse = "|")
    }

    if (has(rep_)) pv[["metacheck:representation"]] <- rep_
    if (has(row$source_file)) pv[["metacheck:sourceFile"]] <- src_of(row$source_file)
    Filter(Negate(is.null), pv)
  })
}

# A keywords value as a flat list of strings. On a paper,
# paper$info$keywords can be a list cell holding the vector, which would
# otherwise be written as a nested array [["a","b"]].
.FAIR_repo_keywords <- function(kw) {
  if (is.null(kw) || !length(kw)) return(NULL)
  if (is.list(kw) && length(kw) == 1) kw <- kw[[1]]
  kw <- as.character(unlist(kw, use.names = FALSE))
  kw <- kw[!is.na(kw) & nzchar(kw)]
  if (length(kw)) as.list(kw) else NULL
}

.FAIR_repo_is_web <- function(x) !is.na(x) & grepl("^https?://", x)

# A module output without the places on this computer it holds, at any
# depth: every file_location (where a file is; also inside code_check's
# version_pin), a repo_url or file_url that is a local path,
# reproducibility_check's sandbox folder, and the full path of `folder` in
# any text (repo_check's report names it), which becomes the folder's own
# name. Used for what is written into logs/.
.FAIR_repo_drop_local <- function(x, folder = NULL) {
  if (is.character(x) && length(folder)) {
    for (f in unique(c(folder, gsub("/", "\\", folder, fixed = TRUE))))
      x <- gsub(f, basename(folder), x, fixed = TRUE)
    return(x)
  }
  if (is.data.frame(x)) {
    x$file_location <- NULL
    for (col in intersect(c("repo_url", "file_url"), names(x)))
      x[[col]][!is.na(x[[col]]) & !.FAIR_repo_is_web(x[[col]])] <- NA
  }
  if (!is.list(x)) return(x)
  if (!is.data.frame(x)) {
    x$file_location <- NULL
    if (is.character(x$sandbox)) x$sandbox <- NULL
    for (k in intersect(c("repo_url", "file_url"), names(x)))
      if (is.character(x[[k]]) && length(x[[k]]) == 1 && !.FAIR_repo_is_web(x[[k]]))
        x[k] <- list(NULL)
  }
  x[] <- lapply(x, .FAIR_repo_drop_local, folder = folder)
  x
}

# MIME type for a file's extension, from the package's one extension table
# (.ext_registry). NULL when none is known, so the field is left out.
.FAIR_repo_encoding_format <- function(file_name) {
  ext <- tolower(tools::file_ext(file_name))
  mime <- .ext_registry$mime[match(ext, .ext_registry$ext)]
  if (length(mime) == 1 && !is.na(mime)) mime else NULL
}

# A field of paper$info, or NULL when absent or empty.
.FAIR_repo_info <- function(paper, field) {
  info <- paper$info %||% list()
  v <- if (field %in% names(info)) info[[field]] else NULL
  if (!length(v) || (length(v) == 1 && is.atomic(v) && (is.na(v) || !nzchar(v))))
    NULL else v
}

.FAIR_repo_doi_url <- function(paper) {
  doi <- .FAIR_repo_info(paper, "doi")
  if (is.null(doi)) NULL else paste0("https://doi.org/", sub("^https?://(dx\\.)?doi.org/", "", doi))
}

# The dataset_description.json of one study.
.FAIR_repo_dataset_description <- function(paper, title, study_label, variables,
                                           reused = character(0)) {
  suffix <- if (nzchar(study_label)) paste0(" — ", study_label) else ""
  desc <- list(
    `@context`       = "https://schema.org/",
    `@type`          = "Dataset",
    name             = paste0(title, suffix),
    description      = paste0("Psych-DS dataset generated by metacheck",
                              if (nzchar(study_label)) paste0(" for ", study_label),
                              "."),
    variableMeasured = variables,
    schemaVersion    = "Psych-DS 1.5.1")

  au <- .FAIR_repo_authors(paper)
  if (nrow(au))
    desc[["author"]] <- lapply(au$name, function(x) list(`@type` = "Person", name = x))
  # the paper that uses the data; the dataset has no DOI of its own yet
  desc[["citation"]] <- .FAIR_repo_doi_url(paper)
  desc[["keywords"]] <- .FAIR_repo_keywords(.FAIR_repo_info(paper, "keywords"))

  # Files this study reuses from another study's folder, which hold them; a
  # plain key, because this file's @context is schema.org alone.
  if (length(reused))
    desc[["reusedMaterials"]] <- lapply(reused, function(p)
      list(contentUrl = p, description = "Reused from another study in this collection."))
  desc[["metacheck:generated"]] <- format(Sys.Date(), "%Y-%m-%d")
  Filter(Negate(is.null), desc)
}

# Per study group, the written paths of files it reuses from another study:
# plan$referenced_by names the OTHER studies whose scripts read a file (see
# data_group_llm()); the file is copied into its own study only.
.FAIR_repo_reused_by_group <- function(plan) {
  out <- list()
  if (!"referenced_by" %in% names(plan)) return(out)
  for (i in seq_len(nrow(plan))) {
    refs <- plan$referenced_by[[i]]
    p <- plan$written[i]
    if (!length(refs) || is.na(p)) next
    for (g in refs) out[[g]] <- unique(c(out[[g]], p))
  }
  out
}

# ── ro-crate-metadata.json ───────────────────────────────────────────────────

# The paper's abstract as one string, for the RO-Crate description: the
# sections typed "abstract" in the paper's text table. "" when there is none.
.FAIR_repo_abstract_text <- function(paper, max_chars = 1500L) {
  if (!.is_paper(paper)) return("")
  tx <- tryCatch(paper_table(paper, "text"), error = function(e) NULL)
  se <- tryCatch(paper_table(paper, "section"), error = function(e) NULL)
  if (!is.data.frame(tx) || !nrow(tx) || !all(c("text", "section_id") %in% names(tx)) ||
      !is.data.frame(se) || !all(c("section_id", "section_type") %in% names(se)))
    return("")
  at <- tx[tx$section_id %in% se$section_id[se$section_type %in% "abstract"], ,
           drop = FALSE]
  if (!nrow(at)) return("")
  if ("text_id" %in% names(at)) at <- at[order(at$text_id), , drop = FALSE]
  txt <- trimws(paste(at$text[nzchar(trimws(at$text))], collapse = " "))
  if (nchar(txt) > max_chars) txt <- paste0(substr(txt, 1L, max_chars), "…")
  txt
}

# A one-element array, the shape of every DDI field in the RO-Crate, holding
# "unknown" when the value is missing.
.FAIR_repo_ddi_array <- function(x) {
  x <- if (!length(x)) NA_character_ else x[1]
  if (is.na(x) || !nzchar(trimws(x))) x <- .FAIR_repo_unknown
  list(x)
}

# One field of the paper's preregistration, from prereg_check's table (one row
# per linked registration). NA when prereg_check was not run, the paper links
# no registration, or the field is empty. Also NA when the paper links more
# than one: papers often register each study separately, with different
# numbers, and nothing yet says which registration covers which study, so
# taking the first would put one study's sample size on every study.
.FAIR_repo_prereg_field <- function(ops, pid, field) {
  pr <- ops[["prereg_check"]]$table
  if (!is.data.frame(pr) || !nrow(pr) || !all(c("paper_id", field) %in% names(pr)))
    return(NA_character_)
  rows <- pr[!is.na(pr$paper_id) & pr$paper_id == pid, , drop = FALSE]
  if (nrow(rows) != 1L) return(NA_character_)
  v <- as.character(rows[[field]][1])
  if (is.na(v) || !nzchar(trimws(v))) NA_character_ else v
}

# How a study was run, with DDI-Lifecycle 3.3 terms: dataCollectionMethodology
# (ddi:DataCollectionMethodology), purpose (ddi:Purpose), populationOfConcern
# (ddi:PopulationOfConcern) and targetSampleSize
# (ddi:OverallTargetSampleSize). They are filled from a single linked
# preregistration when prereg_check was run. Only how the data were collected
# is kept as "unknown" when it is not found; the others are left out, so the
# authors are not given empty fields nobody asked for.
.FAIR_repo_study_design_entity <- function(ops, pid, grp = NULL) {
  get_f <- function(field) .FAIR_repo_prereg_field(ops, pid, field)
  if_found <- function(field) {
    v <- get_f(field)
    if (is.na(v)) NULL else list(v)
  }
  label <- .FAIR_repo_label(grp)
  Filter(Negate(is.null), list(
    `@id`   = paste0("#study-design", .FAIR_repo_id(grp)),
    `@type` = "ResearchProject",
    name    = if (nzchar(label)) paste0("Study design (", label, ")") else "Study design",
    purpose                   = if_found("research_questions"),
    dataCollectionMethodology = .FAIR_repo_ddi_array(get_f("study_design_overview")),
    populationOfConcern       = if_found("data_exclusion_criteria"),
    targetSampleSize          = if_found("sample_size")))
}

# The study's two life-cycle events metacheck cannot find out, as DDI
# LifecycleEvents for the authors to fill in: when the data were collected
# (ddi:DataCollectionDate) and how they were processed into the shared data.
# The scripts that were run are separate events (see
# .FAIR_repo_provenance_entities()).
.FAIR_repo_lifecycle_ids <- c("collection", "processing")

.FAIR_repo_lifecycle_entities <- function(grp = NULL) {
  label <- .FAIR_repo_label(grp)
  of <- if (nzchar(label)) paste0(": ", label) else ""
  base <- paste0("#lifecycle", .FAIR_repo_id(grp))
  list(
    list(
      `@id`        = paste0(base, "-collection"),
      `@type`      = "CreateAction",
      name         = paste0("Data collection", of),
      eventType    = "data collection",
      obtainedDate = .FAIR_repo_ddi_array(.FAIR_repo_unknown)),
    list(
      `@id`       = paste0(base, "-processing"),
      `@type`     = "CreateAction",
      name        = paste0("Data processing", of),
      eventType   = "data processing",
      description = .FAIR_repo_unknown))
}

# The 14 CRediT (ANSI/NISO Z39.104-2022) contributor roles.
.FAIR_repo_credit_roles <- c(
  "conceptualization", "data-curation", "formal-analysis", "funding-acquisition",
  "investigation", "methodology", "project-administration", "resources",
  "software", "supervision", "validation", "visualization",
  "writing-original-draft", "writing-review-editing")

# A role as the paper states it ("Writing - review & editing") as its CRediT
# name ("writing-review-editing"), or NA when it is not a CRediT role.
.FAIR_repo_credit_slug <- function(x) {
  s <- gsub("(^-|-$)", "", gsub("[^a-z]+", "-", tolower(x)))
  ifelse(s %in% .FAIR_repo_credit_roles, s, NA_character_)
}

# One CRediT Role per author, with the roles the paper states for them (by
# their NISO addresses), or "unknown" for the authors to fill in.
.FAIR_repo_credit_entities <- function(authors) {
  lapply(seq_len(nrow(authors)), function(i) {
    roles <- authors$credit[[i]]
    slugs <- .FAIR_repo_credit_slug(roles)
    list(
      `@id`    = paste0("#credit-", i),
      `@type`  = "Role",
      name     = paste("CRediT roles of", authors$name[i]),
      agent    = list(`@id` = authors$id[i]),
      roleName = if (length(roles)) lapply(seq_along(roles), function(k)
        if (is.na(slugs[k])) roles[k] else
          list(`@id` = paste0("https://credit.niso.org/contributor-roles/", slugs[k], "/")))
      else .FAIR_repo_unknown)
  })
}

# ── What is known about the paper and the repository ─────────────────────────

# The paper's authors: name, id (ORCID address, else "#author-<n>"), orcid,
# email, corresponding, affiliation, and credit (a list of the roles the
# paper states). Papers read with bibr carry email, CRediT roles and an
# affiliation table; others may have only names.
.FAIR_repo_authors <- function(paper) {
  au <- paper$author
  empty <- data.frame(name = character(0), id = character(0),
                      orcid = character(0), email = character(0),
                      corresponding = logical(0), affiliation = character(0))
  empty$credit <- list()
  if (!is.data.frame(au) || !nrow(au)) return(empty)
  n <- nrow(au)
  chr <- function(col) {
    v <- if (col %in% names(au) && !is.list(au[[col]])) as.character(au[[col]]) else
      rep(NA_character_, n)
    v[!is.na(v) & !nzchar(trimws(v))] <- NA
    v
  }
  given <- chr("given")
  family <- chr("family")
  name <- trimws(paste(ifelse(is.na(given), "", given), ifelse(is.na(family), "", family)))
  orcid <- chr("orcid")
  orcid <- ifelse(is.na(orcid) | grepl("^https?://", orcid), orcid,
                  paste0("https://orcid.org/", orcid))
  aff <- chr("affiliation")
  # a paper read with bibr has an affiliation table naming its authors
  at <- paper$affiliation
  if (is.data.frame(at) && nrow(at) && "author_ids" %in% names(at) && "author_id" %in% names(au)) {
    label <- if ("institution" %in% names(at)) at$institution else at$text
    if ("text" %in% names(at)) label <- ifelse(is.na(label) | !nzchar(label), at$text, label)
    for (i in which(is.na(aff))) {
      hit <- vapply(at$author_ids, function(ids) au$author_id[i] %in% ids, logical(1))
      if (any(hit)) aff[i] <- paste(unique(stats::na.omit(label[hit])), collapse = "; ")
    }
  }
  credit <- if ("credit_roles" %in% names(au) && is.list(au$credit_roles))
    lapply(au$credit_roles, function(x) as.character(stats::na.omit(unlist(x))))
  else rep(list(character(0)), n)
  out <- data.frame(name = name, orcid = orcid, email = chr("email"),
                    corresponding = if ("corresponding" %in% names(au))
                      au$corresponding %in% TRUE else rep(FALSE, n),
                    affiliation = aff)
  out$id <- ifelse(is.na(out$orcid), paste0("#author-", seq_len(n)), out$orcid)
  out$credit <- credit
  out[nzchar(out$name), , drop = FALSE]
}

# The paper's funding: the statement (from the paper as read by bibr, else
# the sentences funding_check found), and the funders named, when the paper
# was read with bibr. NULL when nothing was found.
.FAIR_repo_funding <- function(paper, ops) {
  statement <- .FAIR_repo_info(paper, "funding_statement")
  ft <- ops[["funding_check"]]$table
  if (is.null(statement) && is.data.frame(ft) && nrow(ft) && "text" %in% names(ft))
    statement <- paste(unique(ft$text), collapse = " ")
  fu <- paper$funding
  funders <- if (is.data.frame(fu) && "funder" %in% names(fu))
    unique(stats::na.omit(as.character(fu$funder))) else character(0)
  awards <- if (is.data.frame(fu) && "award_ids" %in% names(fu))
    unique(stats::na.omit(as.character(unlist(fu$award_ids)))) else character(0)
  if (is.null(statement) && !length(funders)) return(NULL)
  list(statement = statement, funders = funders, awards = awards)
}

# The software a file format needs, for formats that need more than a text
# editor or spreadsheet program.
.FAIR_repo_format_software <- c(
  sav = "IBM SPSS Statistics", zsav = "IBM SPSS Statistics",
  por = "IBM SPSS Statistics", sps = "IBM SPSS Statistics",
  spv = "IBM SPSS Statistics", dta = "Stata", do = "Stata", smcl = "Stata",
  sas7bdat = "SAS", xpt = "SAS", sas = "SAS",
  xlsx = "Microsoft Excel or LibreOffice Calc",
  xls = "Microsoft Excel or LibreOffice Calc", ods = "LibreOffice Calc",
  jasp = "JASP", omv = "jamovi", rds = "R", rdata = "R", rda = "R",
  mat = "MATLAB", m = "MATLAB", inp = "Mplus", psyexp = "PsychoPy",
  iqx = "Inquisit", ebs2 = "E-Prime", es3 = "E-Prime", osexp = "OpenSesame")

# The formats of the repository's files in the copy (not the files metacheck
# wrote): extension, number of files, the software a format needs, and
# whether a CSV copy was written of its data files.
.FAIR_repo_formats <- function(plan, output_dir) {
  ok <- !is.na(plan$written)
  files <- c(plan$written[ok],
             stats::na.omit(plan$original_target[ok & plan$convert %in% TRUE]),
             unlist(plan$sheet_files[ok]))
  ext <- tolower(tools::file_ext(files))
  ext <- ext[nzchar(ext)]
  if (!length(ext)) return(data.frame(ext = character(0), n = integer(0),
                                      software = character(0), csv_copy = logical(0)))
  n <- table(ext)
  converted <- tolower(tools::file_ext(plan$original_target[plan$convert %in% TRUE &
                                                            !is.na(plan$written)]))
  data.frame(ext = names(n), n = as.integer(n),
             software = unname(.FAIR_repo_format_software[names(n)]),
             csv_copy = names(n) %in% converted)
}

# The languages and packages the code uses: languages (name, n files, version
# from a renv.lock or sessionInfo for R) and packages (package, language,
# version from a renv.lock, else NA). Packages that come with the language
# are left out (base R's stats or utils, Python's os or json): they cannot be
# installed, and listing them made `pip install -r requirements.txt` fail. A
# Python package is named as pip installs it (`import sklearn` is
# scikit-learn).
.FAIR_repo_software <- function(ops) {
  cc <- ops[["code_check"]]
  tb <- cc$table
  lang <- if (is.data.frame(tb) && "language" %in% names(tb))
    stats::na.omit(tb$language) else character(0)
  n <- table(lang)
  vp <- cc$version_pin
  r_ver <- unique(stats::na.omit(vp$r_versions %||% character(0)))
  languages <- data.frame(name = names(n), n = as.integer(n))
  languages$version <- ifelse(languages$name == "R" & length(r_ver) > 0,
                              paste(r_ver, collapse = ", "), NA_character_)

  pkgs_of <- function(language) {
    rows <- if (is.data.frame(tb) && "language" %in% names(tb))
      tb[tb$language %in% language, , drop = FALSE] else NULL
    if (is.null(rows) || !nrow(rows)) return(character(0))
    tryCatch(code_packages(rows), error = function(e) character(0))
  }
  r_pkgs <- setdiff(pkgs_of("R"), .repro_base_packages())
  py <- pkgs_of("Python")
  py <- py[!sub("\\..*$", "", py) %in% .repro_py_stdlib_modules()]
  py_pkgs <- unique(.repro_py_pip_name(py))
  renv <- vp$renv_packages
  r_version <- if (is.data.frame(renv) && nrow(renv))
    renv$version[match(r_pkgs, renv$package)] else rep(NA_character_, length(r_pkgs))
  packages <- data.frame(
    package = c(r_pkgs, py_pkgs),
    language = c(rep("R", length(r_pkgs)), rep("Python", length(py_pkgs))),
    version = c(as.character(r_version), rep(NA_character_, length(py_pkgs))))
  list(languages = languages, packages = packages)
}

# The folder's files that are not in the copy, because they could not be read
# or written: their path and type. A file left out because an identical one
# is in the copy is not one of them (see .FAIR_repo_identical()).
.FAIR_repo_not_included <- function(plan, structure_df) {
  same <- plan$same_as %||% rep(NA_character_, nrow(plan))
  rows <- which(is.na(plan$written) & !is.na(plan$target_path) & is.na(same))
  if (!length(rows))
    return(data.frame(path = character(0), type = character(0), reason = character(0)))
  path <- structure_df$file_path[rows] %||% plan$file_name[rows]
  path <- ifelse(is.na(path), plan$file_name[rows], path)
  data.frame(path = path, type = plan$data_type[rows],
             reason = "could not be read or written")
}

# The title in the folder's own README at the top: its first "# " heading, a
# line underlined with "===", or the title: line of an R Markdown or Quarto
# README. NULL when there is no README there, or it marks no title (a plain
# first line is not taken: the TU/e template starts "This readme.text file
# was generated on ...").
.FAIR_repo_readme_title <- function(written, output_dir) {
  top <- written[dirname(written) == "." &
                   grepl("^readme(\\.(md|txt|rmd|qmd|markdown))?$", written, ignore.case = TRUE)]
  for (f in top) {
    lines <- tryCatch(readLines(file.path(output_dir, f), n = 40, warn = FALSE,
                                encoding = "UTF-8"), error = function(e) character(0))
    # a README in another encoding (an older Mac or Windows one: "\xd4"
    # for a curly quote) stopped the conversion in trimws()
    bad <- !validUTF8(lines)
    lines[bad] <- iconv(lines[bad], "latin1", "UTF-8", sub = "")
    lines <- trimws(lines)
    yaml <- grep("^title:", lines, value = TRUE)
    head <- grep("^#\\s+\\S", lines, value = TRUE)
    under <- which(grepl("^=+$", lines) & seq_along(lines) > 1)
    t <- if (length(yaml)) gsub("^title:\\s*|[\"']", "", yaml[1])
         else if (length(head)) sub("^#\\s+", "", head[1])
         else if (length(under) && nzchar(lines[under[1] - 1])) lines[under[1] - 1]
         else NA
    if (!is.na(t) && nzchar(t)) return(t)
  }
  NULL
}

# Everything the RO-Crate and the README are written from, so the two always
# say the same.
.FAIR_repo_collect <- function(paper, pid, ops, plan, structure_df, output_dir) {
  abstract <- .FAIR_repo_abstract_text(paper)
  written <- stats::na.omit(plan$written)
  # every language of the copied scripts, also one code_check did not check
  software <- .FAIR_repo_software(ops)
  code_paths <- plan$written[!is.na(plan$written) & plan$data_type %in% "code"]
  file_langs <- stats::na.omit(unname(code_lang(file.path(output_dir, code_paths))))
  new_langs <- setdiff(unique(file_langs), software$languages$name)
  if (length(new_langs))
    software$languages <- rbind(software$languages, data.frame(
      name = new_langs, n = as.integer(table(file_langs)[new_langs]),
      version = NA_character_))
  licence <- written[grepl("^LICEN[CS]E", basename(written), ignore.case = TRUE)]
  title <- .FAIR_repo_info(paper, "title") %||% .FAIR_repo_readme_title(written, output_dir)
  list(
    pid         = pid,
    title       = title %||% pid,
    title_known = !is.null(title),
    description = if (nzchar(abstract)) abstract else
      "A copy of these research files, reorganised by metacheck.",
    abstract    = abstract,
    keywords    = .FAIR_repo_keywords(.FAIR_repo_info(paper, "keywords")),
    doi_url     = .FAIR_repo_doi_url(paper),
    authors     = .FAIR_repo_authors(paper),
    funding     = .FAIR_repo_funding(paper, ops),
    licence     = if (length(licence)) licence[1] else NA_character_,
    roots       = .FAIR_repo_roots(plan),
    software    = software,
    formats     = .FAIR_repo_formats(plan, output_dir),
    not_included = .FAIR_repo_not_included(plan, structure_df),
    provenance  = .FAIR_repo_provenance_entities(ops, plan),
    design      = lapply(.FAIR_repo_roots(plan), function(root)
      .FAIR_repo_study_design_entity(ops, pid, root$grp)))
}

# Missing-value codes as a named vector (names = codes, values = reasons,
# NA when none was given), from a missing_values JSON string, which is
# either an array of codes or a code -> reason object.
.FAIR_repo_decode_missing <- function(s) {
  if (is.null(s) || length(s) != 1 || is.na(s) || !nzchar(s)) return(NULL)
  out <- tryCatch(jsonlite::fromJSON(s), error = function(e) NULL)
  if (!length(out)) return(NULL)
  v <- unlist(out)
  if (is.null(names(v)) || !any(nzchar(names(v))))
    v <- stats::setNames(rep(NA_character_, length(v)), as.character(v))
  v[!is.na(names(v)) & nzchar(names(v))]
}

# One set of every distinct missing-value code (with its reason) used
# anywhere in the paper's data, or NULL when no variable declares one.
.FAIR_repo_missing_scheme_entity <- function(labels_df) {
  if (!is.data.frame(labels_df) || !"missing_values" %in% names(labels_df))
    return(NULL)
  pooled <- unlist(lapply(labels_df$missing_values, .FAIR_repo_decode_missing))
  if (!length(pooled)) return(NULL)
  key <- paste0(names(pooled), "\r", ifelse(is.na(pooled), "", pooled))
  pooled <- pooled[!duplicated(key)]
  terms <- Map(function(code, reason) Filter(Negate(is.null), list(
    `@type`  = "DefinedTerm",
    termCode = code,
    name     = if (!is.na(reason)) reason)),
    names(pooled), unname(pooled))
  list(
    `@id`   = "#missingvalues",
    `@type` = "DefinedTermSet",
    name    = "Missing-value scheme",
    description = paste0(
      "Codes used across this paper's data to denote a missing response, ",
      "pooled from every variable's declared missing-value codes."),
    hasDefinedTerm = unname(terms))
}

# The written path of a file a script names, or NA: matched by base name
# among the written files; several matches are narrowed to the script's own
# study, and stay NA when that leaves more than one.
.FAIR_repo_resolve_ref <- function(ref, script_group, plan) {
  ok <- !is.na(plan$written)
  cand <- which(ok & tolower(basename(plan$file_name)) == tolower(basename(ref)))
  if (length(cand) > 1 && !is.null(script_group))
    cand <- cand[plan$group[cand] %in% script_group]
  paths <- unique(plan$written[cand])
  if (length(paths) == 1) paths else NA_character_
}

# One RO-Crate CreateAction per script reproducibility_check analysed: the
# script (instrument), the files it reads (object) and writes (result), and
# its place in the run order (schema.org position; scripts without an order
# between them share one). reproducibility_check's `reads`/`writes` hold the
# file names the code names, found by reading it, not by running it.
.FAIR_repo_provenance_entities <- function(ops, plan) {
  repro <- ops[["reproducibility_check"]]$table
  if (!is.data.frame(repro) || !nrow(repro) ||
      !all(c("file_name", "reads", "writes") %in% names(repro))) return(list())

  slug <- function(fn) gsub("(^-|-$)", "", tolower(gsub("[^A-Za-z0-9]+", "-", basename(fn))))
  entities <- list()
  used <- character(0)
  for (i in seq_len(nrow(repro))) {
    code_path <- .FAIR_repo_resolve_ref(repro$file_name[i], NULL, plan)
    if (is.na(code_path)) next
    grp <- plan$group[match(code_path, plan$written)]
    grp <- if (is.na(grp)) NULL else grp
    resolve <- function(refs) {
      refs <- unlist(refs)
      refs <- refs[!is.na(refs) & nzchar(refs)]
      p <- vapply(refs, .FAIR_repo_resolve_ref, character(1), script_group = grp,
                  plan = plan)
      unique(p[!is.na(p)])
    }
    reads <- resolve(repro$reads[i])
    writes <- resolve(repro$writes[i])
    if (!length(reads) && !length(writes)) next

    base <- slug(repro$file_name[i])
    if (!nzchar(base)) base <- as.character(i)
    id <- base
    n <- 1L
    while (id %in% used) {
      n <- n + 1L
      id <- paste0(base, "-", n)
    }
    used <- c(used, id)
    pos <- if ("run_order" %in% names(repro))
      suppressWarnings(as.integer(repro$run_order[i])) else NA_integer_
    ids <- function(p) if (length(p)) lapply(unname(p), function(x) list(`@id` = x))

    entities[[length(entities) + 1]] <- Filter(Negate(is.null), list(
      `@id`      = paste0("#lifecycle-", id),
      `@type`    = "CreateAction",
      name       = paste("Script run:", basename(repro$file_name[i])),
      eventType  = "script run",
      position   = if (!is.na(pos)) pos,
      instrument = list(`@id` = code_path),
      object     = ids(reads),
      result     = ids(writes)))
  }
  entities
}

# One RO-Crate File entity per written file: its type and size, and for a CSV
# written from another data file, the original kept next to it; for a script
# recovered from an output file, that file.
.FAIR_repo_file_entities <- function(plan, output_dir) {
  rows <- which(!is.na(plan$written))
  # a script recovered from an output file is based on that file
  recovered <- !is.na(plan$recovered_source %||% rep(NA_character_, nrow(plan)))
  out <- lapply(rows, function(i) {
    p <- plan$written[i]
    orig <- plan$original_target[i]
    converted <- plan$convert[i] %in% TRUE && !is.na(orig) && nzchar(orig)
    size <- suppressWarnings(file.size(file.path(output_dir, p)))
    # the copy, so a notebook's or Quarto document's language is read from it
    lang <- if (identical(plan$data_type[i], "code"))
      unname(code_lang(file.path(output_dir, p))) else NA
    Filter(Negate(is.null), list(
      `@id`          = p,
      `@type`        = if (!is.na(lang)) list("File", "SoftwareSourceCode") else "File",
      name           = basename(p),
      encodingFormat = .FAIR_repo_encoding_format(p),
      contentSize    = if (!is.na(size)) as.character(size),
      programmingLanguage = if (!is.na(lang)) list(`@id` = .FAIR_repo_lang_id(lang)),
      `metacheck:software` = .FAIR_repo_software_for(p),
      isBasedOn      = if (converted) list(`@id` = orig)
                       else if (recovered[i]) list(`@id` = plan$recovered_source[i])))
  })
  # the CSVs of an Excel file's other sheets
  sheets <- plan$sheet_files %||% rep(list(character(0)), nrow(plan))
  for (i in rows[lengths(sheets[rows]) > 0]) for (p in sheets[[i]]) {
    size <- suppressWarnings(file.size(file.path(output_dir, p)))
    out[[length(out) + 1]] <- Filter(Negate(is.null), list(
      `@id` = p, `@type` = "File", name = basename(p),
      encodingFormat = .FAIR_repo_encoding_format(p),
      contentSize = if (!is.na(size)) as.character(size),
      isBasedOn = if (!is.na(plan$original_target[i])) list(`@id` = plan$original_target[i])))
  }
  # the originals kept next to their CSV files
  orig_rows <- rows[plan$convert[rows] %in% TRUE & !is.na(plan$original_target[rows])]
  originals <- lapply(orig_rows, function(i) {
    p <- plan$original_target[i]
    if (!file.exists(file.path(output_dir, p))) return(NULL)
    Filter(Negate(is.null), list(
      `@id`          = p,
      `@type`        = "File",
      name           = basename(p),
      encodingFormat = .FAIR_repo_encoding_format(p),
      contentSize    = as.character(file.size(file.path(output_dir, p))),
      `metacheck:software` = .FAIR_repo_software_for(p)))
  })
  c(out, Filter(Negate(is.null), originals))
}

# The software a file's format needs, or NULL (see .FAIR_repo_format_software).
.FAIR_repo_software_for <- function(path) {
  s <- .FAIR_repo_format_software[tolower(tools::file_ext(path))]
  if (length(s) == 1 && !is.na(s)) unname(s) else NULL
}

.FAIR_repo_lang_id <- function(lang) paste0("#language-", tolower(gsub("[^A-Za-z0-9]+", "-", lang)))

# Build and write ro-crate-metadata.json (RO-Crate 1.3) at the top of the copy,
# from `info` (.FAIR_repo_collect()). Its @context adds the DDI-Lifecycle
# terms used for how a study was run and metacheck's own prefix. The root
# Dataset ("./") holds every file: with several studies, each study is a
# Dataset part holding its own files. `extra_files` are the files metacheck
# wrote (logs, requirements.txt, README, CHANGES), relative to the top.
#
# "unknown" is written only where the authors are asked for something:
# when and how the data were collected, how they were processed, each
# author's CRediT roles the paper does not state, and the licence.
.FAIR_repo_rocrate_json <- function(info, output_dir, plan,
                                    labels_df = NULL,
                                    extra_files = character(0)) {
  roots <- info$roots
  multi <- length(roots) > 1
  files <- .FAIR_repo_file_entities(plan, output_dir)
  file_ids <- vapply(files, `[[`, character(1), "@id")
  # a repository's own ro-crate-metadata.json is replaced by this one
  keep <- file_ids != "ro-crate-metadata.json"
  files <- files[keep]
  file_ids <- file_ids[keep]
  in_dir <- function(ids, dir) if (nzchar(dir)) startsWith(ids, paste0(dir, "/")) else
    !grepl("^study-", ids)
  ref <- function(ids) lapply(unname(ids), function(x) list(`@id` = x))
  # a file metacheck added to (an author's CHANGES.md) is listed once
  extra_files <- setdiff(unique(extra_files[nzchar(extra_files)]), file_ids)
  extra <- lapply(extra_files, function(p) Filter(Negate(is.null), list(
    `@id` = p, `@type` = "File", name = basename(p),
    encodingFormat = .FAIR_repo_encoding_format(p))))

  reused_by_group <- .FAIR_repo_reused_by_group(plan)
  about_of <- function(root) c(
    ref(paste0("#study-design", .FAIR_repo_id(root$grp))),
    ref(paste0("#lifecycle", .FAIR_repo_id(root$grp), "-", .FAIR_repo_lifecycle_ids)))

  study_parts <- if (multi) lapply(roots, function(root) {
    dt <- plan$data_type[!is.na(plan$written) & plan$group %in% root$grp]
    reused <- reused_by_group[[root$grp]] %||% character(0)
    Filter(Negate(is.null), list(
      `@id`   = paste0(root$dir, "/"),
      `@type` = "Dataset",
      name    = .FAIR_repo_label(root$grp),
      hasPart = c(ref(file_ids[in_dir(file_ids, root$dir)]),
                  ref(extra_files[in_dir(extra_files, root$dir)])),
      `metacheck:hasSharedData`      = any(dt == "data", na.rm = TRUE),
      `metacheck:hasSharedCode`      = any(dt == "code", na.rm = TRUE),
      `metacheck:hasSharedMaterials` = any(dt == "materials", na.rm = TRUE),
      `metacheck:reusesFile` = if (length(reused)) ref(reused),
      about = about_of(root)))
  }) else list()

  # Authors, with their email; a corresponding author is the contact.
  au <- info$authors
  author_entities <- lapply(seq_len(nrow(au)), function(i) Filter(Negate(is.null), list(
    `@id` = au$id[i], `@type` = "Person", name = au$name[i],
    affiliation  = if (!is.na(au$affiliation[i])) au$affiliation[i],
    email        = if (!is.na(au$email[i])) au$email[i],
    contactPoint = if (au$corresponding[i] && !is.na(au$email[i]))
      list(`@id` = paste0("mailto:", au$email[i])))))
  contacts <- lapply(which(au$corresponding & !is.na(au$email)), function(i) list(
    `@id` = paste0("mailto:", au$email[i]), `@type` = "ContactPoint",
    contactType = "corresponding author", email = au$email[i], name = au$name[i]))
  credit <- .FAIR_repo_credit_entities(au)

  paper_entity <- if (!is.null(info$doi_url)) list(list(
    `@id` = info$doi_url, `@type` = "ScholarlyArticle", name = info$title))

  fu <- info$funding
  funder_entities <- if (!is.null(fu)) lapply(seq_along(fu$funders), function(k) list(
    `@id` = paste0("#funder-", k), `@type` = "Organization", name = fu$funders[k]))
  grant <- if (!is.null(fu)) list(Filter(Negate(is.null), list(
    `@id` = "#funding", `@type` = "Grant", name = "Funding",
    description = fu$statement,
    identifier = if (length(fu$awards)) as.list(fu$awards),
    funder = if (length(funder_entities)) ref(paste0("#funder-", seq_along(fu$funders))))))

  sw <- info$software
  lang_entities <- lapply(seq_len(nrow(sw$languages)), function(i) Filter(Negate(is.null), list(
    `@id` = .FAIR_repo_lang_id(sw$languages$name[i]), `@type` = "ComputerLanguage",
    name = sw$languages$name[i],
    version = if (!is.na(sw$languages$version[i])) sw$languages$version[i])))
  package_entities <- lapply(seq_len(nrow(sw$packages)), function(i) Filter(Negate(is.null), list(
    `@id` = paste0("#package-", tolower(sw$packages$language[i]), "-", sw$packages$package[i]),
    `@type` = "SoftwareApplication",
    name = sw$packages$package[i],
    programmingLanguage = list(`@id` = .FAIR_repo_lang_id(sw$packages$language[i])),
    version = if (!is.na(sw$packages$version[i])) sw$packages$version[i])))

  ni <- info$not_included
  not_included <- lapply(seq_len(nrow(ni)), function(i) Filter(Negate(is.null), list(
    `@id` = paste0("#not-included-", i), `@type` = "CreativeWork", name = ni$path[i],
    description = paste("Not in this copy:", ni$reason[i]))))

  missing_entity <- .FAIR_repo_missing_scheme_entity(labels_df)
  provenance <- info$provenance

  root <- Filter(Negate(is.null), list(
    `@id`       = "./",
    `@type`     = "Dataset",
    name        = info$title,
    description = info$description,
    keywords    = info$keywords,
    citation    = if (!is.null(info$doi_url)) list(`@id` = info$doi_url),
    # "unknown" when no paper named them, for the authors to fill in
    author      = if (nrow(au)) ref(au$id) else .FAIR_repo_unknown,
    license     = if (!is.na(info$licence)) list(`@id` = info$licence) else .FAIR_repo_unknown,
    funding     = if (!is.null(grant)) list(`@id` = "#funding"),
    dateCreated = format(Sys.Date(), "%Y-%m-%d"),
    hasPart     = c(if (multi) ref(paste0(vapply(roots, `[[`, character(1), "dir"), "/")),
                    ref(file_ids[in_dir(file_ids, "")]),
                    ref(extra_files[in_dir(extra_files, "")])),
    about       = c(if (!multi) about_of(roots[[1]]),
                    if (!is.null(missing_entity)) ref("#missingvalues"),
                    ref(vapply(provenance, `[[`, character(1), "@id")),
                    ref(vapply(credit, `[[`, character(1), "@id"))),
    `metacheck:softwareRequirements` = c(
      ref(vapply(lang_entities, `[[`, character(1), "@id")),
      ref(vapply(package_entities, `[[`, character(1), "@id"))),
    `metacheck:notIncluded` = if (length(not_included))
      ref(vapply(not_included, `[[`, character(1), "@id"))))
  if (!length(root$`metacheck:softwareRequirements`))
    root$`metacheck:softwareRequirements` <- NULL

  lifecycle <- unlist(lapply(roots, function(root)
    .FAIR_repo_lifecycle_entities(root$grp)), recursive = FALSE)

  descriptor <- list(
    `@id`      = "ro-crate-metadata.json",
    `@type`    = "CreativeWork",
    conformsTo = list(`@id` = "https://w3id.org/ro/crate/1.3"),
    about      = list(`@id` = "./"))

  crate <- list(
    `@context` = list(
      "https://w3id.org/ro/crate/1.3/context",
      list(
        metacheck = "https://scienceverse.org/schema/metacheck/terms/",
        ddi       = "https://ddialliance.org/Specification/DDI-Lifecycle/3.3/",
        populationOfConcern       = "ddi:PopulationOfConcern",
        targetSampleSize          = "ddi:OverallTargetSampleSize",
        obtainedDate              = "ddi:DataCollectionDate",
        eventType                 = "ddi:EventType",
        dataCollectionMethodology = "ddi:DataCollectionMethodology",
        purpose                   = "ddi:Purpose")),
    `@graph` = c(list(descriptor, root), study_parts, files, extra,
                 paper_entity, grant, funder_entities,
                 info$design,
                 if (!is.null(missing_entity)) list(missing_entity),
                 lifecycle, provenance, credit, author_entities, contacts,
                 lang_entities, package_entities, not_included))

  path <- file.path(output_dir, "ro-crate-metadata.json")
  .FAIR_repo_write_json(crate, path)
  invisible(path)
}

.FAIR_repo_write_json <- function(obj, path) {
  dir.create(dirname(path), recursive = TRUE, showWarnings = FALSE)
  json <- jsonlite::toJSON(.FAIR_repo_utf8(obj), auto_unbox = TRUE, pretty = TRUE,
                           na = "null", null = "null")
  con <- file(path, open = "wb", encoding = "UTF-8")
  on.exit(close(con), add = TRUE)
  writeLines(json, con, useBytes = TRUE)
  invisible(path)
}

# ── Other files ──────────────────────────────────────────────────────────────

# Not used for now: FAIR_repo() does not copy the paper's full text into the
# folder (see .FAIR_repo_write_metadata()). Remove the # to use it again.
#
# The paper's full text as documentation/fulltext.txt (or
# documentation/<paper_id>_fulltext.txt), with a "# header" line where a
# section starts. NULL when the paper holds no text.
# .FAIR_repo_write_paper_text <- function(paper, output_dir, pid) {
#   if (!.is_paper(paper)) return(NULL)
#   ft <- tryCatch(paper_table(paper, "text"), error = function(e) NULL)
#   if (!is.data.frame(ft) || !nrow(ft) || !"text" %in% names(ft)) return(NULL)
#   sections <- tryCatch(paper_table(paper, "section"), error = function(e) NULL)
#   scols <- c("section_id", "paper_id", "header")
#   if (is.data.frame(sections) && all(scols %in% names(sections)) &&
#       "section_id" %in% names(ft) && !"header" %in% names(ft))
#     ft <- dplyr::left_join(ft, sections[, scols],
#                            by = intersect(c("section_id", "paper_id"), names(ft)),
#                            relationship = "many-to-many")
#   if ("text_id" %in% names(ft)) ft <- ft[order(ft$text_id), , drop = FALSE]
#   header <- if ("header" %in% names(ft)) ft$header else rep(NA_character_, nrow(ft))
# 
#   lines <- character(0)
#   last <- NA_character_
#   for (i in seq_len(nrow(ft))) {
#     txt <- ft$text[i]
#     if (is.na(txt) || !nzchar(trimws(txt))) next
#     h <- header[i]
#     if (!is.na(h) && nzchar(trimws(h)) && !identical(h, last) &&
#         !identical(trimws(h), trimws(txt))) {
#       if (length(lines)) lines <- c(lines, "")
#       lines <- c(lines, paste0("# ", trimws(h)), "")
#       last <- h
#     }
#     lines <- c(lines, trimws(txt))
#   }
#   if (!length(lines)) return(NULL)
#   path <- file.path(output_dir, "documentation",
#                     if (file.exists(file.path(output_dir, "documentation", "fulltext.txt")))
#                       paste0(pid, "_fulltext.txt") else "fulltext.txt")
#   dir.create(dirname(path), recursive = TRUE, showWarnings = FALSE)
#   con <- file(path, open = "wb", encoding = "UTF-8")
#   on.exit(close(con), add = TRUE)
#   writeLines(lines, con)
#   path
# }

# The list of the original folder's files: each file's path in the folder,
# size and type, and which metacheck version (and language model, if any)
# made the copy.
.FAIR_repo_file_list <- function(structure_df, pid, path) {
  col <- function(nm) structure_df[[nm]] %||% rep(NA_character_, nrow(structure_df))
  loc <- col("file_location")
  files <- lapply(seq_len(nrow(structure_df)), function(i) {
    size <- if (!is.na(loc[i]) && file.exists(loc[i])) file.size(loc[i]) else NA
    Filter(Negate(is.null), list(
      file_path = col("file_path")[i] %||% col("file_name")[i],
      file_size = if (!is.na(size)) size,
      data_type = col("data_type")[i]))
  })
  doc <- list(
    folder = pid,
    generated = format(Sys.time(), "%Y-%m-%dT%H:%M:%S%z"),
    provenance = list(
      software = list(name = "metacheck", version = tryCatch(
        as.character(utils::packageVersion("metacheck")),
        error = function(e) NA_character_)),
      r_version = R.version.string,
      llm = if (isTRUE(llm_use())) list(used = TRUE, model = llm_model()) else
        list(used = FALSE)),
    n_files = nrow(structure_df),
    files = files)
  .FAIR_repo_write_json(doc, path)
}

# The logs/ folder: the list of the original folder's files, and every
# check's results as an R data file (capture_module_tables()), without the
# places on this computer the files are in. Returns the written paths, named
# by path, with a one-line description each.
.FAIR_repo_write_logs <- function(ops, structure_df, pid, output_dir,
                                 folder = NULL) {
  logs_dir <- file.path(output_dir, "logs")
  dir.create(logs_dir, recursive = TRUE, showWarnings = FALSE)
  written <- character(0)

  if (is.data.frame(structure_df) && nrow(structure_df)) {
    file_list <- file.path(logs_dir, paste0(pid, ".files.json"))
    .FAIR_repo_file_list(structure_df, pid, file_list)
    written[file_list] <- "Every file of the original folder, with its path, size and type."
  }

  rds <- tryCatch(capture_module_tables(lapply(ops, .FAIR_repo_drop_local, folder = folder),
                                        logs_dir, paper_id = pid),
                  error = function(e) NULL)
  if (length(rds) && file.exists(rds))
    written[rds] <- "The results of every metacheck check that was run, as an R data file (readRDS())."

  # what each file holds, in a log of its own in the folder, so the copy
  # keeps one README per folder
  if (length(written)) {
    log <- file.path(logs_dir, "log.md")
    writeLines(c("# Log", "",
                 "The records of the metacheck checks behind this copy of the files:",
                 "", sprintf("- `%s`: %s", basename(names(written)), unname(written))),
               log)
    written[log] <- "What each file in this folder holds."
  }
  written
}

# requirements.txt at the top, listing the Python packages the code imports,
# for `pip install -r requirements.txt` (names only: the code does not say
# which versions were used). R packages are not listed here, since pip would
# try to install them; they are in the README and the RO-Crate. Not written
# when the folder has its own requirements.txt, environment.yml or
# pyproject.toml.
.FAIR_repo_write_requirements <- function(software, plan, output_dir) {
  pkgs <- software$packages$package[software$packages$language == "Python"]
  if (!length(pkgs)) return(NULL)
  own <- grepl("^(requirements.*\\.txt|environment\\.ya?ml|pyproject\\.toml)$",
               basename(stats::na.omit(plan$written)), ignore.case = TRUE)
  path <- file.path(output_dir, "requirements.txt")
  if (any(own) || file.exists(path)) return(NULL)
  writeLines(c(
    "# Written by metacheck from the Python packages the code imports.",
    "# These are package names only, not versions: the code shows which",
    "# packages are imported, not which versions were used.",
    pkgs), path)
  path
}

# The changes metacheck made, as lines for CHANGES.md.
.FAIR_repo_change_lines <- function(plan, copied) {
  lines <- paste0(format(Sys.Date(), "%Y-%m-%d"), " - Files reorganised by ",
                  "[metacheck](https://scienceverse.github.io/metacheck/): every ",
                  "file was moved to its place in the Psych-DS layout.")
  conv <- which(plan$convert %in% TRUE & !is.na(plan$written))
  if (length(conv))
    lines <- c(lines, "", "Data files written as CSV files (the original is kept next to each):",
               vapply(conv, function(i) {
                 csvs <- c(plan$written[i], plan$sheet_files[[i]])
                 sprintf("- `%s` from `%s`%s", paste(csvs, collapse = "`, `"),
                         plan$original_target[i],
                         if (length(csvs) > 1) " (one file per sheet)" else "")
               }, character(1)))
  bag <- which(!is.na(plan$written) & startsWith(plan$written, "logs/original-package/"))
  if (length(bag))
    lines <- c(lines, "", paste0(
      "The files of the original archive package (BagIt) are in ",
      "`logs/original-package/`. They list the files as they were before ",
      "this reorganisation, so their paths and checksums do not match this copy:"),
      sprintf("- `%s`", basename(plan$written[bag])))
  if (length(copied$not_tables))
    lines <- c(lines, "", "Data files kept as they are, because they hold no table:",
               sprintf("- `%s`", plan$written[copied$not_tables]))
  same <- which(!is.na(plan$same_as %||% rep(NA, nrow(plan))))
  if (length(same))
    lines <- c(lines, "", "Files left out, because an identical file is in this copy:",
               sprintf("- `%s` (identical to `%s`)",
                       plan$current_path[same] %||% plan$file_name[same],
                       plan$same_as[same]))
  if (nrow(copied$renamed))
    lines <- c(lines, "", "Files given a numbered name, because another file had the same place:",
               sprintf("- `%s` is `%s`", copied$renamed$written, copied$renamed$file_name))
  sc <- copied$scripts
  if (nrow(sc) && any(sc$paths_changed > 0))
    lines <- c(lines, "", "Scripts whose file paths were changed to the new layout:",
               sprintf("- `%s` (%d path%s)", sc$target_path[sc$paths_changed > 0],
                       sc$paths_changed[sc$paths_changed > 0],
                       plural(sc$paths_changed[sc$paths_changed > 0])))
  if (nrow(sc) && any(sc$setwd_removed > 0)) {
    hit <- sc[sc$setwd_removed > 0, , drop = FALSE]
    lines <- c(lines, "", paste0(
      "Scripts whose `setwd()` lines were replaced by a statement that does ",
      "nothing, because they set a folder written out in the script or the ",
      "script's own folder, and the file paths now start at the top folder ",
      "(the original line is kept as a comment). Run the scripts from the top ",
      "folder of this repository:"),
      sprintf("- `%s`%s", hit$target_path,
              ifelse(nzchar(hit$setwd_paths), paste0(" (was `", hit$setwd_paths, "`)"), "")))
  }
  libs <- sc$libraries_added %||% rep("", nrow(sc))
  if (nrow(sc) && any(nzchar(libs)))
    lines <- c(lines, "", paste0(
      "Scripts given `library()` calls at the top, because running them showed ",
      "they use a function of a package they do not load:"),
      sprintf("- `%s` (%s)", sc$target_path[nzchar(libs)], libs[nzchar(libs)]))
  rec <- which(!is.na(plan$written) & !is.na(plan$recovered_row %||% NA))
  if (length(rec))
    lines <- c(lines, "", paste0(
      "Scripts recovered from output files, which hold the code that made ",
      "them (the output file is kept too):"),
      sprintf("- `%s` from `%s`", plan$written[rec],
              ifelse(is.na(plan$recovered_source[rec]),
                     plan$current_path[plan$recovered_row[rec]] %||% "an output file",
                     plan$recovered_source[rec])))
  links <- attr(copied, "readme_links") %||% character(0)
  if (length(links))
    lines <- c(lines, "", paste0(
      "READMEs whose links were changed to the files' new places (the text ",
      "is unchanged):"), sprintf("- `%s`", links))
  lines
}

# Add the changes to the top CHANGES.md (after any the repository has); write
# a README.md in each folder that has none (the top, and each study folder),
# as a readable version of ro-crate-metadata.json; and a CHANGES.md in each
# study folder that has none, as Psych-DS expects. A README or CHANGES file of
# the repository itself is left as it is. Returns the paths written.
.FAIR_repo_write_readme_changes <- function(plan, copied, info, output_dir,
                                            columns_df = NULL, labels_df = NULL) {
  has_file <- function(dir, pattern) {
    w <- stats::na.omit(plan$written)
    w <- if (nzchar(dir)) w[dirname(w) == dir] else w[dirname(w) == "."]
    any(grepl(pattern, basename(w), ignore.case = TRUE))
  }
  written <- character(0)

  changes <- file.path(output_dir, "CHANGES.md")
  old <- if (file.exists(changes)) readLines(changes, warn = FALSE) else character(0)
  writeLines(c(old, if (length(old)) "", .FAIR_repo_change_lines(plan, copied)), changes)
  written <- c(written, changes)

  multi <- length(info$roots) > 1
  if (multi) for (root in info$roots) {
    if (!has_file(root$dir, "^CHANGES")) {
      path <- file.path(output_dir, root$dir, "CHANGES.md")
      writeLines(paste0(format(Sys.Date(), "%Y-%m-%d"), " - Study folder ",
                        "made by metacheck. See CHANGES.md in the top folder."), path)
      written <- c(written, path)
    }
  }

  # the files the copy will hold, including the ones written after this
  will_have <- function(dir_files) unique(c(dir_files, "README.md"))
  study_readmes <- if (multi) unlist(lapply(info$roots, function(root)
    if (!has_file(root$dir, "^README")) paste0(root$dir, "/README.md")))
  all_files <- c(list.files(output_dir, recursive = TRUE), "ro-crate-metadata.json",
                 study_readmes)

  if (!has_file("", "^README")) {
    path <- file.path(output_dir, "README.md")
    .FAIR_repo_write_lines(
      .FAIR_repo_readme_top(info, plan, will_have(all_files), columns_df, labels_df),
      path)
    written <- c(written, path)
  }
  if (multi) for (root in info$roots) {
    if (has_file(root$dir, "^README")) next
    in_dir <- all_files[startsWith(all_files, paste0(root$dir, "/"))]
    path <- file.path(output_dir, root$dir, "README.md")
    .FAIR_repo_write_lines(
      .FAIR_repo_readme_study(info, root, plan,
                              will_have(sub("^[^/]+/", "", in_dir)),
                              columns_df, labels_df),
      path)
    written <- c(written, path)
  }
  written
}

.FAIR_repo_write_lines <- function(lines, path) {
  con <- file(path, open = "wb", encoding = "UTF-8")
  on.exit(close(con), add = TRUE)
  writeLines(.FAIR_repo_utf8(lines), con)
}

# Every text in `x` (at any depth) as valid UTF-8. A text from the data in an
# older Western European encoding (a column "PPFD (\xb5mol m-2 s-1)") is
# read as Latin-1; the JSON with it was invalid, and no JSON reader opened it.
.FAIR_repo_utf8 <- function(x) {
  if (is.character(x)) {
    bad <- !is.na(x) & !validUTF8(x)
    if (any(bad)) x[bad] <- iconv(x[bad], "latin1", "UTF-8", sub = "?")
    return(x)
  }
  if (is.list(x)) x[] <- lapply(x, .FAIR_repo_utf8)
  x
}

# A folder tree of `paths` (relative, with "/"), for a code block. At most
# `max` files are shown.
.FAIR_repo_tree <- function(paths, max = 150L) {
  paths <- sort(unique(paths))
  more <- length(paths) - max
  if (more > 0) paths <- paths[seq_len(max)]
  lines <- character(0)
  prev <- character(0)
  for (p in paths) {
    parts <- strsplit(p, "/", fixed = TRUE)[[1]]
    dirs <- parts[-length(parts)]
    k <- 0L
    while (k < min(length(prev), length(dirs)) && prev[k + 1L] == dirs[k + 1L]) k <- k + 1L
    for (j in seq_along(dirs)[seq_along(dirs) > k])
      lines <- c(lines, paste0(strrep("  ", j - 1L), dirs[j], "/"))
    lines <- c(lines, paste0(strrep("  ", length(dirs)), parts[length(parts)]))
    prev <- dirs
  }
  if (more > 0) lines <- c(lines, sprintf("... and %d more files", more))
  c("```", lines, "```")
}

# Per written data CSV file: a table of its variables, their labels and
# missing-value codes, at most `max` rows.
.FAIR_repo_variable_tables <- function(plan, columns_df, labels_df, rows,
                                       strip = "", max = 30L) {
  if (!is.data.frame(columns_df) || !nrow(columns_df)) return(character(0))
  out <- character(0)
  # one table per CSV: a spreadsheet's sheets each have their own
  entries <- unlist(lapply(rows, function(i) {
    map <- (plan$sheet_map %||% rep(list(character(0)), nrow(plan)))[[i]]
    if (!length(map))
      return(list(list(i = i, key = plan$file_name[i], written = plan$written[i])))
    lapply(names(map), function(s)
      list(i = i, key = .FAIR_repo_sheet_key(plan$file_name[i], s, columns_df),
           written = map[[s]]))
  }), recursive = FALSE)
  for (entry in entries) {
    i <- entry$i
    keep <- columns_df$source_file %in% entry$key
    if ("group" %in% names(columns_df) && !is.na(plan$group[i]))
      keep <- keep & columns_df$group %in% plan$group[i]
    cols <- columns_df[keep, , drop = FALSE]
    if (!nrow(cols)) next
    lab <- if (is.data.frame(labels_df) && nrow(labels_df) &&
               all(c("source_file", "column_name") %in% names(labels_df)))
      labels_df[labels_df$source_file %in% entry$key, , drop = FALSE] else NULL
    m <- if (!is.null(lab)) match(cols$column_name, lab$column_name) else rep(NA, nrow(cols))
    label <- if (!is.null(lab) && "label" %in% names(lab)) lab$label[m] else NA
    mv <- if (!is.null(lab) && "missing_values" %in% names(lab))
      vapply(lab$missing_values[m], function(s) {
        v <- .FAIR_repo_decode_missing(s)
        if (!length(v)) "" else paste(names(v), collapse = ", ")
      }, character(1)) else rep("", nrow(cols))
    esc <- function(x) gsub("|", "\\|", ifelse(is.na(x), "", x), fixed = TRUE)
    # a table of names only says nothing dataset_description.json does not
    if (!any(nzchar(esc(label))) && !any(nzchar(mv))) next
    n <- nrow(cols)
    show <- seq_len(min(n, max))
    out <- c(out, "", paste0("### `", sub(strip, "", entry$written), "`"), "",
             "| Variable | Label | Missing-value codes |", "|---|---|---|",
             sprintf("| %s | %s | %s |", esc(cols$column_name[show]),
                     esc(label[show]), esc(mv[show])))
    if (n > max)
      out <- c(out, "", sprintf("... and %d more variables; see `dataset_description.json`.",
                                n - max))
  }
  out
}

# What the authors are still asked for, as README lines.
.FAIR_repo_still_unknown <- function(info) {
  au <- info$authors
  no_roles <- au$name[lengths(au$credit) == 0]
  c(if (!isTRUE(info$title_known)) "- The title",
    if (!nrow(au)) "- The authors",
    if (is.null(info$doi_url)) "- The paper that uses these files",
    if (is.na(info$licence)) "- The licence under which the data may be reused",
    if (!any(au$corresponding & !is.na(au$email))) "- A contact person and email address",
    if (length(no_roles)) paste0("- The CRediT roles of ", paste(no_roles, collapse = ", ")),
    "- When the data were collected",
    if (any(vapply(info$design, function(d) identical(d$dataCollectionMethodology, list(.FAIR_repo_unknown)), logical(1))))
      "- How the data were collected",
    "- How the data were processed into the shared data")
}

# The README of the top folder: everything in ro-crate-metadata.json, for
# people.
.FAIR_repo_readme_top <- function(info, plan, files, columns_df, labels_df) {
  au <- info$authors
  au_lines <- if (nrow(au)) vapply(seq_len(nrow(au)), function(i) {
    paste0("- ", au$name[i],
           if (!is.na(au$orcid[i])) paste0(" (", au$orcid[i], ")"),
           if (!is.na(au$affiliation[i])) paste0(", ", au$affiliation[i]),
           if (length(au$credit[[i]])) paste0(". CRediT: ", paste(au$credit[[i]], collapse = ", ")))
  }, character(1)) else "Not found."
  contacts <- which(au$corresponding & !is.na(au$email))
  contact <- if (length(contacts))
    paste0(au$name[contacts], " <", au$email[contacts], ">", collapse = "; ") else "unknown"

  multi <- length(info$roots) > 1
  fmt <- info$formats
  fmt_lines <- if (nrow(fmt)) c("| Format | Files | Software needed | CSV copy included |",
                               "|---|---|---|---|",
                               sprintf("| .%s | %d | %s | %s |", fmt$ext, fmt$n,
                                       ifelse(is.na(fmt$software), "", fmt$software),
                                       ifelse(fmt$csv_copy, "yes", ""))) else "No files."

  sw <- info$software
  sw_lines <- c(
    if (nrow(sw$languages)) sprintf("- %s%s (%d file%s)", sw$languages$name,
                                    ifelse(is.na(sw$languages$version), "",
                                           paste0(" ", sw$languages$version)),
                                    sw$languages$n, plural(sw$languages$n)),
    unlist(lapply(unique(sw$packages$language), function(l) {
      p <- sw$packages[sw$packages$language == l, , drop = FALSE]
      paste0("- ", l, " packages: ", paste0(
        p$package, ifelse(is.na(p$version), "", paste0(" (", p$version, ")")),
        collapse = ", "))
    })),
    if (any(sw$packages$language == "Python"))
      "- Install the Python packages with `pip install -r requirements.txt`.")

  prov <- info$provenance
  pos <- vapply(prov, function(e) as.numeric(e$position %||% Inf), numeric(1))
  ids <- function(x) paste0("`", vapply(x %||% list(), `[[`, character(1), "@id"), "`", collapse = ", ")
  script_lines <- vapply(prov[order(pos)], function(e) paste0(
    "1. `", e$instrument[["@id"]], "`",
    if (length(e$object)) paste0(" reads ", ids(e$object)),
    if (length(e$object) && length(e$result)) ";",
    if (length(e$result)) paste0(" writes ", ids(e$result))), character(1))

  data_rows <- which(!is.na(plan$written) &
                       grepl("^(study-[^/]+/)?data/.+\\.csv$", plan$written) &
                       plan$data_type %in% "data")
  var_lines <- .FAIR_repo_variable_tables(plan, columns_df, labels_df, data_rows)
  ni <- info$not_included
  fu <- info$funding

  c(paste0("# ", info$title), "",
    if (nzchar(info$abstract)) c(info$abstract, ""),
    paste0("This folder is a copy of the research files, reorganised by ",
           "[metacheck](https://scienceverse.github.io/metacheck/) on ",
           format(Sys.Date(), "%Y-%m-%d"), ". `ro-crate-metadata.json` holds ",
           "the same information in machine-readable form, and `CHANGES.md` ",
           "lists what metacheck changed."),
    "", "## Authors", "", au_lines, "", paste0("Contact: ", contact),
    if (!is.null(info$doi_url)) c("", "## Paper", "", info$doi_url),
    "", "## Licence", "",
    if (is.na(info$licence)) "unknown" else paste0("See `", info$licence, "`."),
    if (!is.null(fu)) c("", "## Funding", "", fu$statement %||% "",
                        if (length(fu$funders)) paste0("Funders: ", paste(fu$funders, collapse = "; ")),
                        if (length(fu$awards)) paste0("Grants: ", paste(fu$awards, collapse = "; "))),
    "", "## Files", "",
    paste0("The files follow the [Psych-DS](https://psych-ds.github.io/) layout: ",
           "data in `data/`, named `study-<name>_data.csv`, analysis code in ",
           "`analysis/`, materials in `materials/`, documentation in ",
           "`documentation/`",
           if (multi) ", with one `study-<group>/` folder per study" else "",
           ". Each data folder's `dataset_description.json` describes its variables."),
    "", .FAIR_repo_tree(files),
    "", "## File formats", "", fmt_lines,
    if (length(sw_lines)) c("", "## Software", "", sw_lines),
    if (length(script_lines)) c("", "## Scripts", "",
                                "In the order they can be run:", "", script_lines),
    if (length(var_lines)) c("", "## Variables", var_lines)
    else if (length(data_rows)) c("", "## Variables", "",
      paste0("No codebook describes the variables. Their names and values are ",
             "in `dataset_description.json`.")),
    if (nrow(ni)) c("", "## Not included", "",
                    sprintf("- `%s` (%s): %s", ni$path, ni$type, ni$reason)),
    "", "## Still to be filled in", "",
    "metacheck could not find the following:",
    "", .FAIR_repo_still_unknown(info))
}

# The README of a study folder: its files and variables.
.FAIR_repo_readme_study <- function(info, root, plan, files, columns_df, labels_df) {
  rows <- which(!is.na(plan$written) & startsWith(plan$written, paste0(root$dir, "/data/")) &
                  grepl("\\.csv$", plan$written) & plan$data_type %in% "data")
  c(paste0("# ", info$title, " — ", .FAIR_repo_label(root$grp)), "",
    paste0("One study of the research files, reorganised by ",
           "[metacheck](https://scienceverse.github.io/metacheck/). The ",
           "authors, licence, software and scripts are described in ",
           "`ro-crate-metadata.json` in the top folder."),
    "", "## Files", "", .FAIR_repo_tree(files),
    if (length(rows)) c("", "## Variables",
                        .FAIR_repo_variable_tables(plan, columns_df, labels_df, rows,
                                                   strip = paste0("^", root$dir, "/"))))
}
