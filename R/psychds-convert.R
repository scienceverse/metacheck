# Generate a Psych-DS-compliant copy of a repository. This is the file-writing
# counterpart to the `psychds_check` module: the check reports the compliance
# gap, this function fixes it on disk. Ported from datacheck's
# 3_psychds_convert.R, but driven by metacheck's data_check / codebook_check
# outputs and the paper object (no GROBID XML / CrossRef enrichment).

# Representation that receives a numeric statistics block in variableMeasured.
.psychds_numeric_reps <- c("numeric")
# Measurement levels that receive a valuePattern (their distinct values matter).
.psychds_categorical_levels <- c("nominal", "ordinal")
# Quality states that hold no measured content. These columns still get a
# variableMeasured entry (Psych-DS requires every CSV column to be described, so
# omitting them makes the dataset fail validation with CsvColumnMissingFromMetadata),
# but the entry is a minimal stub marked empty rather than a full facet block —
# an all-NA column has no statistics, value pattern, or measurement level to report.
.psychds_empty_quality <- c("empty")

# Build the variableMeasured list for one set of columns. `cols` is a subset of
# data_check's columns table; `labels` is codebook_check's labels table (or
# NULL). Returns a list of PropertyValue objects.
.psychds_variable_measured <- function(cols, labels = NULL) {
  if (is.null(cols) || nrow(cols) == 0) return(list())

  # Scale dictionary for the OSD code cross-reference (loaded once).
  .osd_dict <- tryCatch(get("scales", envir = asNamespace("metacheck")),
                        error = function(e) NULL)
  if (is.null(.osd_dict))
    .osd_dict <- data.frame(name = character(), acronym = character(),
                            code = character(), source = character())

  # Attach labels by source_file + column_name when available.
  if (!is.null(labels) && nrow(labels) > 0 &&
      all(c("source_file", "column_name") %in% names(labels))) {
    keep <- c("source_file", "column_name", "label", "label_status",
              "label_source", "label_method", "codebook_variable",
              "scale", "scale_confidence", "scale_source",
              "value_labels", "missing_values", "question")
    labels <- labels[, intersect(keep, names(labels)), drop = FALSE]
    cols <- merge(cols, labels, by = c("source_file", "column_name"),
                  all.x = TRUE, suffixes = c("", ".lbl"))
  }

  # Every column is described (Psych-DS requires each CSV column to appear in
  # variableMeasured). An empty (all-NA) column gets a minimal stub entry — it
  # has no content to describe beyond "this column exists and holds no data" —
  # while the rest get the full facet schema
  # (representation/quality/measurement_level/concept/unit).
  qual <- tolower(cols$quality %||% "")
  if (nrow(cols) == 0) return(list())

  lapply(seq_len(nrow(cols)), function(i) {
    row <- cols[i, ]

    # Empty column: a minimal PropertyValue naming it and flagging it empty, so
    # the CSV column is documented without inventing statistics for all-NA data.
    if (qual[i] %in% .psychds_empty_quality) {
      return(Filter(Negate(is.null), list(
        `@type` = "PropertyValue",
        name    = row$column_name,
        description = "Empty column (no observed values in the shared data).",
        `metacheck:quality` = "empty",
        `metacheck:source_file` = if (!is.na(row$source_file)) row$source_file else NULL
      )))
    }

    rep_ <- row$representation %||% NA_character_
    lvl  <- row$measurement_level %||% NA_character_
    pv  <- list(`@type` = "PropertyValue", name = row$column_name)

    # Variable description from the codebook, when documented.
    if ("label_status" %in% names(row) && !is.na(row$label_status) &&
        row$label_status %in% c("labelled", "llm") &&
        !is.na(row$label) && nzchar(row$label)) {
      pv[["description"]]                 <- row$label
      pv[["metacheck:labelSource"]]      <- row$label_source
      pv[["metacheck:codebookVariable"]] <- row$codebook_variable
    }

    # Psychometric scale membership (from codebook_check's LLM scale
    # identification). The scale NAME goes in schema.org's native
    # `measurementTechnique` (its intended use — the instrument a variable was
    # measured with); the grouping/confidence go in a namespaced extension, as
    # schema.org has no compositional grouping property. This mirrors DDI's
    # `analysis` variable-group concept ("variables combined into the same
    # index") within Psych-DS's JSON-LD.
    if ("scale" %in% names(row) && !is.na(row$scale) && nzchar(row$scale)) {
      pv[["measurementTechnique"]] <- row$scale
      # Cross-reference to the exported OSD scale definition: the same code the
      # .osd file is written under (scales/{code}.osd), so a reader can jump from
      # a variable to its instrument definition. scale_source also marks how the
      # scale was identified (dictionary/manuscript/generated).
      ssrc <- if ("scale_source" %in% names(row) && !is.na(row$scale_source))
        row$scale_source else NA_character_
      cp <- .osd_code_and_provenance(row$scale, row$column_name, ssrc, .osd_dict)
      pv[["metacheck:scale"]] <- Filter(Negate(is.null), list(
        name       = row$scale,
        code       = cp$code,
        source     = cp$source,
        confidence = if ("scale_confidence" %in% names(row) &&
                         !is.na(row$scale_confidence)) row$scale_confidence else NULL
      ))
    }

    # Measurement level → schema.org has no native property, so record it in a
    # namespaced extension (DDI @classificationLevel). Unit → schema.org
    # `unitText`. Concept → namespaced extension (DDI Variable→Concept).
    if (!is.na(lvl)) pv[["metacheck:measurementLevel"]] <- lvl
    concept <- if ("concept" %in% names(row)) row$concept %||% NA_character_ else NA_character_
    if (!is.na(concept) && nzchar(concept)) pv[["metacheck:concept"]] <- concept
    unit <- if ("unit" %in% names(row)) row$unit %||% NA_character_ else NA_character_
    if (!is.na(unit) && nzchar(unit)) pv[["unitText"]] <- unit
    role <- if ("role" %in% names(row)) row$role %||% NA_character_ else NA_character_
    if (!is.na(role) && nzchar(role)) pv[["metacheck:role"]] <- role

    # Question text (DDI QuestionText).
    question <- if ("question" %in% names(row)) row$question %||% NA_character_ else NA_character_
    if (!is.na(question) && nzchar(question)) pv[["metacheck:question"]] <- question

    # Value labels / code list (DDI CodeList): emit as schema.org PropertyValue
    # children so each code->label pair is machine-readable. The raw JSON is also
    # kept in a namespaced field for round-tripping.
    vl_json <- if ("value_labels" %in% names(row)) row$value_labels %||% NA_character_ else NA_character_
    vl <- .decode_value_labels(vl_json)
    if (!is.null(vl) && length(vl) > 0) {
      pv[["metacheck:valueLabels"]] <- vl_json
      pv[["metacheck:codeList"]] <- unname(lapply(seq_along(vl), function(k)
        list(`@type` = "PropertyValue", value = names(vl)[k], name = unname(vl)[k])))
    }

    # Missing-value scheme (DDI MissingValues): which codes denote missingness,
    # distinguishing sentinels from real values.
    mv_json <- if ("missing_values" %in% names(row)) row$missing_values %||% NA_character_ else NA_character_
    if (!is.na(mv_json) && nzchar(mv_json))
      pv[["metacheck:missingValues"]] <- mv_json

    # Numeric statistics block (numeric representation).
    if (identical(rep_, "numeric")) {
      if ("min" %in% names(row) && !is.na(row$min)) pv[["minValue"]] <- row$min
      if ("max" %in% names(row) && !is.na(row$max)) pv[["maxValue"]] <- row$max
      stat_fields <- c("n", "n_missing", "mean", "sd", "se", "median",
                       "p25", "p75", "iqr", "skewness", "kurtosis")
      stat_block <- lapply(stat_fields, function(f)
        if (f %in% names(row) && !is.na(row[[f]])) row[[f]] else NULL)
      names(stat_block) <- stat_fields
      stat_block <- Filter(Negate(is.null), stat_block)
      if (length(stat_block) > 0) pv[["metacheck:statistics"]] <- stat_block
    }

    # Value pattern for categorical (nominal/ordinal, non-numeric) columns.
    if (!is.na(lvl) && lvl %in% .psychds_categorical_levels &&
        !identical(rep_, "numeric") &&
        "sample_values" %in% names(row) && !is.na(row$sample_values) &&
        nzchar(row$sample_values)) {
      vals <- unique(trimws(strsplit(row$sample_values, "\\|")[[1]]))
      vals <- vals[nzchar(vals)]
      if (length(vals) > 0) pv[["valuePattern"]] <- paste(vals, collapse = "|")
    }

    if (!is.na(rep_)) pv[["metacheck:representation"]] <- rep_
    if (!is.na(row$source_file)) pv[["metacheck:sourceFile"]] <- row$source_file
    Filter(Negate(is.null), pv)
  })
}

# Normalise a keywords value into a flat list of scalar strings for JSON. On a
# scivrs_paper, paper$info$keywords is an AsIs list-column of length 1 whose one
# element is the character vector of keywords — so a naive as.list() keeps the
# outer wrapper and serialises as a nested array [["a","b"]] instead of the
# schema.org-correct ["a","b"]. Unwrap a single list/AsIs cell to its contents,
# then split into scalars. Returns NULL when there are no keywords.
.psychds_keywords <- function(kw) {
  if (is.null(kw) || length(kw) == 0) return(NULL)
  # Unwrap an AsIs / list cell (length-1 holding the real vector).
  if (is.list(kw) && length(kw) == 1) kw <- kw[[1]]
  kw <- unlist(kw, use.names = FALSE)
  kw <- as.character(kw)
  kw <- kw[!is.na(kw) & nzchar(kw)]
  if (length(kw) == 0) return(NULL)
  as.list(kw)
}

# Resolve the online download URL for a source file from data_check's structure
# table. Prefer the direct file URI (file_url, e.g. an OSF download link); fall
# back to the repository URL (repo_url) when there is no direct link (e.g.
# ResearchBox, which exposes no per-file download URL). Returns NA when neither
# is known. `structure_df` is data_check's per-file structure table.
.psychds_source_url <- function(file_name, structure_df) {
  if (is.null(structure_df) || !nrow(structure_df) ||
      !"file_name" %in% names(structure_df)) return(NA_character_)
  # Basename match. file_name is NOT unique across a paper's repositories (see
  # the copy loop's warning), so a duplicated basename resolves to the FIRST
  # matching row — acceptable for a "jump to the source online" link.
  i <- match(file_name, structure_df$file_name)
  if (is.na(i)) return(NA_character_)
  fu <- if ("file_url" %in% names(structure_df)) structure_df$file_url[i] else NA_character_
  if (!is.na(fu) && nzchar(fu)) return(fu)
  ru <- if ("repo_url" %in% names(structure_df)) structure_df$repo_url[i] else NA_character_
  if (!is.na(ru) && nzchar(ru)) return(ru)
  NA_character_
}

# MIME type for a source file's extension, for a DataDownload's encodingFormat.
# Returns NULL for an unknown extension, so the field is simply omitted.
.psychds_encoding_format <- function(file_name) {
  ext <- tolower(tools::file_ext(file_name))
  mimes <- c(csv = "text/csv", tsv = "text/tab-separated-values",
             txt = "text/plain", json = "application/json",
             xlsx = "application/vnd.openxmlformats-officedocument.spreadsheetml.sheet",
             xls = "application/vnd.ms-excel",
             ods = "application/vnd.oasis.opendocument.spreadsheet",
             fods = "application/vnd.oasis.opendocument.spreadsheet",
             sav = "application/x-spss-sav", dta = "application/x-stata-dta",
             rds = "application/x-r-rds", rdata = "application/x-r-data",
             sas7bdat = "application/x-sas-data", por = "application/x-spss-por",
             jasp = "application/x-jasp")
  if (ext %in% names(mimes)) unname(mimes[[ext]]) else NULL
}

# Build the schema.org `distribution` array (a list of DataDownload objects) for
# a study root: one entry per source data file that has a resolvable online URL.
# `data_files` are the source file names (basenames) landing in this root's
# data/; `structure_df` supplies their URLs. Returns NULL when none resolve, so
# `distribution` is dropped rather than emitted empty.
.psychds_distribution <- function(data_files, structure_df) {
  data_files <- unique(data_files[!is.na(data_files) & nzchar(data_files)])
  if (!length(data_files)) return(NULL)
  entries <- lapply(data_files, function(fn) {
    url <- .psychds_source_url(fn, structure_df)
    if (is.na(url)) return(NULL)
    Filter(Negate(is.null), list(
      `@type`        = "DataDownload",
      name           = fn,
      contentUrl     = url,
      encodingFormat = .psychds_encoding_format(fn)))
  })
  entries <- Filter(Negate(is.null), entries)
  if (!length(entries)) NULL else entries
}

# Build the dataset_description.json object for one study. `distribution` is the
# optional schema.org DataDownload list (built by .psychds_distribution) linking
# each source data file to its online download URL; NULL omits the field.
.psychds_dataset_description <- function(paper, study_label, property_values,
                                         distribution = NULL) {
  info <- paper$info %||% list()
  # paper$info may be a tibble; `$` on a missing tibble column warns, so look up
  # fields by name and return NULL when absent (or empty, e.g. a stub paper).
  ival <- function(field) {
    v <- if (field %in% names(info)) info[[field]] else NULL
    if (length(v) == 0) NULL else v
  }

  title <- ival("title")
  name <- if (!is.null(title) && nzchar(title))
    paste0(title, if (nzchar(study_label)) paste0(" — ", study_label) else "")
  else paste0("Dataset", if (nzchar(study_label)) paste0(" — ", study_label) else "")

  desc <- list(
    `@context`       = "https://schema.org/",
    `@type`          = "Dataset",
    name             = name,
    description      = paste0("Psych-DS dataset generated by metacheck",
                             if (nzchar(study_label)) paste0(" for ", study_label) else "",
                             "."),
    variableMeasured = property_values,
    schemaVersion    = "Psych-DS 1.5.1"
  )

  # Where the source data files can be downloaded online (schema.org). One
  # DataDownload per source file with a resolvable URL; omitted when none.
  if (!is.null(distribution) && length(distribution))
    desc[["distribution"]] <- distribution

  # Authors from the paper object.
  if (!is.null(paper$author) && nrow(paper$author) > 0) {
    nm <- trimws(paste(paper$author$given %||% "", paper$author$family %||% ""))
    nm <- nm[nzchar(nm)]
    if (length(nm) > 0)
      desc[["author"]] <- lapply(nm, function(x) list(`@type` = "Person", name = x))
  }
  doi <- ival("doi") %||% NA_character_
  if (!is.na(doi) && nzchar(doi))
    desc[["identifier"]] <- paste0("https://doi.org/", sub("^https?://doi.org/", "", doi))
  kw <- .psychds_keywords(ival("keywords"))
  if (!is.null(kw)) desc[["keywords"]] <- kw

  desc[["metacheck:generated"]] <- format(Sys.Date(), "%Y-%m-%d")
  Filter(Negate(is.null), desc)
}

# The paper's abstract, as a single collapsed string, for use as the collection
# `description`. The abstract is NOT on paper$info (stripped upstream); it lives
# in the text table as the section(s) typed "abstract". Returns "" when the paper
# carries no abstract section (e.g. a stub paper). `max_chars` keeps the metadata
# file compact; the full prose is in documentation/fulltext.txt regardless.
.psychds_abstract_text <- function(paper, max_chars = 1500L) {
  if (!.is_paper(paper)) return("")
  tx <- tryCatch(paper_table(paper, "text"), error = function(e) NULL)
  se <- tryCatch(paper_table(paper, "section"), error = function(e) NULL)
  if (is.null(tx) || !is.data.frame(tx) || !nrow(tx) || !"text" %in% names(tx))
    return("")
  if (is.null(se) || !is.data.frame(se) ||
      !all(c("section_id", "section_type") %in% names(se)) ||
      !"section_id" %in% names(tx))
    return("")
  abs_ids <- se$section_id[se$section_type %in% "abstract"]
  if (!length(abs_ids)) return("")
  at <- tx[tx$section_id %in% abs_ids, , drop = FALSE]
  if (!nrow(at)) return("")
  if ("text_id" %in% names(at)) at <- at[order(at$text_id), , drop = FALSE]
  txt <- trimws(paste(at$text[nzchar(trimws(at$text))], collapse = " "))
  if (!nzchar(txt)) return("")
  if (nchar(txt) > max_chars) txt <- paste0(substr(txt, 1L, max_chars), "…")
  txt
}

# Read the open_practices summary flags (data/code/materials/prereg openness)
# from a resolved module-output list, as a named logical vector. Returns an empty
# vector when open_practices did not run. Used to surface the check outcomes as
# `metacheck:*` collection properties so the corpus is filterable without opening
# each paper's logs.
.psychds_open_flags <- function(ops) {
  op <- ops[["open_practices"]]
  st <- op$summary_table
  if (is.null(st) || !is.data.frame(st) || !nrow(st)) return(logical(0))
  get1 <- function(f) {
    if (!f %in% names(st)) return(NA)
    v <- st[[f]][1]
    if (is.logical(v)) v else NA
  }
  c(data = get1("data_open"), code = get1("code_open"),
    materials = get1("materials_open"), prereg = get1("prereg_open"))
}

# The literal placeholder written into any RO-Crate/DDI field metacheck cannot
# currently extract, so a researcher can grep the file for it and knows exactly
# what to fill in by hand.
.psychds_unknown <- "unknown"

# Wrap a scalar string as a one-element JSON array (the shape every DDI-derived
# field in the RO-Crate uses, e.g. purpose / populationOfConcern), without
# inventing sentence/list boundaries in free text that isn't actually
# structured as a list. NULL/NA/"" collapses to the single-element "unknown"
# array, so the field is always present and always an array.
.psychds_ddi_array <- function(x) {
  x <- if (length(x) == 0) NA_character_ else x[1]
  if (is.na(x) || !nzchar(trimws(x))) x <- .psychds_unknown
  list(x)
}

# Look up one field of the paper's preregistration (prereg_check's `table`,
# long-format: ONE ROW PER LINKED REGISTRATION, columns matching prereg_schema).
# Returns NA when prereg_check did not run (ops has no "prereg_check" entry —
# never forced to run by the converter, only reused opportunistically when the
# caller's chain already included it), or the paper has no linked
# preregistration, or the field is empty.
#
# Returns NA when a paper links MORE THAN ONE registration. Papers routinely
# register each study separately, and those registrations genuinely differ:
# 09567976221098594 has three, of which the one titled "... (Study 3)" declares
# a sample_size of 1500 while the other two declare 2104. An earlier version
# took vals[1], which stamped one arbitrary registration's numbers onto every
# study — a confidently-wrong number, worse than an absent one.
#
# Matching a registration to the study it covers is a real inference problem,
# not a title regex: across the corpus most multi-registration papers have
# IDENTICAL titles for every registration, the study cues that do appear vary
# ("... (Study 3)", "... (Study 2).", "... (schemaVR4)", a bare "Study 3"), and
# the counts often do not line up with the data's study groups at all (one
# paper has 3 registrations against a single group; another 3 against four).
# Until that matching exists (reg_check does some of this), pre-filling is
# limited to the unambiguous single-registration case and everything else stays
# "unknown".
.psychds_prereg_field <- function(ops, pid, field) {
  pr <- ops[["prereg_check"]]$table
  if (is.null(pr) || !is.data.frame(pr) || !nrow(pr) ||
      !all(c("paper_id", field) %in% names(pr))) return(NA_character_)
  rows <- pr[!is.na(pr$paper_id) & pr$paper_id == pid, , drop = FALSE]
  if (nrow(rows) != 1L) return(NA_character_)
  v <- rows[[field]][1]
  if (is.na(v) || !nzchar(trimws(v %||% ""))) return(NA_character_)
  v
}

# Build the study-level contextual entity #study-design: how the study was run.
#
# ONE entity, deliberately. An earlier version split this across #study-design,
# #population and #sampling-plan, which produced three problems: (a)
# populationOfConcern and samplingMethod each appeared TWICE with different
# meanings — a {"@id": ...} pointer on #study-design and prose content on the
# target — so one property name did two incompatible jobs; (b) #population
# carried both populationOfConcern and description, which restated the same
# fact twice; (c) the split was speculative — the two sub-entities were each
# referenced exactly once, by #study-design, and nothing else ever pointed at
# them. Splitting is worth it when a definition is genuinely shared (DDI's
# Universe is reusable across a study series); it is not worth it to hold nine
# fields read in one sitting. Fields are grouped by theme instead: what kind of
# study, who it was about, how they were reached, how many were obtained.
#
# EVERY field below is a real element of DDI-Lifecycle 3.3 or schema.org —
# there are no metacheck-invented terms in this file. The mapping, and the
# checking behind it:
#   dataCollectionMethodology -> ddi:DataCollectionMethodology (Methodology).
#       DDI has no dedicated "study design" element; this is its nearest
#       ("primary or secondary, qualitative or quantitative, mixed method").
#   purpose             -> ddi:Purpose (StudyUnit), "why the study took place".
#   populationOfConcern -> ddi:PopulationOfConcern (Sampling).
#   isInclusive         -> ddi:IsInclusive (Universe): whether the population
#       description is phrased as who is INCLUDED or who is EXCLUDED. DDI
#       deliberately uses one description plus this flag rather than separate
#       inclusion/exclusion fields, which is also how prereg templates ask.
#   samplingMethod      -> ddi:SamplingProcedure (Methodology).
#   samplingFrame       -> ddi:SampleFrame (Sampling).
#   targetSampleSize    -> ddi:OverallTargetSampleSize (Sampling) — PLANNED n.
#   sampleSize          -> ddi:SampleSize        (ResponseRate) — approached.
#   numberOfResponses   -> ddi:NumberOfResponses (ResponseRate) — completed.
#   specificResponseRate-> ddi:SpecificResponseRate (ResponseRate).
# The last four are four distinct numbers, from two different DDI modules: a
# sampling plan's target, and the ResponseRate triple. They coincide in a
# simple experiment where everyone randomised completes, which is informative
# (nobody was lost) rather than redundant; they diverge for any survey or any
# study with attrition, which is where the distinction earns its place.
# An earlier version declared several of these as custom `ssrc:` terms; that
# was wrong — DDI defines them (ResponseRate even names NumberOfResponses
# identically), and they are now mapped to the real elements.
#
# Sourced from a linked preregistration when prereg_check already ran (see
# .psychds_prereg_field()), else "unknown", so a researcher filling the file in
# by hand knows exactly what is missing.
# `purpose` and `funder` are deliberately NOT here — they live on the root
# Dataset instead. Neither is a fact about how the study was RUN (which is what
# this entity holds): purpose is what the whole package is for, and funder is
# bibliographic attribution belonging beside author and identifier. Both
# standards agree: funder is a native schema.org CreativeWork property, and
# ddi:Purpose sits on StudyUnit, DDI's top-level study record — our root.
#
# ONE ENTITY PER STUDY PART when a paper has several (`sr` is a "study-<group>"
# prefix; NULL gives a single unqualified #study-design for a single-study
# paper). Different experiments in one paper routinely have different samples,
# recruitment and N — study 1 sixty undergraduates in a lab, study 3 four
# hundred from Prolific — and a single shared block cannot express that, which
# left a researcher having to pick one study's numbers or cram all of them into
# one string. This matches how the #lifecycle-* and CRediT #role-* entities
# are already scoped.
#
# PREREG PRE-FILL: purpose / study_design_overview / sample_size /
# data_exclusion_criteria are pre-filled from a linked preregistration ONLY
# when the paper links exactly one — see .psychds_prereg_field(). Papers do
# register per study, and those registrations differ, but nothing currently
# maps a registration to the study it covers, so with 2+ registrations every
# field stays "unknown" rather than carrying one arbitrary study's numbers.
# When that matching is built, this is where it plugs in.
.psychds_study_design_entity <- function(ops, pid, sr = NULL) {
  get_f <- function(field) .psychds_prereg_field(ops, pid, field)
  grp   <- if (is.null(sr)) NULL else sub("^study-", "", sr)
  label <- if (is.null(grp)) NULL else paste("Study", toupper(grp))
  list(
    `@id`   = if (is.null(grp)) "#study-design" else paste0("#study-design-", grp),
    `@type` = "ResearchProject",
    name    = if (is.null(label)) "Study design" else paste0("Study design (", label, ")"),
    # What this study was for, and what kind of study it was. `purpose` sits
    # here rather than on the root because each experiment in a multi-study
    # paper usually tests its own question.
    purpose                   = .psychds_ddi_array(get_f("research_questions")),
    dataCollectionMethodology = .psychds_ddi_array(get_f("study_design_overview")),
    # Who it was about. One description plus a flag, per DDI's Universe.
    populationOfConcern = .psychds_ddi_array(get_f("data_exclusion_criteria")),
    isInclusive         = .psychds_unknown,
    # How they were reached.
    samplingMethod = .psychds_ddi_array(.psychds_unknown),
    samplingFrame  = .psychds_ddi_array(.psychds_unknown),
    # How many: planned, approached, completed, and the resulting rate.
    targetSampleSize     = .psychds_ddi_array(get_f("sample_size")),
    sampleSize           = .psychds_ddi_array(.psychds_unknown),
    numberOfResponses    = .psychds_ddi_array(.psychds_unknown),
    specificResponseRate = .psychds_ddi_array(.psychds_unknown))
}

# One collection event per study: WHEN, under WHAT CIRCUMSTANCES and HOW the
# data was obtained, and who outside the author list collected it.
#
# This is DDI's `CollectionEvent` — "a specific event in the collection or
# capture process" — and every field below is one of its documented children,
# so the entity is named for the standard it reproduces rather than invented:
#   obtainedDate        -> ddi:DataCollectionDate, "a date or range of dates
#                          for the described data collection event".
#   collectionSituation -> ddi:CollectionSituation, "the situation in which the
#                          data collection event takes place". Broader than a
#                          geographic point, and deliberately so: whether a
#                          session was supervised, individual or group, or ran
#                          during an exam period affects the data in ways a
#                          building name does not. An earlier version used
#                          schema.org locationCreated, which only carries the
#                          physical place.
#   modeOfCollection    -> ddi:ModeOfCollection, the platform or mode
#                          ("Qualtrics", "jsPsych on lab machines"). Kept
#                          separate from the situation because a platform is
#                          not a circumstance; one field carrying both was
#                          ambiguous.
#   contributor         -> schema.org, "a secondary contributor to the
#                          CreativeWork" — the non-author RA/staff concept,
#                          matching ddi:DataCollectorOrganizationReference
#                          ("organization or individual responsible for the
#                          data collection"). Takes Person/Organization
#                          entities so collectors become identified, creditable
#                          people rather than a free-text sentence; emitted as
#                          an empty array for the team to populate.
#
# This is the FIRST of the study's lifecycle events (see
# .psychds_lifecycle_events() below), carrying the collection-specific fields
# the later stages do not need. It is not called "provenance": in RO-Crate and
# W3C PROV that word denotes the derivation chain (object -> activity ->
# result), which the script-derived events record.
#
# An earlier version carried AsCollected's `dataId`, `additionalInfo` and
# `cleanedWithCode` — dropped: the first two are that web form's own
# bookkeeping (an internal tracking id, a catch-all free-text box) with no
# meaning outside it, and the third (a yes/no "was cleaning done with code") is
# answered far more precisely by the script events, which name the exact file.
#
# metacheck has NO extraction path for any of these — when, under what
# circumstances and how data was collected is not recoverable from a repository
# scan or manuscript parse. They are pre-labelled, pre-linked slots.
.psychds_collection_event_entity <- function(sr) {
  grp <- sub("^study-", "", sr)
  list(
    `@id`   = paste0("#lifecycle-", grp, "-collection"),
    `@type` = "CreateAction",
    name    = paste("Raw data obtained: Study", toupper(grp)),
    eventType           = "raw data obtained",
    startTime           = .psychds_unknown,
    agent               = .psychds_unknown,
    obtainedDate        = .psychds_ddi_array(.psychds_unknown),
    collectionSituation = .psychds_ddi_array(.psychds_unknown),
    modeOfCollection    = .psychds_ddi_array(.psychds_unknown),
    contributor         = list())
}

# The remaining fixed lifecycle stages for one study. Together with the
# collection event above and the script-derived events
# (.psychds_provenance_entities()), these are the study's LifecycleEvent list.
#
# Shape follows DDI's `LifecycleEvent` — "a listing of events in the life cycle
# of a data set, with identification, date, agency and descriptive information
# for each" — whose three components are what took place, when, and who was
# involved. Here: `eventType` (what), `startTime` (when), `agent` (who).
# schema.org's CreateAction already defines agent/startTime, so no custom terms
# are needed and the script-derived events use the SAME shape, just with
# instrument/object/result additionally auto-filled.
#
# The stage LIST is AsCollected's, not DDI's. DDI leaves the event list
# open-ended — you document whatever was significant — which records nothing in
# advance and so prompts a team for nothing; the stages that go undocumented
# are exactly the manual ones. AsCollected instead fixes the stages (raw data,
# cleaning, analysis) and asks who did each. Pre-generating those stubs means a
# team is asked about every stage and can delete what does not apply, rather
# than being asked about none. Collection is emitted separately above because
# it carries extra fields; cleaning and analysis need only what/when/who.
.psychds_lifecycle_stages <- c(
  cleaning = "data cleaning",
  analysis = "data analysis")

.psychds_lifecycle_events <- function(sr) {
  grp <- sub("^study-", "", sr)
  lapply(names(.psychds_lifecycle_stages), function(k) list(
    `@id`     = paste0("#lifecycle-", grp, "-", k),
    `@type`   = "CreateAction",
    name      = paste0(
      toupper(substring(.psychds_lifecycle_stages[[k]], 1, 1)),
      substring(.psychds_lifecycle_stages[[k]], 2),
      ": Study ", toupper(grp)),
    eventType = unname(.psychds_lifecycle_stages[[k]]),
    startTime = .psychds_unknown,
    agent     = .psychds_unknown))
}

# CRediT (ANSI/NISO Z39.104-2022) contributor roles, by their persistent NISO
# URIs. Replaces an earlier invented 9-value vocabulary ("obtained raw data",
# "has copy of raw data", ...) modelled on AsCollected's checkbox grid: CRediT
# is the actual standard for "who did what" on a research output, is already
# what journals collect, and gives each role a resolvable identifier instead of
# a metacheck-local string. The three AsCollected checkboxes with no CRediT
# equivalent ("has a copy of the raw/final data") were dropped rather than
# preserved as custom terms — they are that platform's fraud-deterrence
# bookkeeping, not a property of the dataset.
#
# All 14 roles are emitted. `agent` is always "unknown": metacheck cannot
# attribute contributions among co-authors, so each role is one stub the team
# fills in with the applicable author's @id (or deletes if nobody held it).
#
# Roles are emitted PER STUDY PART when a paper has several, with the study
# named in the human-readable `name` ("Validation (Study EX2)") and carried
# machine-readably by `about`. schema.org's Role type exists precisely to
# "attach additional information to the Role" (its own words) — so `about`
# already IS the scoping mechanism, and an earlier custom `roleScope` property
# duplicating it was removed as redundant. CRediT itself defines no scoping
# qualifier, so the canonical role URI stays untouched in `roleName` and the
# scope lives entirely in schema.org's own machinery. A single-study paper gets
# one unqualified set pointing at the root Dataset: plain conventional CRediT.
.psychds_credit_roles <- c(
  "conceptualization", "data-curation", "formal-analysis", "funding-acquisition",
  "investigation", "methodology", "project-administration", "resources",
  "software", "supervision", "validation", "visualization",
  "writing-original-draft", "writing-review-editing")

# Title-case a CRediT slug for the human-readable label: "formal-analysis" ->
# "Formal analysis", matching CRediT's own capitalisation of its role names.
.psychds_credit_label <- function(slug) {
  words <- strsplit(gsub("-", " ", slug), " ")[[1]]
  paste0(toupper(substring(words[1], 1, 1)), substring(words[1], 2),
         if (length(words) > 1) paste0(" ", paste(words[-1], collapse = " ")) else "")
}

# `sr` is a "study-<group>" prefix, or NULL for a paper-level (unscoped) set.
.psychds_credit_role_entities <- function(sr = NULL) {
  grp   <- if (is.null(sr)) NULL else sub("^study-", "", sr)
  label <- if (is.null(grp)) NULL else paste("Study", toupper(grp))
  lapply(.psychds_credit_roles, function(r) Filter(Negate(is.null), list(
    `@id`    = if (is.null(grp)) paste0("#role-", r) else paste0("#role-", grp, "-", r),
    `@type`  = "Role",
    name     = if (is.null(label)) .psychds_credit_label(r)
               else paste0(.psychds_credit_label(r), " (", label, ")"),
    roleName = list(`@id` = paste0("https://credit.niso.org/contributor-roles/", r, "/")),
    about    = if (is.null(grp)) list(`@id` = "./") else list(`@id` = paste0(sr, "/")),
    agent    = .psychds_unknown)))
}

# Decode a missing_values JSON string (see .encode_missing_values() in
# R/data_check_helpers.R) into a named character vector (names = sentinel
# codes, values = reason labels, possibly NA when no reason was declared). The
# stored JSON is EITHER a bare array of codes (no reason known) OR a
# code->reason object — .decode_value_labels() only handles the object shape,
# so an array is normalised here to code-named entries with an NA reason.
.psychds_decode_missing <- function(s) {
  if (is.null(s) || length(s) != 1 || is.na(s) || !nzchar(s)) return(NULL)
  out <- tryCatch(jsonlite::fromJSON(s), error = function(e) NULL)
  if (is.null(out) || length(out) == 0) return(NULL)
  v <- unlist(out)
  if (is.null(names(v)) || !any(nzchar(names(v))))
    v <- stats::setNames(rep(NA_character_, length(v)), as.character(v))
  v[!is.na(names(v)) & nzchar(names(v))]
}

# Build the paper-wide missing-value DefinedTermSet: one entity pooling every
# DISTINCT (code, reason) pair found anywhere across the whole paper's
# variables, deduplicated. Deliberately a SINGLE shared entity, not one per
# variable or per study — real papers were checked and essentially never use
# more than one or two genuinely distinct conventions (see the corpus scan
# behind this design), and where a paper's codebook does show more than one,
# that reads as inconsistent practice worth pooling into one canonical scheme
# rather than modelling as deliberate multi-scheme design. `labels_df` is
# codebook_check's full (unfiltered by study) labels table. Returns NULL when
# no variable anywhere in the paper declares a missing-value code, so the
# entity — and any reference to it — is omitted entirely rather than emitted
# empty.
.psychds_missing_scheme_entity <- function(labels_df) {
  if (is.null(labels_df) || !nrow(labels_df) ||
      !"missing_values" %in% names(labels_df)) return(NULL)
  mv_strings <- labels_df$missing_values
  mv_strings <- mv_strings[!is.na(mv_strings) & nzchar(mv_strings)]
  if (!length(mv_strings)) return(NULL)

  pooled <- character(0)   # named by code, value = reason (possibly NA)
  for (s in mv_strings) {
    v <- .psychds_decode_missing(s)
    if (is.null(v)) next
    pooled <- c(pooled, v)
  }
  if (!length(pooled)) return(NULL)

  # Deduplicate by (code, reason) pair — the same code with two DIFFERENT
  # reasons across the paper both survive as distinct terms; an exact repeat
  # collapses to one.
  key <- paste0(names(pooled), "", ifelse(is.na(pooled), "", pooled))
  pooled <- pooled[!duplicated(key)]

  terms <- Map(function(code, reason) Filter(Negate(is.null), list(
    `@type`   = "DefinedTerm",
    termCode  = code,
    name      = if (!is.na(reason)) reason else NULL)),
    names(pooled), unname(pooled))

  list(
    `@id`   = "#missingvalues",
    `@type` = "DefinedTermSet",
    name    = "Missing-value scheme",
    description = paste0(
      "Sentinel codes used across this paper's data to denote a missing ",
      "response, pooled from every variable's declared missing-value codes ",
      "into one canonical scheme."),
    hasDefinedTerm = unname(terms))
}

# Build one RO-Crate CreateAction per code file that reproducibility_check
# analysed, recording which data file(s) it reads (object), the code file
# itself (instrument), and which file(s) it writes (result) — schema.org's
# standard action pattern, and the RO-Crate-native alternative to inventing a
# custom "provenance" vocabulary. Uses reproducibility_check's `reads`/
# `writes` list-columns (added to its table specifically for this — see
# repro_file_io() in R/reproducibility_check.R), which are basenames only, so
# they are resolved against the psychds_check placement `plan` (file_name ->
# target_path) to get real @ids into this graph. Only emitted when
# reproducibility_check already ran (opportunistic, like .psychds_prereg_field
# — never forces a new run) AND `plan` is available (the multi-study data
# conversion path); returns list() otherwise, or when no code file resolves to
# at least one placed read/write (an action linking to nothing is not useful
# provenance). isBasedOn is also set on the AFFECTED file entities themselves
# (schema.org's direct derived-from property, equivalent to prov:wasDerivedFrom)
# so a reader following hasPart doesn't need to open the CreateAction to see
# what a file was derived from.
.psychds_provenance_entities <- function(ops, plan) {
  repro <- ops[["reproducibility_check"]]$table
  if (is.null(repro) || !is.data.frame(repro) || !nrow(repro) ||
      !all(c("file_name", "reads", "writes") %in% names(repro))) return(list())
  if (is.null(plan) || !is.data.frame(plan) || !nrow(plan) ||
      !all(c("file_name", "target_path") %in% names(plan))) return(list())

  plan_base <- tolower(basename(plan$file_name))
  resolve <- function(basenames) {
    basenames <- basenames[!is.na(basenames) & nzchar(basenames)]
    if (!length(basenames)) return(character(0))
    i <- match(tolower(basenames), plan_base)
    tp <- plan$target_path[i]
    unique(tp[!is.na(tp) & nzchar(tp)])
  }

  # @id from the script's own filename ("01_clean.R" -> "#provenance-01-clean-r")
  # rather than a bare counter, so the identifier says which script it
  # describes — the same self-describing style as the CRediT role ids
  # (#role-ex1-validation). Non-alphanumerics collapse to "-" because an @id is
  # a URI fragment; a numeric suffix disambiguates the case where two studies
  # each contain a same-named script (a shared "clean.R"), which would
  # otherwise collide.
  slug <- function(fn) {
    s <- tolower(gsub("[^A-Za-z0-9]+", "-", basename(fn)))
    gsub("(^-|-$)", "", s)
  }

  entities <- list()
  used <- character(0)
  for (i in seq_len(nrow(repro))) {
    code_target <- resolve(repro$file_name[i])
    if (!length(code_target)) next   # the code file itself was not placed
    object_targets <- resolve(unlist(repro$reads[i]))
    result_targets <- resolve(unlist(repro$writes[i]))
    if (!length(object_targets) && !length(result_targets)) next

    base <- slug(repro$file_name[i])
    if (!nzchar(base)) base <- as.character(i)
    id <- base
    n  <- 1L
    while (id %in% used) { n <- n + 1L; id <- paste0(base, "-", n) }
    used <- c(used, id)

    run_pos <- if ("run_order" %in% names(repro)) repro$run_order[i] else NA
    run_pos <- suppressWarnings(as.integer(run_pos))

    entities[[length(entities) + 1]] <- Filter(Negate(is.null), list(
      `@id`       = paste0("#lifecycle-", id),
      `@type`     = "CreateAction",
      name        = paste("Script run:", basename(repro$file_name[i])),
      # Same what/when/who shape as the manual lifecycle stages; the difference
      # is only that a script performed this one, so instrument/object/result
      # are auto-filled while startTime/agent still need a human.
      eventType   = "script run",
      startTime   = .psychds_unknown,
      agent       = .psychds_unknown,
      # reproducibility_check's inferred run order (it computes this to decide
      # what can run first). schema.org's native `position`. A PARTIAL order:
      # scripts with no dependency between them share a position, so ties are
      # expected and mean "either order". Omitted when the module could not
      # place a file.
      position    = if (!is.na(run_pos)) run_pos else NULL,
      instrument  = list(`@id` = code_target[1]),
      object      = if (length(object_targets)) lapply(object_targets, function(p) list(`@id` = p)) else NULL,
      result      = if (length(result_targets)) lapply(result_targets, function(p) list(`@id` = p)) else NULL))
  }
  entities
}

# Build and write the multi-study collection metadata as an RO-Crate 1.3
# `ro-crate-metadata.json` at the output root. It layers a custom JSON-LD
# context (the `metacheck`/`ddi` terms below) over the standard RO-Crate
# context, so generic RO-Crate tooling reads the graph while the DDI-Lifecycle
# study-design concepts (populationOfConcern, samplingMethod, ...) stay
# machine-readable rather than hidden in namespaced schema.org extensions. It
# is deliberately NOT named
# dataset_description.json, so the Psych-DS validator (which only opens a file
# of that exact name) never validates it, and the root stays a non-dataset
# collection (Option A).
#
# The @graph's root Dataset (./) lists every part that exists via hasPart: each
# study-<group>/ dataset (named and linked only — ALL variable-level detail,
# including how many there are, stays in that study's own
# dataset_description.json), each root-level shared file, the paper full text,
# and the logs. `study_roots` are the "study-<group>" prefixes; `shared_files`
# the plan target paths that carry no study prefix; `fulltext_rel`/`logs_rel`
# the root-relative paths of those generated artifacts. Returns the written
# path.
.psychds_rocrate_json <- function(paper, output_dir, pid, study_roots,
                                  labels_df = NULL,
                                  shared_files = character(0),
                                  fulltext_rel = character(0),
                                  logs_rel = character(0),
                                  paradata = list(),
                                  open_flags = logical(0),
                                  ops = list(),
                                  plan = NULL) {
  info <- paper$info %||% list()
  ival <- function(field) {
    v <- if (field %in% names(info)) info[[field]] else NULL
    if (length(v) == 0) NULL else v
  }

  title <- ival("title")
  name <- if (!is.null(title) && nzchar(title)) title else
    paste0("Data collection (", pid, ")")

  # One Dataset entity per study root, carrying a variable COUNT only. Full
  # per-variable detail (label, question, scale, missing values, stats) already
  # lives in that study's own dataset_description.json variableMeasured array
  # (built by .psychds_variable_measured()) — duplicating every PropertyValue
  # into the root graph as well produced a root file with one entity per column
  # across the whole paper (thousands, for wide survey data) that was pure
  # redundant duplication of data already on disk, not something a researcher
  # could usefully scan. The study's own metadata file is the cross-reference
  # target for variable-level detail.
  study_parts <- lapply(study_roots, function(sr) {
    grp <- sub("^study-", "", sr)
    Filter(Negate(is.null), list(
      `@id`   = paste0(sr, "/"),
      `@type` = "Dataset",
      name    = paste("Study", toupper(grp)),
      about     = c(
        list(list(`@id` = if (length(study_roots) > 1)
          paste0("#study-design-", grp) else "#study-design")),
        list(list(`@id` = paste0("#lifecycle-", grp, "-collection"))),
        lapply(names(.psychds_lifecycle_stages), function(k)
          list(`@id` = paste0("#lifecycle-", grp, "-", k))))))
  })

  # Per study part: one #study-design for how the study was run, the fixed
  # lifecycle-event stubs (raw data obtained / cleaning / analysis, each with
  # what-when-who), and the 14 CRediT contributor roles. All are scoped per
  # study when a paper has several and unscoped/paper-level when there is only
  # one — a 13-study paper otherwise got 13 collection events and 182 role
  # stubs but a SINGLE shared design block, so one sampleSize had to cover 13
  # different experiments. Script-derived lifecycle events are added separately
  # (they come from reproducibility_check, not from the study list).
  lifecycle_entities <- unlist(lapply(study_roots, function(sr)
    c(list(.psychds_collection_event_entity(sr)), .psychds_lifecycle_events(sr))),
    recursive = FALSE)
  multi <- length(study_roots) > 1
  design_entities <- if (multi)
    lapply(study_roots, function(sr) .psychds_study_design_entity(ops, pid, sr))
  else list(.psychds_study_design_entity(ops, pid))
  role_entities <- if (multi)
    unlist(lapply(study_roots, .psychds_credit_role_entities), recursive = FALSE)
  else .psychds_credit_role_entities()

  # hasPart: root-level shared files (codebooks, materials, documentation), the
  # paper full text, and the provenance logs. Each is just its @id, @type and
  # name — an earlier version also carried a `metacheck:role` label ("paper
  # full text", "metacheck provenance log"), dropped because it restated what
  # the path already says: everything under logs/ is a log, and the file under
  # documentation/ ending _fulltext.txt is the paper text. A custom term that
  # re-encodes a directory convention is not metadata, it is duplication.
  ref_parts <- function(paths, type) {
    paths <- unique(paths[!is.na(paths) & nzchar(paths)])
    lapply(paths, function(p) list(
      `@id` = p, `@type` = type, name = basename(p)))
  }
  file_parts <- c(ref_parts(shared_files, "File"),
                  ref_parts(fulltext_rel, "CreativeWork"),
                  ref_parts(logs_rel, "CreativeWork"))

  # hasPart: one Dataset entry per Behaverse paradata file — the trial-level
  # (response time / stimulus / option) data for an instrument, cross-referenced
  # to the matching scale (OSD) on the canonical instrument id.
  paradata_parts <- lapply(paradata, function(pd) Filter(Negate(is.null), list(
    `@id`     = pd$path,
    `@type`   = "Dataset",
    name      = paste("Paradata:", pd$instrument_id),
    `metacheck:instrument_id` = pd$instrument_id,
    `metacheck:sourceFormat`  = if (nzchar(pd$format %||% "")) pd$format else NULL,
    `metacheck:responseCount` = pd$n_responses,
    `metacheck:scale`         = if (!is.na(pd$osd_link)) pd$osd_link else NULL)))

  # Prefer the paper's own abstract as the human-readable description (makes the
  # corpus text-searchable); fall back to a factual sentence about the structure.
  abstract <- .psychds_abstract_text(paper)
  description <- if (nzchar(abstract)) abstract else paste0(
    "A collection of ", length(study_roots), " Psych-DS datasets (one per ",
    "study) generated by metacheck. Each study-*/ part is an independently ",
    "valid Psych-DS dataset; this collection root is intentionally not itself ",
    "a Psych-DS dataset.")

  # Authors as RO-Crate Person contextual entities. An ORCID iD (when GROBID
  # extracted one) becomes the entity's own @id, per RO-Crate convention
  # (https://orcid.org/... is a resolvable, globally unique identifier); authors
  # without one get a document-local @id instead. Affiliation is carried as free
  # text (metacheck has no ROR resolution) rather than "unknown", since an
  # absent affiliation is not the same gap as an unresolved one.
  author_entities <- list()
  author_refs <- list()
  if (!is.null(paper$author) && nrow(paper$author) > 0) {
    au <- paper$author
    for (i in seq_len(nrow(au))) {
      nm <- trimws(paste(au$given[i] %||% "", au$family[i] %||% ""))
      if (!nzchar(nm)) next
      orcid <- au$orcid[i] %||% NA_character_
      aid <- if (!is.na(orcid) && nzchar(orcid)) orcid
      else paste0("#author-", i)
      aff <- au$affiliation[i] %||% NA_character_
      author_entities[[length(author_entities) + 1]] <- Filter(Negate(is.null), list(
        `@id` = aid, `@type` = "Person", name = nm,
        affiliation = if (!is.na(aff) && nzchar(aff)) aff else NULL))
      author_refs[[length(author_refs) + 1]] <- list(`@id` = aid)
    }
  }

  # Root Dataset entity (RO-Crate's Root Data Entity), @id "./".
  # funder sits here rather than on #study-design: it is bibliographic
  # attribution about the work as a whole, belonging beside author and
  # identifier, not a how-it-was-run detail. It has no extraction path and is
  # always "unknown". `purpose` was briefly here too but moved to the per-study
  # #study-design — each experiment in a multi-study paper usually tests its
  # own question, and one paper-level purpose cannot express that.
  root <- Filter(Negate(is.null), list(
    `@id`       = "./",
    `@type`     = "Dataset",
    name        = name,
    description = description,
    funder      = .psychds_unknown,
    author      = if (length(author_refs)) author_refs else NULL,
    hasPart     = c(study_parts, file_parts, paradata_parts)))

  doi <- ival("doi") %||% NA_character_
  if (!is.na(doi) && nzchar(doi))
    root[["identifier"]] <- paste0("https://doi.org/",
                                   sub("^https?://doi.org/", "", doi))
  kw <- .psychds_keywords(ival("keywords"))
  if (!is.null(kw)) root[["keywords"]] <- kw

  # Open-practices check outcomes as filterable metadata, so the corpus can be
  # queried (e.g. "papers with no shared data") without opening each log. Only
  # emitted for flags open_practices actually resolved (non-NA).
  flag_map <- c(data = "metacheck:hasSharedData",
                code = "metacheck:hasSharedCode",
                materials = "metacheck:hasSharedMaterials",
                prereg = "metacheck:isPreregistered")
  for (k in names(flag_map)) {
    v <- if (k %in% names(open_flags)) open_flags[[k]] else NA
    if (!is.na(v)) root[[flag_map[[k]]]] <- isTRUE(v)
  }

  # dateCreated is schema.org's own property for "when this dataset/file was
  # generated" — metacheck:generated duplicated it under a namespaced name for
  # no reason, since there is no distinct concept it was adding.
  root[["dateCreated"]] <- format(Sys.Date(), "%Y-%m-%d")

  # Missing-value scheme: one entity pooling every distinct sentinel code
  # found anywhere in the paper (see .psychds_missing_scheme_entity()), or
  # NULL when no variable declares one — in which case it is omitted from
  # both hasPart/about and the graph, rather than emitted empty.
  missing_entity <- .psychds_missing_scheme_entity(labels_df)

  # Code -> data provenance: one CreateAction per code file reproducibility_check
  # analysed (see .psychds_provenance_entities()), only when that module already
  # ran and at least one read/write resolved to a placed file. list() when
  # neither holds, so nothing is added to about/graph.
  provenance_entities <- .psychds_provenance_entities(ops, plan)

  # Each per-study #study-design is reachable from its own study part's
  # `about`, so the root only names the paper-level one (single-study papers).
  root[["about"]] <- c(if (!multi) list(list(`@id` = "#study-design")),
                       if (!is.null(missing_entity)) list(list(`@id` = "#missingvalues")),
                       lapply(provenance_entities, function(e) list(`@id` = e$`@id`)))
  root <- Filter(Negate(is.null), root)

  descriptor <- list(
    `@id`   = "ro-crate-metadata.json",
    `@type` = "CreativeWork",
    conformsTo = list(`@id` = "https://w3id.org/ro/crate/1.3"),
    about      = list(`@id` = "./"))

  graph <- c(list(descriptor, root),
             design_entities,
             if (!is.null(missing_entity)) list(missing_entity),
             lifecycle_entities,
             provenance_entities,
             role_entities,
             author_entities)

  crate <- list(
    `@context` = list(
      "https://w3id.org/ro/crate/1.3/context",
      list(
        # metacheck's own prefix, for the metacheck:* properties used across
        # the graph (variableCount, hasSharedData, ...). Previously UNDECLARED,
        # which meant strict JSON-LD expansion silently DROPPED every one of
        # them. Points at the namespace this project already uses for its
        # schemas (see inst/schema/*.json `$id`), not an invented one.
        metacheck = "https://scienceverse.org/schema/metacheck/terms/",
        ddi       = "https://ddialliance.org/Specification/DDI-Lifecycle/3.3/",
        # Every term below is a REAL DDI-Lifecycle 3.3 element. There are no
        # metacheck-invented terms in this context: an earlier version
        # declared eight of these under a fabricated `ssrc:` namespace whose
        # URL 404s, and several of those (notably NumberOfResponses, which DDI
        # names identically) were standard all along.
        populationOfConcern       = "ddi:PopulationOfConcern",
        samplingMethod            = "ddi:SamplingProcedure",
        samplingFrame             = "ddi:SampleFrame",
        targetSampleSize          = "ddi:OverallTargetSampleSize",
        obtainedDate              = "ddi:DataCollectionDate",
        modeOfCollection          = "ddi:ModeOfCollection",
        collectionSituation       = "ddi:CollectionSituation",
        eventType                 = "ddi:EventType",
        dataCollectionMethodology = "ddi:DataCollectionMethodology",
        purpose                   = "ddi:Purpose",
        sampleSize                = "ddi:SampleSize",
        numberOfResponses         = "ddi:NumberOfResponses",
        specificResponseRate      = "ddi:SpecificResponseRate",
        isInclusive               = "ddi:IsInclusive"
        # description, contributor, agent, startTime, funder, roleName, about
        # and agent are schema.org natives already supplied by the RO-Crate
        # context above, so they need no declaration here.
        )),
    `@graph` = graph)

  path <- file.path(output_dir, "ro-crate-metadata.json")
  .psychds_write_json(crate, path)
  invisible(path)
}

# Resolve the module outputs a converter needs, reusing anything already
# computed and running only what is missing. This is what lets a converter be
# called on a captured report()/report_module_run() result (a chain object) and
# skip re-running the modules — the same way modules reuse each other's outputs
# within a chain.
#
# `paper` may be a plain paper object OR a captured chain result (the list
# returned by report()/report_module_run(), or a single metacheck_module_output).
# `needed` is the module vector.
#
# report()/report_module_run() attach the paper to their result as a "paper"
# attribute, so a captured result carries everything: its module outputs are
# reused and the real paper (title/authors/DOI) is recovered from the attribute.
# When no paper is recoverable at all, the paper_id is taken from the module
# tables and a minimal stub paper stands in for the metadata builders.
#
# Returns list(ops = <named module outputs>, paper = <paper>, pid = <id string>).
.converter_resolve <- function(paper, needed,
                               local_path = NULL, local_only = FALSE,
                               model = llm_model(), params = list(),
                               download = "all", skip_types = NULL,
                               peek_zips = FALSE, max_file_size = 100,
                               max_download_size = 500, cache = FALSE) {
  # Coerce whatever we were handed into (a) a named list of module outputs we
  # can reuse, and (b) the underlying paper object (when available).
  chain <- paper

  if (inherits(chain, "metacheck_module_output")) {
    # report_module_run() returns the LAST module's output, with every EARLIER
    # module nested in $prev_outputs. Reuse them ALL — not just the last one —
    # so the converter uses the data_check result the chain already produced
    # (with its download="all" file_locations) instead of re-running data_check
    # with default args (download="data"), which would leave code/supplemental
    # files with file_location = NA and make the converter drop them as "not
    # downloaded". This was the cause of code/supplemental files (e.g. .Rmd
    # analysis scripts) missing from the Psych-DS output.
    reuse <- chain$prev_outputs %||% list()
    this <- chain
    this$prev_outputs <- NULL
    reuse[[chain$module %||% "unknown"]] <- this
  } else if (is.list(chain) && !inherits(chain, "scivrs_paper") &&
             any(needed %in% names(chain))) {
    reuse <- chain           # already a named list of module outputs
  } else {
    reuse <- list()          # a plain paper: nothing to reuse
  }

  # Recover the underlying paper: an explicit paper arg, else the "paper"
  # attribute report()/report_module_run() attaches to its result, else a
  # module output that still carries $paper. Otherwise NULL.
  real_paper <- if (inherits(paper, "scivrs_paper")) paper else NULL
  if (is.null(real_paper)) {
    att <- attr(chain, "paper")
    if (inherits(att, "scivrs_paper")) real_paper <- att
  }
  if (is.null(real_paper) && length(reuse) > 0) {
    for (mo in reuse) {
      if (!is.null(mo$paper) && inherits(mo$paper, "scivrs_paper")) {
        real_paper <- mo$paper; break
      }
    }
  }

  # Reuse is all-or-nothing: the modules chain through get_prev_outputs, which
  # only sees outputs produced within the same report_module_run() call, so a
  # reused output cannot feed a freshly-run one. If every needed module is
  # already present we reuse them wholesale; otherwise we re-run the full chain
  # from the paper (which wires the modules together correctly). This still
  # skips all work when a complete captured result is handed in.
  if (all(needed %in% names(reuse))) {
    ops <- reuse[needed]
  } else {
    if (is.null(real_paper))
      stop("Cannot run the module(s) ",
           paste(setdiff(needed, names(reuse)), collapse = ", "),
           " because no paper object is available. Pass the paper object (not ",
           "only a partial captured report result) as `paper`.", call. = FALSE)
    # A captured chain that already ran data_check (with the caller's download
    # policy) but is MISSING one of the needed modules forces a full re-run,
    # which re-downloads and re-queries the LLM. Warn so the caller can add the
    # missing module(s) to their chain and avoid it — most often psychds_check,
    # which is easy to omit from a check-suite vector.
    if (length(reuse) > 0)
      warning("convert_psychds re-ran the full check chain because the captured ",
              "result was missing: ",
              paste(setdiff(needed, names(reuse)), collapse = ", "),
              ". Add it/them to the modules you run so the downloaded files and ",
              "LLM results are reused instead of recomputed.", call. = FALSE)
    # The re-run MUST reproduce the caller's download policy: data_check defaults
    # to download = "data", which would leave code/supplemental files with
    # file_location = NA and drop them from the dataset. Thread the same download
    # args through (download defaults to "all" for archiving; the size caps still
    # gate and warn about large files/repos).
    #
    # When this re-run was FORCED by an incomplete captured chain (reuse is
    # non-empty but missing a needed module), the caller already downloaded these
    # files once — most likely into the persistent on-disk cache. Force
    # cache = TRUE for the re-run so it reads those files back from
    # .metacheck_repo_cache instead of re-fetching them into a throwaway temp dir
    # (which is what convert_psychds's default cache = FALSE would do). A genuine
    # from-scratch convert (nothing to reuse) still honours the caller's `cache`.
    rerun_cache <- if (length(reuse) > 0) TRUE else cache
    ops <- report_module_run(
      real_paper, needed,
      args = list(data_check = list(local_path = local_path,
                                    local_only = local_only,
                                    model = model, params = params,
                                    download = download,
                                    skip_types = skip_types,
                                    peek_zips = peek_zips,
                                    max_file_size = max_file_size,
                                    max_download_size = max_download_size,
                                    cache = rerun_cache)))
  }

  # Surface an upstream module failure with its real error, rather than letting
  # the converter fall over later with a cryptic "empty plan" / "no columns"
  # message. A failed module carries traffic_light == "fail" and its error text
  # in $report (see report_module_run()).
  for (m in needed) {
    mo <- ops[[m]]
    if (!is.null(mo) && identical(mo$traffic_light, "fail")) {
      msg <- paste(mo$report %||% "unknown error", collapse = " ")
      hint <- ""
      if (grepl("maximum number of calls", msg)) {
        # Suggest a limit above the number of calls the run actually needed.
        needed_calls <- suppressWarnings(as.integer(
          sub(".*would make ([0-9]+) calls.*", "\\1", msg)))
        suggest <- if (!is.na(needed_calls)) needed_calls + 10L else 200L
        hint <- sprintf(
          "\n\nRaise the limit above what the run needs, e.g. llm_max_calls(%d).",
          suggest)
      }
      stop("The '", m, "' module failed, so there is nothing to convert:\n  ",
           paste(mo$report %||% "unknown error", collapse = "\n  "), hint,
           call. = FALSE)
    }
  }

  # paper_id: from the real paper, else recovered from any module table.
  pid <- if (!is.null(real_paper)) paper_id(real_paper) else character(0)
  if (length(pid) == 0) {
    for (mo in ops) {
      for (tbl in list(mo$table, mo$summary_table, mo$structure)) {
        if (!is.null(tbl) && "paper_id" %in% names(tbl)) {
          got <- unique(tbl$paper_id[!is.na(tbl$paper_id)])
          if (length(got) > 0) { pid <- got; break }
        }
      }
      if (length(pid) > 0) break
    }
  }
  pid <- if (length(pid) == 0) "dataset" else as.character(pid[[1]])

  # When the underlying paper is unrecoverable (only a captured result was
  # given), stand in a minimal paper carrying just the recovered id, so the
  # dataset-metadata builders have a valid paper object (empty title/authors).
  if (is.null(real_paper)) real_paper <- paper(id = pid)

  # `ops` is the subset the converter needs (data_check/codebook_check/
  # psychds_check). `all_ops` is EVERY captured module output when a full chain
  # was handed in (reuse holds them all) — used for the provenance logs so the
  # logs/ RDS + checks JSON record the whole run, not just the converter's three
  # modules. When modules were re-run from a bare paper (no reuse), the full set
  # is just `ops`.
  all_ops <- if (length(reuse)) reuse else ops

  list(ops = ops, all_ops = all_ops, paper = real_paper, pid = pid)
}

# Build the "why is there nothing to convert" explanation. When a paper linked
# a repository that was found but could not be *listed* (a GitHub repo over the
# size gate, a private OSF component), the bare "no repository" / "empty plan"
# message is misleading — the repository exists, it was just skipped. This spells
# out which repo, why, and how to include it: raise the caps to fetch it, or
# download it manually and point data_check at the local copy. `ops` is the
# resolved module-output list; returns "" when no repo was gated.
.converter_gated_hint <- function(ops) {
  g <- ops[["data_check"]]$gated_repos %||% ops[["repo_check"]]$gated_repos
  if (is.null(g) || nrow(g) == 0) return("")
  # One paper often links the same repo by several deep URLs (…/tree/master/x,
  # …/tree/master/y); collapse to the repo root so each is reported once.
  g$root <- sub("/(tree|blob)/.*$", "", g$repo_url)
  g <- g[!duplicated(paste(g$root, g$repo_error)), , drop = FALSE]
  lines <- vapply(seq_len(nrow(g)), function(i) {
    sprintf("  - %s: %s", g$root[i],
            g$repo_error[i] %||% "could not be listed")
  }, character(1))
  paste0(
    "\n\nA linked repository was found but not downloaded, so there were no ",
    "files to process:\n",
    paste(lines, collapse = "\n"),
    "\n\nTo include it, either raise the size limits (e.g. ",
    "`max_download_size` / `max_file_size`, or `github_gate = FALSE` for a ",
    "GitHub repo) and re-run, or download the repository manually and pass its ",
    "folder to data_check as a local repository: ",
    "`data_check(paper, local_path = \"/path/to/downloaded/repo\")`.")
}

.psychds_write_json <- function(obj, path) {
  dir.create(dirname(path), recursive = TRUE, showWarnings = FALSE)
  json <- jsonlite::toJSON(obj, auto_unbox = TRUE, pretty = TRUE, na = "null")
  writeLines(json, path, useBytes = TRUE)
  invisible(path)
}

# Write the paper's extracted full text as a plain UTF-8 text file under the
# dataset root's `documentation/` folder, so the release carries the manuscript
# prose the checks were run against (not just the data). The per-unit text table
# is read via paper_table(paper, "text") — the same accessor search_text() uses —
# and joined to paper_table(paper, "section") for section headers; we reconstruct
# readable text by ordering on text_id and emitting a "# <header>" line whenever
# the section changes. Returns the written path, or NULL when the paper carries no
# full text (e.g. a stub paper recovered from a captured result).
#
# `documentation/` always sits at the true output root. In a flat (single-study)
# dataset that folder is unambiguous, so the file is `fulltext.txt`. In a
# multi-study dataset the studies live in `study-*/` subtrees and the root
# `documentation/` is shared context, so the file is pid-qualified
# (`<pid>_fulltext.txt`) to keep it identifiable.
.psychds_write_paper_text <- function(paper, output_dir, pid, multi_study) {
  if (!.is_paper(paper)) return(NULL)
  ft <- tryCatch(paper_table(paper, "text"), error = function(e) NULL)
  if (is.null(ft) || !is.data.frame(ft) || nrow(ft) == 0) return(NULL)
  if (!"text" %in% names(ft)) return(NULL)

  # Attach section headers the same way search_text() does, when available.
  sections <- tryCatch(paper_table(paper, "section"), error = function(e) NULL)
  scols <- c("section_id", "paper_id", "header", "section_type")
  if (!is.null(sections) && is.data.frame(sections) &&
      all(scols %in% names(sections)) && "section_id" %in% names(ft)) {
    ft <- dplyr::left_join(ft, sections[, scols],
                           by = intersect(c("section_id", "paper_id"), names(ft)),
                           relationship = "many-to-many")
  }

  # Order by reading order when available; otherwise keep the given order.
  if ("text_id" %in% names(ft)) ft <- ft[order(ft$text_id), , drop = FALSE]

  header <- if ("header" %in% names(ft)) ft$header else rep(NA_character_, nrow(ft))
  lines <- character(0)
  last_header <- NA_character_
  for (i in seq_len(nrow(ft))) {
    txt <- ft$text[i]
    if (is.na(txt) || !nzchar(trimws(txt))) next
    h <- header[i]
    # Emit a section header line when the header changes (and is real text that
    # is not just a repeat of the paragraph itself).
    if (!is.na(h) && nzchar(trimws(h)) &&
        !identical(h, last_header) && !identical(trimws(h), trimws(txt))) {
      if (length(lines) > 0) lines <- c(lines, "")
      lines <- c(lines, paste0("# ", trimws(h)), "")
      last_header <- h
    }
    lines <- c(lines, trimws(txt))
  }
  if (length(lines) == 0) return(NULL)

  doc_dir <- file.path(output_dir, "documentation")
  dir.create(doc_dir, recursive = TRUE, showWarnings = FALSE)
  fname <- if (isTRUE(multi_study)) paste0(pid, "_fulltext.txt") else "fulltext.txt"
  path <- file.path(doc_dir, fname)
  # UTF-8, BOM-free (writeLines with a UTF-8 connection adds no BOM).
  con <- file(path, open = "wb", encoding = "UTF-8")
  on.exit(close(con), add = TRUE)
  writeLines(lines, con, useBytes = FALSE)
  invisible(path)
}

# Build a REDUCED file manifest from data_check's structure table, for when no
# full manifest was written at run time (data_check's `manifest` arg unset). The
# full manifest (.data_check_write_manifest) needs run-time state the module
# output does not carry — the download gate tables, per-file skip reasons, HEAD
# size probes — so this fallback records only what the finished output knows: the
# file inventory with each file's downloaded/not-downloaded status, plus the same
# `provenance` block. It writes the same top-level shape (files[] + provenance)
# so write_file_manifest() can still read it. Returns the written path.
.psychds_reduced_manifest <- function(structure_df, pid, path) {
  cols <- function(nm) if (!is.null(structure_df[[nm]])) structure_df[[nm]] else
    rep(NA_character_, nrow(structure_df))
  loc  <- cols("file_location")
  downloaded <- !is.na(loc) & nzchar(loc) & file.exists(loc %||% "")

  entries <- lapply(seq_len(nrow(structure_df)), function(i) {
    sz <- suppressWarnings(as.numeric(cols("file_size")[i]))
    if (isTRUE(downloaded[i])) {
      on_disk <- suppressWarnings(file.size(loc[i]))
      if (!is.na(on_disk)) sz <- on_disk
    }
    Filter(Negate(is.null), list(
      file_name   = cols("file_name")[i],
      file_path   = cols("file_path")[i] %||% cols("file_name")[i],
      repo_url    = cols("repo_url")[i],
      file_url    = cols("file_url")[i],
      file_size   = if (!is.na(sz)) sz else NULL,
      data_type   = cols("data_type")[i],
      data_format = cols("data_format")[i],
      downloaded  = downloaded[i],
      status      = if (isTRUE(downloaded[i])) "downloaded" else "not_downloaded"
    ))
  })

  provenance <- list(
    software  = list(name = "metacheck", version = tryCatch(
      as.character(utils::packageVersion("metacheck")),
      error = function(e) NA_character_)),
    r_version = R.version.string,
    platform  = R.version$platform,
    prod_date = format(Sys.time(), "%Y-%m-%dT%H:%M:%S%z"),
    llm       = if (isTRUE(llm_use()))
      list(used = TRUE, model = llm_model()) else list(used = FALSE),
    # This manifest is the reduced form: it lacks the full manifest's per-file
    # skip reasons and gate details (those need data_check's run-time state).
    manifest_kind = "reduced"
  )

  doc <- Filter(Negate(is.null), list(
    paper_id     = pid,
    generated    = format(Sys.time(), "%Y-%m-%dT%H:%M:%S%z"),
    provenance   = provenance,
    n_files      = nrow(structure_df),
    n_downloaded = sum(downloaded),
    files        = entries
  ))
  dir.create(dirname(path), recursive = TRUE, showWarnings = FALSE)
  json <- jsonlite::toJSON(doc, auto_unbox = TRUE, pretty = TRUE, na = "null")
  writeLines(json, path, useBytes = TRUE)
  invisible(path)
}

# Write the provenance `logs/` folder for a converted dataset and return a named
# character vector of the files written (name = one-line description, for the
# README). Folds the three previously-separate artifacts into the conversion:
#   * <pid>.manifest.json — the file inventory. The FULL manifest is reused when
#     data_check wrote one this run (ops$data_check$manifest_path exists on disk);
#     otherwise a reduced manifest is built from the structure table.
#   * <pid>.checks.json    — per-paper check summaries + findings, when any check
#     module ran (capture_check_results on the module-output list).
#   * <pid>.rds            — full lossless module tables, when a module produced a
#     non-empty table (capture_module_tables).
# `ops` is convert_psychds's resolved named list of module outputs (a chain the
# capture_* functions accept directly). logs/ always sits at the dataset root.
.psychds_write_logs <- function(ops, structure_df, pid, output_dir,
                                all_ops = NULL) {
  logs_dir <- file.path(output_dir, "logs")
  dir.create(logs_dir, recursive = TRUE, showWarnings = FALSE)
  written <- character(0)
  # The checks JSON and module-tables RDS record the WHOLE run, so use every
  # captured module (all_ops) when available; the converter's `ops` is only the
  # 3 modules it needs to build the archive. The manifest below stays on `ops`
  # (it is data_check-specific). Falls back to `ops` when no full set was passed.
  log_ops <- if (!is.null(all_ops) && length(all_ops)) all_ops else ops

  # 1. Manifest — always. Reuse the full one if present, else reduced.
  manifest_dest <- file.path(logs_dir, paste0(pid, ".manifest.json"))
  full <- ops[["data_check"]]$manifest_path
  if (!is.null(full) && length(full) && file.exists(full[[1]])) {
    file.copy(full[[1]], manifest_dest, overwrite = TRUE)
    written[manifest_dest] <-
      "File inventory recording every repository file and whether it was downloaded (full manifest, with per-file skip reasons)."
  } else if (!is.null(structure_df) && is.data.frame(structure_df) &&
             nrow(structure_df) > 0) {
    .psychds_reduced_manifest(structure_df, pid, manifest_dest)
    written[manifest_dest] <-
      "File inventory recording every repository file and whether it was downloaded (reduced manifest)."
  }

  # 2. Checks JSON — when any module output is present (the checks that ran).
  has_modules <- any(vapply(log_ops, function(mo)
    inherits(mo, "metacheck_module_output") || is.list(mo), logical(1)))
  if (length(log_ops) > 0 && has_modules) {
    checks_path <- tryCatch(
      capture_check_results(log_ops, logs_dir, paper_id = pid),
      error = function(e) NULL)
    if (!is.null(checks_path) && file.exists(checks_path))
      written[checks_path] <-
        "Per-paper results of the metacheck checks that were run: each module's outcome plus its individual findings."
  }

  # 3. Tables RDS — when at least one module produced a non-empty table.
  has_table <- any(vapply(log_ops, function(mo)
    is.data.frame(mo$table) && nrow(mo$table) > 0, logical(1)))
  if (has_table) {
    rds_path <- tryCatch(
      capture_module_tables(log_ops, logs_dir, paper_id = pid),
      error = function(e) NULL)
    if (!is.null(rds_path) && file.exists(rds_path))
      written[rds_path] <-
        "Full, lossless module output tables (R data file) for later reloading and analysis without re-running the checks."
  }

  # 4. Statistical output — reproducibility_check() now writes this ITSELF, as a
  # statistical_output/ folder (results_long.csv + one ISA-JSON per source file)
  # sibling to data/ in its own materialised layout (R/stat-output.R's
  # stat_output_write(), called from inst/modules/reproducibility_check.R). That
  # folder only persists on disk when the module ran with keep_sandbox = TRUE,
  # surfaced as attr(reproducibility_check_output, "sandbox"); when present, we
  # copy it here into the FINAL archive, sibling to output_dir/data/, so it
  # survives alongside the rest of the converted dataset.
  repro_out <- log_ops[["reproducibility_check"]]
  sandbox <- if (!is.null(repro_out)) attr(repro_out, "sandbox") else NULL
  if (!is.null(sandbox) && length(sandbox) == 1 && !is.na(sandbox)) {
    src_dir <- file.path(sandbox, "statistical_output")
    if (dir.exists(src_dir)) {
      dest_dir <- file.path(output_dir, "statistical_output")
      dir.create(dest_dir, recursive = TRUE, showWarnings = FALSE)
      copied <- file.copy(list.files(src_dir, full.names = TRUE), dest_dir,
                          overwrite = TRUE)
      if (any(copied))
        written[dest_dir] <-
          "Statistical results extracted from the paper's JASP/jamovi file(s) and/or executed R code, typed with the STATO ontology: one combined results_long.csv (one row per extracted statistic, each with a result_id identifying the code — file + source line, or file + analysis heading — that produced it) and one ISA-JSON document per source file."
    }
  }

  written
}

# Write a requirements.txt at the archive root listing the distinct packages the
# paper's code loads. Names only, no versions — static analysis cannot know the
# versions used. Never overwrites an existing requirements.txt / renv.lock /
# DESCRIPTION (an authors' real dependency file wins). Returns the path written,
# or NULL when there is nothing to write.
#
# code_check is NOT in the converter's `needed` set (keeping it out avoids
# forcing a full chain re-run for captured results that lack it). So its packages
# are sourced from `ops` when the caller happened to run it, and otherwise
# code_check is run here on its own — reusing the files already downloaded for
# the conversion (repo_check's cached table + the on-disk cache), so this adds a
# read pass over the code files, not another download.
.psychds_write_requirements <- function(ops, output_dir, paper = NULL,
                                        local_path = NULL, local_only = FALSE) {
  cc <- ops[["code_check"]]
  if (is.null(cc) && !is.null(paper)) {
    cc <- tryCatch(
      module_run(paper, "code_check",
                 local_path = local_path, local_only = local_only),
      error = function(e) NULL)
  }
  pkgs <- tryCatch(code_packages(cc$table), error = function(e) character(0))
  if (length(pkgs) == 0) return(NULL)

  # Respect an authors' real dependency file if one was archived at the root.
  existing <- list.files(output_dir,
                         pattern = "^(requirements\\.txt|renv\\.lock|DESCRIPTION)$",
                         ignore.case = TRUE)
  if (length(existing) > 0) return(NULL)

  path <- file.path(output_dir, "requirements.txt")
  lines <- c(
    "# Auto-generated by metacheck from static analysis of the code files in",
    "# this dataset. These are package NAMES only, not pinned versions: the",
    "# source shows which packages are loaded, not which versions were used.",
    "# For exact, reproducible R dependencies, run renv::init() in a checkout",
    "# of the original code, which resolves real installed versions.",
    pkgs
  )
  ok <- tryCatch({ writeLines(lines, path, useBytes = TRUE); TRUE },
                 error = function(e) FALSE)
  if (ok) path else NULL
}

# Append (or create) a "Provenance logs" section to the dataset-root README,
# listing each file written into logs/ and what it holds. `logs_written` is the
# named vector from .psychds_write_logs() (name = path, value = description).
.psychds_write_logs_readme <- function(logs_written, output_dir, readme_path) {
  if (!length(logs_written)) return(invisible())
  # logs/ always sits at the dataset root, so the README-relative path of each
  # file is just "logs/<basename>".
  rel <- file.path("logs", basename(names(logs_written)))
  lines <- c(
    "",
    "## Provenance logs",
    "",
    paste0("The `logs/` folder holds the metacheck records behind this ",
           "conversion — the provenance of what was checked and packaged:"),
    "",
    vapply(seq_along(logs_written), function(i)
      sprintf("- **`%s`** — %s", rel[i], unname(logs_written)[i]), character(1)),
    "")
  con_lines <- if (file.exists(readme_path))
    readLines(readme_path, warn = FALSE) else character(0)
  writeLines(c(con_lines, lines), readme_path, useBytes = TRUE)
  invisible()
}

# Copy a file, dropping a leading UTF-8 BOM (EF BB BF) if present. Byte-level so
# it works for any encoding/content. Returns TRUE on success.
.psychds_copy_no_bom <- function(src, dest) {
  tryCatch({
    n <- file.info(src)$size
    if (is.na(n) || n == 0) return(file.copy(src, dest, overwrite = TRUE))
    con <- file(src, "rb"); on.exit(close(con), add = TRUE)
    bytes <- readBin(con, "raw", n)
    if (length(bytes) >= 3 &&
        bytes[1] == as.raw(0xEF) && bytes[2] == as.raw(0xBB) &&
        bytes[3] == as.raw(0xBF)) {
      bytes <- bytes[-(1:3)]
    }
    out <- file(dest, "wb"); on.exit(close(out), add = TRUE)
    writeBin(bytes, out)
    TRUE
  }, error = function(e) FALSE)
}

# Robustly coerce a plan column to logical (it may arrive as logical, or as
# character "TRUE"/"FALSE" after a JSON round-trip).
isTRUE_vec <- function(x) {
  if (is.logical(x)) return(!is.na(x) & x)
  tolower(as.character(x)) %in% c("true", "1")
}

# Write a genuine, Psych-DS-clean CSV from a non-CSV data source (xlsx, sav,
# dta, ...). Reads the file in full via data_read_head() — the same reader
# data_check uses, so the columns match variableMeasured — and writes UTF-8,
# BOM-free, no row names. Returns FALSE if the source cannot be read as a table.
.psychds_write_data_csv <- function(src, dest) {
  tryCatch({
    df <- data_read_head(src, n_rows = Inf)
    if (is.null(df) || !is.data.frame(df) || ncol(df) == 0) return(FALSE)
    # write.csv adds no BOM; force UTF-8 for non-ASCII headers/values.
    con <- file(dest, open = "wb", encoding = "UTF-8")
    on.exit(close(con), add = TRUE)
    utils::write.csv(df, con, row.names = FALSE, na = "")
    TRUE
  }, error = function(e) FALSE)
}

# Recover the analyses from a .jasp as a readable code artifact: a numbered,
# human-readable summary (one line per analysis) followed by the verbatim
# analyses.json for completeness. Returns TRUE if written. The analyses are the
# closest thing a .jasp has to "code" — a structured record of the tests run.
.psychds_write_jasp_code <- function(src, dest) {
  tryCatch({
    j <- read_jasp(src)
    summary <- .jasp_analyses_summary(j$analyses)
    if (!length(summary) && is.null(j$analyses)) return(FALSE)
    raw <- tryCatch(jsonlite::toJSON(j$analyses, auto_unbox = TRUE, pretty = TRUE),
                    error = function(e) NULL)
    lines <- c(
      paste0("# Analyses recovered from ", basename(src)),
      "# These are the JASP analyses stored in the file (not runnable R code).",
      "",
      if (length(summary)) summary else "(no analyses recorded)",
      "",
      "# --- Raw analyses.json ---",
      if (!is.null(raw)) raw else "(unavailable)")
    con <- file(dest, open = "wb", encoding = "UTF-8")
    on.exit(close(con), add = TRUE)
    writeLines(lines, con, useBytes = TRUE)
    TRUE
  }, error = function(e) FALSE)
}

# Recover the analyses from a .omv as a readable code artifact — the jamovi
# counterpart of .psychds_write_jasp_code. read_omv()$analyses is already a
# character vector (one line per analysis, each carrying the reproducible
# R-syntax call when recoverable), so unlike JASP there is no nested
# analyses.json to dump — the summary IS the recovered content. Returns TRUE if
# written, FALSE when the file records no analyses.
.psychds_write_omv_code <- function(src, dest) {
  tryCatch({
    summary <- read_omv(src)$analyses
    if (!length(summary)) return(FALSE)
    lines <- c(
      paste0("# Analyses recovered from ", basename(src)),
      "# These are the jamovi analyses stored in the file, with the reproducible",
      "# R syntax jamovi records for each (not a runnable script on its own).",
      "",
      summary)
    con <- file(dest, open = "wb", encoding = "UTF-8")
    on.exit(close(con), add = TRUE)
    writeLines(lines, con, useBytes = TRUE)
    TRUE
  }, error = function(e) FALSE)
}

#' Generate a Psych-DS-compliant copy of a repository
#'
#' Writes a [Psych-DS](https://psych-ds.github.io/) dataset directory for a
#' paper's data repository: files are copied to their standard locations, a
#' `dataset_description.json` is generated (with `variableMeasured` built from
#' the extracted columns and codebook labels), and `README` / `CHANGES` stubs
#' are added when missing. This is the generator counterpart of the
#' `psychds_check` module — run the check first to preview the gap.
#'
#' Multi-study repositories (when `data_check` assigned study groups under
#' `llm_use(TRUE)`) are written as `study-<group>/` subdirectories, each a
#' self-contained, independently-valid Psych-DS dataset. Files that belong to no
#' single study (a whole-repo README/codebook, shared materials) sit at the
#' collection root beside the study folders (following BIDS, which places shared
#' content at the root rather than in a pseudo-subject). The root is then a
#' *collection* of datasets, not itself a Psych-DS dataset, so it carries a
#' machine-readable `ro-crate-metadata.json` (RO-Crate 1.3 JSON-LD, with a
#' custom context for DDI-Lifecycle-inspired study-design/variable terms)
#' instead of a root `dataset_description.json` — validate each `study-*/` folder.
#' Original files whose contents cannot be read (no local copy) are skipped with
#' a note.
#'
#' @param paper a paper object (see [read()]), **or** a captured result of
#'   `report(paper, ...)` / `report_module_run(paper, ...)`. When a captured
#'   result containing all of `data_check` / `codebook_check` / `psychds_check`
#'   is passed, those outputs are reused (with the paper recovered from the
#'   result) instead of re-running; otherwise the modules are run.
#' @param output_dir directory to write the Psych-DS dataset into; created if
#'   needed. Defaults to `"psychds/<paper_id>"` under the working directory.
#' @param refresh_osf whether to fetch a fresh OSF file listing even if one was
#'   already retrieved this session. The default (`FALSE`) reuses the cached
#'   listing, so building the dataset right after running the checks does not
#'   re-query (and risk being throttled by) the OSF API. Set `TRUE` to force a
#'   fresh listing. Downloaded file contents are cached on disk regardless.
#' @param local_path,local_only passed to `data_check` when its output is not
#'   already available (see `data_check()`)
#' @param model,params passed to the underlying modules when `llm_use(TRUE)`
#' @param overwrite whether to overwrite an existing `output_dir`. When `FALSE`
#'   (the default) and `output_dir` already exists, the function messages and
#'   skips rather than erroring (the returned list has `existed = TRUE`).
#' @param download,skip_types,peek_zips,max_file_size,max_download_size,cache
#'   passed to `data_check` **only when the modules must be re-run** (i.e. when
#'   `paper` is a bare paper or a captured chain missing a needed module). They
#'   are ignored when the chain already carries `data_check`/`codebook_check`/
#'   `psychds_check`, whose downloaded files are reused as-is. Defaults match
#'   `data_check`, except `download` defaults to `"all"` (the archive should
#'   carry every file, not only the readable data subset); the `max_file_size`
#'   and `max_download_size` caps still gate and warn about large files/repos.
#'
#' @returns (invisibly) a list with `output_dir`, `n_files_copied`,
#'   `n_studies`, `descriptions` (paths of written dataset_description.json
#'   files), `collection` (path of the root `ro-crate-metadata.json` for a
#'   multi-study collection, else empty), `fulltext` (path of the paper's full-text file
#'   under `documentation/`, if written), and `logs` (paths written into
#'   `logs/`: the file manifest, and the check results / module tables when those
#'   modules ran). When an existing `output_dir` was skipped, the list
#'   additionally contains `existed = TRUE` and the counts are zero.
#'
#' @details
#' If the reused/re-run chain includes `reproducibility_check`'s result AND that
#' call was made with `keep_sandbox = TRUE`, its `statistical_output/` folder
#' (extracted, STATO-typed statistics — see [reproducibility_check()]) is copied
#' into `output_dir/statistical_output/`, sibling to `output_dir/data/`. This
#' converter does **not** run `reproducibility_check` itself and does not force
#' `keep_sandbox = TRUE` on your behalf (doing so would mean every conversion
#' also runs — or even executes — the paper's code, whether or not you asked
#' for statistical output); when the chain does not carry a kept sandbox, this
#' folder is silently omitted from the archive.
#' @export
#' @examples
#' \dontrun{
#' # Capture the checks, then build the folder — the modules are reused, not
#' # re-run, and the OSF listing is not fetched again:
#' res <- report(paper, c("data_check", "codebook_check", "psychds_check"))
#' convert_psychds(res)
#'
#' # Or call directly on the paper; the modules are run as needed:
#' convert_psychds(paper)
#' }
convert_psychds <- function(paper, output_dir = NULL,
                            refresh_osf = FALSE,
                            local_path = NULL, local_only = FALSE,
                            model = llm_model(), params = list(),
                            overwrite = FALSE,
                            download = "all",
                            skip_types = NULL, peek_zips = FALSE,
                            max_file_size = 100, max_download_size = 500,
                            cache = FALSE) {
  # ── Gather the placement plan + data behind it ──────────────────────────────
  # Reuse the session's cached OSF listing unless a fresh one is requested, so
  # building right after the checks doesn't re-query (and risk throttling) OSF.
  old_osf_cache <- getOption("metacheck.osf.cache", TRUE)
  options(metacheck.osf.cache = !isTRUE(refresh_osf))
  on.exit(options(metacheck.osf.cache = old_osf_cache), add = TRUE)

  # Reuse whatever the checks already computed (when `paper` is a captured
  # report()/report_module_run() result); otherwise run the chain, so
  # codebook_check / psychds_check consume data_check's result via
  # get_prev_outputs.
  needed <- c("data_check", "codebook_check", "psychds_check")
  resolved <- .converter_resolve(paper, needed,
                                 local_path = local_path, local_only = local_only,
                                 model = model, params = params,
                                 download = download, skip_types = skip_types,
                                 peek_zips = peek_zips,
                                 max_file_size = max_file_size,
                                 max_download_size = max_download_size,
                                 cache = cache)
  ops     <- resolved$ops
  all_ops <- resolved$all_ops %||% ops   # full run for the provenance logs
  paper <- resolved$paper
  dc <- ops[["data_check"]]
  structure_df <- dc$structure
  columns_df   <- dc$table
  labels_df <- ops[["codebook_check"]]$table
  scales_osd <- ops[["codebook_check"]]$scales_osd
  plan      <- ops[["psychds_check"]]$table
  pid <- resolved$pid
  if (is.null(output_dir)) output_dir <- file.path("psychds", pid)

  if (is.null(plan) || nrow(plan) == 0) {
    # No shared data to convert, but the paper is still worth archiving: write a
    # metadata-only collection carrying the manuscript full text and the
    # provenance logs (checks + manifest). The root gets an `ro-crate-metadata.json`
    # (with zero study parts) — the SAME uniform shape every metacheck paper root
    # has. NOTE: trove_find_collections() still keys on the old `collection.json`
    # filename and will not discover roots written under the new name; it is
    # superseded by scienceverse and left as-is pending its own removal. We do
    # NOT write scales here — text-only scales inferred from prose are not
    # archived.
    message("No data files to convert: writing a metadata-only collection ",
            "(manuscript full text + logs).", .converter_gated_hint(ops))

    if (dir.exists(output_dir) && !overwrite) {
      message("Psych-DS output already exists, skipping: ", output_dir,
              ". Set overwrite = TRUE to replace it.")
      return(invisible(list(
        output_dir = output_dir, n_files_copied = 0L, n_studies = 0L,
        descriptions = character(0), skipped = character(0),
        existed = TRUE, empty_plan = TRUE
      )))
    }
    if (dir.exists(output_dir) && overwrite) unlink(output_dir, recursive = TRUE)
    dir.create(output_dir, recursive = TRUE, showWarnings = FALSE)

    # Manuscript full text into documentation/fulltext.txt (flat => not pid-qualified).
    fulltext_path <- .psychds_write_paper_text(paper, output_dir, pid,
                                               multi_study = FALSE)

    # Provenance logs (checks JSON, manifest when available) + README entry.
    logs_written <- .psychds_write_logs(ops, structure_df, pid, output_dir,
                                        all_ops = all_ops)
    .psychds_write_logs_readme(logs_written, output_dir,
                               file.path(output_dir, "README.md"))

    # Dependency list (packages the code loads) at the archive root.
    .psychds_write_requirements(all_ops, output_dir, paper = paper,
                                local_path = local_path, local_only = local_only)

    # Root ro-crate-metadata.json indexing the full text and logs (no study parts).
    root_norm <- normalizePath(output_dir, winslash = "/", mustWork = FALSE)
    to_rel <- function(p) {
      if (is.null(p) || !length(p)) return(character(0))
      pn <- normalizePath(p, winslash = "/", mustWork = FALSE)
      ifelse(startsWith(pn, paste0(root_norm, "/")),
             substring(pn, nchar(root_norm) + 2L), pn)
    }
    collection_path <- .psychds_rocrate_json(
      paper, output_dir, pid, study_roots = character(0),
      fulltext_rel = to_rel(fulltext_path),
      logs_rel     = to_rel(names(logs_written)),
      open_flags   = .psychds_open_flags(all_ops),
      ops          = all_ops)

    message("Wrote metadata-only Psych-DS collection to ",
            normalizePath(output_dir, mustWork = FALSE),
            " (0 data file(s), ro-crate-metadata.json",
            if (!is.null(fulltext_path)) ", paper full text" else "",
            if (length(logs_written) > 0)
              paste0(", ", length(logs_written), " log file(s)") else "",
            ").\n")

    return(invisible(list(
      output_dir = output_dir,
      n_files_copied = 0L,
      n_studies = 0L,
      descriptions = character(0),
      collection = collection_path %||% character(0),
      fulltext = fulltext_path %||% character(0),
      logs = names(logs_written),
      skipped = character(0),
      copy_failed = character(0),
      empty_plan = TRUE
    )))
  }

  if (dir.exists(output_dir) && !overwrite) {
    message("Psych-DS output already exists, skipping: ", output_dir,
            ". Set overwrite = TRUE to replace it.")
    return(invisible(list(
      output_dir = output_dir, n_files_copied = 0L, n_studies = 0L,
      descriptions = character(0), skipped = character(0), existed = TRUE
    )))
  }
  if (dir.exists(output_dir) && overwrite) unlink(output_dir, recursive = TRUE)
  dir.create(output_dir, recursive = TRUE, showWarnings = FALSE)

  # A local copy is needed to copy the file's bytes. Resolve each plan row to its
  # source POSITIONALLY: psychds_check builds its table straight from
  # structure_df (one row per file, same order), so plan row i is structure row i.
  #
  # A name-keyed lookup (setNames(file_location, file_name)) is WRONG here and
  # silently corrupts the output: `file_name` is a basename and is NOT unique
  # across a paper's repositories — several OSF components each ship their own
  # `demographics.csv` / `learning_2.csv`. `loc[[name]]` returns only the FIRST
  # match, so every study's copy of a duplicated name got the same (first)
  # component's bytes: study-ex2/data/demographics.csv contained study 1's
  # participants. No column is unique enough to key on (in one real paper: 138
  # rows, 68 distinct file_name, 102 distinct file_path, 118 distinct
  # file_location), so the row index is the only correct identifier.
  #
  # Fall back to the name map only if the plan is NOT row-aligned with
  # structure_df (a caller passing a filtered/re-ordered plan), where positional
  # lookup would be worse than an imperfect name match.
  row_aligned <- nrow(plan) == nrow(structure_df) &&
    identical(as.character(plan$file_name), as.character(structure_df$file_name))
  loc_fallback <- stats::setNames(structure_df$file_location,
                                  structure_df$file_name)
  src_of <- function(i) {
    if (row_aligned) structure_df$file_location[i]
    else loc_fallback[[plan$file_name[i]]]
  }
  if (!row_aligned)
    warning("The conversion plan is not row-aligned with the file structure; ",
            "falling back to a file-name lookup, which cannot tell apart ",
            "same-named files from different repositories.", call. = FALSE)

  # ── Copy files to their target locations ────────────────────────────────────
  n_copied    <- 0L
  skipped     <- character(0) # files not on disk (never downloaded)
  skipped_i   <- integer(0)   # their plan-row indices, to group by type below
  copied_i    <- integer(0)   # plan rows actually written into the dataset
  copy_failed <- character(0) # files on disk that failed to copy (I/O error)
  # Does the plan mark this file for CSV conversion (a non-CSV data source)?
  plan_convert <- if ("convert" %in% names(plan)) isTRUE_vec(plan$convert) else
    rep(FALSE, nrow(plan))
  plan_orig_target <- if ("original_target" %in% names(plan))
    plan$original_target else rep(NA_character_, nrow(plan))

  for (i in seq_len(nrow(plan))) {
    # An NA target marks a row psychds_check deliberately EXCLUDED from the release
    # (a consumed `archive` container whose inner files were already extracted to
    # their own rows). Skip it silently: it is not "missing on disk" (do not add it
    # to `skipped`), it is intentionally not copied. Without this guard,
    # file.path(output_dir, NA) would write a bogus file literally named "NA".
    if (is.na(plan$target_path[i]) || !nzchar(plan$target_path[i])) next
    src <- src_of(i)
    if (is.null(src) || is.na(src) || !nzchar(src) || !file.exists(src)) {
      skipped   <- c(skipped, plan$file_name[i])
      skipped_i <- c(skipped_i, i)
      next
    }
    dest <- file.path(output_dir, plan$target_path[i])
    dir.create(dirname(dest), recursive = TRUE, showWarnings = FALSE)

    if (isTRUE(plan_convert[i])) {
      # Non-CSV data source (xlsx/sav/dta/...): write a REAL CSV at the _data.csv
      # target from the fully-read data (not a byte copy of the original, which
      # would be an invalid CSV), and keep the untouched original beside it so
      # the release retains the authored artifact.
      wrote_csv <- .psychds_write_data_csv(src, dest)
      if (wrote_csv) {
        n_copied <- n_copied + 1L
        copied_i <- c(copied_i, i)
      } else copy_failed <- c(copy_failed, plan$file_name[i])
      # A .jasp also bundles the ANALYSES that were run: recover them as a
      # readable code artifact beside the data CSV, so the release carries both
      # the data (CSV) and the analysis provenance (the "code").
      if (grepl("\\.jasp$", src, ignore.case = TRUE))
        .psychds_write_jasp_code(src, sub("_data\\.csv$", "_jasp_code.txt", dest))
      # A .omv (jamovi) likewise bundles the analyses that were run; recover them
      # as the jamovi counterpart artifact beside the data CSV.
      if (grepl("\\.omv$", src, ignore.case = TRUE))
        .psychds_write_omv_code(src, sub("_data\\.csv$", "_omv_code.txt", dest))
      # Copy the original alongside (original extension).
      if (!is.na(plan_orig_target[i]) && nzchar(plan_orig_target[i])) {
        odest <- file.path(output_dir, plan_orig_target[i])
        dir.create(dirname(odest), recursive = TRUE, showWarnings = FALSE)
        if (file.copy(src, odest, overwrite = TRUE)) {
          n_copied <- n_copied + 1L
          if (!wrote_csv) copied_i <- c(copied_i, i)
        } else copy_failed <- c(copy_failed, plan$file_name[i])
      }
    } else if (grepl("\\.csv$", dest, ignore.case = TRUE) &&
        grepl("^(study-[^/]+/)?data/", plan$target_path[i])) {
      # Data CSVs must be BOM-free: a UTF-8 BOM makes the first column header
      # read as "﻿id", which then mismatches variableMeasured (a Psych-DS
      # error).
      ok <- .psychds_copy_no_bom(src, dest)
      if (ok) {
        n_copied <- n_copied + 1L
        copied_i <- c(copied_i, i)
      } else copy_failed <- c(copy_failed, plan$file_name[i])
    } else if (file.copy(src, dest, overwrite = TRUE)) {
      n_copied <- n_copied + 1L
      copied_i <- c(copied_i, i)
    } else {
      copy_failed <- c(copy_failed, plan$file_name[i])
    }
  }

  # ── Study roots: derive from the target paths' study-<group>/ prefixes ───────
  # Only from plan rows whose file actually landed on disk: a study group whose
  # every file was skipped (never downloaded) must not become a directory
  # holding nothing but a generated dataset_description.json with an empty
  # variableMeasured — that is noise, not a dataset.
  planned_dirs <- unique(sub("^(study-[^/]+)/.*$", "\\1",
                             grep("^study-", plan$target_path, value = TRUE)))
  study_dirs <- unique(sub("^(study-[^/]+)/.*$", "\\1",
                           grep("^study-", plan$target_path[copied_i],
                                value = TRUE)))
  empty_study_dirs <- setdiff(planned_dirs, study_dirs)
  if (length(empty_study_dirs) > 0)
    message(sprintf(paste0(
      "%d planned study director%s omitted because none of %s files were ",
      "available on disk: %s."),
      length(empty_study_dirs),
      if (length(empty_study_dirs) == 1) "y was" else "ies were",
      if (length(empty_study_dirs) == 1) "its" else "their",
      paste(empty_study_dirs, collapse = ", ")))
  study_roots <- if (length(planned_dirs) > 0) study_dirs else ""

  # ── Write dataset_description.json per study root ───────────────────────────
  descriptions <- character(0)
  for (sr in study_roots) {
    # Columns belonging to this study root (by group), else all when flat.
    if (nzchar(sr)) {
      grp <- sub("^study-", "", sr)
      root_cols <- if (!is.null(columns_df) && "group" %in% names(columns_df))
        columns_df[!is.na(columns_df$group) & columns_df$group == grp, ,
                   drop = FALSE] else columns_df[0, , drop = FALSE]
      study_label <- paste("Study", toupper(grp))
    } else {
      root_cols <- columns_df
      study_label <- ""
    }
    pv <- .psychds_variable_measured(root_cols, labels_df)
    # Source data files landing in this root's data/ folder, resolved to their
    # online download URLs. The plan is row-aligned with structure_df (see the
    # copy loop), so a data-target plan row i maps to structure row i, whose
    # file_name is the source basename that carries the URL.
    data_prefix <- paste0(if (nzchar(sr)) paste0(sr, "/") else "", "data/")
    # `%in% TRUE` folds the NA that startsWith() returns for excluded rows (NA
    # target; see the copy loop) down to FALSE, so an excluded container never
    # leaks an NA file_name into the study's distribution metadata below.
    is_root_data <- startsWith(plan$target_path, data_prefix) %in% TRUE
    root_data_files <- if (row_aligned)
      structure_df$file_name[is_root_data] else plan$file_name[is_root_data]
    distribution <- .psychds_distribution(root_data_files, structure_df)
    desc <- .psychds_dataset_description(paper, study_label, pv, distribution)
    dest <- file.path(output_dir, sr, "dataset_description.json")
    .psychds_write_json(desc, dest)
    descriptions <- c(descriptions, dest)

    # README / CHANGES stubs when missing at this root.
    root_has <- function(pat) any(grepl(pat,
      basename(plan$target_path[startsWith(plan$target_path,
        if (nzchar(sr)) paste0(sr, "/") else "")]), ignore.case = TRUE))
    # Psych-DS expects README and CHANGES to carry a .md/.txt extension.
    readme_dest  <- file.path(output_dir, sr, "README.md")
    changes_dest <- file.path(output_dir, sr, "CHANGES.md")
    if (!root_has("^README") && !file.exists(readme_dest))
      writeLines(c(paste0("# ", desc$name),
                   "", "Psych-DS dataset generated by metacheck.",
                   "See dataset_description.json for machine-readable metadata."),
                 readme_dest)
    if (!root_has("^CHANGES") && !file.exists(changes_dest))
      writeLines(c(paste0(
        format(Sys.Date(), "%Y-%m-%d"),
        " - Repository converted by ",
        "[Metacheck](https://www.scienceverse.org/metacheck/). CHANGES.md ",
        "document added, which can be used to log the version history of the ",
        "dataset (describing changes, updates and corrections).")),
        changes_dest)
  }

  # ── Multi-study collection root ─────────────────────────────────────────────
  # In a multi-study repository the output root is a COLLECTION of datasets (each
  # study-<group>/), not itself a Psych-DS dataset — so it gets NO root
  # dataset_description.json (Option A: a Psych-DS validator run on the root
  # correctly reports it is not a dataset; validate each study-<group>/ instead).
  # It does get BIDS-style root README/CHANGES, and a machine-readable
  # ro-crate-metadata.json (RO-Crate 1.3 JSON-LD, with a custom metacheck/ddi context
  # for DDI-Lifecycle-inspired study-design/variable terms). ro-crate-metadata.json
  # is deliberately NOT named dataset_description.json, so the Psych-DS
  # validator — which only ever opens a file of that exact name — never sees it.
  multi_study <- length(planned_dirs) > 0
  if (multi_study) {
    root_has_root <- function(pat) any(grepl(pat,
      basename(plan$target_path[!grepl("^study-", plan$target_path)]),
      ignore.case = TRUE))
    readme_dest  <- file.path(output_dir, "README.md")
    changes_dest <- file.path(output_dir, "CHANGES.md")
    if (!root_has_root("^README") && !file.exists(readme_dest))
      writeLines(c(
        "# Multi-study data collection",
        "",
        paste0("This folder is a collection of ", length(study_roots),
               " Psych-DS datasets, one per study, generated by metacheck. Each ",
               "`study-*/` subfolder is a complete, independently-valid Psych-DS ",
               "dataset with its own `dataset_description.json`. Files shared ",
               "across studies (this README, cross-study codebooks and ",
               "materials) live here at the collection root."),
        "",
        paste0("See `ro-crate-metadata.json` for a machine-readable description ",
               "of the collection and its parts (RO-Crate 1.3 JSON-LD)."),
        "",
        paste0("**Note:** the collection root is intentionally *not* itself a ",
               "Psych-DS dataset (it has no root `dataset_description.json`). To ",
               "validate, point the validator at each `study-*/` folder.")),
        readme_dest)
    if (!root_has_root("^CHANGES") && !file.exists(changes_dest))
      writeLines(c(paste0(
        format(Sys.Date(), "%Y-%m-%d"),
        " - Collection assembled by ",
        "[Metacheck](https://www.scienceverse.org/metacheck/). CHANGES.md ",
        "added to log the version history of the collection.")),
        changes_dest)
  }
  # ro-crate-metadata.json itself is written near the end, once the full text
  # and logs exist, so it can index them as parts too (see below).

  # ── Write identified scales as OpenScales OSD files ─────────────────────────
  # One .osd per NAMED scale, flat at scales/{code}.osd, plus a section in the
  # dataset-root README explaining the provenance markers. Unnamed detections are
  # skipped (they are not scales). See .scales_to_osd() / .osd_write_scales().
  #
  # Trial-level PARADATA (response times, trial/stimulus channels from Behaverse /
  # Inquisit / E-Prime / jsPsych source files) is normalised to Behaverse `trial`
  # documents at paradata/<instrument>.json (see R/behaverse-convert.R) — one file
  # per instrument, the full response data, nothing deleted. The OSD and the
  # paradata file cross-reference each other on the canonical instrument id, so we
  # pre-scan the paradata instrument keys, write the OSDs (which embed the link),
  # then write the paradata (linking back to the matching OSDs).
  paradata_keys <- .bh_paradata_keys(output_dir)
  osd_codes     <- .osd_write_scales(scales_osd, output_dir, structure_df,
                                     paradata_keys = paradata_keys)
  n_scales_written <- attr(osd_codes, "n_written") %||% 0L
  paradata_index <- .osd_write_paradata(output_dir, osd_codes = as.character(osd_codes),
                                        study_name = pid, model = model, params = params)

  # ── Write the paper's full text into documentation/ (always at the root) ─────
  # The release should carry the manuscript prose the checks read, not just the
  # data. multi_study is true when the plan produced study-<group>/ subtrees, in
  # which case the shared root documentation/ file is pid-qualified.
  fulltext_path <- .psychds_write_paper_text(
    paper, output_dir, pid, multi_study = length(planned_dirs) > 0)

  # ── Write the provenance logs/ folder (manifest, checks, tables) ─────────────
  # Fold the previously-separate manifest / checks / tables artifacts into the
  # conversion: the manifest is always written (full one reused when data_check
  # produced it, else reduced), the checks/tables are written when the modules
  # that generate them ran. Each written file is explained in the root README.
  logs_written <- .psychds_write_logs(ops, structure_df, pid, output_dir,
                                      all_ops = all_ops)
  .psychds_write_logs_readme(logs_written, output_dir,
                             file.path(output_dir, "README.md"))

  # Dependency list (packages the code loads) at the archive root.
  .psychds_write_requirements(all_ops, output_dir, paper = paper,
                              local_path = local_path, local_only = local_only)

  # ── Multi-study collection metadata (ro-crate-metadata.json) ────────────────
  # Written last so it can index every part that exists: each study dataset, the
  # root-level shared files (codebooks/materials/documentation), the paper full
  # text, and the logs. Not named dataset_description.json, so the Psych-DS
  # validator ignores it. Single-study (flat) datasets get no
  # ro-crate-metadata.json — their root dataset_description.json already
  # describes them.
  collection_path <- NULL
  if (length(planned_dirs) > 0) {
    # Paths of the extra generated artifacts, relative to the output root.
    root_norm <- normalizePath(output_dir, winslash = "/", mustWork = FALSE)
    to_rel <- function(p) {
      if (is.null(p) || !length(p)) return(character(0))
      pn <- normalizePath(p, winslash = "/", mustWork = FALSE)
      ifelse(startsWith(pn, paste0(root_norm, "/")),
             substring(pn, nchar(root_norm) + 2L), pn)
    }
    collection_path <- .psychds_rocrate_json(
      paper, output_dir, pid, study_roots,
      labels_df = labels_df,
      shared_files = grep("^study-", plan$target_path, value = TRUE,
                          invert = TRUE),
      fulltext_rel = to_rel(fulltext_path),
      logs_rel     = to_rel(names(logs_written)),
      paradata     = paradata_index,
      open_flags   = .psychds_open_flags(all_ops),
      ops          = all_ops,
      plan         = plan)
  }

  # Explain the files that were NOT placed in the dataset. A file is not added
  # when it was not downloaded — which can be intentional (an asset the release
  # links to rather than mirrors, via `skip_types`), or because `download` was
  # not `"all"`, or a size cap skipped it. The converter only sees "not on disk",
  # so it states the fact and points to the manifest for the per-file reason,
  # rather than prescribing a single (possibly wrong) fix. The breakdown by type
  # usually makes the cause obvious (e.g. all assets => a deliberate skip_types).
  if (length(skipped) > 0) {
    n_total <- nrow(plan)
    types   <- tolower(plan$data_type[skipped_i] %||% "other")
    types[is.na(types) | !nzchar(types)] <- "other"
    by_type <- sort(table(types), decreasing = TRUE)
    breakdown <- paste(sprintf("%d %s", by_type, names(by_type)), collapse = ", ")
    only_assets <- length(by_type) == 1 && names(by_type)[1] == "asset"
    hint <- if (only_assets)
      paste0("These are assets (stimuli/media); if you excluded them on purpose ",
             "(skip_types = \"asset\") the release should link to them instead of ",
             "hosting them.")
    else
      paste0("Files are only added when downloaded. Run data_check with ",
             "download = \"all\" (and without skip_types) to include more; a ",
             "per-file reason is recorded in the data_check manifest.")
    message(
      sprintf(paste0(
        "%d of %d repository file%s %s not added to the Psych-DS dataset ",
        "(not downloaded; by type: %s).\n  %s"),
        length(skipped), n_total, plural(length(skipped)),
        if (length(skipped) == 1) "was" else "were", breakdown, hint))
  }

  if (length(copy_failed) > 0)
    message(
      sprintf("%d file%s could not be copied despite being downloaded ",
              length(copy_failed), plural(length(copy_failed))),
      "(a read/write error): ",
      paste(utils::head(copy_failed, 5), collapse = ", "),
      if (length(copy_failed) > 5) ", ..." else "")

  message("Wrote Psych-DS ",
          if (!is.null(collection_path)) "collection" else "dataset",
          " to ", normalizePath(output_dir, mustWork = FALSE),
          " (", n_copied, " file(s), ", length(descriptions),
          " dataset description(s)",
          if (!is.null(collection_path)) ", ro-crate-metadata.json" else "",
          if (n_scales_written > 0) paste0(", ", n_scales_written, " scale(s)") else "",
          if (length(paradata_index) > 0)
            paste0(", ", length(paradata_index), " paradata file(s)") else "",
          if (!is.null(fulltext_path)) ", paper full text" else "",
          if (length(logs_written) > 0)
            paste0(", ", length(logs_written), " log file(s)") else "",
          ").\n")
  if (!is.null(collection_path))
    message("  The collection root is not itself a Psych-DS dataset; ",
            "validate each study-*/ folder (see ro-crate-metadata.json / README).")

  invisible(list(
    output_dir     = output_dir,
    n_files_copied = n_copied,
    n_studies      = length(study_roots),
    descriptions   = descriptions,
    collection     = collection_path %||% character(0),
    n_scales       = n_scales_written,
    n_paradata     = length(paradata_index),
    fulltext       = fulltext_path %||% character(0),
    logs           = names(logs_written),
    skipped        = skipped,
    copy_failed    = copy_failed
  ))
}

# Write the identified scales as OpenScales OSD files under output_dir/scales/,
# one file per named scale, flat at scales/{code}.osd, and append a
# provenance-explaining section to the dataset-root README.md. Objects flagged
# `write = FALSE` (unnamed detections) are skipped. Returns the number written.
.osd_write_scales <- function(scales_osd, output_dir, structure_df = NULL,
                              paradata_keys = character(0)) {
  if (is.null(scales_osd) || !length(scales_osd))
    return(structure(character(0), n_written = 0L))
  writeable <- Filter(function(o) isTRUE(attr(o, "write")), scales_osd)
  if (!length(writeable)) return(structure(character(0), n_written = 0L))

  index <- list()   # rows for the README: code, name, source, provenance
  used <- character(0)   # codes already written, to disambiguate collisions
  for (osd in writeable) {
    code <- attr(osd, "code") %||% "SCALE"
    # Disambiguate a repeated code so a later scale does not overwrite an earlier
    # one at scales/{code}.osd. This is common for unnamed blocks, where several
    # non-adjacent same-prefix runs (e.g. three "response" blocks) all slug to the
    # same name. Suffix -2, -3, ... on collision, and keep the OSD's own
    # scale_info$code in sync with the path it is written to.
    if (code %in% used) {
      i <- 2L
      while (paste0(code, "-", i) %in% used) i <- i + 1L
      code <- paste0(code, "-", i)
      osd$definition$scale_info$code <- code
    }
    used <- c(used, code)
    # Attach the online download URL of each source data file next to the scale's
    # existing source_files list, so a reader of the .osd can jump to the data the
    # scale was extracted from. Same file_url-then-repo_url rule as the dataset
    # distribution. Parallel to source_files; omitted when no URL resolves.
    src_files <- osd$definition$metacheck$source_files
    if (!is.null(src_files) && length(src_files)) {
      urls <- vapply(unlist(src_files, use.names = FALSE),
                     function(fn) .psychds_source_url(fn, structure_df) %||% NA_character_,
                     character(1))
      urls <- urls[!is.na(urls) & nzchar(urls)]
      if (length(urls))
        osd$definition$metacheck$source_urls <- as.list(unname(urls))
    }
    # Files live flat, next to each other: scales/<code>.osd (no per-scale
    # subfolder). Guard against over-long paths (OneDrive/Windows' ~260-char
    # limit); .safe_write_path shortens + warns.
    osd_path <- .safe_write_path(file.path(output_dir, "scales",
                                           paste0(code, ".osd")))
    # Cross-reference to the instrument's Behaverse paradata file, when trial-
    # level paradata for this instrument was (or will be) written. The join key is
    # the canonical instrument id; the OSD points at ../paradata/<key>.json and the
    # Behaverse Instrument.link points back here. Emitted as namespaced keys the
    # OSD spec ignores (same mechanism as metacheck:reference_item).
    key <- .bh_instrument_key(code)
    if (key %in% paradata_keys) {
      osd$definition$scale_info[["metacheck:behaverseInstrumentId"]] <- key
      osd$definition$scale_info[["metacheck:paradata"]] <-
        paste0("../paradata/", key, ".json")
    }

    dir.create(dirname(osd_path), recursive = TRUE, showWarnings = FALSE)
    json <- jsonlite::toJSON(osd, auto_unbox = TRUE, pretty = TRUE, null = "null")
    writeLines(json, osd_path, useBytes = TRUE)
    mc <- osd$definition$metacheck %||% list()
    index[[length(index) + 1L]] <- list(
      code = code,
      name = osd$definition$scale_info$name %||% "",
      source = mc$scale_source %||% "",
      provenance = mc$provenance %||% "")
  }

  .osd_write_readme_section(index, file.path(output_dir, "README.md"))
  # The canonical instrument keys of the scales written, so paradata linking uses
  # exactly the same key set.
  attr(used, "n_written") <- length(writeable)
  used
}

# Append (or create) a "Psychometric scales" section to the dataset-root README,
# listing each written scale and explaining the three provenance markers so the
# archive is not mistaken for an authoritative registry.
.osd_write_readme_section <- function(index, readme_path) {
  if (!length(index)) return(invisible())
  lines <- c(
    "",
    "## Psychometric scales",
    "",
    paste0("metacheck identified ", length(index), " psychometric scale",
           plural(length(index)),
           " in this dataset and exported each as an OpenScales OSD file under ",
           "`scales/{code}.osd`. This is an archive of what metacheck *found*, ",
           "not an authoritative scale registry. The **Provenance** column below ",
           "marks how confidently each scale was identified:"),
    "",
    "- **dictionary match** — matched a known instrument in metacheck's scale dictionary (OpenScales-derived or curated).",
    "- **named in manuscript** — a named instrument identified from the manuscript text, not in the dictionary.",
    "- **metacheck-generated label** — a construct label *generated by metacheck* from the item wording. This is **not** a recognised named instrument, only metacheck's inference of what the items measure. Treat it as a starting point, not a definition.",
    "- **detected block, unnamed** — a coherent block of same-prefix rating columns that metacheck could **not** name. Recorded for its structure (its items and response scale) only.",
    "",
    "| Code | Scale | Provenance |",
    "|------|-------|------------|")
  rows <- vapply(index, function(r) sprintf("| `%s` | %s | %s |",
    r$code, if (nzchar(r$name)) r$name else "(unnamed)",
    switch(r$source,
      dictionary = "dictionary match",
      manuscript = "named in manuscript",
      self_generated = "metacheck-generated label",
      unnamed_block = "detected block, unnamed",
      r$source)), character(1))

  con_lines <- if (file.exists(readme_path))
    readLines(readme_path, warn = FALSE) else character(0)
  writeLines(c(con_lines, lines, rows, ""), readme_path, useBytes = TRUE)
  invisible()
}
