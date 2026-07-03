#' Preregistration Check
#'
#' @description
#' Retrieve information from preregistrations in a standardised way,
#' and make them easier to check.
#'
#' @details
#' The Preregistration Check module identifies preregistrations on the OSF and AsPredicted based on links in the manuscript, retrieves the preregistration text, and organizes the information into a template. The module then uses regular expressions to identify text from AsPredicted, and the API to retrieve text from the OSF. The information in the preregistration is returned.
#'
#' The module can’t extract information from non-structured preregistration templates (i.e., where the preregistration is uploaded in a single text field) and it can’t retrieve information in preregistrations that are stored as text documents on the OSF.
#'
#' OSF "Open-Ended Registration" objects are ambiguous: the template is used both to preregister a study and simply to archive data or materials, so the presence of an Open-Ended Registration does not on its own establish that a study was preregistered (see \doi{10.1177/25152459241296031}). The `oer` argument controls how these are treated:
#'
#' \describe{
#'   \item{`"include"`}{(default) every Open-Ended Registration is counted as a preregistration. This preserves the historical behaviour, but over-counts: in a development sample of 71 open-ended registrations from psychology papers it treated all of them as preregistrations while 15 (about one in five) were data/materials archives. `"text"` or `"llm"` are recommended for accuracy.}
#'   \item{`"text"`}{an Open-Ended Registration is counted as a preregistration only if the sentence describing its link in the manuscript contains an explicit, non-negated preregistration label (or pre-analysis plan / study protocol), attributed to that link so a preregistration at a different link does not count. High precision, so it rarely miscounts an archive, but it under-counts some genuine preregistrations. No API needed.}
#'   \item{`"text_broad"`}{as `"text"` but with broader keywords that match any registration mention (including trial registries and retrospectively registered studies). Higher recall, lower specificity - use when you would rather over-flag registrations for a human to review than miss any. No API needed.}
#'   \item{`"llm"`}{an LLM classifies each Open-Ended Registration, using the manuscript's description of its link as the primary signal and the registration's own OSF description as a fallback when the manuscript is uninformative. It was the most accurate mode in the development sample, but it requires an API key (`llm_use(TRUE)`) and is not deterministic; it falls back to `"text"` when LLM use is not enabled (the report states which method ran).}
#' }
#'
#' Dedicated preregistration templates (e.g. OSF Preregistration, AsPredicted) and AsPredicted links are always counted regardless of `oer`. Open-Ended Registrations that are not counted are still listed in the report so they can be checked manually.
#'
#' If you want to extend the package to be able to download information from other preregistration sites, reach out to the Metacheck development team.

#'
#' @keywords method
#'
#' @author Daniel Lakens (\email{D.Lakens@tue.nl})
#' @author Lisa DeBruine (\email{lisa.debruine@glasgow.ac.uk})
#'
#' @import dplyr
#' @import tidyr
#' @import httr
#' @import jsonlite
#'
#' @param paper a paper object or paperlist object
#' @param oer how to treat OSF "Open-Ended Registration" objects: `"include"` (count all, the default), `"text"` (count only when the manuscript describes the link as a preregistration), `"text_broad"` (as `"text"` but with broader, higher-recall keywords), or `"llm"` (classify with an LLM, requires `llm_use(TRUE)`). See Details.
#'
#' @returns a list
prereg_check <- function(paper, oer = c("include", "text", "text_broad", "llm")) {
  oer <- match.arg(oer)
  # paper <- psychsci[[218]] # to test
  # and paper <- xml[["09567976251396084"]] for multiple aspredicted
  # osf: paper <- xml[["09567976221114055"]]
  # osf: paper <- psychsci[31:40]

  # table ----
  links_ap <- aspredicted_links(paper)
  links_osf <- osf_links(paper)

  ## no links ----
  if (nrow(links_ap) == 0 && nrow(links_osf) == 0) {
    resp <- list(
      traffic_light = "na",
      summary_text = "No preregistration links were found.",
      summary_table = data.frame(
        paper_id = paper_id(paper),
        preregistration = 0
      )
    )
    return(resp)
  }

  ## AsPredicted preregs ----
  table_ap <- suppressMessages(
    aspredicted_info(links_ap$href)
  )
  ap_schema_table <- ap_schema(table_ap)

  ## OSF prereg ----
  # keep the paper -> guid mapping so OER gating can check the manuscript that
  # actually linked the registration (matters for paperlists)
  osf_link_map <- data.frame(
    paper_id = links_osf$paper_id,
    guid = osf_check_id(links_osf$href),
    stringsAsFactors = FALSE
  )
  osf_link_map <- osf_link_map[!is.na(osf_link_map$guid), , drop = FALSE]
  osf_ids <- unique(osf_link_map$guid)
  link_types <- osf_type(osf_ids)
  reg_ids <- osf_ids[link_types == "registrations" & !is.na(link_types)]

  ## no registrations ----
  if (length(reg_ids) == 0 & nrow(table_ap) == 0) {
    resp <- list(
      traffic_light = "na",
      summary_text = sprintf(
        "We found %d OSF link%s, but no registrations.",
        nrow(links_osf), nrow(links_osf) |> plural()
      ),
      na_replace = 0,
      summary_table = data.frame(
        paper_id = paper_id(paper),
        preregistration = 0
      )
    )
    return(resp)
  }

  ## get reg info from OSF ----
  urls <- sprintf(
    "https://api.osf.io/v2/registrations/%s",
    reg_ids #paste(reg_ids, collapse = ",")
  )

  # have to iterate, process then merge
  # because pagination > 10 usually returns unmergeable dfs
  ps <- lapply(urls, \(url) {
    reg_info <- osf_get_all_pages(url)

    if (length(reg_info) == 0) return(NULL)

    info <- reg_info
    template <- info$attributes$registration_supplement
    osf31 <- info$attributes$registration_responses$q25

    if (info$attributes$withdrawn) {
      withdrawn(info)
    } else if (template == "OSF Preregistration" &&
      !is.null(osf31) && !is.na(osf31)) {
      osf_pr_31(info)
    } else if (template == "OSF Preregistration") {
      osf_pr_28(info)
    } else if (template == "Open-Ended Registration") {
      oer_schema(info)
    } else if (template == "Prereg Challenge") {
      prc(info)
    } else if (template == "Preregistration Template from AsPredicted.org") {
      prap(info)
    } else if (template == "Pre-Registration in Social Psychology (van 't Veer & Giner-Sorolla, 2016): Pre-Registration") {
      prsp(info)
    } else if (template == "Replication Recipe (Brandt et al., 2013): Pre-Registration") {
      rrbrandt(info)
    } else if (template == "OSF-Standard Pre-Data Collection Registration") {
      osfpre(info)
    }
  })

  ## OER gating ----
  # "Open-Ended Registration" is ambiguous (preregistration vs data/materials
  # archive). Depending on `oer`, only count it when the manuscript corroborates
  # it (or, for "llm", when the classifier does). Each schema carries its own
  # template_name, id and (for OERs) description, so we gate off the schema list.
  templates <- vapply(ps, function(s) {
    if (is.null(s)) NA_character_ else as.character(s$template_name %||% NA_character_)[1]
  }, character(1))
  guids <- vapply(ps, function(s) {
    if (is.null(s)) NA_character_ else as.character(s$id %||% NA_character_)[1]
  }, character(1))
  is_oer <- !is.na(templates) & templates == "Open-Ended Registration"

  reg_confirmed <- rep(TRUE, length(ps))
  if (any(is_oer) && oer %in% c("text", "text_broad")) {
    kw <- if (oer == "text_broad") PREREG_KW_BROAD else PREREG_KW
    neg <- if (oer == "text_broad") NEG_PREREG_BROAD else NEG_PREREG
    reg_confirmed <- vapply(seq_along(ps), function(i) {
      if (!is_oer[i]) return(TRUE)
      linking <- osf_link_map$paper_id[osf_link_map$guid == guids[i]]
      has_nonneg_prereg(oer_link_sentence(paper, guids[i], linking), kw, neg)
    }, logical(1))
  } else if (any(is_oer) && oer == "llm") {
    reg_confirmed <- oer_llm_confirmed(paper, ps, is_oer, guids, osf_link_map)
  }
  # which method actually ran ("llm" falls back to "text" when llm_use() is off)
  oer_used <- if (oer == "llm" && !llm_use()) "text" else oer

  # confirmed registrations (+ AsPredicted) are counted; unconfirmed Open-Ended
  # Registrations are surfaced separately for manual review
  unconfirmed_schemas <- ps[is_oer & !reg_confirmed]
  ps <- ps[reg_confirmed]

  # make sure all items are not lists
  prereg_schemas <- c(ps, list(ap_schema_table)) |>
    lapply(\(x) lapply(x, paste, collapse = "\n\n"))
  prereg_info <- do.call(dplyr::bind_rows, prereg_schemas)

  if (nrow(prereg_info)) {
    paper_ids <- data.frame(
      paper_id = c(links_ap$paper_id, links_osf$paper_id),
      link = c(links_ap$href, links_osf$href)
    )
    paper_ids$link <- gsub("^(https://)?", "https://", paper_ids$link)

    prereg_info <- dplyr::left_join(prereg_info, paper_ids, by = "link")
  }

  ## unconfirmed Open-Ended Registrations + guidance ----
  collapse_schema <- function(schemas) {
    if (length(schemas) == 0) return(data.frame())
    do.call(dplyr::bind_rows,
      lapply(schemas, \(x) lapply(x, paste, collapse = "\n\n")))
  }
  unconfirmed_info <- collapse_schema(unconfirmed_schemas)

  guidance <- c(
    "For metascientific articles demonstrating the rate of deviations from preregistrations, see:",
    format_ref(vandenAkker2024),
    "For educational material on how to report deviations from preregistrations, see:",
    format_ref(Lakens2024),
    "On how OSF registrations (including open-ended ones) are created and shared, see:",
    format_ref(Ensinck2025)
  )

  unconfirmed_note <- NULL
  if (nrow(unconfirmed_info) > 0) {
    unconfirmed_link_table <- data.frame(
      id = link(unconfirmed_info$link, unconfirmed_info$id),
      title = unconfirmed_info$title,
      template = unconfirmed_info$template_name
    )
    reason <- if (oer_used == "llm") {
      "an LLM did not classify them as preregistrations"
    } else {
      sprintf(
        "the manuscript does not describe %s as a preregistration",
        ifelse(nrow(unconfirmed_info) == 1, "its link", "their links")
      )
    }
    unconfirmed_note <- c(
      sprintf(
        "We found %d OSF Open-Ended Registration%s that %s not counted as a preregistration because %s. Open-Ended Registrations are often used to archive data or materials rather than to preregister a study, so please check %s manually.",
        nrow(unconfirmed_info), nrow(unconfirmed_info) |> plural(),
        ifelse(nrow(unconfirmed_info) == 1, "is", "are"), reason,
        ifelse(nrow(unconfirmed_info) == 1, "it", "them")
      ),
      scroll_table(unconfirmed_link_table)
    )
  }

  ## no confirmed preregistrations ----
  if (nrow(prereg_info) == 0) {
    resp <- list(
      traffic_light = "na",
      summary_text = if (nrow(unconfirmed_info) == 0) {
        "We found registrations, but none could be confirmed as preregistrations."
      } else {
        sprintf(
          "We found %d OSF Open-Ended Registration%s, but could not confirm %s as a preregistration.",
          nrow(unconfirmed_info), nrow(unconfirmed_info) |> plural(),
          ifelse(nrow(unconfirmed_info) == 1, "it", "them")
        )
      },
      na_replace = 0,
      summary_table = data.frame(
        paper_id = paper_id(paper),
        preregistration = 0
      ),
      report = c(unconfirmed_note, collapse_section(guidance))
    )
    return(resp)
  }

  # traffic light ----
  tl <- "info"

  # summary_text ----
  summary_text <- sprintf(
    "We found %d preregistration%s.",
    nrow(prereg_info), nrow(prereg_info) |> plural()
  )

  # report ----
  has_sample_size <- "sample_size" %in% names(prereg_info)
  report_text <- sprintf(
    "Meta-scientific research has shown that deviations from preregistrations are often not reported or checked, and that the most common deviations concern the sample size. We recommend manually checking the full preregistration at the link%s above%s.",
    nrow(prereg_info) |> plural(),
    ifelse(has_sample_size, ", and have provided the preregistered sample size", "")
  )

  prereg_link_table <- data.frame(
    id = link(prereg_info$link, prereg_info$id),
    title = prereg_info$title,
    template = prereg_info$template_name
  )

  if (has_sample_size) {
    samplesize_table <- prereg_info[, c("id", "sample_size")]
  } else {
    samplesize_table <- NULL
  }

  ## summary output for paperlists ----
  summary_table <- dplyr::count(prereg_info, paper_id,
    name = "preregistration",
    .drop = FALSE
  )

  ## prereg table ----
  # Remove columns where all values are NA
  prereg_table <- prereg_info[
    , colSums(!is.na(prereg_info)) > 0
  ] |>
    t() |>
    as.data.frame()

  # Add row names as a proper column (first column)
  prereg_table <- cbind(Field = rownames(prereg_table), prereg_table)

  # Rename columns "Preregistration 1", "Preregistration 2", ...
  n_prereg <- ncol(prereg_table) - 1 # subtract the 'Field' column
  colnames(prereg_table)[-1] <- paste0("Preregistration ", seq_len(n_prereg))

  report <- c(
    summary_text,
    scroll_table(prereg_link_table),
    report_text,
    scroll_table(samplesize_table),
    unconfirmed_note,
    collapse_section(
      scroll_table(prereg_table, maxrows = 5),
      "Full Preregistration"
    ),
    collapse_section(guidance)
  )

  # return a list ----
  list(
    table = prereg_info,
    summary_table = summary_table,
    na_replace = 0,
    traffic_light = tl,
    report = report,
    summary_text = summary_text
  )
}

# references

vandenAkker2024 <- bibentry(
  bibtype = "Article",
  title = "The potential of preregistration in psychology: Assessing preregistration producibility and preregistration-study consistency",
  author = c(
    person("O. R.", "van den Akker"),
    person("M.", "Bakker"),
    person("M. A. L. M.", "van Assen"),
    person("C. R.", "Pennington"),
    person("L.", "Verweij"),
    person("M. M.", "Elsherif"),
    person("A.", "Claesen"),
    person("S. D. M.", "Gaillard"),
    person("S. K.", "Yeung"),
    person("J.-L.", "Frankenberger"),
    person("K.", "Krautter"),
    person("J. P.", "Cockcroft"),
    person("K. S.", "Kreuer"),
    person("T. R.", "Evans"),
    person("F. M.", "Heppel"),
    person("S. F.", "Schoch"),
    person("M.", "Korbmacher"),
    person("Y.", "Yamada"),
    person("N.", "Albayrak-Aydemir"),
    person("J. M.", "Wicherts")
  ),
  journal = "Psychological Methods",
  year = 2024,
  doi = "10.1037/met0000687"
)

Lakens2024 <- bibentry(
  bibtype = "Article",
  author  = "Lakens, Daniël",
  year    = 2024,
  title   = "When and How to Deviate From a Preregistration",
  journal = "Collabra: Psychology",
  volume  = 10,
  number  = 1,
  pages   = "117094",
  doi     = "10.1525/collabra.117094"
)

Ensinck2025 <- bibentry(
  bibtype = "Article",
  author  = c(
    person(c("Eline", "N.", "F."), "Ensinck"),
    person("Daniël", "Lakens")
  ),
  year    = 2025,
  title   = "An Inception-Cohort Study Quantifying How Many Registered Studies Are Publicly Shared",
  journal = "Advances in Methods and Practices in Psychological Science",
  volume  = 8,
  number  = 1,
  doi     = "10.1177/25152459241296031"
)

# helper functions ----

## Open-Ended Registration gating ----
# Positive keywords for a preregistration-like object: explicit preregistration
# labels, AsPredicted, pre-analysis plans, and (scoped) study/registered
# protocols. NEG_PREREG marks negated mentions ("not preregistered").
PREREG_KW <- "pre[-\\s]*regist|aspredicted|registered on the osf|osf\\s+registration|time-?stamped (?:hypothes|preregist)|pre[-\\s]?analysis plan|(?:study|registered|trial)\\s+protocol"
NEG_PREREG <- "(\\bno(?:ne|t)?\\b|\\bnon-?\\b|\\bnever\\b|\\bwithout\\b|\\bneither\\b|wasn'?t|weren'?t|did not|was not|were not|not been)[^.;]{0,40}pre[-\\s]*regist|non-?pre[-\\s]*regist|pre[-\\s]*regist[^.;]{0,25}\\b(was|were)\\b[^.;]{0,15}\\b(not|none)\\b"
# broad keywords for oer = "text_broad": any registration mention (also matches
# trial registries, trading specificity for recall). The negation also excludes
# explicitly post-hoc / retrospective registrations, which are not preregistrations.
PREREG_KW_BROAD <- "regist|aspredicted|analysis plan|time-?stamped hypothes"
NEG_PREREG_BROAD <- "(\\bno(?:ne|t)?\\b|\\bnon-?\\b|\\bnever\\b|\\bwithout\\b|\\bneither\\b|wasn'?t|weren'?t|did not|was not|were not|not been)[^.;]{0,40}regist|non-?regist|regist[^.;]{0,25}\\b(was|were)\\b[^.;]{0,15}\\b(not|none)\\b|(?:post[-\\s]?hoc|retrospective(?:ly)?)[^.;]{0,25}regist|regist[^.;]{0,25}(?:post[-\\s]?hoc|retrospective)"

# TRUE if the evidence text contains an explicit, non-negated preregistration
# statement (negation is kept sentence-local so a nearby unrelated "not" does
# not cancel a genuine statement).
has_nonneg_prereg <- function(text_vec, kw = PREREG_KW, neg = NEG_PREREG) {
  if (length(text_vec) == 0) {
    return(FALSE)
  }
  sents <- unlist(strsplit(paste(text_vec, collapse = " "),
    "(?<=[.!?])\\s+",
    perl = TRUE
  ))
  sents <- sents[grepl(kw, sents, perl = TRUE, ignore.case = TRUE)]
  if (length(sents) == 0) {
    return(FALSE)
  }
  any(!grepl(neg, sents, perl = TRUE, ignore.case = TRUE))
}

# Manuscript evidence for a specific link: the full sentence(s) that mention it in
# the linking paper(s), used by BOTH the text gate and the LLM. A whole sentence
# keeps a coordinated shared subject ("The initial registration ... and an update
# ...") with the link, while a neighbouring sentence about a different object is
# excluded. Returns character(0) when the link is not in the body text (footnote /
# reference target). References excluded.
oer_link_sentence <- function(paper, guid, linking_ids) {
  para <- text_search(paper, guid, return = "paragraph")
  if (is.null(para) || nrow(para) == 0) return(character(0))
  id_col <- if ("paper_id" %in% names(para)) "paper_id" else "id"
  para <- para[para[[id_col]] %in% linking_ids, , drop = FALSE]
  sect_col <- intersect(c("section_type", "section"), names(para))
  if (length(sect_col)) {
    s <- para[[sect_col[1]]]
    para <- para[is.na(s) | s != "references", , drop = FALSE]
  }
  if (nrow(para) == 0) return(character(0))
  text <- paste(para$text, collapse = " ")
  # de-space URLs GROBID may have broken ("osf. io/") so a link (and its sentence)
  # stays intact through sentence-splitting
  text <- gsub("https?\\s*:\\s*/\\s*/\\s*", "https://", text, ignore.case = TRUE)
  text <- gsub("osf\\s*\\.\\s*io\\s*/\\s*", "osf.io/", text, ignore.case = TRUE)
  sents <- unlist(strsplit(text, "(?<=[.!?])\\s+", perl = TRUE))
  # case-insensitive: OSF links also appear uppercase / in DOI form (OSF.IO/2F6QS)
  unique(trimws(sents[grepl(tolower(guid), tolower(sents), fixed = TRUE)]))
}

# Ask an LLM to classify each Open-Ended Registration as a preregistration vs a
# data/materials archive, from its OSF description plus the manuscript sentence
# describing its link. `schemas` is the list of registration schema objects and
# `guids`/`is_oer` are aligned to it. Returns a logical vector aligned to
# `schemas`. Requires llm_use(TRUE); falls back to the text gate when not enabled.
oer_llm_confirmed <- function(paper, schemas, is_oer, guids, osf_link_map) {
  conf <- rep(TRUE, length(schemas))
  idx <- which(is_oer)
  if (length(idx) == 0) {
    return(conf)
  }

  if (!llm_use()) {
    message("oer = 'llm' requires llm_use(TRUE); falling back to oer = 'text'.")
    conf[idx] <- vapply(idx, function(i) {
      linking <- osf_link_map$paper_id[osf_link_map$guid == guids[i]]
      has_nonneg_prereg(oer_link_sentence(paper, guids[i], linking))
    }, logical(1))
    return(conf)
  }

  texts <- vapply(idx, function(i) {
    descr <- paste(schemas[[i]]$description %||% "", collapse = " ")
    linking <- osf_link_map$paper_id[osf_link_map$guid == guids[i]]
    ev <- paste(oer_link_sentence(paper, guids[i], linking), collapse = " ")
    paste0(
      "OSF object description: ", descr,
      "\n\nManuscript text describing THIS link: ", ev
    )
  }, character(1))

  system_prompt <- paste(
    "Decide whether a specific OSF link is used as a PREREGISTRATION or is just an ARCHIVE of data/materials.",
    "Base the decision primarily on the MANUSCRIPT text describing this link:",
    "(1) If the manuscript describes this link's content (data, code, materials, supplement) and does NOT call it",
    "a preregistration, pre-analysis plan, or study/registered protocol, answer FALSE - even if the OSF description",
    "mentions a preregistration, which may refer to the broader project or a different link.",
    "(2) If the manuscript labels this link a preregistration / pre-analysis plan / protocol, answer TRUE; trust the",
    "authors' explicit label (do not judge whether it was methodologically ideal).",
    "(3) Only if the manuscript text is uninformative about this link (e.g. a bare URL) fall back to the OSF",
    "description: answer TRUE if that description is itself a preregistration (states hypotheses and/or an analysis",
    "plan set in advance) or labels the object as one; otherwise FALSE.",
    "Answer FALSE if the preregistration mention is negated. Answer with exactly one word: TRUE or FALSE."
  )

  res <- llm(texts, system_prompt, params = list(seed = 1))
  conf[idx] <- grepl("^TRUE", toupper(trimws(res$answer)))
  conf
}

## AsPredicted Schema

ap_schema <- function(table_ap) {
  if (nrow(table_ap) == 0) {
    return(data.frame())
  }

  ap_id <- table_ap$ap_url |>
    sub("^https://aspredicted\\.org/", "", x = _) |>
    sub("\\.pdf.*", "", x = _) |>
    sub("blind\\.php\\?x\\=", "", x = _)

  data.frame(
    template_name = "AsPredicted",
    id = ap_id,
    link = table_ap$ap_url,
    title = table_ap$AP_title,
    date_created = table_ap$AP_created,
    existing_data_explanation = table_ap$AP_data,
    research_questions = table_ap$AP_hypotheses,
    design_dependent_variables = table_ap$AP_key_dv,
    study_design_overview = table_ap$AP_conditions,
    statistical_tests = table_ap$AP_analyses,
    outliers_and_exclusions = table_ap$AP_outliers,
    sample_size = table_ap$AP_sample_size,
    additional_comments = table_ap$AP_anything_else
  )
}

## Common OSF
common_osf <- function(info) {
  ra <- info$attributes

  list(
    template_name = ra$registration_supplement,
    title = ra$title,
    id = info$id,
    link = paste0("https://osf.io/", info$id),
    date_created = ra$date_created,
    date_modified = ra$date_modified,
    date_registered = ra$date_registered,
    embargo_end_date = ra$embargo_end_date,
    ia_url = ra$ia_url
  )
}


## Withdrawn ----
withdrawn <- function(info) {
  ra <- info$attributes

  common <- common_osf(info)
  extra <- list(
    description = "WITHDRAWN"
  )

  c(common, extra)
}

## OSF-Standard Pre-Data Collection Registration ----
osfpre <- function(info) {
  ra <- info$attributes
  prereg_answers <- ra$registration_responses

  common <- common_osf(info)

  extra <- list(
    data_collection_started = prereg_answers$datacompletion,
    data_looked = prereg_answers$looked,
    additional_comments = prereg_answers$comments
  )

  c(common, extra)
}

## Open-Ended Registration ----
oer_schema <- function(info) {
  ra <- info$attributes
  prereg_answers <- ra$registration_responses

  common <- common_osf(info)

  extra <- list(
    description = prereg_answers$summary,
    sample_size = "The authors did not use a template. See the full preregistration for the sample size."
  )

  c(common, extra)
}

## Preregistration Template from AsPredicted.org ----
prap <- function(info) {
  ra <- info$attributes
  prereg_answers <- ra$registration_responses

  common <- common_osf(info)

  extra <- list(
    data_collection_started = prereg_answers$data,
    research_questions = prereg_answers$hypothesis,
    design_dependent_variables = prereg_answers$dependent,
    study_design_overview = prereg_answers$conditions,
    statistical_tests = prereg_answers$analyses,
    outliers_and_exclusions = prereg_answers$outliers,
    sample_size = prereg_answers$sample,
    study_type = paste(c(
      prereg_answers$study_type,
      prereg_answers$study_type_other
    ), collapse = " "),
    additional_comments = prereg_answers$other
  )

  c(common, extra)
}


## OSF Preregistration 31 ----

osf_pr_31 <- function(info) {
  ra <- info$attributes
  prereg_answers <- ra$registration_responses

  common <- common_osf(info)

  extra <- list(
    authors = prereg_answers$q2,
    description = prereg_answers$q3,
    research_questions = prereg_answers$q4,
    study_type = prereg_answers$q5,
    blinding = paste(c(
      prereg_answers$q6,
      prereg_answers$q7
    ), collapse = " "),
    study_design_overview = prereg_answers$q8.question,
    randomization = prereg_answers$q9,
    data_collection_started = prereg_answers$q10,
    existing_data_explanation = prereg_answers$q11,
    data_collection_procedures = prereg_answers$q12.question,
    sample_size = prereg_answers$q13,
    sample_size_rationale = prereg_answers$q14,
    stopping_rule = prereg_answers$q15,
    design_independent_variables = prereg_answers$q16.question,
    design_dependent_variables = prereg_answers$q17.question,
    indices = prereg_answers$q18.question,
    statistical_tests = prereg_answers$q19.question,
    transformations = prereg_answers$q20,
    inference_criteria = prereg_answers$q21,
    data_exclusion_criteria = prereg_answers$q22,
    outliers_and_exclusions = prereg_answers$q23,
    exploratory_analyses = prereg_answers$q24,
    additional_comments = prereg_answers$q25
  )

  c(common, extra)
}

## OSF Preregistration 28 ----
osf_pr_28 <- function(info) {
  ra <- info$attributes
  prereg_answers <- ra$registration_responses

  common <- common_osf(info)

  extra <- list(
    description = prereg_answers$q2,
    # research_questions = prereg_answers$q3,
    study_type = prereg_answers$q3,
    blinding = paste(c(
      prereg_answers$q4,
      prereg_answers$q5
    ), collapse = " "),
    study_design_overview = prereg_answers$q6.question,
    # randomization = prereg_answers$q9,
    data_collection_started = prereg_answers$q8,
    existing_data_explanation = prereg_answers$q9,
    data_collection_procedures = prereg_answers$q10.question,
    sample_size = prereg_answers$q11,
    sample_size_rationale = prereg_answers$q12,
    stopping_rule = prereg_answers$q13,
    design_independent_variables = prereg_answers$q14.question,
    design_dependent_variables = prereg_answers$q15.question,
    indices = prereg_answers$q16.question,
    statistical_tests = prereg_answers$q17.question,
    # statistical_tests = prereg_answers$q20,
    inference_criteria = prereg_answers$q19,
    data_exclusion_criteria = prereg_answers$q20,
    outliers_and_exclusions = prereg_answers$q21,
    exploratory_analyses = prereg_answers$q22,
    additional_comments = prereg_answers$q23
  )

  c(common, extra)
}

## Prereg Challenge ----
prc <- function(info) {
  ra <- info$attributes
  prereg_answers <- ra$registration_responses

  blinding <- {
    x <- unlist(prereg_answers$q15, use.names = FALSE)
    if (length(x) == 0) NA_character_ else paste(x, collapse = " ")
  }

  common <- common_osf(info)

  extra <- list(
    description = prereg_answers$q3,
    research_questions = prereg_answers$q4,
    data_collection_started = prereg_answers$q5,
    existing_data_explanation = prereg_answers$q6,
    data_collection_procedures = prereg_answers$q7.question,
    sample_size = prereg_answers$q8,
    sample_size_rationale = prereg_answers$q9,
    stopping_rule = prereg_answers$q10,
    design_independent_variables = prereg_answers$q11.question,
    design_dependent_variables = prereg_answers$q12.question,
    indices = prereg_answers$q13.question,
    study_type = prereg_answers$q14,
    blinding = blinding,
    study_design_overview = prereg_answers$q16.question,
    randomization = prereg_answers$q17,
    statistical_tests = prereg_answers$q19.question,
    transformations = prereg_answers$q20,
    additional_analyses = prereg_answers$q21,
    inference_criteria = prereg_answers$q22,
    data_exclusion_criteria = prereg_answers$q23,
    outliers_and_exclusions = prereg_answers$q24,
    exploratory_analyses = prereg_answers$q25
  )

  c(common, extra)
}

## Pre-Registration in Social Psychology (van 't Veer & Giner-Sorolla, 2016): Pre-Registration ----
prsp <- function(info) {
  ra <- info$attributes
  prereg_answers <- ra$registration_responses

  common <- common_osf(info)

  extra <- list(
    research_questions =
      paste(c(
        prereg_answers$`description-hypothesis.question1a`,
        prereg_answers$`84-5`
      ), collapse = " "),
    hypotheses_interactions =
      paste(c(
        prereg_answers$`description-hypothesis.question2a`,
        prereg_answers$`84-7`
      ), collapse = " "),
    manipulation_checks =
      paste(c(
        prereg_answers$`description-hypothesis.question3a`,
        prereg_answers$`84-9`
      ), collapse = " "),
    theoretical_rationale =
      paste(c(
        prereg_answers$`recommended-hypothesis.question5a`,
        prereg_answers$`recommended-hypothesis.question6a`,
        prereg_answers$`84-14`, prereg_answers$`84-16`
      ), collapse = " "),
    design_independent_variables =
      paste(c(
        prereg_answers$`description-methods.design.question2a`,
        prereg_answers$`84-23`
      ), collapse = " "),
    design_dependent_variables =
      paste(c(
        prereg_answers$`description-methods.design.question2b`,
        prereg_answers$`84-25`
      ), collapse = " "),
    design_covariates_moderators =
      prereg_answers$`description-methods.design.question3b`,
    data_exclusion_criteria =
      paste(c(
        prereg_answers$`description-methods.planned-sample.question4b`,
        prereg_answers$`84-30`
      ), collapse = " "),
    data_collection_procedures =
      paste(
        c(
          prereg_answers$`description-methods.planned-sample.question5b`,
          prereg_answers$`description-methods.procedure.question10b`,
          prereg_answers$`84-32`, prereg_answers$`84-44`,
          prereg_answers$`84-47`, prereg_answers$`84-49`
        ),
        collapse = " "
      ),
    sample_size =
      paste(c(
        prereg_answers$`description-methods.planned-sample.question6b`,
        prereg_answers$`84-34`, prereg_answers$`84-36`
      ), collapse = " "),
    stopping_rule =
      paste(c(
        prereg_answers$`description-methods.planned-sample.question7b`,
        prereg_answers$`84-38`
      ), collapse = " "),
    outliers_and_exclusions =
      paste(c(
        prereg_answers$`description-methods.exclusion-criteria.question8b`,
        prereg_answers$`84-41`
      ), collapse = " "),
    fail_safe_exclusion_levels = prereg_answers$`recommended-methods.procedure.question9b`,
    indices =
      paste(c(
        prereg_answers$`confirmatory-analyses-first.first.question1c`,
        prereg_answers$`confirmatory-analyses-second.second.question1c`,
        prereg_answers$`confirmatory-analyses-third.third.question1c`,
        prereg_answers$`confirmatory-analyses-fourth.fourth.question1c`,
        prereg_answers$`confirmatory-analyses-further.further.question1c`,
        prereg_answers$`84-56`, prereg_answers$`84-68`,
        prereg_answers$`84-80`, prereg_answers$`84-92`
      ), collapse = " "),
    statistical_tests =
      paste(c(
        prereg_answers$`confirmatory-analyses-first.first.question2c`,
        prereg_answers$`confirmatory-analyses-second.second.question2c`,
        prereg_answers$`confirmatory-analyses-third.third.question2c`,
        prereg_answers$`confirmatory-analyses-fourth.fourth.question2c`,
        prereg_answers$`confirmatory-analyses-further.further.question2c`,
        prereg_answers$`84-58`, prereg_answers$`84-70`,
        prereg_answers$`84-82`, prereg_answers$`84-94`,
        prereg_answers$`84-126`
      ), collapse = " "),
    rationale_covariate =
      paste(c(
        prereg_answers$`confirmatory-analyses-first.first.question3c`,
        prereg_answers$`confirmatory-analyses-second.second.question3c`,
        prereg_answers$`confirmatory-analyses-third.third.question3c`,
        prereg_answers$`confirmatory-analyses-fourth.fourth.question3c`,
        prereg_answers$`confirmatory-analyses-further.further.question3c`,
        prereg_answers$`84-62`, prereg_answers$`84-74`,
        prereg_answers$`84-84`, prereg_answers$`84-96`
      ), collapse = " "),
    variables_roles_in_analyses =
      paste(c(
        prereg_answers$`confirmatory-analyses-first.first.question4c`,
        prereg_answers$`confirmatory-analyses-second.second.question4c`,
        prereg_answers$`confirmatory-analyses-third.third.question4c`,
        prereg_answers$`confirmatory-analyses-fourth.fourth.question4c`,
        prereg_answers$`confirmatory-analyses-further.further.question4c`,
        prereg_answers$`84-60`, prereg_answers$`84-72`,
        prereg_answers$`84-86`, prereg_answers$`84-98`
      ), collapse = " "),
    inference_criteria =
      paste(c(
        prereg_answers$`confirmatory-analyses-first.first.question5c`,
        prereg_answers$`confirmatory-analyses-second.second.question5c`,
        prereg_answers$`confirmatory-analyses-third.third.question5c`,
        prereg_answers$`confirmatory-analyses-fourth.fourth.question5c`,
        prereg_answers$`confirmatory-analyses-further.further.question5c`,
        prereg_answers$`84-64`, prereg_answers$`84-76`,
        prereg_answers$`84-88`, prereg_answers$`84-100`
      ), collapse = " "),
    multiple_testing_correction =
      paste(c(
        prereg_answers$`recommended-analysis.specify.question6c`,
        prereg_answers$`84-116`
      ), collapse = " "),
    missing_data_handling =
      paste(c(
        prereg_answers$`recommended-analysis.specify.question7c`,
        prereg_answers$`84-118`
      ), collapse = " "),
    reliability_criteria =
      paste(c(
        prereg_answers$`recommended-analysis.specify.question8c`,
        prereg_answers$`84-120`
      ), collapse = " "),
    transformations =
      paste(c(
        prereg_answers$`recommended-analysis.specify.question9c`,
        prereg_answers$`84-122`
      ), collapse = " "),
    assumptions_and_contingencies =
      paste(c(
        prereg_answers$`recommended-analysis.specify.question10c`,
        prereg_answers$`84-124`
      ), collapse = " "),
    data_collection_started =
      paste(c(
        prereg_answers$`datacompletion`,
        prereg_answers$`84-130`
      ), collapse = " "),
    data_looked =
      paste(c(
        prereg_answers$`looked`,
        prereg_answers$`84-134`
      ), collapse = " "),
    project_dates_start_end =
      paste(c(
        prereg_answers$`dataCollectionDates`,
        prereg_answers$`84-138`
      ), collapse = " "),
    additional_comments =
      paste(c(
        prereg_answers$`additionalComments`,
        prereg_answers$`84-140`
      ), collapse = " ")
  )

  c(common, extra)
}

## Replication Recipe (Brandt et al., 2013): Pre-Registration" ----
rrbrandt <- function(info) {
  ra <- info$attributes
  prereg_answers <- ra$registration_responses

  common <- common_osf(info)

  extra <- list(
    description = prereg_answers$item1,
    replication_importance = prereg_answers$item2,
    effect_size_original = prereg_answers$item3,
    confidence_interval_original = prereg_answers$item4,
    original_sample_size = prereg_answers$item5,
    original_study_conducted = prereg_answers$item6,
    region = prereg_answers$item7,
    original_population = prereg_answers$item8,
    original_data_collection = prereg_answers$item9,
    original_materials_available = prereg_answers$item10,
    assumptions_and_contingencies = prereg_answers$item11,
    data_collection_location = prereg_answers$item12,
    blinding = paste(
      prereg_answers$item13,
      prereg_answers$item14
    ),
    sample_size = prereg_answers$item15,
    sample_size_rationale = prereg_answers$item16,
    instruction_similarities = prereg_answers$item17,
    measure_similarities = prereg_answers$item18,
    stimuli_similarities = prereg_answers$item19,
    procedure_similarities = prereg_answers$item20,
    location_similarities = prereg_answers$item21,
    remuneration_similarities = prereg_answers$item22,
    participant_similarities = prereg_answers$item23,
    differences_influencing_effects = paste(
      prereg_answers$item24,
      prereg_answers$item25
    ),
    data_exclusion_criteria = prereg_answers$item26,
    statistical_tests = prereg_answers$item27,
    inference_criteria = prereg_answers$item28
  )

  c(common, extra)
}


# prereg schema ----
prereg_schema <- data.frame(
  id = NA_character_,
  date_created = NA_character_,
  template_name = NA_character_,
  registration_narrative_summary = NA_character_,
  title = NA_character_,
  authors = NA_character_,
  description = NA_character_,
  research_questions = NA_character_,
  hypotheses_main = NA_character_,
  hypotheses_interactions = NA_character_,
  manipulation_checks = NA_character_,
  theoretical_rationale = NA_character_,
  additional_comments = NA_character_,
  project_dates_start_end = NA_character_,
  study_type = NA_character_,
  study_design_overview = NA_character_,
  design_independent_variables = NA_character_,
  design_dependent_variables = NA_character_,
  design_covariates_moderators = NA_character_,
  blinding = NA_character_,
  randomization = NA_character_,
  manipulated_variables = NA_character_,
  measured_variables = NA_character_,
  indices = NA_character_,
  existing_data = NA_character_,
  existing_data_explanation = NA_character_,
  data_collection_procedures = NA_character_,
  data_collection_location = NA_character_,
  data_collection_started = NA_character_,
  data_looked = NA_character_,
  sample_size = NA_character_,
  sample_size_rationale = NA_character_,
  stopping_rule = NA_character_,
  fail_safe_exclusion_levels = NA_character_,
  statistical_tests = NA_character_,
  additional_analyses = NA_character_,
  transformations = NA_character_,
  inference_criteria = NA_character_,
  multiple_testing_correction = NA_character_,
  assumptions_and_contingencies = NA_character_,
  variables_roles_in_analyses = NA_character_,
  rationale_covariate = NA_character_,
  reliability_criteria = NA_character_,
  data_exclusion_criteria = NA_character_,
  outliers_and_exclusions = NA_character_,
  missing_data_handling = NA_character_,
  exploratory_analyses = NA_character_,
  replication_description = NA_character_,
  replication_importance = NA_character_,
  effect_size_original = NA_character_,
  confidence_interval_original = NA_character_,
  original_study_conducted = NA_character_,
  region = NA_character_,
  original_sample_size = NA_character_,
  original_population = NA_character_,
  original_data_collection = NA_character_,
  original_materials_available = NA_character_,
  instruction_similarities = NA_character_,
  measure_similarities = NA_character_,
  stimuli_similarities = NA_character_,
  procedure_similarities = NA_character_,
  location_similarities = NA_character_,
  remuneration_similarities = NA_character_,
  participant_similarities = NA_character_,
  differences_influencing_effects = NA_character_,
  date_modified = NA_character_,
  date_registered = NA_character_,
  embargo_end_date = NA_character_,
  ia_url = NA_character_
)

