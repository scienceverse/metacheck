#' Add an author
#'
#' @param study A study list object with class scivrs_study
#' @param surname a character string with the author's last name(s)
#' @param given a character string with the author's given name(s)
#' @param orcid the author's unique ORCiD (see https://orcid.org/)
#' @param roles a vector of roles from the CRediT taxonomy (see https://casrai.org/credit/); use credit_roles() to view the full list
#' @param ... further info to add to author object
#'
#' @return A study object with class scivrs_study
#' @export
#' @keywords internal
#'
add_author <- function(study, surname, given = "",
                       orcid = NULL,
                       roles = c(), ...) {

  idx <- get_idx(study, section = "authors")

  study$authors[[idx]] <- author(surname, given, orcid, roles, ...)

  invisible(study)
}

#' Author in CRediT Format
#'
#' @param surname a character string with the author's last name(s)
#' @param given a character string with the author's given name(s)
#' @param orcid the author's unique ORCiD (see https://orcid.org/)
#' @param roles a vector of roles from the CRediT taxonomy (see https://casrai.org/credit/); use credit_roles() to view the full list
#' @param ... further info to add to author object
#'
#' @return a list with class scivrs_author
#' @export
#' @keywords internal
author <- function(surname, given = "", orcid = NULL, roles = c(), ...) {
  role_names <- credit_roles("name")

  # check roles are in list
  if (is.numeric(roles)) {
    roles <- role_names[roles]
  }

  chk_roles <- names(roles)

  if (is.null(chk_roles)) { chk_roles <- roles }
  bad_roles <- chk_roles[!(chk_roles %in% role_names)]
  if (length(bad_roles)) {
    # check for abbreviations
    bad_roles <- bad_roles[!(bad_roles %in% credit_roles("abbr"))]
    if (length(bad_roles)) {
      stop("These roles do not exist in the CRediT taxonomy: ",
           paste(bad_roles, collapse = ", "), "\n  See http://credit.casrai.org/")
    }
    # convert to correct names
    chk_roles <- role_names[credit_roles("abbr") %in% chk_roles]
  }

  if (!is.null(orcid)) orcid <- check_orcid(orcid)
  a <- list(
    orcid = orcid,
    name = list(surname = surname, given = given),
    roles = chk_roles
  )
  a <- c(a, list(...))

  class(a) <- c("scivrs_author", "list")

  a
}

#' CRediT Roles
#'
#' @param display Whether to display the category names, explanations, or abbreviations
#'
#' @return list of roles
#' @export
#'
#' @examples
#' credit_roles()
credit_roles <- function(display = c("explain", "names", "abbr")) {
  roles <- list(
    "Conceptualization" = "Ideas; formulation or evolution of overarching research goals and aims.",
    "Data curation" = "Management activities to annotate (produce metadata), scrub data and maintain research data (including software code, where it is necessary for interpreting the data itself) for initial use and later re-use.",
    "Formal analysis" = "Application of statistical, mathematical, computational, or other formal techniques to analyse or synthesize study data.",
    "Funding acquisition" = "Acquisition of the financial support for the project leading to this publication.",
    "Investigation" = "Conducting a research and investigation process, specifically performing the experiments, or data/evidence collection.",
    "Methodology" = "Development or design of methodology; creation of models.",
    "Project administration" = "Management and coordination responsibility for the research activity planning and execution.",
    "Resources" = "Provision of study materials, reagents, materials, patients, laboratory samples, animals, instrumentation, computing resources, or other analysis tools.",
    "Software" = "Programming, software development; designing computer programs; implementation of the computer code and supporting algorithms; testing of existing code components.",
    "Supervision" = "Oversight and leadership responsibility for the research activity planning and execution, including mentorship external to the core team.",
    "Validation" = "Verification, whether as a part of the activity or separate, of the overall replication/reproducibility of results/experiments and other research outputs.",
    "Visualization" = "Preparation, creation and/or presentation of the published work, specifically visualization/data presentation.",
    "Writing - original draft" = "Preparation, creation and/or presentation of the published work, specifically writing the initial draft (including substantive translation).",
    "Writing - review & editing" = "Preparation, creation and/or presentation of the published work by those from the original research group, specifically critical review, commentary or revision -- including pre- or post-publication stages."
  )

  abbr <- c(
    "con",
    "dat",
    "ana",
    "fun",
    "inv",
    "met",
    "adm",
    "res",
    "sof",
    "sup",
    "val",
    "vis",
    "dra",
    "edi"
  )

  if ("explain" == display[1]) {
    for (i in 1:length(roles)) {
      cname <- names(roles)[i]
      cdesc <- roles[[i]]
      paste0("[", i, "/", abbr[i], "] ", cname, ": ", cdesc, "\n") |>
        cat()
    }
  } else if ("abbr" == display[1]) {
    abbr
  } else {
    names(roles)
  }
}




#' Check validity of ORCiD
#'
#' @param orcid a 16-character ORCiD in bare or URL format
#'
#' @return a formatted 16-character ORCiD or FALSE
#' @export
#'
#' @examples
#' check_orcid("0000-0002-7523-5539")
#' check_orcid("0000-0002-0247-239X")
#' check_orcid("https://orcid.org/0000-0002-0247-239X")
#' check_orcid("0000-0002-0247-2394") # incorrect, return FALSE
check_orcid <- function(orcid) {
  baseDigits <- gsub("[^0-9X]", "", orcid)

  if (nchar(baseDigits) != 16) {
    if (verbose()) {
      warning("The ORCiD ", orcid, " is not valid.")
    }
    return(FALSE)
  }

  total <- 0
  for (i in 1:(nchar(baseDigits)-1)) {
    digit <- substr(baseDigits, i, i) |> as.integer()
    total <- (total + digit) * 2
  }
  remainder <- total %% 11;
  result <- (12 - remainder) %% 11;
  result <- ifelse(result == 10, "X", result)

  if (result == substr(baseDigits, 16, 16)) {
    paste(substr(baseDigits, 1, 4),
          substr(baseDigits, 5, 8),
          substr(baseDigits, 9, 12),
          substr(baseDigits, 13, 16),
          sep = "-")
  } else {
    if (verbose()) {
      warning("The ORCiD ", orcid, " is not valid.")
    }
    return(FALSE)
  }
}



#' Get ORCiD from Name
#'
#' @param family The family (last) name to search for
#' @param given An optional given (first) name to search for. Initials will be converted from, e.g., L M to L\* M\*
#'
#' @return A vector of matching ORCiDs
#' @export
#'
#' @examples
#' \donttest{
#'   get_orcid("DeBruine", "Lisa")
#' }
get_orcid <- function(family, given = "*") {
  if (is.null(family) || trimws(family) == "") {
    stop("You must include a family name")
  }

  if (is.null(given) || trimws(given) == "") {
    given <- "*"
  }

  query <- "https://pub.orcid.org/v3.0/search/?q=family-name:%s+AND+given-names:%s"

  given2 <- given |>
    trimws() |>
    gsub("^(\\w)\\.?$", "\\1\\*", x = _) |> # single initial
    gsub("^(.)\\.?\\s", "\\1\\* ", x = _) |> # initial initial
    gsub("\\s(.)\\.?$", " \\1\\*", x = _) |> # ending initial
    gsub("\\s(.)\\.?\\s", " \\1\\* ", x = _) |> # internal initial
    utils::URLencode()

  family2 <- trimws(family) |> utils::URLencode()
  url <- sprintf(query, family2, given2) |> url("rb")
  on.exit(close(url))

  xml <- tryCatch(xml2::read_xml(url), error = function(e) {})
  if (is.null(xml)) {
    warning("ORCID search failed")
    return("")
  }

  orcid <- xml_find(xml, "//common:path")

  n <- length(orcid)
  if (n == 0) {
    message("No ORCID found for ", given, " ", family)
  } else if (n > 1) {
    message("Multiple (", n, ") ORCIDs found for ", given, " ", family)
  }

  return(orcid)
}

#' Create a study object
#'
#' Create a new study object
#'
#' @param name The name of the study
#' @param ... further arguments to add
#' @return A study object with class scivrs_study
#'
#' @export
#' @keywords internal
study <- function(name = "Demo Study", ...) {
  # make empty study object
  study <- c(
    list(name = name),
    list(
      info = list(...),
      authors = list(),
      hypotheses = list(),
      methods = list(),
      data = list(),
      analyses = list()
    )
  )

  class(study) <- c("scivrs_study", "list")
  class(study$authors) <- c("scivrs_authors", "list")

  invisible(study)
}

#' Get index from id
#'
#' Get the index from id for an item in the hypotheses, analyses, or data sections of a study object
#'
#' @param study A study list object with class scivrs_study
#' @param id The id for the section (index or character) if NULL, assigns to the last item in the list
#' @param section The section to search, c("hypotheses", "analyses", "data")
#' @return A numeric index
#' @keywords internal
#'
get_idx <- function(study, id = NULL, section = "hypotheses") {
  n <- length(study[[section]])
  idx <- n + 1
  if (length(grep("^\\d+$", id))) { # is an integer
    if (n >= id) idx <- as.numeric(id)
  } else if (is.character(id) & n > 0) {
    for (i in 1:n) {
      if (study[[section]][[i]]$id == id) idx <- i
    }
  }

  return(idx)
}

#' Get Person Details for ORCiD
#'
#' @param orcid a vector of ORCiDs
#'
#' @return A data frame of details
#' @export
#'
#' @examples
#' \donttest{
#'   orcids <- c("0000-0002-0247-239X", "0000-0002-7523-5539")
#'   orcid_person(orcids)
#' }
orcid_person <- function(orcid) {
  details <- lapply(orcid, \(x) {
    path <- file.path("https://pub.orcid.org/v3.0", x, "person")

    xml <- tryCatch({
      url <- suppressWarnings(url(path, "rb"))
      on.exit(close(url))
      xml2::read_xml(url)
    },
    error = function(e) { e$message })

    if (is.character(xml)) {
      return(data.frame(
        orcid = x,
        error = xml
      ))
    }

    list(
      orcid = x,
     # name  = xml_find(xml, "//personal-details:credit-name", ";"),
      given = xml_find(xml, "//personal-details:given-names", " "),
      family =  xml_find(xml, "//personal-details:family-name", " "),
      email = xml_find(xml, "//email:email //email:email") |> list(),
      country = xml_find(xml, "//address:country", ";"),
      keywords = xml_find(xml, "//keyword:content") |> list(),
      urls = xml_find(xml, "//researcher-url:url") |> list()
    )
  })

  dplyr::bind_rows(details)
}
