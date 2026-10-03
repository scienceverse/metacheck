# bibr export schema 12.x: read, and write with paper_write(schema_version)

bibr12 <- function(name) test_path("fixtures", "bibr12", paste0(name, ".json"))

test_that("read a bibr 12.0 export", {
  path <- bibr12("PMC4383902")
  json <- jsonlite::read_json(path)
  paper <- read(path)

  expect_s3_class(paper, "scivrs_paper")
  expect_true(paper_validate(paper))
  expect_equal(paper$paper_id, "PMC4383902")

  # metadata and source make the info table
  expect_equal(paper$info$title, json$metadata$title)
  expect_equal(paper$info$doi, "10.1093/nar/gku1061")
  expect_equal(paper$info$abstract, json$metadata$abstract)
  expect_equal(paper$info$file_name, "PMC4383902.xml")
  expect_equal(paper$info$sha256, json$source$sha256)
  expect_equal(paper$info$input_format, "jats")
  expect_equal(paper$info$schema_version, "12.0")

  # a group author, and the affiliation table
  expect_equal(paper$author$literal, "The Europe PMC Consortium")
  expect_true(is.na(paper$author$family))
  expect_equal(nrow(paper$affiliation), 4)
  expect_equal(paper$affiliation$author_ids[[1]], 1L)

  # sections in order, and the sentences
  expect_equal(paper$section$section_id, seq_along(json$section))
  expect_equal(paper$section$header,
               vapply(json$section, \(s) s$header, character(1)))
  expect_equal(paper$text$text, vapply(json$text, \(t) t$text, character(1)))

  # references, and a citation that resolves to its reference
  expect_equal(paper$bib$bib_id, 1:17)
  cite <- paper$xref[paper$xref$xref_type == "bib", ][1, ]
  expect_equal(cite$contents, "(1)")
  expect_match(paper$text$text[[cite$text_id]], "NAR database issue (1)",
               fixed = TRUE)
  ref <- paper$bib[paper$bib$bib_id == cite$target_id, ]
  expect_equal(ref$title, "UKPMC: a full text article resource for the life sciences")
  expect_equal(ref$doi, "10.1093/nar/gkq1063")

  # a figure: label, caption and its caption row, which has no section
  fig <- paper$figure[1, ]
  expect_equal(fig$label, "1")
  expect_match(fig$caption, "^Figure 1\\. \\(a\\) Total scope of content")
  expect_equal(paper$text$text[[fig$text_id]], fig$caption)
  expect_true(is.na(paper$text$section_id[[fig$text_id]]))
  expect_in(fig$section_id, paper$section$section_id)
  ref <- paper$xref[paper$xref$xref_type == "figure", ][1, ]
  expect_equal(ref$contents, "Figure 1a")
  expect_equal(ref$target_id, fig$figure_id)

  # a footnote
  expect_equal(paper$footnote$label, "\u2020")
  note <- paper$text[paper$footnote$text_id, ]
  expect_match(note$text, "list of authors of the Europe PMC Consortium")
  expect_true(is.na(note$section_id))

  # no caption or footnote pseudo-sections: every section holds body text
  body <- paper$text[!paper$text$text_id %in%
                       c(paper$figure$text_id, paper$footnote$text_id), ]
  expect_false(anyNA(body$section_id))

  # text search still finds captions and footnotes
  found <- text_search(paper, "list of authors of the Europe PMC Consortium")
  expect_equal(found$text_id, paper$footnote$text_id)

  # how the paper was made
  expect_equal(paper$extraction$producer$name, "bibr")
  expect_null(paper$extraction$converter)
  expect_equal(paper$extraction$warnings[[1]]$code, "STATEMENT_LEXICAL_FALLBACK")
})

test_that("read a bibr 12.0 table", {
  paper <- read(bibr12("probe_html"))
  expect_true(paper_validate(paper))

  tbl <- paper$table[1, ]
  expect_equal(tbl$label, "1")
  expect_equal(tbl$caption, "Table 1. Values")
  expect_equal(paper$text$text[[tbl$text_id]], "Table 1. Values")
  expect_true(is.na(paper$text$section_id[[tbl$text_id]]))
  expect_equal(paper$table$contents[[1]], list(c("A", "B"), c("1", "2")))

  # references that name no row have no target
  sup <- paper$xref[paper$xref$xref_type %in% c("equation", "section",
                                                 "supplementary"), ]
  expect_equal(nrow(sup), 4)
  expect_true(all(is.na(sup$target_id)))
})

test_that("read bibr 12.0 statistics, matches and images", {
  paper <- read(bibr12("full"))
  expect_true(paper_validate(paper))

  # metacheck keeps degrees of freedom in parentheses; 12.x writes "28"
  expect_equal(paper$eq$df, "(28)")
  expect_equal(paper$eq$verbatim, "t(28) = 3.42")

  # match tables, with the given/family columns metacheck's modules read
  expect_equal(paper$info_match$score, 0.99)
  expect_equal(paper$bib_match$author[[1]][[1]]$family, "Smith")
  expect_equal(paper$bib_match$authors[[1]],
               data.frame(given = "Jane", family = "Smith"))
  expect_equal(paper$affiliation_match$service_id, "https://ror.org/0abcde123")
  expect_equal(paper$funding_match$funder_doi, "10.13039/100000001")

  # figure images are dropped unless asked for
  docx <- read(bibr12("probe_docx"))
  expect_true(is.na(docx$figure$image))
  docx <- read(bibr12("probe_docx"), include_images = TRUE)
  expect_match(docx$figure$image, "^data:image/png;base64,")
})

test_that("modules read citations of bibr 12.x papers", {
  paper <- read(bibr12("PMC4383902"))
  n_cite <- sum(paper$xref$xref_type == "bib")

  mo <- module_run(paper, "ref_consistency")
  expect_equal(mo$summary_table$n_bib, 17)
  expect_equal(mo$summary_table$n_xrefs, n_cite)
})

test_that("ref_accuracy scores bibr 12.x matches 0-1", {
  paper <- read(bibr12("full"))
  # a reference without a DOI, and a match scoring 0.98 with one (the module
  # suggests the DOIs of matches that have no title)
  paper$bib$doi <- NA_character_
  paper$bib$text_id <- 2L
  paper$bib_match$title <- NA_character_
  expect_equal(paper$bib_match$score, 0.98)

  mo <- module_run(paper, "ref_accuracy")
  expect_match(paste(mo$report, collapse = "\n"), "10.1234/prior", fixed = TRUE)

  # for an older paper, 0.98 is far below the default suggest_score of 70
  paper$info$schema_version <- NA_character_
  mo <- module_run(paper, "ref_accuracy")
  expect_no_match(paste(mo$report, collapse = "\n"), "10.1234/prior",
                  fixed = TRUE)
})

test_that("a later bibr 12.x file reads, ignoring keys it does not know", {
  json <- jsonlite::read_json(bibr12("probe_html"))
  json$schema_version <- "12.1"
  json$new_table <- list(list(new_id = 1L))
  json$metadata$new_field <- "new"
  json$text[[1]]$new_field <- "new"
  json$extraction$new_block <- list(new_field = "new")
  path <- file.path(withr::local_tempdir(), "probe_html.json")
  jsonlite::write_json(json, path, auto_unbox = TRUE, null = "null")

  paper <- read(path)
  orig <- read(bibr12("probe_html"))
  expect_true(paper_validate(paper))
  expect_equal(paper$info$schema_version, "12.1")
  expect_equal(names(paper), names(orig))
  expect_equal(paper$info$title, orig$info$title)
  expect_equal(paper$text, orig$text)
  expect_equal(paper$extraction$new_block, list(new_field = "new"))
})

test_that("only bibr schema 12.x is read from files with a schema_version", {
  json <- jsonlite::read_json(bibr12("probe_html"))
  path <- file.path(withr::local_tempdir(), "v11.json")

  json$schema_version <- "11.0"
  jsonlite::write_json(json, path, auto_unbox = TRUE, null = "null")
  expect_error(.read_bibr(path), "schema 11.0 is not supported")

  json$schema_version <- "13.0"
  jsonlite::write_json(json, path, auto_unbox = TRUE, null = "null")
  expect_error(.read_bibr(path), "schema 13.0 is not supported")
})

test_that("paper_write writes bibr 12.0 that reads back the same", {
  save_path <- withr::local_tempdir()

  for (name in c("PMC4383902", "probe_html", "full", "preprint")) {
    path <- bibr12(name)
    paper <- read(path)
    json_path <- paper_write(paper, save_path = save_path,
                             schema_version = "12.0")
    if (requireNamespace("jsonvalidate", quietly = TRUE)) {
      expect_valid_bibr12(json_path)
    }

    # nothing is lost: the rewrite differs from bibr's JSON only in converter
    orig <- jsonlite::read_json(path)
    json <- jsonlite::read_json(json_path)
    expect_null(orig$extraction$converter)
    expect_equal(json$extraction$converter,
                 list(name = "metacheck",
                      version = as.character(packageVersion("metacheck")),
                      build_sha = NULL))
    orig$extraction["converter"] <- NULL
    json$extraction["converter"] <- NULL
    expect_equal(json, orig)

    # and it reads back the same paper
    paper2 <- read(json_path)
    paper$extraction["converter"] <- NULL
    paper2$extraction["converter"] <- NULL
    expect_equal(paper2, paper)
  }

  # images are kept when read with them
  paper <- read(bibr12("probe_docx"), include_images = TRUE)
  json_path <- paper_write(paper, save_path = save_path,
                           schema_version = "12.0")
  expect_equal(jsonlite::read_json(json_path)$figure,
               jsonlite::read_json(bibr12("probe_docx"))$figure)
})

test_that("paper_write keeps its default output", {
  paper <- read(bibr12("probe_html"))
  save_path <- withr::local_tempdir()

  # without schema_version, the paper object is saved as before
  json_path <- paper_write(paper, save_path = save_path)
  json <- jsonlite::read_json(json_path)
  expect_null(json$schema_version)
  expect_equal(json$info[[1]]$title, paper$info$title)

  # 12.0 needs a 12.x paper
  expect_error(paper_write(demopaper(), save_path = save_path,
                           schema_version = "12.0"),
               "metacheck's older format")
  expect_error(paper_write(paper, save_path = save_path,
                           schema_version = "11.0"),
               "schema_version must be NULL or \"12.0\"")

  # a paper read from a later 12.x file is not rewritten
  paper$info$schema_version <- "12.1"
  expect_error(paper_write(paper, save_path = save_path,
                           schema_version = "12.0"),
               "read from bibr export schema 12.1")
})

test_that("paper_write converts bib_match rows from add_bib_match()", {
  paper <- read(bibr12("full"))
  # the columns add_bib_match() makes: given/family authors, CrossRef scores
  paper$bib_match <- data.frame(
    bib_id = 1L, service = "crossref", service_id = NA_character_,
    score = 61.7, bib_type = "article", doi = "10.1234/PRIOR",
    title = "A Prior Study", publisher = NA_character_, year = 2020L,
    date = NA_character_, container = "Journal of Things"
  )
  paper$bib_match$authors <- list(data.frame(given = "Jane", family = "Smith"))
  paper$bib_match$editors <- list(data.frame(given = character(0),
                                             family = character(0)))

  json_path <- paper_write(paper, save_path = withr::local_tempdir(),
                           schema_version = "12.0")
  if (requireNamespace("jsonvalidate", quietly = TRUE)) {
    expect_valid_bibr12(json_path)
  }

  json <- jsonlite::read_json(json_path)
  match <- json$bib_match[[1]]
  expect_equal(match$author, list(list(given = "Jane", family = "Smith")))
  expect_null(match$editor)
  expect_null(match$score)
  expect_equal(match$bib_type, "journal_article")
  expect_equal(match$doi, "10.1234/prior")
  codes <- vapply(json$extraction$warnings, \(w) w$code, character(1))
  expect_in("METACHECK_MATCH_SCORE_NOT_0_1", codes)
})
