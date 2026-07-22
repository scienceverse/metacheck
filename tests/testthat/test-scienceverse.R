# Tests for add_to_scienceverse() / scienceverse_checks(): shredding a metacheck
# collection export into a searchable SQLite archive. All run offline against a
# hand-built minimal collection fixture in tempdir() — no convert_psychds(), no
# network, no LLM. The DB packages are optional (Suggests), so skip when absent.

# Build a minimal but complete collection root on disk, matching the shape
# convert_psychds() writes: collection.json + one study-*/dataset_description.json
# + one scales/*.osd + logs/{manifest,checks}.json + logs/<doi>.rds.
# `doi` is the collection.json identifier (a real DOI URL). The folder name is
# deliberately different from the DOI, to prove the key comes from identifier,
# not basename.
make_collection_fixture <- function(doi = "https://doi.org/10.1234/test.001",
                                    pid = "test.001") {
  root <- file.path(tempdir(), paste0("svfix_", as.integer(runif(1, 1, 1e6))))
  dir.create(file.path(root, "study-s1", "data"), recursive = TRUE, showWarnings = FALSE)
  dir.create(file.path(root, "scales"), recursive = TRUE, showWarnings = FALSE)
  dir.create(file.path(root, "logs"), recursive = TRUE, showWarnings = FALSE)
  dir.create(file.path(root, "documentation"), recursive = TRUE, showWarnings = FALSE)

  writeLines(jsonlite::toJSON(list(
    "@context" = "https://schema.org/", "@type" = "Collection",
    name = "Stress and coping under load", identifier = doi,
    description = "A fixture collection.",
    author = list(list(name = "A. Author"), list(name = "B. Writer")),
    keywords = list("stress", "coping"),
    dateCreated = "2026-01-01"
  ), auto_unbox = TRUE, pretty = TRUE), file.path(root, "collection.json"))

  writeLines(jsonlite::toJSON(list(
    "@context" = "https://schema.org/", "@type" = "Dataset",
    name = "Study S1", description = "fixture study", schemaVersion = "Psych-DS 0.4.0",
    variableMeasured = list(
      list("@type" = "PropertyValue", name = "anxiety",
           description = "anxiety score", "metacheck:concept" = "likert",
           "metacheck:measurementLevel" = "ratio", "metacheck:role" = "measure",
           minValue = 1, maxValue = 7,
           "metacheck:statistics" = list(n = 100, mean = 3.5, sd = 1.2),
           "metacheck:source_file" = "s1.csv"),
      list("@type" = "PropertyValue", name = "cond",
           description = "condition", "metacheck:concept" = "nominal",
           "metacheck:measurementLevel" = "nominal", "metacheck:role" = "iv")
    )
  ), auto_unbox = TRUE, pretty = TRUE),
  file.path(root, "study-s1", "dataset_description.json"))

  writeLines("id,anxiety,cond\n1,4,a\n2,3,b", file.path(root, "study-s1", "data", "s1.csv"))

  writeLines(jsonlite::toJSON(list(
    osd_version = "1.0",
    definition = list(
      scale_info = list(name = "Perceived Stress Scale", code = "pss",
                        abbreviation = "PSS"),
      likert_options = list(points = 5),
      items = list(list(id = "PSS1"), list(id = "PSS2")),
      metacheck = list(scale_source = "identified", confidence = "high")
    )
  ), auto_unbox = TRUE, pretty = TRUE), file.path(root, "scales", "pss.osd"))

  writeLines(jsonlite::toJSON(list(
    paper_id = doi, generated = "2026-01-01T00:00:00+0000",
    provenance = list(software = list(name = "metacheck", version = "0.1.0"),
                      r_version = "R 4.5.3", platform = "test",
                      prod_date = "2026-01-01", llm = list(used = TRUE, model = "test/model"),
                      manifest_kind = "reduced"),
    n_files = 2, n_downloaded = 1,
    files = list(
      list(file_name = "analysis.R", file_path = "analysis.R",
           repo_url = "https://osf.io/aaa/", file_url = "https://osf.io/download/aaa/",
           file_size = 1234, data_type = "code", data_format = NULL,
           downloaded = TRUE, status = "downloaded"),
      list(file_name = "s1.csv", file_path = "s1.csv",
           repo_url = "https://osf.io/bbb/", file_url = "https://osf.io/download/bbb/",
           file_size = 55, data_type = "data", data_format = "csv",
           downloaded = TRUE, status = "downloaded")
    )
  ), auto_unbox = TRUE, pretty = TRUE, null = "null"),
  file.path(root, "logs", paste0(pid, ".manifest.json")))

  # Two findings: one stat (F value) and one open_practices (goes to other_findings).
  writeLines(jsonlite::toJSON(list(
    paper_id = doi, generated = "2026-01-01T00:00:00+0000",
    checks = list(
      list(paper_id = doi, module = "stat_check", traffic_light = "na",
           summary_text = "stats found",
           counts = "{\"n\":1}"),
      list(paper_id = doi, module = "open_practices", traffic_light = "green",
           summary_text = "open", counts = "{\"data_open\":true}")
    ),
    findings = list(
      list(paper_id = doi, module = "stat_check", text = "F(1, 98) = 7.30, p = .008",
           text_id = "1", f_reported = 7.30, df1 = 1, df2 = 98, p_value = 0.008),
      list(paper_id = doi, module = "stat_check", text = "F(1, 98) = 2.10, p = .15",
           text_id = "2", f_reported = 2.10, df1 = 1, df2 = 98, p_value = 0.15),
      list(paper_id = doi, module = "open_practices", text = "Data available on OSF.",
           text_id = "3", data = TRUE)
    )
  ), auto_unbox = TRUE, pretty = TRUE, null = "null"),
  file.path(root, "logs", paste0(pid, ".checks.json")))

  # A real checks-result object saved as .rds (gzip by default), so blob tests
  # exercise the true storage format. The power module carries a rich
  # classification table INCLUDING a list-column, to exercise the non-atomic
  # flattening in .sv_module_tables (a plain toJSON would error on it).
  power_tbl <- data.frame(
    text = c("We aimed for 200 participants.", "N = 120 planned."),
    sample_size = c(200L, 120L), alpha_level = c(0.05, 0.05),
    power = c(NA, 0.8), effect_size = c(0.3, 0.3),
    complete = c(FALSE, FALSE), stringsAsFactors = FALSE)
  power_tbl$notes <- list(c("a", "b"), "single")   # a list-column
  saveRDS(list(paper_id = doi, generated = "2026-01-01",
               modules = list(
                 stat_check = list(traffic_light = "na"),
                 power = list(traffic_light = "na", table = power_tbl))),
          file.path(root, "logs", paste0(pid, ".rds")))

  writeLines("This is the manuscript full text mentioning stress and coping.",
             file.path(root, "documentation", paste0(pid, "_fulltext.txt")))

  root
}

test_that("add_to_scienceverse shreds a collection into typed tables", {
  skip_if_not_installed("DBI")
  skip_if_not_installed("RSQLite")

  root <- make_collection_fixture()
  db <- tempfile(fileext = ".sqlite")
  add_to_scienceverse(root, db, quiet = TRUE)

  con <- DBI::dbConnect(RSQLite::SQLite(), db)
  withr::defer(suppressWarnings(DBI::dbDisconnect(con)))

  # structural rows
  expect_equal(DBI::dbGetQuery(con, "SELECT COUNT(*) n FROM papers")$n, 1L)
  expect_equal(DBI::dbGetQuery(con, "SELECT COUNT(*) n FROM studies")$n, 1L)
  expect_equal(DBI::dbGetQuery(con, "SELECT COUNT(*) n FROM variables")$n, 2L)
  expect_equal(DBI::dbGetQuery(con, "SELECT COUNT(*) n FROM scales")$n, 1L)
  expect_equal(DBI::dbGetQuery(con, "SELECT COUNT(*) n FROM files")$n, 2L)
  expect_equal(DBI::dbGetQuery(con, "SELECT COUNT(*) n FROM checks")$n, 2L)

  # provenance carried onto the paper row
  p <- DBI::dbGetQuery(con, "SELECT * FROM papers")
  expect_equal(p$doi, "https://doi.org/10.1234/test.001")
  expect_equal(p$llm_model, "test/model")
  expect_equal(p$llm_used, 1L)
  expect_equal(p$n_studies, 1L)
  expect_true(grepl("stress", p$fulltext))
})

test_that("findings are routed to split tables with none dropped", {
  skip_if_not_installed("DBI")
  skip_if_not_installed("RSQLite")

  root <- make_collection_fixture()
  db <- tempfile(fileext = ".sqlite")
  add_to_scienceverse(root, db, quiet = TRUE)
  con <- DBI::dbConnect(RSQLite::SQLite(), db)
  withr::defer(suppressWarnings(DBI::dbDisconnect(con)))

  stat  <- DBI::dbGetQuery(con, "SELECT COUNT(*) n FROM stat_findings")$n
  other <- DBI::dbGetQuery(con, "SELECT COUNT(*) n FROM other_findings")$n
  code  <- DBI::dbGetQuery(con, "SELECT COUNT(*) n FROM code_findings")$n
  expect_equal(stat, 2L)     # two stat_check findings
  expect_equal(other, 1L)    # one open_practices finding
  expect_equal(code, 0L)
  # total routed == total in source (nothing dropped)
  expect_equal(stat + other + code, 3L)

  # f_reported stored as a real number, filterable
  hits <- DBI::dbGetQuery(con,
    "SELECT text FROM stat_findings WHERE f_reported > 5")
  expect_equal(nrow(hits), 1L)
  expect_true(grepl("7.30", hits$text))
})

test_that("FTS search composes with a numeric stat filter", {
  skip_if_not_installed("DBI")
  skip_if_not_installed("RSQLite")

  root <- make_collection_fixture()
  db <- tempfile(fileext = ".sqlite")
  add_to_scienceverse(root, db, quiet = TRUE)
  con <- DBI::dbConnect(RSQLite::SQLite(), db)
  withr::defer(suppressWarnings(DBI::dbDisconnect(con)))

  # "stress in the title" AND an F value > 5
  q <- DBI::dbGetQuery(con, "
    SELECT DISTINCT p.doi
    FROM papers p
    JOIN papers_fts ON papers_fts.doi = p.doi
    JOIN stat_findings s ON s.doi = p.doi
    WHERE papers_fts MATCH 'title:stress' AND s.f_reported > 5")
  expect_equal(nrow(q), 1L)
  expect_equal(q$doi, "https://doi.org/10.1234/test.001")

  # a title term that is absent returns nothing
  none <- DBI::dbGetQuery(con,
    "SELECT doi FROM papers_fts WHERE papers_fts MATCH 'title:elephant'")
  expect_equal(nrow(none), 0L)
})

test_that("re-adding the same DOI is idempotent", {
  skip_if_not_installed("DBI")
  skip_if_not_installed("RSQLite")

  root <- make_collection_fixture()
  db <- tempfile(fileext = ".sqlite")
  add_to_scienceverse(root, db, quiet = TRUE)
  add_to_scienceverse(root, db, quiet = TRUE)   # again

  con <- DBI::dbConnect(RSQLite::SQLite(), db)
  withr::defer(suppressWarnings(DBI::dbDisconnect(con)))
  expect_equal(DBI::dbGetQuery(con, "SELECT COUNT(*) n FROM papers")$n, 1L)
  expect_equal(DBI::dbGetQuery(con, "SELECT COUNT(*) n FROM variables")$n, 2L)
  expect_equal(DBI::dbGetQuery(con, "SELECT COUNT(*) n FROM stat_findings")$n, 2L)
  expect_equal(DBI::dbGetQuery(con, "SELECT COUNT(*) n FROM papers_fts")$n, 1L)
})

test_that("scienceverse_checks round-trips the stored checks blob", {
  skip_if_not_installed("DBI")
  skip_if_not_installed("RSQLite")

  root <- make_collection_fixture()
  db <- tempfile(fileext = ".sqlite")
  add_to_scienceverse(root, db, quiet = TRUE)
  con <- DBI::dbConnect(RSQLite::SQLite(), db)
  withr::defer(suppressWarnings(DBI::dbDisconnect(con)))

  obj <- scienceverse_checks(con, "https://doi.org/10.1234/test.001")
  orig <- readRDS(file.path(root, "logs", "test.001.rds"))
  expect_equal(obj, orig)
  expect_null(scienceverse_checks(con, "does.not.exist"))
})

test_that("a path without collection.json is rejected", {
  skip_if_not_installed("DBI")
  skip_if_not_installed("RSQLite")

  empty <- file.path(tempdir(), paste0("svempty_", as.integer(runif(1, 1, 1e6))))
  dir.create(empty, showWarnings = FALSE)
  db <- tempfile(fileext = ".sqlite")
  expect_error(add_to_scienceverse(empty, db, quiet = TRUE), "No collection roots")
})

# ── query layer (the engine behind scienceverse_app()) ───────────────────────

# Build a small archive once for the query tests.
build_query_db <- function() {
  root <- make_collection_fixture()
  db <- tempfile(fileext = ".sqlite")
  add_to_scienceverse(root, db, quiet = TRUE)
  db
}

test_that("scienceverse_papers searches metadata and full text", {
  skip_if_not_installed("DBI")
  skip_if_not_installed("RSQLite")
  con <- scienceverse_connect(build_query_db())
  withr::defer(suppressWarnings(DBI::dbDisconnect(con)))

  # bare term hits the fulltext ("stress and coping")
  expect_equal(nrow(scienceverse_papers(con, "coping")), 1L)
  # field term hits the title
  expect_equal(nrow(scienceverse_papers(con, "title:stress")), 1L)
  # a term present nowhere returns nothing
  expect_equal(nrow(scienceverse_papers(con, "title:elephant")), 0L)
  # counts columns come back
  p <- scienceverse_papers(con, "")
  expect_true(all(c("n_variables", "n_scales") %in% names(p)))
  expect_equal(p$n_variables, 2)
})

test_that("scienceverse_findings applies numeric range filters", {
  skip_if_not_installed("DBI")
  skip_if_not_installed("RSQLite")
  con <- scienceverse_connect(build_query_db())
  withr::defer(suppressWarnings(DBI::dbDisconnect(con)))

  # fixture has F = 7.30 and 2.10; F >= 5 keeps only the first
  hi <- scienceverse_findings(con, "stat_findings",
                              ranges = list(f_reported = c(5, NA)))
  expect_equal(nrow(hi), 1L)
  expect_true(grepl("7.30", hi$text))

  # a two-sided range
  both <- scienceverse_findings(con, "stat_findings",
                                ranges = list(f_reported = c(2, 3)))
  expect_equal(nrow(both), 1L)
  expect_true(grepl("2.10", both$text))

  # text filter composes with range
  none <- scienceverse_findings(con, "stat_findings", text = "elephant",
                                ranges = list(f_reported = c(5, NA)))
  expect_equal(nrow(none), 0L)

  # an invalid table name errors
  expect_error(scienceverse_findings(con, "not_a_table"), "must be one of")
})

test_that("scienceverse_scales and _files query correctly", {
  skip_if_not_installed("DBI")
  skip_if_not_installed("RSQLite")
  con <- scienceverse_connect(build_query_db())
  withr::defer(suppressWarnings(DBI::dbDisconnect(con)))

  s <- scienceverse_scales(con, "stress")
  expect_equal(nrow(s), 1L)
  expect_equal(s$n_papers, 1)

  # files: exact data_type filter
  code <- scienceverse_files(con, data_type = "code")
  expect_equal(nrow(code), 1L)
  expect_equal(code$file_name, "analysis.R")
  # field search
  csv <- scienceverse_files(con, "data_format:csv")
  expect_equal(nrow(csv), 1L)
})

test_that("scienceverse_connect errors on a missing archive", {
  skip_if_not_installed("DBI")
  skip_if_not_installed("RSQLite")
  expect_error(
    scienceverse_connect(tempfile(fileext = ".sqlite")),
    "No scienceverse archive")
})

# ── scale items, summary, and per-check tables ───────────────────────────────

# Extend the fixture's one scale with items + an English translation, so the
# scale_items extraction has something to read.
make_items_fixture <- function() {
  root <- make_collection_fixture()
  osd <- jsonlite::fromJSON(file.path(root, "scales", "pss.osd"),
                            simplifyVector = FALSE)
  osd$definition$items <- list(
    list(id = "PSS1", text_key = "PSS1", type = "likert"),
    list(id = "PSS2", text_key = "PSS2", type = "likert"))
  osd$translations <- list(en = list(
    PSS1 = "I felt unable to control the important things.",
    PSS2 = "I felt confident about handling my problems."))
  writeLines(jsonlite::toJSON(osd, auto_unbox = TRUE, pretty = TRUE),
             file.path(root, "scales", "pss.osd"))
  root
}

test_that("scale items are extracted with their question text", {
  skip_if_not_installed("DBI")
  skip_if_not_installed("RSQLite")
  db <- tempfile(fileext = ".sqlite")
  add_to_scienceverse(make_items_fixture(), db, quiet = TRUE)
  con <- scienceverse_connect(db)
  withr::defer(suppressWarnings(DBI::dbDisconnect(con)))

  code <- DBI::dbGetQuery(con, "SELECT code FROM scales LIMIT 1")$code
  items <- scienceverse_scale_items(con, code)
  expect_equal(nrow(items), 2L)
  expect_equal(items$item_id, c("PSS1", "PSS2"))
  expect_true(any(grepl("control the important things", items$text)))
})

test_that("scienceverse_scales excludes unnamed scales", {
  skip_if_not_installed("DBI")
  skip_if_not_installed("RSQLite")
  # add an unnamed_block scale row directly and confirm it is filtered out
  db <- tempfile(fileext = ".sqlite")
  add_to_scienceverse(make_collection_fixture(), db, quiet = TRUE)
  con <- scienceverse_connect(db)
  withr::defer(suppressWarnings(DBI::dbDisconnect(con)))
  DBI::dbExecute(con, paste("INSERT INTO scales (doi, scale, code, source)",
    "VALUES ('x', '', 'blk', 'unnamed_block')"))
  DBI::dbExecute(con, paste("INSERT INTO scales (doi, scale, code, source)",
    "VALUES ('y', 'Real Scale', 'real', 'manuscript')"))
  s <- scienceverse_scales(con)
  expect_false(any(s$code == "blk"))
  expect_true(any(s$code == "real"))
})

test_that("scienceverse_check_table serves findings and repo_files", {
  skip_if_not_installed("DBI")
  skip_if_not_installed("RSQLite")
  db <- tempfile(fileext = ".sqlite")
  add_to_scienceverse(make_collection_fixture(), db, quiet = TRUE)
  con <- scienceverse_connect(db)
  withr::defer(suppressWarnings(DBI::dbDisconnect(con)))

  doi <- DBI::dbGetQuery(con, "SELECT doi FROM papers LIMIT 1")$doi
  # stat_check rows come from stat_findings
  ct <- scienceverse_check_table(con, doi, "stat_check")
  expect_true(nrow(ct) >= 1)
  expect_false(any(c("doi", "module") %in% names(ct)))
  # checks_of lists the modules
  cof <- scienceverse_checks_of(con, doi)
  expect_true("stat_check" %in% cof$module)
})

test_that("module_tables stores each module's full table (rich columns)", {
  skip_if_not_installed("DBI")
  skip_if_not_installed("RSQLite")
  db <- tempfile(fileext = ".sqlite")
  add_to_scienceverse(make_collection_fixture(), db, quiet = TRUE)
  con <- scienceverse_connect(db)
  withr::defer(suppressWarnings(DBI::dbDisconnect(con)))

  doi <- DBI::dbGetQuery(con, "SELECT doi FROM papers LIMIT 1")$doi

  # power's rich classification survives (it was dropped by the findings schema)
  pw <- scienceverse_check_table(con, doi, "power")
  expect_true(all(c("sample_size", "alpha_level", "power", "effect_size",
                    "complete") %in% names(pw)))
  expect_equal(nrow(pw), 2L)
  expect_equal(pw$sample_size, c(200, 120))

  # the list-column was flattened (not dropped, not an error)
  expect_true("notes" %in% names(pw))
  expect_true(any(grepl("a; b", pw$notes)))
})
