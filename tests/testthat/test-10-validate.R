#setwd("tests/testthat/")

test_that("exists", {
  expect_true(is.function(papercheck::validate))
  expect_no_error(help <- help(validate, papercheck))
  #expect_equal(help$topic, "validate")
})

test_that("errors", {
  sample <- data.frame(
    xml = c("examples/to_err_is_human.xml")
  )
  expect_error(validate("not-a-module", sample),
               "There were no modules that matched not-a-module",
               fixed = TRUE)

  module <- "all-p-values"

  sample <- data.frame(x = 1)
  expect_error(validate(module, sample), "The sample needs a column called xml")

  sample <- data.frame( xml = c() )
  expect_error(validate(module, sample), "The sample has no rows")

  sample <- data.frame( xml = "nope.xml" )
  expect_error(validate(module, sample), "None of the xml files could be found")

  sample <- data.frame(
    xml = c("examples/to_err_is_human.xml", "nope.xml")
  )
  expect_error(validate(module, sample),
               "Some (1) of the xml files could not be found: nope.xml",
               fixed = TRUE)
})

test_that("basic", {
  path <- "validate"

  module <- "all-p-values"
  paper <- file.path(path, "to_err_is_human.xml") |> read_grobid()
  res <- module_run(paper, module)

  sample <- data.frame(
    xml = "to_err_is_human.xml",
    report = res$report,
    traffic_light = res$traffic_light
  )

  results_table <- data.frame(
    xml = "to_err_is_human.xml",
    text = res$table$text
  )
  #write.csv(sample, paste0(path, "/sample.csv"), row.names = FALSE)
  #write.csv(results_table, paste0(path, "/res_table.csv"), row.names = FALSE)

  v <- validate(module, sample, results_table, path)
  expect_equal(class(v), "ppchk_validate")
  expect_equal(v$tables_matched, 1)
  expect_equal(v$reports_matched, 1)
  expect_equal(v$tl_matched, 1)

  # check file types
  samples <- list(file.path(path, "sample.csv"),
                  file.path(path, "sample.xls"),
                  file.path(path, "sample.xlsx"))

  for (s in samples) {
    v <- validate(module, s, results_table, path = path)
    expect_equal(class(v), "ppchk_validate")
    expect_equal(v$tables_matched, 1)
    expect_equal(v$reports_matched, 1)
    expect_equal(v$tl_matched, 1)
  }
})


test_that("tables", {
  # create validation directory in temp dir
  valdir <- tempdir() |> file.path("validate")
  dir.create(valdir, showWarnings = FALSE)

  # copy built-in XML files to xml directory
  xmldir <- file.path(valdir, "xml")
  dir.create(xmldir, showWarnings = FALSE)
  xmls <- list.files(demodir(), "\\.xml$", full.names = TRUE)
  file.copy(xmls, xmldir)

  sample <- data.frame(
    xml =  file.path("xml", list.files(xmldir)),
    table = c("faceresearch.org", "https://osf.io/mwzuq", "https://osf.io/pwtrh"),
    report = c("a", "s", ""), # this module has no report
    traffic_light = c("info", "red", "info")
  )

  v <- validate("all-urls", sample, path = valdir)
  expect_equal(names(v$results_table), c("xml", "text"))
  expect_equal(v$tables_matched, 1)
  expect_equal(v$reports_matched, 1/3)
  expect_equal(v$tl_matched, 2/3)
  expect_equal(v$sample$table_check, c(T, T, T))
  expect_equal(v$sample$report_check, c(F, F, T))
  expect_equal(v$sample$tl_check, c(T, F, T))

  exp_table <- data.frame(
    xml = rep(list.files(xmldir), c(2, 3, 2)) |> file.path("xml", x = _),
    text = c("faceresearch.org", "stumbleupon.com",
             rep("https://osf.io/mwzuq", 3),
             rep("https://osf.io/pwtrh", 2)),
    header = c("Participants", "Participants",
               "Methods", "Procedure", "Analysis",
               "Intro", "Attitude")
  )

  v <- validate("all-urls", sample, exp_table, path = valdir)
  expect_equal(names(v$results_table), c("xml", "text", "header"))
  expect_equal(v$tables_matched, 2/3)
  expect_equal(v$reports_matched, 1/3)
  expect_equal(v$tl_matched, 2/3)
  expect_equal(v$sample$table_check, c(T, T, F))
  expect_equal(v$sample$report_check, c(F, F, T))
  expect_equal(v$sample$tl_check, c(T, F, T))
})
