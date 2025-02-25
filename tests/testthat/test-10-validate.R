#setwd("tests/testthat/")

test_that("exists", {
  expect_true(is.function(papercheck::validate))
  expect_no_error(helplist <- help(validate, papercheck))
})

test_that("errors", {
  sample <- data.frame(
    id = c("examples/to_err_is_human")
  )
  expect_error(validate("not-a-module", sample),
               "There were no modules that matched not-a-module",
               fixed = TRUE)

  module <- "all-p-values"

  sample <- data.frame(x = 1)
  expect_error(validate(module, sample), "The sample needs a column called id")

  sample <- data.frame( id = c() )
  expect_error(validate(module, sample), "The sample has no rows")

  sample <- data.frame( id = "nope.xml" )
  expect_error(validate(module, sample), "None of the xml files could be found")

  sample <- data.frame(
    id = c("examples/to_err_is_human", "nope")
  )
  expect_error(validate(module, sample),
               "Some (1) of the xml files could not be found: nope",
               fixed = TRUE)
})

test_that("basic", {
  path <- "validate"

  module <- "all-p-values"
  paper <- file.path(path, "to_err_is_human.xml") |> read_grobid()
  res <- module_run(paper, module)

  sample <- data.frame(
    id = "to_err_is_human",
    report = res$report,
    traffic_light = res$traffic_light
  )

  expected <- data.frame(
    id = "to_err_is_human",
    text = res$table$text
  )
  #write.csv(sample, paste0(path, "/sample.csv"), row.names = FALSE)
  #write.csv(expected, paste0(path, "/res_table.csv"), row.names = FALSE)

  v <- validate(module, sample, expected, path)
  expect_equal(class(v), "ppchk_validate")
  expect_equal(v$table_matched, 1)
  expect_equal(v$report_matched, 1)
  expect_equal(v$tl_matched, 1)
  expect_equal(expected, v$table)

  # check file types
  samples <- list(file.path(path, "sample.csv"),
                  file.path(path, "sample.xls"),
                  file.path(path, "sample.xlsx"))

  for (s in samples) {
    v <- validate(module, s, expected, path = path)
    expect_equal(class(v), "ppchk_validate")
    expect_equal(v$table_matched, 1)
    expect_equal(v$report_matched, 1)
    expect_equal(v$tl_matched, 1)
    expect_equal(expected, v$table)
  }
})

test_that("partial", {
  path <- "validate"

  module <- "all-p-values"

  sample <- data.frame(
    id = "to_err_is_human",
    traffic_light = "info"
  )

  expected <- data.frame(
    id = "to_err_is_human",
    text = c("p = 0.005", "p = 0.152", "p > .05")
  )

  v <- validate(module, sample, path = path)
  expect_equal(class(v), "ppchk_validate")
  expect_equal(v$tl_matched, 1)
  expect_true(is.na(v$table_matched))
  expect_true(is.na(v$report_matched))
  expect_equal(expected, v$table[ ,c("id", "text")])
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
    id =  file.path("xml", list.files(xmldir)) |>
      gsub("\\.xml", "", x = _),
    table = c("faceresearch.org", "https://osf.io/mwzuq", "https://osf.io/pwtrh"),
    report = c("a", "s", ""), # this module has no report
    traffic_light = c("info", "red", "info")
  )

  v <- validate("all-urls", sample, path = valdir)
  expect_equal(names(v$table), c("id", "text"))
  expect_equal(v$table_matched, 3/3)
  expect_equal(v$report_matched, 1/3)
  expect_equal(v$tl_matched, 2/3)
  expect_equal(v$sample$table_check, c(T, T, T))
  expect_equal(v$sample$report_check, c(F, F, T))
  expect_equal(v$sample$tl_check, c(T, F, T))

  expected <- data.frame(
    id = rep(list.files(xmldir), c(3, 3, 3)) |>
      file.path("xml", x = _) |>
      gsub("\\.xml", "", x = _),
    text = c("faceresearch.org", "stumbleupon.com", "osf.io/he4ty",
             rep("https://osf.io/mwzuq", 3),
            rep("https://osf.io/pwtrh", 3)),
    header = c("Participants", "Participants", "Data availability",
               "Methods", "Procedure", "Analysis",
               "X", "Attitude", NA)
  )

  v <- validate("all-urls", sample, expected, path = valdir)
  expect_equal(names(v$table), c("id", "text", "header"))
  expect_equal(v$table_matched, 2/3)
  expect_equal(v$report_matched, 1/3)
  expect_equal(v$tl_matched, 2/3)
  expect_equal(v$sample$table_check, c(T, T, F))
  expect_equal(v$sample$report_check, c(F, F, T))
  expect_equal(v$sample$tl_check, c(T, F, T))
})
