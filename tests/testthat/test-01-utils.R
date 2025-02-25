test_that("site_down", {
  expect_error(site_down("notarealwebsite"),
               "The website notarealwebsite is not available")
  expect_error(site_down("notarealwebsite", "No %s"),
               "No notarealwebsite")

  expect_true(site_down("notarealwebsite", error = FALSE))

  skip_if_offline("localhost")

  expect_false(site_down("localhost"))
  expect_false(site_down("http://localhost"))
  expect_false(site_down("https://localhost"))
  expect_false(site_down("localhost/otherstuff"))
})

test_that("demo functions", {

  d <- demodir()
  e <- system.file("grobid", package = "papercheck")
  expect_equal(d, e)

  x <- demoxml()
  expect_true(all(grepl("\\.xml$", x)))

  p <- demopdf()
  expect_true(all(grepl("\\.pdf$", p)))
})

test_that("concat_tables", {
  papers <- read_grobid(demodir())

  refs <- concat_tables(papers, c("references"))
  expect_equal(nrow(refs), 48)

  ids <- unique(refs$id)
  expect_equal(length(ids), length(papers))
})

test_that("print.scivrs_paper", {
  paper <- demoxml() |> read_grobid()
  op <- capture_output(print(paper))
  op.sv <- capture_output(print.scivrs_paper(paper))

  expect_equal(op, op.sv)
  expect_true(grepl("to_err_is_human", op))
})

test_that("verbose", {
  expect_equal(verbose(FALSE), FALSE)
  expect_equal(verbose(), FALSE)
  expect_equal(verbose(TRUE), TRUE)
  expect_equal(verbose(), TRUE)
})
