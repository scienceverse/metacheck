test_that(".onLoad", {
  op.defaults <- c(
    metacheck.verbose = TRUE,
    metacheck.llm_max_calls = 30L,
    metacheck.llm.use = FALSE,
    metacheck.llm.model = "llama-3.3-70b-versatile",
    metacheck.osf.delay = 0,
    metacheck.osf.api = "https://api.osf.io/v2",
    metacheck.osf.api.calls = 0
  )

  # op.current <- names(op.defaults) |> sapply(getOption)
  names(op.defaults) |> sapply(\(o) options(setNames(list(NULL), o)))
  op.null <- names(op.defaults) |> sapply(getOption)
  expect_true(sapply(op.null, is.null) |> all())

  metacheck:::.onLoad()
  op.reset <- names(op.defaults) |> sapply(getOption)
  expect_false(sapply(op.reset, is.null) |> any())
  expect_equal(op.reset, op.defaults)
})

test_that(".onAttach", {
  op <- capture_message(metacheck:::.onAttach())
  expect_true(grepl("Welcome to metacheck", op))
  expect_true(grepl("This is alpha software", op))
})

test_that("demo functions", {

  d <- demodir()
  e <- system.file("grobid", package = "metacheck")
  expect_equal(d, e)

  x <- demoxml()
  expect_true(all(grepl("\\.xml$", x)))

  p <- demopdf()
  expect_true(all(grepl("\\.pdf$", p)))
})

test_that("concat_tables", {
  papers <- read(demodir())

  bibs <- concat_tables(papers, c("bib"))
  expect_equal(nrow(bibs), 48)

  ids <- unique(bibs$id)
  expect_equal(length(ids), length(papers))
})


test_that("is_paper_list", {
  expect_equal(is_paper_list(psychsci), TRUE)
  expect_equal(is_paper_list(psychsci[1]), TRUE)
  expect_equal(is_paper_list(psychsci[[1]]), FALSE)
  expect_equal(is_paper_list(list(1,3,5)), FALSE)
  expect_equal(is_paper_list(NULL), FALSE)

  # empty lists return TRUE
  expect_equal(is_paper_list(psychsci[c()]), TRUE)
  expect_equal(is_paper_list(list()), TRUE)
})

test_that("print.scivrs_paper", {
  paper <- demoxml() |> read()
  op <- capture_output(print(paper))
  op.sv <- capture_output(print.scivrs_paper(paper))
  expected <- "---------------\nto_err_is_human\n---------------\n\nTo Err is Human: An Empirical Investigation\n\n* Sections: 4\n* Sentences: 27\n* Bibliography: 4\n* X-Refs: 2\n"

  expect_equal(op, expected)
  expect_equal(op, op.sv)
  expect_true(grepl("to_err_is_human", op))
})

test_that("print.scivrs_paperlist", {
  x <- psychsci[1:3]
  op <- capture_output(print(x))
  op.sv <- capture_output(print.scivrs_paperlist(x))

  expect_true(grepl("# A tibble: 3", op, fixed = TRUE))
  expect_equal(op, op.sv)
})

test_that("[.scivrs_paperlist", {
  # subsetting maintains class
  x <- psychsci[1:3]
  expect_s3_class(psychsci, "scivrs_paperlist")
  expect_s3_class(x, "scivrs_paperlist")
})

test_that("verbose", {
  expect_equal(verbose(FALSE), FALSE)
  expect_equal(verbose(), FALSE)
  expect_equal(verbose(TRUE), TRUE)
  expect_equal(verbose(), TRUE)
  expect_equal(verbose(0), FALSE)
  expect_equal(verbose("FALSE"), FALSE)
  expect_equal(verbose(1), TRUE)
  expect_equal(verbose("TRUE"), TRUE)

  expect_error(verbose("G"))
  expect_invisible(verbose(TRUE))
  expect_visible(verbose())
})

test_that("llm_use", {
  expect_equal(llm_use(FALSE), FALSE)
  expect_equal(llm_use(), FALSE)
  expect_equal(llm_use(TRUE), TRUE)
  expect_equal(llm_use(), TRUE)
  expect_equal(llm_use(0), FALSE)
  expect_equal(llm_use("FALSE"), FALSE)
  expect_equal(llm_use(1), TRUE)
  expect_equal(llm_use("TRUE"), TRUE)

  expect_error(llm_use("G"))
  expect_invisible(llm_use(TRUE))
  expect_visible(llm_use())
})

test_that("email", {
  orig <- email()
  e <- "debruine@gmail.com"
  expect_invisible(email(email = e))
  expect_error(email("email"))
  expect_equal(email(), e)
  expect_equal(email(email = e), e)
  expect_equal(email(), e)
  expect_visible(email())
  email(orig)
})

test_that("online", {
  skip_if_offline("google.com")

  expect_true(online())
  expect_true(online("google.com"))
  expect_true(online("http://google.com"))
  expect_true(online("https://google.com"))
  expect_true(online("https://google.com/images"))

  expect_false(online("notasite"))
})


