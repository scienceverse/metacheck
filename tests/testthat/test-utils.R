test_that(".onLoad", {
  op.defaults <- c(
    metacheck.verbose = TRUE,
    metacheck.llm_max_calls = 30L,
    metacheck.llm.use = FALSE,
    metacheck.llm.model = "groq",
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


test_that("llm_use", {
  expect_true(is.function(metacheck::llm_use))
  expect_no_error(helplist <- help(llm_use, metacheck))

  expect_error(llm_use("G"))
  expect_invisible(llm_use(TRUE))
  expect_visible(llm_use())

  expect_equal(llm_use(FALSE), FALSE)
  expect_equal(llm_use(), FALSE)
  expect_equal(llm_use(TRUE), TRUE)
  expect_equal(getOption("metacheck.llm.use"), TRUE)
  expect_equal(llm_use(0), FALSE)
  expect_equal(llm_use("FALSE"), FALSE)

  # llm_use() only true if online & API
  llm_use(1)
  expect_equal(getOption("metacheck.llm.use"), TRUE)
  llm_use("TRUE")
  expect_equal(getOption("metacheck.llm.use"), TRUE)
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


#httptest2::start_capturing()
httptest2::use_mock_api()

test_that(".batch_query", {
  expect_true(is.function(metacheck:::.batch_query))

  expect_error(.batch_query())

  urls <- c()
  obs <- .batch_query(urls)
  exp <- list()
  expect_equal(obs, exp)

  urls <- "notawebsite"
  expect_warning(obs <- .batch_query(urls), "notawebsite")

  # skip_api("httpbin.org")

  urls <- "https://httpbin.org/get"
  obs <- .batch_query(urls)
  expect_equal(length(obs), 1)

  urls <- c("https://httpbin.org/status/429",
            "https://httpbin.org/status/404",
            "https://httpbin.org/status/200")
  batch_size <- 2
  msg <- "X"
  delay = 0
  obs <- .batch_query(urls, batch_size, msg, delay)
  expect_equal(length(obs), 3)
  expect_equal(obs[[1]]$status_code, 429)
  expect_equal(obs[[2]]$status_code, 404)
  expect_equal(obs[[3]]$status_code, 200)
})

httptest2::stop_mocking()
#httptest2::stop_capturing()
