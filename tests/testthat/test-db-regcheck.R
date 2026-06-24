# a raw RegCheck API result, as returned by GET /api/v1/comparisons/{task_id}
canned_regcheck_result <- list(
  items = list(
    list(
      dimension = "Sample size",
      deviation_judgement = "yes",
      paper_content_summary = "The paper reports 120 participants [PAPER_0001].",
      registration_content_summary = "The registration plans 100 participants [REG_0001].",
      deviation_information = "The paper sample (120) exceeds the preregistered sample (100).",
      paper_content_quotes = "[PAPER_0001] We recruited 120 participants.",
      registration_content_quotes = "[REG_0001] We will collect 100 participants."
    ),
    list(
      dimension = "Hypotheses",
      deviation_judgement = "no",
      paper_content_summary = "H1: condition A > B.",
      registration_content_summary = "H1: condition A > B.",
      deviation_information = "The hypotheses are consistent.",
      paper_content_quotes = "[PAPER_0002] We predicted A > B.",
      registration_content_quotes = "[REG_0002] We predict A > B."
    ),
    list(
      dimension = "Exclusion criteria",
      deviation_judgement = "missing",
      paper_content_summary = "Not described.",
      registration_content_summary = "Exclude RTs under 200 ms.",
      deviation_information = "The paper does not report exclusions, so this cannot be assessed.",
      paper_content_quotes = "",
      registration_content_quotes = "[REG_0003] We will exclude RTs under 200 ms."
    )
  )
)

test_that("regcheck_base_url defaults", {
  withr::local_envvar(REGCHECK_BASE_URL = "")

  # local fork for ollama, hosted app for API clients
  expect_equal(regcheck_base_url("ollama"), "http://localhost:8000")
  expect_match(regcheck_base_url("groq"), "^https://")
  expect_match(regcheck_base_url("openai"), "^https://")

  # an explicit URL always wins, and trailing slashes are stripped
  expect_equal(regcheck_base_url("ollama", "https://my.server/"),
               "https://my.server")
  expect_equal(regcheck_base_url("groq", "http://localhost:9999//"),
               "http://localhost:9999")

  # the env var beats the client default but not an explicit URL
  withr::local_envvar(REGCHECK_BASE_URL = "https://env.server/")
  expect_equal(regcheck_base_url("ollama"), "https://env.server")
  expect_equal(regcheck_base_url("ollama", "https://arg.server"),
               "https://arg.server")
})

test_that("regcheck_compare input validation", {
  # no token for a hosted client that requires one
  withr::local_envvar(REGCHECK_API_TOKEN = "")
  expect_error(regcheck_compare("paper text", "prereg text", client = "groq"),
               "REGCHECK_API_TOKEN")

  withr::local_envvar(REGCHECK_API_TOKEN = "test-token")

  # need exactly one of prereg_text / registration_id
  expect_error(regcheck_compare("paper text"),
               "exactly one")
  expect_error(
    regcheck_compare("paper text", "prereg text",
                     registration_id = "NCT01234567"),
    "exactly one"
  )

  # paper_text must be a non-empty string
  expect_error(regcheck_compare("", "prereg text"), "paper_text")
  expect_error(regcheck_compare(c("a", "b"), "prereg text"), "paper_text")
})

test_that(".regcheck_sanitize makes text latin-1 safe", {
  # greek letters and stats symbols become readable equivalents
  expect_equal(
    metacheck:::.regcheck_sanitize("α = .05, η² = 0.15"),
    "alpha = .05, eta2 = 0.15"
  )
  expect_equal(
    metacheck:::.regcheck_sanitize("p ≤ .05 – a “result”"),
    'p <= .05 - a "result"'
  )

  # anything else outside latin-1 is stripped rather than crashing
  out <- metacheck:::.regcheck_sanitize("ok 中文 ok")
  expect_false(is.na(out))
  expect_match(out, "^ok +ok$")

  # latin-1 text (including accents) passes through unchanged
  expect_equal(metacheck:::.regcheck_sanitize("Daniël Lakens"),
               "Daniël Lakens")
})

test_that("unknown client gives a friendly error", {
  withr::local_envvar(REGCHECK_API_TOKEN = "tok")
  expect_error(
    regcheck_compare("paper text", "prereg text", client = "gpt5"),
    regexp = "groq|openai|deepseek",
    ignore.case = TRUE
  )
  expect_error(
    regcheck_compare("paper text", "prereg text", client = "gpt5"),
    regexp = "regcheck",
    ignore.case = TRUE
  )
})

test_that("missing token for hosted client gives a friendly error", {
  withr::local_envvar(REGCHECK_API_TOKEN = "")
  expect_error(
    regcheck_compare("paper text", "prereg text", client = "groq"),
    regexp = "REGCHECK_API_TOKEN"
  )
  expect_error(
    regcheck_compare("paper text", "prereg text", client = "groq"),
    regexp = "edit_r_environ"
  )
})

test_that("401 for ollama points to regcheck_start_local", {
  withr::local_envvar(REGCHECK_API_TOKEN = "bad-token")
  testthat::local_mocked_bindings(
    req_perform = function(...) {
      rlang::abort("HTTP 401 Unauthorized", class = c("httr2_http_401", "httr2_http", "error"))
    },
    .package = "httr2"
  )
  expect_error(
    regcheck_compare("paper text", "prereg text", client = "ollama"),
    regexp = "local RegCheck server rejected"
  )
})

test_that("401 for hosted client points to .Renviron", {
  withr::local_envvar(REGCHECK_API_TOKEN = "bad-token")
  testthat::local_mocked_bindings(
    req_perform = function(...) {
      rlang::abort("HTTP 401 Unauthorized", class = c("httr2_http_401", "httr2_http", "error"))
    },
    .package = "httr2"
  )
  expect_error(
    regcheck_compare("paper text", "prereg text", client = "groq"),
    regexp = "REGCHECK_API_TOKEN"
  )
})

test_that("connection refused for ollama points to regcheck_start_local", {
  withr::local_envvar(REGCHECK_API_TOKEN = "tok")
  testthat::local_mocked_bindings(
    req_perform = function(...) {
      rlang::abort("Could not connect to server", class = c("httr2_failure", "error"))
    },
    .package = "httr2"
  )
  expect_error(
    regcheck_compare("paper text", "prereg text", client = "ollama"),
    regexp = "regcheck_start_local"
  )
})

test_that("regcheck_tidy", {
  tidy <- regcheck_tidy(canned_regcheck_result)

  expect_s3_class(tidy, "data.frame")
  expect_equal(nrow(tidy), 3)
  expect_equal(names(tidy), c(
    "dimension", "deviation_judgement", "paper_summary", "prereg_summary",
    "deviation_information", "paper_quotes", "prereg_quotes"
  ))
  expect_equal(tidy$dimension,
               c("Sample size", "Hypotheses", "Exclusion criteria"))
  expect_equal(tidy$deviation_judgement, c("yes", "no", "missing"))

  # missing fields become NA rather than erroring
  partial <- list(items = list(list(dimension = "Sample size")))
  tidy_partial <- regcheck_tidy(partial)
  expect_equal(nrow(tidy_partial), 1)
  expect_true(is.na(tidy_partial$deviation_judgement))

  # empty / absent items give a 0-row data frame with the same columns
  empty <- regcheck_tidy(list(items = list()))
  expect_equal(nrow(empty), 0)
  expect_equal(names(empty), names(tidy))
  expect_equal(nrow(regcheck_tidy(list())), 0)
})
