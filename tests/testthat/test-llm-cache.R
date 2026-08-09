# Offline tests for the on-disk LLM response cache (R/llm-cache.R). No network:
# these exercise key hashing, read/write round-trips, the toggle, and clearing.
# The cache dir is redirected to a tempdir so the user's real cache is untouched.

local_cache_dir <- function(env = parent.frame()) {
  d <- file.path(tempdir(), paste0("llmcache_", as.integer(runif(1, 1, 1e7))))
  dir.create(d, showWarnings = FALSE, recursive = TRUE)
  withr::local_envvar(METACHECK_LLM_CACHE_DIR = d, .local_envir = env)
  d
}

test_that("llm_cache() toggles and validates", {
  withr::local_options(metacheck.llm.cache = TRUE)
  expect_true(llm_cache())
  expect_invisible(llm_cache(FALSE))
  expect_false(llm_cache())
  llm_cache(TRUE)
  expect_true(llm_cache())
  expect_error(llm_cache("yes"), "TRUE or FALSE")
})

test_that("cache key is stable and sensitive to inputs", {
  k1 <- metacheck:::.llm_cache_key("hi", "sys", NULL, "groq/x", list(temperature = 0))
  k2 <- metacheck:::.llm_cache_key("hi", "sys", NULL, "groq/x", list(temperature = 0))
  expect_identical(k1, k2)
  # different text / prompt / model / params all change the key
  expect_false(k1 == metacheck:::.llm_cache_key("bye", "sys", NULL, "groq/x", list(temperature = 0)))
  expect_false(k1 == metacheck:::.llm_cache_key("hi", "other", NULL, "groq/x", list(temperature = 0)))
  expect_false(k1 == metacheck:::.llm_cache_key("hi", "sys", NULL, "groq/y", list(temperature = 0)))
  expect_false(k1 == metacheck:::.llm_cache_key("hi", "sys", NULL, "groq/x", list(temperature = 1)))
  # param order does not matter
  expect_identical(
    metacheck:::.llm_cache_key("hi", "s", NULL, "m", list(a = 1, b = 2)),
    metacheck:::.llm_cache_key("hi", "s", NULL, "m", list(b = 2, a = 1))
  )
})

test_that("cache put/get round-trips and misses return NULL", {
  local_cache_dir()
  key <- metacheck:::.llm_cache_key("hi", "sys", NULL, "m", list())
  df  <- data.frame(answer = "yes", .join_key. = "hi")
  metacheck:::.llm_cache_put(key, df, raw = list(reasoning = "because"))
  hit <- metacheck:::.llm_cache_get(key)
  expect_identical(hit$df, df)
  expect_equal(hit$raw$reasoning, "because")
  expect_null(metacheck:::.llm_cache_get("no-such-key"))
})

test_that("llm_cache_clear removes entries and reports the count", {
  local_cache_dir()
  for (i in 1:3) {
    k <- metacheck:::.llm_cache_key(paste0("t", i), "s", NULL, "m", list())
    metacheck:::.llm_cache_put(k, data.frame(x = i))
  }
  expect_equal(llm_cache_clear(), 3)
  expect_equal(llm_cache_clear(), 0)
})
