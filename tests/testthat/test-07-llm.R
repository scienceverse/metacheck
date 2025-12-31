# tests that don't require a web connections
test_that("llm", {
  expect_true(is.function(metacheck::llm))
  expect_no_error(helplist <- help(llm, metacheck))

  expect_error(llm(seed = 8675309))
  expect_error(llm("hi", seed = 8675309))

  # temperature
  expect_error(llm("hi", "repeat this", temperature = "a",seed = 8675309),
               "The argument `temperature` must be a positive number",
               fixed = TRUE)
  expect_error(llm("hi", "repeat this", temperature = -3, seed = 8675309),
               "The argument `temperature` must be between 0.0 and 2.0",
               fixed = TRUE)
  expect_error(llm("hi", "repeat this", temperature = 2.1, seed = 8675309),
               "The argument `temperature` must be between 0.0 and 2.0",
               fixed = TRUE)

  # top_p
  expect_error(llm("hi", "repeat this", top_p = "a", seed = 8675309),
               "The argument `top_p` must be a positive number",
               fixed = TRUE)
  expect_error(llm("hi", "repeat this", top_p = -3, seed = 8675309),
               "The argument `top_p` must be between 0.0 and 1.0",
               fixed = TRUE)
  expect_error(llm("hi", "repeat this", top_p = 2.1, seed = 8675309),
               "The argument `top_p` must be between 0.0 and 1.0",
               fixed = TRUE)
})


test_that("llm_model", {
  expect_true(is.function(metacheck::llm_model))
  expect_no_error(helplist <- help(llm_model, metacheck))

  orig_model <- llm_model()

  expect_error(llm_model(T))
  expect_equal(orig_model, llm_model())

  model <- "llama-3.1-8b-instant"
  llm_model(model)
  expect_equal(llm_model(), model)

  llm_model(orig_model)
  expect_equal(llm_model(), orig_model)
})

test_that("llm_max_calls", {
  expect_true(is.function(metacheck::llm_max_calls))
  expect_no_error(helplist <- help(llm_max_calls, metacheck))

  n <- getOption("metacheck.llm_max_calls")
  n2 <- llm_max_calls()
  expect_true(is.integer(n))
  expect_true(n > 0)
  expect_equal(n, n2)

  expect_error(llm_max_calls("a"), "n must be a number")
  expect_equal(getOption("metacheck.llm_max_calls"), n)

  expect_warning(llm_max_calls(0), "n must be greater than 0")
  expect_equal(getOption("metacheck.llm_max_calls"), n)

  expect_no_error(llm_max_calls(8))
  expect_equal(getOption("metacheck.llm_max_calls"), 8)

  text <- data.frame(
    text = 1:20,
    id = 1:20
  )
  expect_error(llm(text, "summarise", seed = 8675309),
               "This would make 20 calls to the LLM")

  # return to original value
  expect_no_error(llm_max_calls(n))
  expect_equal(llm_max_calls(), n)
})


test_that("llm_use FALSE", {
  # error on llm_use() == FALSE
  llm_use(FALSE)
  text <- c("hello", "number", "ten", 12)
  query <- "Is this a number? Answer only 'TRUE' or 'FALSE'"
  expect_error(is_number <- llm(text, query, seed = 1), "llm_use")
})

# tests that require api.groq.com

# httptest::start_capturing()
# httptest::use_mock_api()

test_that("llm_use TRUE", {
  llm_use(TRUE)
  skip_if_offline("api.groq.com")

  text <- c("hello", "number", "ten", 12)
  query <- "Is this a number? Answer only 'TRUE' or 'FALSE'"
  is_number <- llm(text, query, seed = 8675309)
  expect_equal(is_number$text, text)
  expect_equal(is_number$answer[[1]], "FALSE")
  expect_equal(is_number$answer[[4]], "TRUE")

  expect_error(llm("hi", "repeat this", model = "not a model", seed = 8675309), "not available")

  # duplicates should only generate 1 query
  text <- c("A", "A", 1, 1)
  query <- "Is this a letter? Answer only 'TRUE' or 'FALSE'"
  is_letter <- llm(text, query, seed = 12345)

  expect_equal(is_letter$text, text)
  expect_equal(is_letter$answer[[1]], is_letter$answer[[2]])
  expect_equal(is_letter$answer[[3]], is_letter$answer[[4]])

  expect_equal(is_letter$time[[2]], 0)
  expect_equal(is_letter$time[[4]], 0)
  expect_equal(is_letter$tokens[[2]], 0)
  expect_equal(is_letter$tokens[[4]], 0)
  expect_true(is_letter$time[[1]] > 0)
  expect_true(is_letter$time[[3]] > 0)
  expect_true(is_letter$tokens[[1]] > 0)
  expect_true(is_letter$tokens[[3]] > 0)
})

test_that("sample size", {
  skip_if_offline("api.groq.com")

  papers <- read(demodir())
  text <- search_text(papers, section = "method", return = "section")
  query <- "What is the sample size of this study (e.g., the number of participants tested?

  Please give your answer exactly like this: 'XXX (XX men, XX women)', with the total number first, then any subsets in parentheses. If there is not enough information to answer, answer 'NA'"

  # needs seed to work with httptest
  res <- llm(text, query, seed = 8675309)

  expect_equal(res$text, text$text)
  expect_equal(res$id, c("eyecolor", "incest"))
  # expect_equal(res$answer[[1]], "300 (150 men, 150 women)")
  # expect_equal(res$answer[[2]], "1998 (666 men, 1332 women)")

  ## text vector
  text_vector <- text$text[text$id == text$id[[1]]]
  res2 <- llm(text_vector, query, seed = 8675309)
  expect_equal(names(res2), c("text", "answer", "time", "tokens"))
  expect_equal(res2$answer[[1]], res$answer[[1]])
})

#test_that("exceeds tokens", {
  ## big text (new model has a much bigger limit)
  # text <- psychsci[7] |> search_text(return = "id")
  # # nchar(text$text)
  # query <- "Respond with the exact text"
  # expect_message(
  #   expect_warning(
  #     answer <- llm(text, query),
  #     "tokens/rate_limit_exceeded", fixed = TRUE),
  #   "requests left", fixed = TRUE)
#})

test_that("rate limiting", {
  skip("Rate limiting test")
  skip_if_offline("api.groq.com")

  text <- c(LETTERS, 0:7)
  query <- "Respond with the exact text"

  # rate limited at 30 RPM - probably more now
  llm_max_calls(40)
  answer <- llm(text, query, seed = 1)
  expect_true(all(!is.na(answer$answer)))

  llm_max_calls(30)
})


test_that("llm_model_list", {
  expect_true(is.function(metacheck::llm_model_list))
  expect_no_error(helplist <- help(llm_model_list, metacheck))

  skip_if_offline("api.groq.com")

  models <- llm_model_list()
  expect_equal(names(models), c("id", "owned_by", "created", "context_window"))
  expect_true(llm_model() %in% models$id)
})

#httptest::stop_mocking()
# httptest::stop_capturing()
