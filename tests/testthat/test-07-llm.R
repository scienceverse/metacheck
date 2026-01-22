# tests that don't require a web connections
test_that("llm", {
  expect_true(is.function(metacheck::llm))
  expect_no_error(helplist <- help(llm, metacheck))

  llm_use(TRUE)

  expect_error(llm())
  expect_error(llm("hi"))

  # top_p
  params <-  list(top_p = "a")
  expect_error(llm("hi", "repeat this", params = params),
               "`top_p` must be a number",
               fixed = TRUE)
  params <- list(top_p = -3)
  expect_error(llm("hi", "repeat this", params = params),
               "`top_p` must be a number",
               fixed = TRUE)

  llm_use(FALSE)
  expect_error(llm("hi", "repeat this", model = "groq"),
               "llm_use(TRUE)",
               fixed = TRUE)

})


test_that("llm_model", {
  expect_true(is.function(metacheck::llm_model))
  expect_no_error(helplist <- help(llm_model, metacheck))

  orig_model <- llm_model()

  expect_error(llm_model(T))
  expect_equal(orig_model, llm_model())

  model <- "groq/llama-3.1-8b-instant"
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
  llm_use(TRUE)
  expect_error(llm(text, "summarise"),
               "This would make 20 calls to the LLM")

  # return to original value
  expect_no_error(llm_max_calls(n))
  expect_equal(llm_max_calls(), n)
})


# tests that require api.groq.com

# httptest::start_capturing()
# httptest::use_mock_api()

test_that("llm_use TRUE", {
  skip_llm()
  llm_use(TRUE)
  llm_model("groq")

  text <- c("hello", "number", "ten", 12)
  system_prompt <- "Is this a number? Answer only 'TRUE' or 'FALSE'"
  is_number <- llm(text, system_prompt)
  expect_equal(is_number$text, text)
  expect_equal(is_number$answer[[1]], "FALSE")
  expect_equal(is_number$answer[[4]], "TRUE")

  expect_error(llm("hi", "repeat this", model = "not a model"),
               "Can't find provider")

  # duplicates should only generate 1 system_prompt
  text <- c("A", "A", 1, 1)
  system_prompt <- "Is this a letter A-Z? Answer only 'TRUE' or 'FALSE'"
  is_letter <- llm(text, system_prompt)

  expect_equal(is_letter$text, text)
  expect_equal(is_letter$answer[[1]], is_letter$answer[[2]])
  expect_equal(is_letter$answer[[3]], is_letter$answer[[4]])
})

test_that("sample size", {
  skip_llm()
  llm_use(TRUE)
  llm_model("groq")

  papers <- read(demodir())
  text <- search_text(papers, section = "method", return = "section")
  system_prompt <- "What is the sample size of this study (e.g., the number of participants tested?

  Please give your answer exactly like this: 'XXX (XX men, XX women)', with the total number first, then any subsets in parentheses. If there is not enough information to answer, answer 'NA'"

  res <- llm(text, system_prompt)

  expect_equal(res$text, text$text)
  expect_equal(res$id, c("eyecolor", "incest"))

  ## text vector
  text_vector <- text$text[text$id == text$id[[1]]]
  res2 <- llm(text_vector, system_prompt)
  expect_in(names(res2), c("text", "answer"))
  expect_equal(res2$answer[[1]], res$answer[[1]])
})


test_that("llm_model_list", {
  expect_true(is.function(metacheck::llm_model_list))
  expect_no_error(helplist <- help(llm_model_list, metacheck))

  expect_error(llm_model_list("notamodel"), "Invalid platform")

  skip_llm()

  models <- llm_model_list("groq")
  expect_contains(names(models), c("platform", "id", "created_at"))

  groq_models <- models_groq()
  expect_contains(names(groq_models), c("id", "created_at"))
  expect_false("platform" %in% names(groq_models))
  expect_true(inherits(groq_models$created_at, "Date"))

  all <- llm_model_list()
  expect_gt(unique(all$platform) |> length(), 1)
  expect_contains(all$id, groq_models$id)
})

test_that("gemini", {
  skip_llm()
  llm_use(TRUE)

  text <- LETTERS[1:2]
  system_prompt <- "Is this a vowel? Answer only 'TRUE' or 'FALSE'."
  model <- "google_gemini"
  obs <- llm(text, system_prompt, model = model)
  expect_equal(unclass(obs$answer),
               as.character(c(T, F)))
})

test_that("openai", {
  skip_llm()
  llm_use(TRUE)

  text <- LETTERS[1:2]
  system_prompt <- "Is this a vowel? Answer only 'TRUE' or 'FALSE'."
  model <- "openai"
  obs <- llm(text, system_prompt, model = model)
  expect_equal(unclass(obs$answer),
               as.character(c(T, F)))
})
