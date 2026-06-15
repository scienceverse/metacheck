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

test_that("llm_use", {
  expect_true(is.function(metacheck::llm_use))
  expect_no_error(helplist <- help(llm_use, metacheck))

  expect_error(llm_use("no"))

  obs <- llm_use(TRUE)
  expect_true(obs)
  expect_true(llm_use())

  obs <- llm_use(FALSE)
  expect_false(obs)
  expect_false(llm_use())
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

test_that("llm_use TRUE", {
  skip_if(Sys.getenv("GROQ_API_KEY") == "")
  llm_use(TRUE)
  llm_model("groq")

  text <- c("hello", "number", "ten", 12)
  system_prompt <- "Is this a number? Answer only 'TRUE' or 'FALSE'"
  is_number <- llm(text, system_prompt)
  expect_equal(is_number$text, text)
  expect_equal(is_number$answer[[1]], "FALSE")
  expect_equal(is_number$answer[[4]], "TRUE")

  expect_warning(x <- llm("hi", "repeat this", model = "not a model"),
               "Can't find provider")

  # duplicates should only generate 1 system_prompt
  text <- c("A", "A", 1, 1)
  system_prompt <- "Is this a letter A-Z? Answer only 'TRUE' or 'FALSE'"
  is_letter <- llm(text, system_prompt)

  expect_equal(is_letter$text, text)
  expect_equal(is_letter$answer[[1]], is_letter$answer[[2]])
  expect_equal(is_letter$answer[[3]], is_letter$answer[[4]])
}, "mock")


test_that("llm_model_list", {
  expect_true(is.function(metacheck::llm_model_list))
  expect_no_error(helplist <- help(llm_model_list, metacheck))

  expect_error(llm_model_list("notamodel"), "Invalid platform")

  o <- llm_model_list("ollama")
  expect_equal(nrow(o), 2)

  httptest2::without_internet({
    o <- llm_model_list("ollama")
    expect_equal(nrow(o), 0)
  })
}, "mock")

test_that(".llm_model_list_groq", {
  expect_true(is.function(metacheck:::.llm_model_list_groq))

  expect_error(.llm_model_list_groq(1))

  g1 <- .llm_model_list_groq()
  g2 <- llm_model_list("groq")
  expect_in("platform", names(g2))
  expect_disjoint(names(g1), "platform")
  expect_setequal(g1$id, g2$id)
  expect_true(inherits(g1$created_at, "Date"))
}, "mock")


test_that("gemini", {
  #skip_llm()
  llm_use(TRUE)

  text <- LETTERS[1:2]
  system_prompt <- "Is this a vowel? Answer only 'TRUE' or 'FALSE'."
  model <- "google_gemini"
  obs <- llm(text, system_prompt, model = model)
  expect_equal(unclass(obs$answer),
               as.character(c(T, F)))
}, "mock")


test_that(".llm_ollama_native", {
  expect_true(is.function(metacheck:::.llm_ollama_native))

  text <- "A"
  system_prompt <- "Is this a vowel? Answer only 'TRUE' or 'FALSE'."
  model <- "qwen2.5:3b"
  resp <- .llm_ollama_native(text, system_prompt, model)
  expect_in(resp, c("TRUE", "FALSE"))

  resp2 <- llm(text, system_prompt, model = "ollama/qwen2.5:3b")
  expect_message(resp3 <- llm(text, system_prompt, model = "ollama"), "Using model")

  expect_equal(names(resp2), c("text", "answer"))
  expect_equal(names(resp3), c("text", "answer"))

  expect_error(.llm_ollama_native(text, system_prompt, "notamodel"))

  expect_error(llm(text, system_prompt, model = "ollama/notamodel"),
               "Ollama is installed, but the model notamodel is not available")

  # TODO: test thinking
  # model <- "ollama/smollm:135m"
  # default <- llm(text, system_prompt, model = model)
  # think <- llm(text, system_prompt, model = model, params = list(think = TRUE))
  # nothink <- llm(text, system_prompt, model = model, params = list(think = FALSE))
}, "mock")


# test_that("no internet", {
#   httptest2::without_internet({
#     expect_error(
#       llm("A", "Is this a vowel?", model = "ollama"),
#       "Ollama is not running"
#     )
#   })
# })


test_that(".unnest_result", {
  expect_true(is.function(metacheck:::.unnest_result))

  expect_error(.unnest_result(bad_arg))

  # Structured extraction
  # llm_use(TRUE)
  # chat <- ellmer::chat(
  #   name = "groq/openai/gpt-oss-safeguard-20b",
  #   system_prompt = "Classify the input.",
  #   params = list(temperature = 0)
  # )
  #
  # type <- ellmer::type_object(
  #   n_letters = ellmer::type_integer("How many letters in the input"),
  #   is_number = ellmer::type_boolean("Whether the input is a number")
  # )
  #
  # result <- chat$chat_structured("hello", type = type)

  result <- list(n_letters = 5L, is_number = FALSE)
  df <- .unnest_result(result)
  exp <- data.frame(n_letters = 5, is_number = FALSE)
  expect_equal(df, exp)
})

