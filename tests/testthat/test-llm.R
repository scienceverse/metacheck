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

test_that("llm fails fast when model is unset or params are malformed", {
  withr::local_options(metacheck.llm.use = TRUE)
  model0 <- llm_model()
  withr::defer(llm_model(model0))

  llm_model(NULL)
  expect_error(
    llm("hi", "repeat this"),
    "No LLM model set",
    fixed = TRUE
  )

  expect_error(
    llm("hi", "repeat this", model = "groq/llama-3.1-8b-instant", params = 1),
    "params must be a named list",
    fixed = TRUE
  )
})

test_that(".llm_error_message surfaces the provider's error body", {
  # A bare "HTTP 400 Bad Request." hides the provider's actual reason (e.g.
  # Groq's oversized-prompt message); when the condition carries the httr2
  # response — directly or via its parent, as ellmer wraps it — the body's
  # error message is appended.
  resp <- httr2::response_json(
    status_code = 400,
    body = list(error = list(message = "Please reduce the length of the messages."))
  )
  e <- simpleError("HTTP 400 Bad Request.")
  e$resp <- resp
  msg <- metacheck:::.llm_error_message(e)
  expect_match(msg, "HTTP 400 Bad Request", fixed = TRUE)
  expect_match(msg, "Please reduce the length of the messages", fixed = TRUE)

  # response on the parent condition (ellmer-style wrapping)
  wrapper <- simpleError("Failed to call chat API.")
  wrapper$parent <- e
  expect_match(metacheck:::.llm_error_message(wrapper),
               "Please reduce the length of the messages", fixed = TRUE)

  # no response attached: the original message comes back unchanged
  plain <- simpleError("boom")
  expect_identical(metacheck:::.llm_error_message(plain), "boom")
})

test_that(".llm_json_retryable catches JSON parse/code-fence failures", {
  e1 <- simpleError("Failed to generate JSON")
  expect_true(metacheck:::.llm_json_retryable(e1))

  e2 <- simpleError(
    "lexical error: invalid char in json text. ```json { \"studies\": [] }"
  )
  expect_true(metacheck:::.llm_json_retryable(e2))

  e3 <- simpleError("HTTP 401 Unauthorized")
  expect_false(metacheck:::.llm_json_retryable(e3))
})

test_that("llm routes vllm/<model> through chat_vllm", {
  withr::local_options(
    metacheck.llm.use = TRUE,
    metacheck.llm.cache = FALSE,
    metacheck.llm.vllm.base_url = "https://example.test/v1"
  )

  called <- FALSE
  seen <- list()
  testthat::local_mocked_bindings(
    chat_vllm = function(model, base_url, credentials,
                         system_prompt, params, ...) {
      called <<- TRUE
      seen$model <<- model
      seen$base_url <<- base_url
      seen$token <<- credentials()
      structure(list(
        chat = function(text, echo = FALSE) "TRUE"
      ), class = "Chat")
    },
    .package = "ellmer"
  )

  withr::local_envvar(VLLM_API_KEY = "test-key")
  out <- llm("hello", "Answer TRUE", model = "vllm/GLM-5.2-NVFP4")

  expect_true(called)
  expect_identical(seen$model, "GLM-5.2-NVFP4")
  expect_identical(seen$base_url, "https://example.test/v1")
  expect_identical(seen$token, "test-key")
  expect_identical(out$answer[[1]], "TRUE")
})

test_that("llm reports clear error when vllm base URL is not configured", {
  withr::local_options(
    metacheck.llm.use = TRUE,
    metacheck.llm.cache = FALSE,
    metacheck.llm.vllm.base_url = NULL
  )

  out <- suppressWarnings(llm("hello", "Answer TRUE", model = "vllm/GLM-5.2-NVFP4"))
  expect_true(out$error[[1]])
  expect_match(out$error_msg[[1]], "metacheck.llm.vllm.base_url", fixed = TRUE)
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

  llm_model(NULL)
  expect_null(llm_model())

  llm_model(orig_model)
  expect_equal(llm_model(), orig_model)
})

test_that("llm_max_calls", {
  expect_true(is.function(metacheck::llm_max_calls))
  expect_no_error(helplist <- help(llm_max_calls, metacheck))

  model0 <- llm_model()
  withr::defer(llm_model(model0))
  llm_model("groq/llama-3.1-8b-instant")

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


# https://github.com/scienceverse/metacheck/issues/337 -- these tests used to
# rely on recorded HTTP fixtures under tests/testthat/apis/api.groq.com/,
# which drifted out of sync (a bare "groq" model resolves to whichever model
# ellmer/Groq currently default to -- openai/gpt-oss-20b at the time of this
# fix -- and no fixture was ever recorded for that specific model, so every
# request hashed to "unexpected request" and silently fell through to a real
# network call, or NA on failure). Mocking ellmer::chat() directly (as
# "llm handles an empty structured result without error" below already does
# for the structured path) tests the same llm() logic without depending on
# live network access, a real API key, or fixture files staying in sync with
# whatever Groq's current default model happens to be.

test_that("llm warns on an unrecognised provider", {
  # ellmer rejects an unknown provider before any network call is made, so
  # this needs no mock -- kept in its own test (not inside "llm_use TRUE"
  # below) so an active local_mocked_bindings(chat = ...) elsewhere can never
  # accidentally swallow this real ellmer::chat() dispatch behaviour.
  withr::local_options(metacheck.llm.use = TRUE, metacheck.llm.cache = FALSE)
  expect_warning(x <- llm("hi", "repeat this", model = "not a model"),
               "Can't find provider")
})

test_that("llm_use TRUE", {
  withr::local_options(metacheck.llm.use = TRUE, metacheck.llm.cache = FALSE)

  testthat::local_mocked_bindings(
    chat = function(...) structure(list(
      chat = function(text, echo = FALSE) {
        is_num <- !is.na(suppressWarnings(as.numeric(text)))
        if (is_num) "TRUE" else "FALSE"
      }
    ), class = "Chat"), .package = "ellmer")

  text <- c("hello", "number", "ten", 12)
  system_prompt <- "Is this a number? Answer only 'TRUE' or 'FALSE'"
  is_number <- llm(text, system_prompt, model = "groq/x")
  expect_equal(is_number$text, text)
  expect_equal(is_number$answer[[1]], "FALSE")
  expect_equal(is_number$answer[[4]], "TRUE")

  # duplicates should only generate 1 call per unique text
  call_count <- 0
  testthat::local_mocked_bindings(
    chat = function(...) structure(list(
      chat = function(text, echo = FALSE) {
        call_count <<- call_count + 1
        if (grepl("^[A-Za-z]$", text)) "TRUE" else "FALSE"
      }
    ), class = "Chat"), .package = "ellmer")

  text <- c("A", "A", 1, 1)
  system_prompt <- "Is this a letter A-Z? Answer only 'TRUE' or 'FALSE'"
  is_letter <- llm(text, system_prompt, model = "groq/x")

  expect_equal(is_letter$text, text)
  expect_equal(is_letter$answer[[1]], is_letter$answer[[2]])
  expect_equal(is_letter$answer[[3]], is_letter$answer[[4]])
  expect_equal(call_count, 2) # 4 inputs, 2 unique texts
})


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
  # See the issue-337 note above test-llm.R:205 -- same stale-fixture problem,
  # different provider (Gemini instead of Groq).
  withr::local_options(metacheck.llm.use = TRUE, metacheck.llm.cache = FALSE)

  testthat::local_mocked_bindings(
    chat = function(...) structure(list(
      chat = function(text, echo = FALSE) {
        if (text %in% c("A", "E", "I", "O", "U")) "TRUE" else "FALSE"
      }
    ), class = "Chat"), .package = "ellmer")

  text <- LETTERS[1:2]
  system_prompt <- "Is this a vowel? Answer only 'TRUE' or 'FALSE'."
  model <- "google_gemini"
  obs <- llm(text, system_prompt, model = model)
  expect_equal(unclass(obs$answer),
               as.character(c(T, F)))
})


test_that(".llm_ollama_native", {
  expect_true(is.function(metacheck:::.llm_ollama_native))

  text <- "A"
  system_prompt <- "Is this a vowel? Answer only 'TRUE' or 'FALSE'."
  model <- "qwen2.5:3b"
  resp <- .llm_ollama_native(text, system_prompt, model)
  expect_in(resp, c("TRUE", "FALSE"))

  resp2 <- llm(text, system_prompt, model = "ollama/qwen2.5:3b")
  expect_message(resp3 <- llm(text, system_prompt, model = "ollama"), "Using model")

  # llm() also returns `error`/`error_msg` (which call failed, and why), so the
  # check is that the answer columns are there, not that they are the only ones.
  expect_in(c("text", "answer"), names(resp2))
  expect_in(c("text", "answer"), names(resp3))

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

test_that("llm handles an empty structured result without error", {
  # A wrapped empty array ({variables: []}) unnests to 0 rows; the join must
  # not error and inputs should come back with NA extracted columns.
  withr::local_options(metacheck.llm.use = TRUE, metacheck.llm.cache = FALSE)
  ts <- ellmer::type_object(variables = ellmer::type_array(
    ellmer::type_object(variable_name = ellmer::type_string(),
                        label = ellmer::type_string())))

  # every call empty
  testthat::local_mocked_bindings(
    chat = function(...) structure(list(
      chat_structured = function(text, type) list(variables = list())),
      class = "Chat"), .package = "ellmer")
  res <- llm(text = data.frame(text = c("a", "b")), text_col = "text",
             system_prompt = "x", type = ts, model = "groq/x")
  expect_equal(nrow(res), 2)
  expect_false(any(c("variable_name", "label") %in% names(res)) &&
                 any(!is.na(res$variable_name)))

  # mixed: first input returns a row, second is empty
  n <- 0
  testthat::local_mocked_bindings(
    chat = function(...) structure(list(chat_structured = function(text, type) {
      n <<- n + 1
      if (n == 1) list(variables = list(list(variable_name = "dv", label = "outcome")))
      else list(variables = list())
    }), class = "Chat"), .package = "ellmer")
  res2 <- llm(text = data.frame(text = c("has", "empty")), text_col = "text",
              system_prompt = "x", type = ts, model = "groq/x")
  expect_equal(res2$variable_name, c("dv", NA))
})

