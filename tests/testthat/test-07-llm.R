test_that("exists", {
  expect_true(is.function(llm))
  skip_on_cran()
  skip_if_offline("api.groq.com")

  expect_error(llm())
  expect_error(llm("hi"))

  expect_error(llm("hi", "repeat this", model = "not a model"), "not available")

  # temperature
  expect_error(llm("hi", "repeat this", temperature = "a"),
               "The argument `temperature` must be a positive number",
               fixed = TRUE)
  expect_error(llm("hi", "repeat this", temperature = -3),
               "The argument `temperature` must be between 0.0 and 2.0",
               fixed = TRUE)
  expect_error(llm("hi", "repeat this", temperature = 2.1),
               "The argument `temperature` must be between 0.0 and 2.0",
               fixed = TRUE)

  # top_p
  expect_error(llm("hi", "repeat this", top_p = "a"),
               "The argument `top_p` must be a positive number",
               fixed = TRUE)
  expect_error(llm("hi", "repeat this", top_p = -3),
               "The argument `top_p` must be between 0.0 and 1.0",
               fixed = TRUE)
  expect_error(llm("hi", "repeat this", top_p = 2.1),
               "The argument `top_p` must be between 0.0 and 1.0",
               fixed = TRUE)
})

test_that("llm_max_calls", {
  expect_true(is.function(llm_max_calls))

  n <- getOption("papercheck.llm_max_calls")
  expect_true(is.integer(n))
  expect_true(n > 0)

  expect_error(llm_max_calls("a"), "n must be a number")
  expect_equal(getOption("papercheck.llm_max_calls"), n)

  expect_warning(llm_max_calls(0), "n must be greater than 0")
  expect_equal(getOption("papercheck.llm_max_calls"), n)

  expect_no_error(llm_max_calls(8))
  expect_equal(getOption("papercheck.llm_max_calls"), 8)

  skip_on_cran()
  skip_on_covr()
  skip_if_offline("api.groq.com")

  text <- data.frame(
    text = 1:20,
    id = 1:20
  )
  expect_error(llm(text, "summarise"),
               "This would make 20 calls to the LLM")

  # return to original value
  expect_no_error(llm_max_calls(n))

  expect_equal(llm_max_calls(), n)
})

test_that("basic", {
  skip_on_cran()
  skip_if_offline("api.groq.com")
  skip_if(Sys.getenv("GROQ_API_KEY") == "", message = "Requires groq API key")

  text <- c("hello", "number", "ten", 12)
  query <- "Is this a number? Answer only 'TRUE' or 'FALSE'"
  expect_message( is_number <- llm(text, query, seed = 8675309) )

  expect_equal(is_number$text, text)
  expect_equal(is_number$answer[[1]], "FALSE")
  expect_equal(is_number$answer[[4]], "TRUE")

  # duplicates should only generate 1 query
  text <- c("A", "A", 1, 1)
  query <- "Is this a letter? Answer only 'TRUE' or 'FALSE'"
  expect_message( is_letter <- llm(text, query, seed = 12345) )

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
  skip_on_cran()
  skip_if_offline("api.groq.com")
  skip_if(Sys.getenv("GROQ_API_KEY") == "", message = "Requires groq API key")

  papers <- read_grobid(demodir())
  text <- search_text(papers, section = "method", return = "section")
  query <- "What is the sample size of this study (e.g., the number of participants tested?

  Please give your answer exactly like this: 'XXX (XX men, XX women)', with the total number first, then any subsets in parentheses. If there is not enough infomation to answer, answer 'NA'"


  expect_message( res <- llm(text, query) )

  expect_equal(res$text, text$text)
  expect_equal(res$id, c("eyecolor", "incest"))
  # expect_equal(res$answer[[1]], "300 (150 men, 150 women)")
  # expect_equal(res$answer[[2]], "1998 (666 men, 1332 women)")

  ## text vector
  text_vector <- text$text[text$id == text$id[[1]]]
  expect_message( res2 <- llm(text_vector, query) )
  expect_equal(names(res2), c("text", "answer", "time", "tokens"))
  expect_equal(res2$answer[[1]], res$answer[[1]])
})

test_that("exceeds tokens", {
  skip_on_cran()
  skip_if_offline("api.groq.com")
  skip_if(Sys.getenv("GROQ_API_KEY") == "", message = "Requires groq API key")

  ## big text (new model has a much bigger limit)
  # text <- psychsci[7] |> search_text(return = "id")
  # # nchar(text$text)
  # query <- "Respond with the exact text"
  # expect_message(
  #   expect_warning(
  #     answer <- llm(text, query),
  #     "tokens/rate_limit_exceeded", fixed = TRUE),
  #   "requests left", fixed = TRUE)
})

test_that("rate limiting", {
  skip("Very long test")
  skip_on_cran()
  skip_if_offline("api.groq.com")
  skip_if(Sys.getenv("GROQ_API_KEY") == "", message = "Requires groq API key")

  text <- c(LETTERS, 0:7)
  query <- "Respond with the exact text"

  # rate limited at 30 RPM
  llm_max_calls(40)
  expect_message( answer <- llm(text, query) )
  expect_true(all(!is.na(answer$answer)))

  llm_max_calls(30)
})

test_that("llm_model", {
  orig_model <- llm_model()

  expect_error(llm_model(T))
  expect_equal(orig_model, llm_model())

  model <- "llama-3.1-8b-instant"
  llm_model(model)
  expect_equal(llm_model(), model)

  llm_model(orig_model)
  expect_equal(llm_model(), orig_model)
})

test_that("llm_model_list", {
  skip_on_cran()
  skip_if_offline("api.groq.com")
  skip_if(Sys.getenv("GROQ_API_KEY") == "", message = "Requires groq API key")

  models <- llm_model_list()
  expect_equal(names(models), c("id", "owned_by", "created", "context_window"))
  expect_true(llm_model() %in% models$id)
})

test_that("json_expand", {
  table <- data.frame(
    id = 1:5,
    answer = c(
      '{"number": "1", "letter": "A", "bool": true}',
      '{"number": "2", "letter": "B", "bool": "FALSE"}',
      '{"number": "3", "letter": "", "bool": null}',
      'oh no, the LLM misunderstood',
      '{"number": "5", "letter": ["E", "F"], "bool": false}'
    )
  )

  expanded <- json_expand(table)
  expect_equal(names(expanded), c("id", "answer", "number", "letter", "bool", "error"))
  expect_equal(typeof(expanded$number), "integer")
  expect_equal(typeof(expanded$letter), "character")
  expect_equal(typeof(expanded$bool), "logical")
  expect_equal(expanded$letter, c("A", "B", "", NA, "E; F"))
  expect_equal(expanded$bool, c(TRUE, FALSE, NA, NA, FALSE))
})
