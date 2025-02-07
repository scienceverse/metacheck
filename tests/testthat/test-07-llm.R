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

test_that("max calls", {
  expect_true(is.function(set_llm_max_calls))

  n <- getOption("papercheck.llm_max_calls")
  expect_true(is.integer(n))
  expect_true(n > 0)

  expect_error(set_llm_max_calls("a"), "n must be a number")
  expect_equal(getOption("papercheck.llm_max_calls"), n)

  expect_warning(set_llm_max_calls(0), "n must be greater than 0")
  expect_equal(getOption("papercheck.llm_max_calls"), n)

  expect_no_error(set_llm_max_calls(8))
  expect_equal(getOption("papercheck.llm_max_calls"), 8)

  text <- data.frame(
    text = 1:20,
    id = 1:20
  )
  expect_error(llm(text, "summarise"),
               "This would make 20 calls to the LLM")

  # return to original value
  expect_no_error(set_llm_max_calls(n))
})

test_that("basic", {
  skip_on_cran()
  skip_if_offline("api.groq.com")
  skip_if(Sys.getenv("GROQ_API_KEY") == "", message = "Requires groq API key")

  papers <- read_grobid(demodir())
  text <- search_text(papers, section = "method", return = "section")
  query <- "What is the sample size of this study (e.g., the number of participants tested?

  Please give your answer exactly like this: 'XXX (XX men, XX women)', with the total number first, then any subsets in parentheses. If there is not enough infomation to answer, answer 'NA'"


  suppressMessages( res <- llm(text, query, include_query = TRUE) )

  expect_equal(res$query[[1]], query)
  expect_equal(res$text, text$text)
  # expect_equal(res$answer[[1]], "300 (150 men, 150 women)")
  # expect_equal(res$answer[[2]], "1998 (666 men, 1332 women)")
  #expect_equal(res$id, c("eyecolor.xml", "incest.xml"))

  ## text vector
  text_vector <- text$text[text$id == text$id[[1]]]
  suppressMessages( res2 <- llm(text_vector, query) )
  expect_equal(names(res2), c("text", "answer", "time", "tokens"))
  expect_equal(res2$answer[[1]], res$answer[[1]])
})


