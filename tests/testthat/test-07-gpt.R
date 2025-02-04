test_that("exists", {
  expect_true(is.function(gpt))

  expect_error(gpt("hi", "repeat this", chunk_size = 0),
               "The argument `chunk_size` must be larger than 0",
               fixed = TRUE)
  expect_error(gpt("hi", "repeat this", chunk_size = "a"),
               "The argument `chunk_size` must be a positive integer",
               fixed = TRUE)
  expect_error(gpt("hi", "repeat this", chunk_overlap = "a"),
               "The argument `chunk_overlap` must be a positive integer",
               fixed = TRUE)
  expect_error(gpt("hi", "repeat this", chunk_overlap = -100),
               "The argument `chunk_overlap` must be 0 or larger",
               fixed = TRUE)
  expect_error(gpt("hi", "repeat this", chunk_overlap = 1000),
               "The argument `chunk_overlap` must be smaller than `chunk_size`",
               fixed = TRUE)
  expect_error(gpt("hi", "repeat this", temperature = "a"),
               "The argument `temperature` must be a positive number",
               fixed = TRUE)
  expect_error(gpt("hi", "repeat this", temperature = -3),
               "The argument `temperature` must be between 0.0 and 2.0",
               fixed = TRUE)
  expect_error(gpt("hi", "repeat this", temperature = 2.1),
               "The argument `temperature` must be between 0.0 and 2.0",
               fixed = TRUE)
})

test_that("max calls", {
  expect_true(is.function(set_gpt_max_calls))

  n <- getOption("papercheck.gpt_max_calls")
  expect_true(is.integer(n))
  expect_true(n > 0)

  expect_error(set_gpt_max_calls("a"), "n must be a number")
  expect_equal(getOption("papercheck.gpt_max_calls"), n)

  expect_warning(set_gpt_max_calls(0), "n must be greater than 0")
  expect_equal(getOption("papercheck.gpt_max_calls"), n)

  expect_no_error(set_gpt_max_calls(8))
  expect_equal(getOption("papercheck.gpt_max_calls"), 8)

  text <- data.frame(
    text = 1:20,
    id = 1:20
  )
  expect_error(gpt(text, "summarise"),
               "This would make 20 calls to chatGPT")

  # return to original value
  expect_no_error(set_gpt_max_calls(n))
})

test_that("basic", {
  skip_on_cran()
  skip_if_offline(host = "chat.openai.com")
  skip_if(Sys.getenv("CHATGPT_KEY") == "", message = "Requires ChatGPT API key")

  s <- read_grobid(demodir())
  text <- search_text(s, section = "method", return = "section")
  query <- "What is the sample size of this study (e.g., the number of participants tested?"
  context <- "Please give your answer exactly like this: 'XXX (XX men, XX women)', with the total number first, then any subsets in parentheses."


  expect_message( res <- gpt(text, query, context, include_query = TRUE) )

  expect_equal(res$query[[1]], query)
  expect_equal(res$context[[1]], context)
  # expect_equal(res$answer[[1]], "300 (150 men, 150 women)")
  # expect_equal(res$answer[[2]], "1998 (666 men, 1332 women)")
  expect_equal(res$id, c("eyecolor.xml", "incest.xml"))

  ## text vector
  text_vector <- text$text[text$id == text$id[[1]]]
  expect_message( res2 <- gpt(text_vector, query, context) )
  expect_equal(names(res2), c("id", "answer", "cost"))
  expect_equal(res2$answer[[1]], res$answer[[1]])
})

test_that("multiple group by", {
  skip("long")
  skip_on_cran()
  skip_if_offline(host = "chat.openai.com")
  skip_if(Sys.getenv("CHATGPT_KEY") == "", message = "Requires ChatGPT API key")

  s <- read_grobid(demodir())
  text <- search_text(s, return = "section", section = c("method", "results"))
  groups <- dplyr::summarise(text, .by = c(id, section))

  query <- "Summarise this text"
  context <- "Answer in one short sentence, for a scientific audience"
  expect_message( res <- gpt(text, query, context, group_by = c("id", "section")) )
  expect_equal(names(res), c("id", "section", "answer", "cost"))
  expect_equal(res[, 1:2], groups)
})

test_that("changing options", {
  skip("Requires ChatGPT API key")
  res$answer

  size100 <- gpt(text, query, context, chunk_size = 100, chunk_overlap = 25)
  size100$answer

  overlap200 <- gpt(text, query, context, chunk_overlap = 200)
  overlap200$answer

  temp1 <- gpt(text, query, context, temperature = 1)
  temp1$answer

  temp2 <- gpt(text, query, context, temperature = 2)
  temp2$answer
})
