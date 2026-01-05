test_that("pytacheck", {
  expect_true(is.function(metacheck::pytacheck))
  expect_no_error(helplist <- help(pytacheck, metacheck))

  expect_error(pytacheck(bad_arg))

  skip_if_offline()

  file_path <- demopdf()
  data <- pytacheck(file_path)
  expect_equal(data$info$input_format, "pdf")
  title <- "To Err is Human: An Empirical Investigation"
  expect_equal(data$info$title, title)


  skip_if_quick()
  # published vs preprint PDFS
  title <- "“You’re Just Envious”: Inferring Benign and Malicious Envy From Facial Expressions and Contextual Information"

  file_path <- test_path("fixtures", "formats", "published.pdf")
  published <- pytacheck(file_path)
  expect_equal(published$info$input_format, "pdf")
  expect_equal(published$info$title, title)

  file_path <- test_path("fixtures", "formats", "preprint.pdf")
  preprint <- pytacheck(file_path)
  expect_equal(preprint$info$input_format, "pdf")
  expect_equal(preprint$info$title, title)
})
