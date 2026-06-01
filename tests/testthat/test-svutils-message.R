test_that("message", {
  expect_true(is.function(svutils::message))

  expect_error(message(bad_arg))
})

test_that("message", {
  verbose(TRUE)

  msg <- capture_messages(message("hi"))

  if (interactive()) {
    expect_equal(msg, "\033[32mhi\033[39m\n")
  } else {
    expect_equal(msg, "hi\n")
  }

  verbose(FALSE)

  msg <- capture_messages(message("hi"))
  expect_equal(msg, character(0))
})
