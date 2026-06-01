test_that("rep_if", {
  x <- list()
  obs <- rep_if(x, 1)
  exp <- list()
  expect_equal(obs, exp)

  x <- list(NULL, 2)
  obs <- rep_if(x, 1)
  exp <- list(1, 2)
  expect_equal(obs, exp)

  x <- list(NA, 2)
  obs <- rep_if(x, 1, NA)
  exp <- list(1, 2)
  expect_equal(obs, exp)

  x <- list("", 2)
  obs <- rep_if(x, 1, "")
  exp <- list(1, 2)
  expect_equal(obs, exp)

  x <- list("", 2, NA)
  obs <- rep_if(x, 1, c("", NA))
  exp <- list(1, 2, 1)
  expect_equal(obs, exp)

  x <- list(1, NULL, 3, NULL, NULL)
  y <- 1:5
  obs <- rep_if(x, y)
  exp <- as.list(1:5)
  expect_equal(obs, exp)

  x <- c("", "B", "X")
  y <- LETTERS[1:3]
  obs <- rep_if(x, y, c("", "X"))
  exp <- LETTERS[1:3]
  expect_equal(obs, exp)
})


test_that("verbose", {
  expect_true(is.function(svutils::verbose))
  expect_no_error(helplist <- help(verbose, svutils))

  expect_equal(verbose(FALSE), FALSE)
  expect_equal(verbose(), FALSE)
  expect_equal(verbose(TRUE), TRUE)
  expect_equal(verbose(), TRUE)
  expect_equal(verbose(0), FALSE)
  expect_equal(verbose("FALSE"), FALSE)
  expect_equal(verbose(1), TRUE)
  expect_equal(verbose("TRUE"), TRUE)

  expect_error(verbose("G"))
  expect_invisible(verbose(FALSE))
  expect_visible(verbose())
})

test_that("online", {
  expect_true(is.function(svutils::online))
  expect_no_error(helplist <- help(online, svutils))

  skip_if_offline("google.com")

  expect_true(online())
  expect_true(online("google.com"))
  expect_true(online("http://google.com"))
  expect_true(online("https://google.com"))
  expect_true(online("https://www.google.com"))
  expect_true(online("https://google.com/images"))

  expect_false(online("notasite"))
})


