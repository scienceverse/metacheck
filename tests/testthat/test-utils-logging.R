test_that("logpath", {
  expect_true(is.function(metacheck:::logpath))

  expect_error(logpath("x"))

  lp <- logpath()
  expect_match(lp, "metacheck\\.log")
})

test_that("logger", {
  expect_true(is.function(metacheck::logger))
  expect_no_error(helplist <- help(logger, metacheck))

  # first write to temp log
  log_path <- withr::local_tempfile(fileext = ".log")
  obs_path <- logger("test", list(a = 1, b = TRUE, c = "hi"), log_path)
  expect_equal(log_path, obs_path)

  log <- lastlog(1, log_path)
  obs <- log[c(1, 3:5)]
  exp <- list(label = "test", a = 1, b = TRUE, c = "hi")
  expect_equal(obs, exp)

  # write to main log
  obs_path <- logger("test")
  dt <- Sys.time() |> format("%Y-%m-%d %H:")
  expect_true(obs_path != log_path)
  log <- lastlog(1, obs_path)
  expect_equal(log$label, "test")
  expect_equal(substr(log$dt, 1, 14), dt)
})

test_that("logger truncate at 1000", {
  skip("30-second test") # logs write about 30/second
  log_path <- withr::local_tempfile(fileext = ".log")
  for (i in 1:1010) {
    logger(i, list(i = i), log_path)
  }
  logs <- lastlog(1:1010, log_path)
  expect_equal(nrow(logs), 1000)
  expect_equal(logs$label, 1010:11)
  expect_equal(logs$i, 1010:11)
})

test_that("lastlog", {
  expect_true(is.function(metacheck::lastlog))
  expect_no_error(helplist <- help(lastlog, metacheck))

  expect_error(lastlog("A"), "^i")

  # set up 2 log items
  log_path <- withr::local_tempfile(fileext = ".log")
  logger("test A", list(msg = "hi"), log_path)
  logger("test B", list(msg = "hi again"), log_path)

  log1 <- lastlog(1, log_path)
  log2 <- lastlog(2, log_path)
  logs <- lastlog(1:2, log_path)

  expect_equal(logs$label, c("test B", "test A"))
  expect_equal(log1$label, "test B")
  expect_equal(log2$label, "test A")
})

test_that("UTF-8 conversion", {
  latin1 <- iconv("\x96", from = "latin1", to = "latin1")
  utf8 <- iconv(latin1, from = "latin1", to = "UTF-8")
  logger("test1", latin1)
  obs <- lastlog(1)
  expect_equal(obs$label, "test1")
  expect_equal(obs$error, utf8)

  logger("test2", latin1)
  obs <- lastlog(1:2)
  expect_equal(obs$label[[1]], "test2")
  expect_equal(obs$error[[1]], utf8)
  expect_equal(obs$label[[2]], "test1")
  expect_equal(obs$error[[2]], utf8)
})

