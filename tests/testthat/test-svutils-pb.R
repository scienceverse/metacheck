test_that("pb", {
  expect_true(is.function(svutils::pb))
  expect_no_error(helplist <- help(pb, svutils))

  verbose(TRUE)
  expect_error(pb())
})

test_that("basic", {
  verbose(TRUE)

  # ticks
  pbar <- pb(10)
  for (i in 1:10) {
    expect_false(pbar$finished)
    pbar$tick()
  }
  expect_true(pbar$finished)
  expect_error(pbar$tick())

  # terminate
  pbar <- pb(10)
  pbar$terminate()
  expect_error(pbar$tick())

  # spin
  pbar <- pb(NA, "(:spin)")
  for (i in 1:100) {
    pbar$tick()
  }
  pbar$terminate()
  expect_true(pbar$finished)
  expect_error(pbar$tick())

  # what
  pbar <- pb(NA, ":what is the letter")
  pbar$tick(tokens = list(what = "A"))
  # expect_equal(pbar$.__enclos_env__$private$last_draw, "A is the letter")
  pbar$tick(tokens = list(what = "B"))
  # expect_equal(pbar$.__enclos_env__$private$last_draw, "B is the letter")
  pbar$terminate()
  expect_true(pbar$finished)
  expect_error(pbar$tick())
})
