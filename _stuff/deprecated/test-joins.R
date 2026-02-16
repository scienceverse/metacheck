test_that("pre_join", {
  expect_true(is.function(metacheck::pre_join))

  nu <- 1:3
  ch <- as.character(nu)
  x <- data.frame(id = nu, a = nu, b = nu)
  y <- data.frame(id = ch, a = ch, c = ch)

  expect_error(pre_join())
  expect_error(pre_join(x))

  fix <- pre_join(x, y)
  expect_equal(typeof(fix$x$id), "character")
  expect_equal(typeof(fix$y$id), "character")
  expect_equal(typeof(fix$x$a), "character")
  expect_equal(typeof(fix$y$a), "character")
  expect_equal(typeof(fix$x$b), "integer")
  expect_equal(typeof(fix$y$c), "character")

  fix <- pre_join(x, y, "id")
  expect_equal(typeof(fix$x$id), "character")
  expect_equal(typeof(fix$y$id), "character")
  expect_equal(typeof(fix$x$a), "integer")
  expect_equal(typeof(fix$y$a), "character")
  expect_equal(typeof(fix$x$b), "integer")
  expect_equal(typeof(fix$y$c), "character")

  fix <- pre_join(x, y, c("id", b = "c"))
  expect_equal(typeof(fix$x$id), "character")
  expect_equal(typeof(fix$y$id), "character")
  expect_equal(typeof(fix$x$a), "integer")
  expect_equal(typeof(fix$y$a), "character")
  expect_equal(typeof(fix$x$b), "character")
  expect_equal(typeof(fix$y$c), "character")
})
