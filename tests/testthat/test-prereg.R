test_that("exists", {
  expect_true(is.function(papercheck::aspredicted_links))
  expect_no_error(helplist <- help(aspredicted_links, papercheck))
})

test_that("errors", {
  expect_error(aspredicted_links(bad_arg))
})

test_that("defaults", {
  links <- aspredicted_links(psychsci)
  expect_equal(names(links)[[1]], "text")
  expect_true(all(grepl("^https://aspredicted\\.org", links$text)))
  expect_equal(nrow(links), 78)
  expect_equal(links, unique(links))

  sentences <- expand_text(links, psychsci)

  paper <- data.frame(id = 1,
                      s = 1:10,
                      p = 1,
                      text = c("</aspredicted.org/stuff>", "hi",
                              "<https://aspredicted.org/stuff>", "hi",
                              "<https://aspredicted.org/ stuff>", "hi",
                              "<https://aspredicted.org/blind.php?", " x=stuff> hi",
                              "<https://aspredicted> .org/stuff.pdf", "hi"
                              ))
  links <- aspredicted_links(paper)
  exp <- c("https://aspredicted.org/stuff",
           "https://aspredicted.org/stuff",
           "https://aspredicted.org/stuff",
           "https://aspredicted.org/blind.php?x=stuff",
           "https://aspredicted.org/stuff.pdf")
  expect_equal(links$text, exp)
})
