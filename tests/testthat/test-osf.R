verbose(FALSE)
# options(papercheck.osf.api = "https://api.osf.io/v2/")
# osf_delay(0)

test_that("exists", {
  expect_true(is.function(papercheck::osf_check_id))

  expect_true(is.function(papercheck::osf_links))
  expect_no_error(helplist <- help(osf_links, papercheck))

  expect_true(is.function(papercheck::osf_retrieve))
  expect_no_error(helplist <- help(osf_retrieve, papercheck))

  expect_true(is.function(papercheck::osf_info))
  expect_no_error(helplist <- help(osf_info, papercheck))

  expect_true(is.function(papercheck::osf_delay))
  expect_no_error(helplist <- help(osf_delay, papercheck))

  expect_true(is.function(papercheck::summarize_contents))
  expect_no_error(helplist <- help(summarize_contents, papercheck))

})

test_that("osf_api_check", {
  status <- osf_api_check()
  possible <- c("ok", "too many requests",
                "server error", "unknown")
  expect_true(status %in% possible)
})

test_that("osf_headers", {
  header <- osf_headers()
  expect_equal(header$`User-Agent`, "Papercheck")
})

test_that("osf_links", {
  paper <- psychsci$`0956797614557697`
  obs <- osf_links(paper)
  exp <- c("osf.io/e2aks", "osf.io/tvyxz/")
  expect_equal(obs$text, exp)

  # has view-only link across sentences
  paper <- psychsci$`0956797615569889`
  obs <- osf_links(paper)
  exp <- "osf.io/t9j8e/? view_only=f171281f212f4435917b16a9e581a73b"
  expect_equal(obs$text, exp)

  # check vo links
  info <- osf_info("t9j8e")
  expect_equal(info$osf_type, "private")
  expect_equal(info$public, FALSE)

  skip("long")
  obs <- osf_links(psychsci)
  ids <- osf_check_id(obs$text)
})

test_that("osf_check_id", {
  # 5-letter
  osf_id <- "pngda"
  checked_id <- osf_check_id(osf_id)
  expect_equal(checked_id, osf_id)

  # vector
  osf_id <- c("pngda", "8c3kb")
  checked_id <- osf_check_id(osf_id)
  expect_equal(checked_id, osf_id)

  # vector with invalid values
  osf_id <- c("pngda", "xxx", "8c3kb")
  expect_warning(checked_id <- osf_check_id(osf_id))
  expect_equal(checked_id, c("pngda", NA, "8c3kb"))

  # waterbutler id
  osf_id <- "6846ed88e49694cd45ab8375"
  checked_id <- osf_check_id(osf_id)
  expect_equal(checked_id, osf_id)

  # invalidwaterbutler id
  osf_id <- "6846ed894cd45ab8375"
  expect_warning(checked_id <- osf_check_id(osf_id))
  expect_true(is.na(checked_id))

  # urls
  osf_id <- "https://osf.io/pngda"
  checked_id <- osf_check_id(osf_id)
  expect_equal(checked_id, "pngda")

  osf_id <- "http://osf.io/pngda"
  checked_id <- osf_check_id(osf_id)
  expect_equal(checked_id, "pngda")

  # url with no http
  osf_id <- "osf.io/pngda"
  checked_id <- osf_check_id(osf_id)
  expect_equal(checked_id, "pngda")

  # deal with rogue whitespace
  osf_id <- "osf .io/pngda"
  checked_id <- osf_check_id(osf_id)
  expect_equal(checked_id, "pngda")

  osf_id <- "xx"
  expect_warning(checked_id <- osf_check_id(osf_id))
  expect_true(is.na(checked_id))

  # view-only link
  osf_id <- "https://osf.io/pngda/?view_only=5acf039f24ac4ea28afec473548dd7f4"
  checked_id <- osf_check_id(osf_id)
  expect_equal(checked_id, "pngda")
})

test_that("osf_get_all_pages", {
  osf_api <- getOption("papercheck.osf.api")

  # fewer than 10
  url <- sprintf("%s/nodes/pngda/files/osfstorage/", osf_api)
  data <- osf_get_all_pages(url)
  expect_equal(nrow(data), 2)

  # more than 10
  url <- sprintf("%s/nodes/yt32c/files/osfstorage/", osf_api)
  data <- osf_get_all_pages(url)
  expect_equal(nrow(data), 14)

  # no results
  url <- sprintf("%s/nodes/y6a34/files/osfstorage/", osf_api)
  data <- osf_get_all_pages(url)
  expect_equal(data, list())
})

test_that("osf_files", {
  osf_id <- "pngda"
  data <- osf_files(osf_id)
  expect_equal(nrow(data), 2)

  osf_id <- "yt32c"
  data <- osf_files(osf_id)
  expect_equal(nrow(data), 14)
  expect_equal(data$filetype, rep("data", 14))

  osf_id <- "y6a34"
  data <- osf_files(osf_id)
  expect_equal(nrow(data), 0)
})

test_that("osf_children", {
  osf_id <- "pngda"
  data <- osf_children(osf_id)
  expect_equal(nrow(data), 2)

  osf_id <- "y6a34"
  data <- osf_children(osf_id)
  expect_equal(nrow(data), 0)
})

test_that("osf_info", {
  skip("long")
  # project
  osf_id <- "pngda"
  info <- osf_info(osf_id)
  expect_equal(info$osf_id, osf_id)
  expect_equal(info$osf_type, "nodes")
  expect_equal(info$name, "Papercheck Test")

  # component
  osf_id <- "6nt4v"
  info <- osf_info(osf_id)
  expect_equal(info$osf_id, osf_id)
  expect_equal(info$osf_type, "nodes")
  expect_equal(info$name, "Processed Data")

  # file
  osf_id <- "75qgk"
  info <- osf_info(osf_id)
  expect_equal(info$osf_id, osf_id)
  expect_equal(info$osf_type, "files")
  expect_equal(info$kind, "file")
  expect_equal(info$name, "processed-data.csv")

  # preprint
  osf_id <- "xp5cy"
  info <- osf_info(osf_id)
  expect_true(grepl(osf_id, info$osf_id))
  expect_equal(info$osf_type, "preprints")
  expect_equal(info$name, "Understanding mixed effects models through data simulation")

  # user
  # osf_id <- "4i578"
  # info <- osf_info(osf_id)
  # expect_equal(info$osf_id, osf_id)
  # expect_equal(info$osf_type, "users")
  # expect_equal(info$name, "Lisa DeBruine")

  # reg
  osf_id <- "8c3kb"
  info <- osf_info(osf_id)
  expect_equal(info$osf_id, osf_id)
  expect_equal(info$osf_type, "registrations")
  expect_equal(info$name, "Understanding mixed effects models through data simulation")

  # private
  osf_id <- "ybm3c"
  info <- osf_info(osf_id)
  expect_equal(info$osf_id, osf_id)
  expect_equal(info$osf_type, "private")
  expect_equal(info$public, FALSE)

  # invalid
  osf_id <- "xx"
  expect_warning(info <- osf_info(osf_id))
  expect_equal(info$osf_id, osf_id)
  expect_equal(info$osf_type, "invalid")

  # valid but not found
  osf_id <- "xxxxx"
  expect_warning(info <- osf_info(osf_id))
  expect_equal(info$osf_id, osf_id)
  expect_equal(info$osf_type, "unfound")
})


test_that("osf_retrieve", {
  examples <- c(project = "pngda",
                component = "https://osf.io/6nt4v",
                private = "ybm3c",
                file = "osf.io/75qgk",
                preprint = "xp5cy",
                #user = "4i578",
                reg = "8c3kb",
                duplicate = "6nt4v",
                bad = "xx")
  osf_url <- data.frame(
    url = examples,
    type = names(examples)
  )
  expect_warning(table <- osf_retrieve(osf_url))
  expect_true(!"project" %in% names(table))
  expect_equal(table$url, osf_url$url)
  expect_equal(table$type, osf_url$type)
  expect_equal(table[2, 3:10], table[7, 3:10], ignore_attr = TRUE)

  # vector
  osf_url <- "pngda"
  table <- osf_retrieve(osf_url)
  expect_equal(table$osf_url, osf_url)
  expect_equal(table$name, "Papercheck Test")

  # table with id_col, find project
  osf_url <- data.frame(
    id = 100,
    osf_id = "pngda"
  )
  id_col <- "osf_id"
  table <- osf_retrieve(osf_url, id_col, find_project = TRUE)
  expect_equal(table$osf_id, osf_url$osf_id)
  expect_equal(table$name, "Papercheck Test")
  expect_equal(table$project, "pngda")

  # recursive
  osf_url <- "yt32c"
  table <- osf_retrieve(osf_url, recursive = TRUE)
  expect_equal(nrow(table), 15)
  expect_equal(table$parent, rep(c("ckjef", "yt32c"), c(1, 14)))

  # recursive with duplicates and NA vector
  osf_url <- c("yt32c", "yt32c", NA)
  table <- osf_retrieve(osf_url, recursive = TRUE)
  expect_equal(nrow(table), 1 + 14)

  # recursive with duplicates and NA table
  osf_url <- data.frame(parent_id = c("yt32c", "yt32c", NA),
                        n = 1:3)
  expect_warning(table <- osf_retrieve(osf_url, recursive = TRUE))
  expect_equal(nrow(table), 3 + 14)
  expect_equal(table$n, c(1:3, rep(NA, 14)))
})

test_that("osf_parent_project", {
  # has parent project
  osf_id <- "yt32c"
  parent <- osf_parent_project(osf_id)
  expect_equal(parent, "pngda")

  # is a parent project
  osf_id <- "pngda"
  parent <- osf_parent_project(osf_id)
  expect_equal(parent, "pngda")

  # preprint
  osf_id <- "xp5cy"
  parent <- osf_parent_project(osf_id)
  expect_equal(parent, "3cz2e")

  # invalid ID
  osf_id <- "pda"
  expect_warning(parent <- osf_parent_project(osf_id))
  expect_true(is.na(parent))
})

test_that("summarize_contents", {
  skip_on_cran()
  skip_on_covr()
  skip_if_offline("api.osf.io")

  osf_id <- "pngda"
  contents <- osf_retrieve(osf_id, recursive = TRUE)

  summary <- summarize_contents(contents)

  readme <- dplyr::filter(summary, name == "README")
  expect_equal(readme$best_guess, "readme")
  expect_equal(readme$is_data, FALSE)
  expect_equal(readme$is_code, FALSE)
  expect_equal(readme$is_codebook, FALSE)
  expect_equal(readme$is_readme, TRUE)

  # handle zero results and/or OSF down
  summary <- summarize_contents(data.frame())
  expect_equal(nrow(summary), 0)
})


test_that("rate limiting", {
  skip("long")

  osf_id <- "pngda"

  for (i in 1:110) {
    info <- osf_info(osf_id)
  }
})
