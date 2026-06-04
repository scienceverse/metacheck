test_that("osf_links", {
  expect_true(is.function(metacheck::osf_links))
  expect_no_error(helplist <- help(osf_links, metacheck))

  exp <- c("osf.io/e2aks", "osf.io/tvyxz/", "osf.com/nope")
  paper <- test_paper(url = exp)
  obs <- osf_links(paper)
  expect_equal(obs$href, exp[1:2])
})

test_that("osf_type", {
  expect_true(is.function(metacheck::osf_type))
  expect_no_error(helplist <- help(osf_type, metacheck))

  examples <- list(project = "pngda",
                   component = "https://osf.io/6nt4v",
                   private = "ybm3c",
                   file = "osf.io/75qgk",
                   preprint = "xp5cy",
                   user = "4i578",
                   reg = "8c3kb",
                   bad = "xx")


  otype <- osf_type(examples$project)
  expect_equal(otype, "nodes")

  otype <- osf_type(examples$component)
  expect_equal(otype, "nodes")

  otype <- osf_type(examples$private)
  expect_equal(otype, "nodes")

  otype <- osf_type(examples$file)
  expect_equal(otype, "files")

  otype <- osf_type(examples$preprint)
  expect_equal(otype, "preprints")

  otype <- osf_type(examples$user)
  expect_equal(otype, "users")

  otype <- osf_type(examples$reg)
  expect_equal(otype, "registrations")

  expect_warning(otype <- osf_type(examples$bad))
  expect_equal(otype, NA_character_)
}, "mock")

test_that("osf_check_id", {
  expect_true(is.function(metacheck::osf_check_id))
  expect_no_error(helplist <- help(osf_check_id, metacheck))

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

  # waterbutler url
  osf_id <- "https://osf.io/j3gcx/files/osfstorage/6846ed88e49694cd45ab8375"
  checked_id <- osf_check_id(osf_id)
  expect_equal(checked_id, "6846ed88e49694cd45ab8375")

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

  # file storage
  osf_id <- "https://osf.io/j3gcx/files/osfstorage"
  checked_id <- osf_check_id(osf_id)
  expect_equal(checked_id, "j3gcx")

  osf_id <- "xx"
  expect_warning(checked_id <- osf_check_id(osf_id))
  expect_true(is.na(checked_id))

  # view-only link
  osf_id <- "https://osf.io/pngda/?view_only=5acf039f24ac4ea28afec473548dd7f4"
  checked_id <- osf_check_id(osf_id)
  expect_equal(checked_id, "pngda?view_only=5acf039f24ac4ea28afec473548dd7f4")

  # vector
  osf_id <- c(
    "6846ed88e49694cd45ab8375",
    "https://osf.io/j3gcx/files/osfstorage/6846ed88e49694cd45ab8375",
    "PNGDA",
    "pngda",
    "https://osf.io/pngda",
    "http://osf.io/pngda",
    "osf.io/pngda",
    "osf .io/pngda",
    "https://osf.io/pngda/files/osfstorage",
    "https://osf.io/pngda/?view_only=5acf039f24ac4ea28afec473548dd7f4",
    "https://osf.io/pngda?view_only=5acf039f24ac4ea28afec473548dd7f4",
    "xx",
    "6846ed88e49694cd45a"
  )

  # produces two warnings
  expect_warning(expect_warning(obs <- osf_check_id(osf_id)))
  exp <- rep(c("6846ed88e49694cd45ab8375", "pngda", NA_character_), c(2, 9, 2))
  exp[10:11] <- "pngda?view_only=5acf039f24ac4ea28afec473548dd7f4"
  expect_equal(obs, exp)
})


test_that("osf_delay", {
  expect_true(is.function(metacheck::osf_delay))
  expect_no_error(helplist <- help(osf_delay, metacheck))

  expect_gte(osf_delay(), 0)

  obs <- osf_delay(.005)
  expect_equal(obs, .005)
  expect_equal(osf_delay(), .005)

  osf_delay(0)
})

test_that("osf_api_check", {
  status <- osf_api_check()
  expect_equal(status, "OK")

  httptest2::without_internet({
    expect_error(osf_api_check())
    expect_warning(osf_api_check(on_error = "warn"))
    expect_no_condition(osf_api_check(on_error = "ignore"))
  })
}, "mock")


test_that("osf_get_all_pages", {
  osf_api <- getOption("metacheck.osf.api")

  # fewer than 10
  url <- sprintf("%s/nodes/pngda/files/osfstorage/", osf_api)
  data <- osf_get_all_pages(url)
  files <- c("test-folder", "README", "papercheck.png")
  expect_true(all(files %in% data$attributes$name))

  # more than 10
  url <- sprintf("%s/nodes/yt32c/files/osfstorage/", osf_api)
  data <- osf_get_all_pages(url)
  expect_equal(nrow(data), 14)

  # no results
  url <- sprintf("%s/nodes/y6a34/files/osfstorage/", osf_api)
  data <- osf_get_all_pages(url)
  expect_equal(data, dplyr::tibble())

  # limit pages
  url <- sprintf("%s/preprints/", osf_api)
  data <- osf_get_all_pages(url, 1)
  expect_equal(nrow(data), 10)

  data <- osf_get_all_pages(url, 2)
  expect_equal(nrow(data), 20)

  url <- sprintf("%s/preprints/?page=5", osf_api)
  data <- osf_get_all_pages(url, 5)
  expect_equal(nrow(data), 10)
}, "mock")


test_that("osf_info", {
  expect_true(is.function(metacheck::osf_info))
  expect_no_error(helplist <- help(osf_info, metacheck))

  examples <- c(project = "pngda",
                component = "https://osf.io/6nt4v",
                private = "ybm3c",
                file = "osf.io/75qgk",
                preprint = "xp5cy",
                user = "4i578",
                reg = "8c3kb",
                duplicate = "6nt4v",
                bad = "xx")
  osf_url <- data.frame(
    url = examples,
    type = names(examples)
  )
  expect_warning(table <- osf_info(osf_url))
  expect_equal(table$url, osf_url$url)
  expect_equal(table$type, osf_url$type)
  expect_equal(table[2, 3:10], table[8, 3:10], ignore_attr = TRUE)

  self <- c(
    "https://api.osf.io/v2/nodes/pngda/",
    "https://api.osf.io/v2/nodes/6nt4v/",
    "https://api.osf.io/v2/nodes/ybm3c/",
    "https://api.osf.io/v2/files/6846ed6a29684b023953943e/",
    "https://api.osf.io/v2/preprints/xp5cy_v1/",
    "https://api.osf.io/v2/users/4i578/",
    "https://api.osf.io/v2/registrations/8c3kb/",
    "https://api.osf.io/v2/nodes/6nt4v/",
    NA
  )
  expect_equal(table$self, self)

  # vector
  osf_url <- "pngda"
  table <- osf_info(osf_url)
  expect_equal(table$osf_url, osf_url)
  expect_equal(table$name, "Papercheck Test")

  # table with id_col
  osf_url <- data.frame(
    id = 100,
    osf_id = "pngda"
  )
  id_col <- "osf_id"
  table <- osf_info(osf_url, id_col)
  expect_equal(table$osf_id, osf_url$osf_id)
  expect_equal(table$name, "Papercheck Test")
  expect_equal(table$project, "pngda")

  # only one URL
  osf_url <- "https://osf.io/pngda"
  table <- osf_info(osf_url)
  expect_equal(table$name, "Papercheck Test")

  # no links
  paper <- test_paper("No links")
  osf_url <- osf_links(paper)
  info <- osf_info(osf_url, recursive = TRUE)
  expect_equal(nrow(info), 0)
  expect_equal(osf_url, info)
}, "mock")

test_that("osf_info - recursive", {
  # recursive
  osf_url <- "yt32c"
  table <- osf_info(osf_url, recursive = TRUE)
  expect_equal(nrow(table), 16)
  expect_equal(table$parent[1:2], c("ckjef", "yt32c"))

  # recursive with duplicates and NA vector
  osf_url <- c("yt32c", "yt32c", NA)
  table <- osf_info(osf_url, recursive = TRUE)
  expect_equal(nrow(table), 1 + 15)

  # recursive with duplicates and NA table
  osf_url <- data.frame(parent_id = c("yt32c", "yt32c", NA),
                        n = 1:3)
  expect_warning(table <- osf_info(osf_url, recursive = TRUE))
  expect_equal(nrow(table), 3 + 15)
  expect_equal(table$n, c(1:3, rep(NA, 15)))
}, "mock")

test_that("osf_info recursive", {
  # folders can only have wb IDs,
  # files only have wb IDs until someone looks at them on the web
  #  and then they get 5-letter guids
  # currently just using wb IDs for all files

  osf_url <- "j3gcx"
  info <- osf_info(osf_url, recursive = TRUE)
  folders <- paste0("nest-", 1:4) |> c("empty")
  files <- paste0("test-", 1:4, ".txt")
  expect_in(folders, info$name)
  expect_in(files, info$name)

  folder_ids <- info$osf_id[info$kind %in% "folder"]
  expect_in(folder_ids[1:5], info$parent)

  # contains github and osfstorage files
  osf_url <- "mc45x"
  info <- osf_info(osf_url, recursive = TRUE)
  files <- sprintf("%02d.R", 1:10)
  expect_in(files, info$name)
}, "mock")

test_that("osf_info - view_only", {
  skip_if_offline("api.osf.io")

  osf_id <- "iywec?view_only=f171281f212f4435917b16a9e581a73b"
  self <- "https://api.osf.io/v2/nodes/iywec/?view_only=f171281f212f4435917b16a9e581a73b"

  obs <- osf_info(osf_id)
  expect_equal(obs$osf_type, "nodes")
  expect_equal(obs$self, self)

  skip_if_quick()

  obs <- osf_info(osf_id, recursive = TRUE)
  exp <- c("Objects", "osfstorage", "comb.jpg")
  expect_contains(obs$name, exp)
})

