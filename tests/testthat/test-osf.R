verbose(FALSE)
# options(metacheck.osf.api = "https://api.osf.io/v2/")
# osf_delay(0)

# skip if requires OSF API
osf_skip <- function() {
  return(NULL) # do not skip with httptest set up

  # skip("Skip OSF") # skips all tests that require API
  #
  # # skips tests if contraindicated
  # skip_if_offline()
  # skip_on_cran()
  # skip_on_covr()
  # skip_if_not(osf_api_check() == "ok")
}

test_that("exists", {
  expect_true(is.function(metacheck::osf_check_id))

  expect_true(is.function(metacheck::osf_links))
  expect_no_error(helplist <- help(osf_links, metacheck))

  expect_true(is.function(metacheck::osf_retrieve))
  expect_no_error(helplist <- help(osf_retrieve, metacheck))

  expect_true(is.function(metacheck::osf_info))
  expect_no_error(helplist <- help(osf_info, metacheck))

  expect_true(is.function(metacheck::osf_delay))
  expect_no_error(helplist <- help(osf_delay, metacheck))

  expect_true(is.function(metacheck::summarize_contents))
  expect_no_error(helplist <- help(summarize_contents, metacheck))

  expect_true(is.function(metacheck::osf_file_download))
  expect_no_error(helplist <- help(osf_file_download, metacheck))
})

# httptest::start_capturing()
httptest::with_mock_api({

test_that("osf_api_check", {
  status <- osf_api_check()
  possible <- c("ok", "too many requests",
                "server error", "unknown")
  expect_true(status %in% possible)
})

test_that("osf_headers", {
  header <- osf_headers()
  expect_equal(header$`User-Agent`, "metacheck")
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

  obs <- osf_links(psychsci[1:50])
  ids <- osf_check_id(obs$text)
  expect_true(all(nchar(ids) == 5))
})

test_that("osf_check_id", {
  osf_skip()

  # check vo links
  info <- osf_info("t9j8e")
  expect_equal(info$osf_type, "private")
  expect_equal(info$public, FALSE)

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
  expect_equal(checked_id, "pngda")

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
  expect_equal(obs, exp)
})

test_that("osf_get_all_pages", {
  osf_skip()

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
  expect_equal(data, list())

  # limit pages
  url <- sprintf("%s/preprints/", osf_api)
  data <- osf_get_all_pages(url, 1)
  expect_equal(nrow(data), 10)

  data <- osf_get_all_pages(url, 2)
  expect_equal(nrow(data), 20)

  url <- sprintf("%s/preprints/?page=5", osf_api)
  data <- osf_get_all_pages(url, 5)
  expect_equal(nrow(data), 10)
})

test_that("osf_files", {
  osf_skip()

  osf_id <- "pngda"
  data <- osf_files(osf_id)
  expect_equal(nrow(data), 3)

  osf_id <- "yt32c"
  data <- osf_files(osf_id)
  expect_equal(nrow(data), 14)
  expect_equal(data$filetype, rep("data", 14))

  osf_id <- "y6a34"
  data <- osf_files(osf_id)
  expect_equal(nrow(data), 0)
})

test_that("osf_children", {
  osf_skip()

  osf_id <- "pngda"
  data <- osf_children(osf_id)
  expect_equal(nrow(data), 3)

  osf_id <- "y6a34"
  data <- osf_children(osf_id)
  expect_equal(nrow(data), 0)
})

test_that("osf_info", {
  osf_skip()

  # project
  osf_id <- "pngda"
  osf_api_calls(0)
  info <- osf_info(osf_id)
  expect_equal(osf_api_calls(), 1)
  expect_equal(info$osf_id, osf_id)
  expect_equal(info$osf_type, "nodes")
  expect_equal(info$name, "Papercheck Test")


  # component
  osf_id <- "6nt4v"
  osf_api_calls(0)
  info <- osf_info(osf_id)
  expect_equal(osf_api_calls(), 1)
  expect_equal(info$osf_id, osf_id)
  expect_equal(info$osf_type, "nodes")
  expect_equal(info$name, "Processed Data")

  # file
  osf_id <- "75qgk"
  osf_api_calls(0)
  info <- osf_info(osf_id)
  expect_equal(osf_api_calls(), 1) # checks nodes first
  expect_equal(info$osf_id, osf_id)
  expect_equal(info$osf_type, "files")
  expect_equal(info$kind, "file")
  expect_equal(info$name, "processed-data.csv")

  # preprint
  osf_id <- "xp5cy"
  osf_api_calls(0)
  info <- osf_info(osf_id)
  expect_equal(osf_api_calls(), 1) # checks nodes & files first
  expect_true(grepl(osf_id, info$osf_id))
  expect_equal(info$osf_type, "preprints")
  expect_equal(info$name, "Understanding mixed effects models through data simulation")

  # reg
  osf_id <- "8c3kb"
  osf_api_calls(0)
  info <- osf_info(osf_id)
  expect_equal(osf_api_calls(), 1) # checks nodes, files, preprints first
  expect_equal(info$osf_id, osf_id)
  expect_equal(info$osf_type, "registrations")
  expect_equal(info$name, "Understanding mixed effects models through data simulation")

  # user
  osf_id <- "4i578"
  osf_api_calls(0)
  info <- osf_info(osf_id)
  expect_equal(osf_api_calls(), 1) # checks nodes, files, preprints, reg first
  expect_equal(info$osf_id, osf_id)
  expect_equal(info$osf_type, "users")
  expect_equal(info$name, "Lisa DeBruine")

  # private
  osf_id <- "ybm3c"
  info <- osf_info(osf_id)
  expect_equal(info$osf_id, osf_id)
  expect_equal(info$osf_type, "private")
  expect_equal(info$public, FALSE)

  # view-only (private)
  osf_id <- "https://osf.io/ybm3c/?view_only=5acf039f24ac4ea28afec473548dd7f4"
  info <- osf_info(osf_id)
  expect_equal(info$osf_id, "ybm3c")
  expect_equal(info$osf_type, "private")

  # view-only (public)
  osf_id <- "https://osf.io/pngda/?view_only=5acf039f24ac4ea28afec473548dd7f4"
  info <- osf_info(osf_id)
  expect_equal(info$osf_id, "pngda")
  expect_equal(info$osf_type, "nodes")
  expect_equal(info$name, "Papercheck Test")

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

  # # multiple nodes
  # osf_id <- c("mc45x", "y6a34")
  # info <- osf_info(osf_id)
  # expect_equal(info$osf_id, osf_id)
  # expect_equal(info$osf_type, c("nodes", "nodes"))
  #
  # # multiple different types
  # osf_id <- c("mc45x", "y6a34", "4i578")
  # info <- osf_info(osf_id)
  # weird false positive of preprint/3j9rf_v1
})


test_that("osf_retrieve", {
  osf_skip()

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
  #expect_true(!"project" %in% names(table))
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

  # only one URL
  osf_url <- "https://osf.io/pngda"
  table <- osf_retrieve(osf_url)
  expect_equal(table$name, "Papercheck Test")

  osf_url <- "https://osf.io/ybm3c/?view_only=5acf039f24ac4ea28afec473548dd7f4"
  table <- osf_retrieve(osf_url)
  expect_equal(table$osf_url, osf_url)
  expect_equal(table$osf_id, "ybm3c")

  # children of private
  osf_url <- "https://osf.io/ybm3c/?view_only=5acf039f24ac4ea28afec473548dd7f4"
  table <- osf_retrieve(osf_url, recursive = TRUE)
  expect_equal(table$osf_url, osf_url)
  expect_equal(table$osf_id, "ybm3c")

  # no links
  paper <- psychsci[[180]]
  osf_url <- osf_links(paper)
  info <- osf_retrieve(osf_url, recursive = TRUE, find_project = TRUE)
  expect_equal(nrow(info), 0)
  expect_equal(osf_url, info)
})

test_that("osf_retrieve recursive", {
  osf_skip()

  # folders can only have wb IDs,
  # files only have wb IDs until someone looks at them on the web
  #  and then they get 5-letter guids
  # currently just using wb IDs for all files

  osf_url <- "j3gcx"
  info <- osf_retrieve(osf_url, recursive = TRUE)
  folders <- paste0("nest-", 1:4) |> c("empty")
  files <- paste0("test-", 1:4, ".txt")
  expect_true(all(folders %in% info$name))
  expect_true(all(files %in% info$name))

  # contains github and osfstorage files
  osf_url <- "mc45x"
  info <- osf_retrieve(osf_url, recursive = TRUE)
})

test_that("osf_id vs wb_id", {
  osf_skip()

  osf_id <- "k6gbt"
  osf_info <- osf_info(osf_id)

  osf_id <- "6846ed88e49694cd45ab8375"
  wb_info <- osf_info(osf_id)

  expect_equal(osf_info[, 1:11], wb_info[, 1:11])
})

test_that("osf_parent_project", {
  osf_skip()

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
  # handle zero results and/or OSF down
  summary <- summarize_contents(data.frame())
  expect_equal(nrow(summary), 0)

  osf_skip()

  osf_id <- "pngda"
  contents <- osf_retrieve(osf_id, recursive = TRUE)

  summary <- summarize_contents(contents)

  readme <- dplyr::filter(summary, name == "README")
  expect_equal(unique(readme$file_category), "readme")
})

}) # end mock api
# httptest::stop_capturing()

test_that("add_filetype", {
  # edge case classification
  files <- c(
    "datarelease.pdf" = "text",    # pdf cannot be data or code
    "my_r_code.pdf" = "text",
    "data.sas" = "stats",          # sas is always code
    "codebook.sas" = "stats",
    "codebook.pdf" = "text"
  )
  ft <- filetype(names(files))
  expect_equal(ft, files)
})

test_that("edge case summarise", {
  # edge case classification
  # category is from OSF, so can be: analysis, communication, data, hypothesis, instrumentation, methods and measures, procedure, project, software, other, but mostly uncategorized (NA)
  contents <- dplyr::tribble(
    ~name,              ~category, ~classify,
    "datarelease.pdf",  NA,         NA,        # pdf cannot be data or code
    "data.pdf",         "data",     NA,        # what about qual data?
    "my_r_code.pdf",    NA,         NA,
    "readme.xls",       "project",  "data",    # is an xls file always data?
    "data.sas",         NA,         "code",    # sas is always code
    "codebook.sas",     NA,         "code",
    "readme.sas",       NA,         "code",
    "codebook.pdf",     NA,         "codebook" # not a great format but possible
  )
  contents$filetype <- filetype(contents$name)

  summary <- summarize_contents(contents)
  expect_equal(summary$file_category, contents$classify)
})

verbose(TRUE)
