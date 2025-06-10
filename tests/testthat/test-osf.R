test_that("exists", {
  expect_true(is.function(papercheck::osf_retrieve))
  expect_no_error(helplist <- help(osf_retrieve, papercheck))

  expect_true(is.function(papercheck::osf_contents))
  expect_no_error(helplist <- help(osf_contents, papercheck))

  expect_true(is.function(papercheck::osf_delay))
  expect_no_error(helplist <- help(osf_delay, papercheck))

  expect_true(is.function(papercheck::summarize_contents))
  expect_no_error(helplist <- help(summarize_contents, papercheck))

})

test_that("errors", {
  skip_on_cran()
  skip_on_covr()
  skip_if_offline("osf.io")

  # invalid ID
  id <- "noid"
  expect_warning(invalid <- osf_retrieve(id))
  expect_equal(invalid$id, id)
  expect_equal(invalid$osf_type, "invalid")

  # valid but unfound ID
  id <- "noid2"
  expect_warning(unfound <- osf_retrieve(id))
  expect_equal(unfound$id, id)
  expect_equal(unfound$osf_type, "unfound")
})

test_that("osf_retrieve", {
  skip_on_cran()
  skip_on_covr()
  skip_if_offline("api.osf.io")

  # user
  id <- "4i578"
  user <- osf_retrieve(id)
  expect_equal(user$name, "Lisa DeBruine")
  expect_equal(user$id, id)
  expect_equal(user$orcid, "0000-0002-7523-5539")
  expect_equal(user$osf_type, "user")

  # project
  id <- "pngda"
  project <- osf_retrieve(id)
  expect_equal(project$name, "Papercheck Test")
  expect_equal(project$id, id)
  expect_equal(project$osf_type, "node")
  expect_equal(project$category, "project")
  expect_true(is.na(project$parent))
  expect_equal(project$project, id)

  # component
  id <- "6nt4v"
  comp <- osf_retrieve(id)
  expect_equal(comp$name, "Processed Data")
  expect_equal(comp$id, id)
  expect_equal(comp$osf_type, "node")
  expect_equal(comp$category, "data")
  expect_equal(comp$parent, "ckjef")
  expect_equal(comp$project, project$id)

  # file ----
  id <- "75qgk"
  file <- osf_retrieve(id)
  expect_equal(file$name, "processed-data.csv")
  expect_equal(file$id, id)
  expect_equal(file$osf_type, "file")
  #expect_true(file$downloads > 0)
  expect_equal(file$parent, comp$id)
  expect_equal(file$project, project$id)

  # file parent project
  id <- "75qgk"
  parent_proj <- osf_parent_project(id)
  expect_equal(parent_proj, project$id)

  # registration
  id <- "8c3kb"
  reg <- osf_retrieve(id)
  expect_equal(reg$name, "Understanding mixed effects models through data simulation")
  expect_equal(reg$id, id)
  expect_equal(reg$osf_type, "registration")
  expect_equal(reg$parent, "3cz2e")
  expect_equal(reg$project, "3cz2e")

  # duplicate IDs
  id <- c("4i578", "4i578")
  expect_message(user <- osf_retrieve(id), "Retrieving info from 4i578...", fixed = TRUE)
  expect_equal(user$name, rep("Lisa DeBruine", 2))
  expect_equal(user$id, id)
  expect_equal(user$orcid, rep("0000-0002-7523-5539", 2))
  expect_equal(user$osf_type, rep("user", 2))

  # URL
  id <- "osf.io/pngda"
  project <- osf_retrieve(id)
  expect_equal(project$name, "Papercheck Test")
  expect_equal(project$id, id)
  expect_equal(project$osf_id, "pngda")
  expect_equal(project$osf_type, "node")
  expect_equal(project$category, "project")
  expect_true(is.na(project$parent))
  expect_equal(project$project, "pngda")

  # https://URL
  id <- "https://osf.io/pngda"
  project <- osf_retrieve(id)
  expect_equal(project$name, "Papercheck Test")
  expect_equal(project$id, id)
  expect_equal(project$osf_id, "pngda")
  expect_equal(project$osf_type, "node")
  expect_equal(project$category, "project")
  expect_true(is.na(project$parent))
  expect_equal(project$project, "pngda")

  # list
  id <- c(user = "4i578", project = "pngda", component = "6nt4v", file = "75qgk", reg = "8c3kb")
  osf_list <- osf_retrieve(id)
  expect_equal(osf_list$id, unname(id))
  expect_equal(osf_list$osf_type, c("user", "node", "node", "file", "registration"))
})

test_that("osf_contents", {
  #skip("Long process")
  skip_on_cran()
  skip_on_covr()
  skip_if_offline("api.osf.io")

  id <- "pngda"
  contents <- osf_contents(id)
  expect_true(all(c("ckjef", "j3gcx", "6nt4v") %in% contents$osf_id))

  # duplicate links
  id <- c("pngda", "pngda")
  contents2 <- osf_contents(id)
  expect_equal(contents, contents2)

  # multiple ids
  id <- c("j3gcx", "6nt4v")
  contents <- osf_contents(id)
  expect_true(all(c("6846ed88e49694cd45ab8375", "6846ed6a29684b023953943e") %in% contents$osf_id))
  expect_true(all(id %in% contents$parent))

  # > 10 contents
  id <- "yt32c"
  contents <- osf_contents(id)
  expect_equal(contents$type, "missing")
})

test_that("summarize_contents", {
  skip_on_cran()
  skip_on_covr()
  skip_if_offline("api.osf.io")

  id <- "pngda"
  contents <- osf_contents(id)

  summary <- summarize_contents(contents)
})

osf_links <- module_run(psychsci, "osf_check")
osf_info <- osf_retrieve(osf_links$table$text)
