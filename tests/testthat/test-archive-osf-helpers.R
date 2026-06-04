test_that(".osf_pat_validate", {
  expect_true(is.function(metacheck::.osf_pat_validate))
  osf_pat <- Sys.getenv("OSF_PAT")

  # httptest2::without_internet({
  #   expect_warning(obs <- .osf_pat_validate("BADPAT"),
  #                  "could not be validated")
  #   expect_false(obs)
  # })

  skip_if_not(online("https://api.osf.io/v2/preprints/khbvy/"))

  # real PAT (if set)
  if (osf_pat != "") {
    obs <- .osf_pat_validate()
    expect_true(obs)
  }

  # bad PAT - direct - resets env variable
  expect_warning(obs <- .osf_pat_validate("BADPAT"))
  expect_false(obs)
  expect_equal(Sys.getenv("OSF_PAT"), "")

  if (FALSE) { # sometimes borks the PAT?
    # unset PAT
    withr::local_envvar(OSF_PAT = "")
    obs <- .osf_pat_validate()
    expect_false(obs)

    # bad PAT - from env - resets env variable
    withr::local_envvar(OSF_PAT = "NOTAREALPAT")
    expect_warning(obs <- .osf_pat_validate())
    expect_false(obs)
    expect_equal(Sys.getenv("OSF_PAT"), "")
  }
})


test_that(".osf_headers", {
  req <- httr2::request("https://api.osf.io")

  # real PAT
  osf_pat <- Sys.getenv("OSF_PAT")
  if (osf_pat != "") {
    obs <- .osf_headers(req)
    x <- obs$headers$`Authorization`
    expect_equal(typeof(x), "weakref")
  }

  # PAT unset
  withr::local_envvar(OSF_PAT = "")
  obs <- .osf_headers(req)
  expect_s3_class(obs, "httr2_request")
  expect_equal(obs$headers$`User-Agent`, "metacheck")
  expect_null(obs$headers$`Authorization`)

  # PAT set to fake PAT
  withr::local_envvar(OSF_PAT = "NOPTAREALPAT")
  obs <- .osf_headers(req)
  expect_s3_class(obs, "httr2_request")
  expect_equal(obs$headers$`User-Agent`, "metacheck")
  x <- obs$headers$`Authorization`
  expect_equal(typeof(x), "weakref")
}, "mock")

test_that(".osf_parent_project", {
  # has parent project
  osf_id <- "yt32c"
  parent <- .osf_parent_project(osf_id)
  expect_equal(parent, "pngda")

  # is a parent project
  osf_id <- "pngda"
  parent <- .osf_parent_project(osf_id)
  expect_equal(parent, "pngda")

  # preprint
  osf_id <- "xp5cy"
  parent <- .osf_parent_project(osf_id)
  expect_equal(parent, "3cz2e")

  # invalid ID
  osf_id <- "pda"
  expect_warning(parent <- .osf_parent_project(osf_id))
  expect_true(is.na(parent))
}, "mock")

test_that(".osf_file_data", {
  url <- "https://api.osf.io/v2/nodes/mc45x/files/"
  data <- osf_get_all_pages(url)
  obs <- .osf_file_data(data)
  expect_equal(obs$provider, c("osfstorage", "github"))

  osf <- "https://api.osf.io/v2/nodes/mc45x/files/osfstorage/"
  osf_data <- osf_get_all_pages(osf)
  osf_obs <- .osf_file_data(osf_data)
  expect_equal(osf_obs$parent, obs$osf_id[[1]])

  github <- "https://api.osf.io/v2/nodes/mc45x/files/github/"
  gh_data <- osf_get_all_pages(github)
  gh_obs <- .osf_file_data(gh_data)
  exp <- c("/code/", "/folder/", "/good-example.R", "/README.md")
  expect_equal(gh_obs$path, exp)

  code <- "https://api.osf.io/v2/nodes/mc45x/files/github/code/"
  code_data <- osf_get_all_pages(code)
  code_obs <- .osf_file_data(code_data)
  expect_in(code_obs$filetype, "code")
  expect_equal(code_obs$path, sprintf("/code/%02d.R", 1:25))
  expect_in(code_obs$parent, "mc45x")
}, "mock")


test_that(".osf_info", {
  expect_true(is.function(metacheck::.osf_info))
  expect_no_error(helplist <- help(.osf_info, metacheck))

  # waterbutler
  osf_id <- "68472f93b21328dc7f539482"
  info <- .osf_info(osf_id)
  expect_equal(info$name, "test-folder")
  expect_equal(info$osf_type, "files")
  expect_equal(info$kind, "folder")

  # project
  osf_id <- "pngda"
  info <- .osf_info(osf_id)
  expect_equal(info$osf_id, osf_id)
  expect_equal(info$osf_type, "nodes")
  expect_equal(info$name, "Papercheck Test")
  expect_equal(info$children, "https://api.osf.io/v2/nodes/pngda/children/")
  expect_equal(info$files, "https://api.osf.io/v2/nodes/pngda/files/")

  # component
  osf_id <- "6nt4v"
  info <- .osf_info(osf_id)
  expect_equal(info$osf_id, osf_id)
  expect_equal(info$osf_type, "nodes")
  expect_equal(info$name, "Processed Data")
  expect_equal(info$children, "https://api.osf.io/v2/nodes/6nt4v/children/")
  expect_equal(info$files, "https://api.osf.io/v2/nodes/6nt4v/files/")

  # file
  osf_id <- "75qgk"
  info <- .osf_info(osf_id)
  expect_equal(info$osf_id, osf_id)
  expect_equal(info$osf_type, "files")
  expect_equal(info$kind, "file")
  expect_equal(info$name, "processed-data.csv")

  # preprint
  osf_id <- "xp5cy"
  info <- .osf_info(osf_id)
  expect_true(grepl(osf_id, info$osf_id))
  expect_equal(info$osf_type, "preprints")
  expect_equal(info$name, "Understanding mixed effects models through data simulation")

  # reg
  osf_id <- "8c3kb"
  info <- .osf_info(osf_id)
  expect_equal(info$osf_id, osf_id)
  expect_equal(info$osf_type, "registrations")
  expect_equal(info$name, "Understanding mixed effects models through data simulation")

  # user
  osf_id <- "4i578"
  info <- .osf_info(osf_id)
  expect_equal(info$osf_id, osf_id)
  expect_equal(info$osf_type, "users")
  expect_equal(info$name, "Lisa DeBruine")
}, "mock")

test_that(".osf_info - complex", {
  # private
  osf_id <- "ybm3c"
  info <- .osf_info(osf_id)
  expect_equal(info$osf_id, osf_id)
  #expect_equal(info$osf_type, "private") # isn't private if logged in as Lisa
  expect_equal(info$public, FALSE)

  # invalid
  osf_id <- "xx"
  expect_warning(info <- .osf_info(osf_id))
  expect_equal(info$osf_id, osf_id)
  expect_equal(info$osf_type, "invalid")

  # valid but not found
  osf_id <- "xxxxx"
  expect_warning(info <- .osf_info(osf_id))
  expect_equal(info$osf_id, osf_id)
  expect_equal(info$osf_type, "unfound")

  # multiple nodes
  osf_id <- c("mc45x", "y6a34")
  info <- .osf_info(osf_id)
  expect_equal(info$osf_id, osf_id)
  expect_equal(info$osf_type, c("nodes", "nodes"))

  # multiple different types
  osf_id <- c("mc45x", "y6a34", "4i578")
  info <- .osf_info(osf_id)
  expect_equal(info$osf_type, c("nodes", "nodes", "users"))

  #weird false positive of preprint/3j9rf_v1
}, "mock")

test_that("osf_id vs wb_id", {
  osf_id <- "k6gbt"
  osf_info <- .osf_info(osf_id)

  osf_id <- "6846ed88e49694cd45ab8375"
  wb_info <- .osf_info(osf_id)

  expect_equal(osf_info[, 2:11], wb_info[, 2:11])
}, "mock")
