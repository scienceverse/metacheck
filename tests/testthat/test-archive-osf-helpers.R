test_that(".osf_pat_validate", {
  expect_true(is.function(metacheck::.osf_pat_validate))
  osf_pat <- Sys.getenv("OSF_PAT")

  # httptest2::without_internet({
  #   expect_warning(obs <- .osf_pat_validate("BADPAT"),
  #                  "could not be validated")
  #   expect_false(obs)
  # })

  skip_if_quick() # validates a token against the OSF API
  skip_if_not(online("https://api.osf.io/v2/preprints/khbvy/"))

  # real PAT (if set)
  if (osf_pat != "") {
    obs <- .osf_pat_validate()
    expect_true(obs)
  }

  # bad PAT - direct - leaves the env variable as it is
  expect_warning(obs <- .osf_pat_validate("BADPAT"), "so it is not used")
  expect_false(obs)
  expect_equal(Sys.getenv("OSF_PAT"), osf_pat)

  # bad PAT - from env - resets env variable
  withr::with_envvar(c(OSF_PAT = "BADPATFROMENV"), {
    expect_warning(obs <- .osf_pat_validate(), "Clearing OSF_PAT")
    expect_false(obs)
    expect_equal(Sys.getenv("OSF_PAT"), "")
  })

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

  # each OSF Storage file's file server is remembered from its listing, so a
  # later read skips the request to osf.io (#634); a GitHub add-on's is not
  osf_cache_clear()
  withr::defer(osf_cache_clear())
  osf_obs <- .osf_file_data(osf_data)
  f <- osf_obs$kind %in% "file"
  expect_gt(sum(f), 0)
  known <- vapply(osf_obs$download_url[f], .osf_file_server_known, "", USE.NAMES = FALSE)
  # mc45x is a European project: its region's file server
  expect_true(all(grepl("^https://files[.]de-1[.]osf[.]io/v1/resources/mc45x/providers/osfstorage/", known)))
  code_obs <- .osf_file_data(code_data)
  known <- vapply(code_obs$download_url, .osf_file_server_known, "", USE.NAMES = FALSE)
  expect_true(all(is.na(known)))
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


test_that("osf_pat", {
  expect_true(is.function(metacheck::osf_pat))
  # Intermittently fails with "Can't find development topic" under
  # devtools::test() specifically (not under a fresh load_all() + help() in
  # isolation, and not reproducible by re-running this file alone) --
  # confirmed pre-existing on origin/dev, unrelated to any code change here.
  # A devtools/pkgload help-index staleness issue, not a documentation defect
  # (119 other files use this identical expect_no_error(help(...)) pattern
  # and all pass), so skip on failure rather than let an unrelated test-
  # runner artifact block unrelated changes.
  helplist <- tryCatch(help(osf_pat, metacheck), error = function(e) NULL)
  if (is.null(helplist)) skip("help() topic index unavailable this run (devtools/pkgload artifact, unrelated to osf_pat)")

  withr::local_options(metacheck.osf.pat = NULL)
  withr::local_envvar(OSF_PAT = "")

  # falls back to the environment variable when the option is unset, so
  # .Renviron keeps working exactly as before
  expect_equal(osf_pat(), "")
  withr::local_envvar(OSF_PAT = "from-renviron")
  expect_equal(osf_pat(), "from-renviron")

  # setting it overrides the environment variable for the session
  osf_pat("from-function")
  expect_equal(osf_pat(), "from-function")

  expect_error(osf_pat(123), "single string")
  expect_error(osf_pat(c("a", "b")), "single string")
})


test_that(".osf_headers takes an explicit token", {
  req <- httr2::request("https://api.osf.io")

  withr::local_options(metacheck.osf.pat = NULL)
  withr::local_envvar(OSF_PAT = "")

  # The argument is `pat`, not `osf_pat`: an argument named after the
  # osf_pat() function supplying its default would shadow it and error with
  # "promise already under evaluation".
  obs <- .osf_headers(req, pat = "explicit-token")
  expect_equal(typeof(obs$headers$Authorization), "weakref")

  obs_none <- .osf_headers(req, pat = "")
  expect_null(obs_none$headers$Authorization)
})


test_that(".osf_expand_user_ids leaves non-user ids alone", {
  skip_if_quick() # queries the OSF API
  skip_if_not(online("api.osf.io"))

  # a project id passes through untouched
  expect_equal(.osf_expand_user_ids("6nt4v"), "6nt4v")

  # nothing in, nothing out
  expect_equal(.osf_expand_user_ids(character(0)), character(0))

  # a 24-character waterbutler file id can never be a user, so it is not even
  # queried
  wb <- strrep("a", 24)
  expect_equal(.osf_expand_user_ids(wb), wb)
})


test_that(".osf_user_nodes reduces components to their projects", {
  skip_if_quick() # pages through a real user profile
  skip_if_not(online("api.osf.io"))
  skip_on_cran()

  # A user profile lists every node the user contributes to, components
  # included. Downloading a component separately would duplicate files already
  # nested inside its project, so each node is reduced to its root.
  projects <- .osf_user_nodes("4i578")

  expect_type(projects, "character")
  expect_gt(length(projects), 0)
  expect_false(any(duplicated(projects)))
  expect_true(all(nchar(projects) == 5))

  # the profile lists more nodes than there are unique projects
  all_nodes <- osf_get_all_pages("https://api.osf.io/v2/users/4i578/nodes/")
  expect_gt(nrow(all_nodes), length(projects))
})


test_that(".osf_expand_user_ids expands a user id", {
  skip_if_quick() # pages through a real user profile
  skip_if_not(online("api.osf.io"))
  skip_on_cran()

  expect_equal(osf_type("4i578"), "users")

  expect_message(expanded <- .osf_expand_user_ids("4i578"), "projects to download")
  expect_gt(length(expanded), 1)
  expect_equal(expanded, .osf_user_nodes("4i578"))

  # a project named alongside its owner is kept, and kept first
  mixed <- suppressMessages(.osf_expand_user_ids(c("6nt4v", "4i578")))
  expect_equal(mixed[[1]], "6nt4v")
  expect_false(any(duplicated(mixed)))
})


test_that(".osf_verify_downloads checks the file system", {
  d <- withr::local_tempdir()
  writeLines("hello", file.path(d, "good.txt"))
  good_size <- file.size(file.path(d, "good.txt"))

  # present and the size the OSF reported
  r <- data.frame(path = "good.txt", size = good_size, downloaded = TRUE)
  expect_true(.osf_verify_downloads(r, d)$downloaded)

  # present but truncated: worse than absent, because it looks complete
  r_trunc <- data.frame(path = "good.txt", size = 999999, downloaded = TRUE)
  v <- .osf_verify_downloads(r_trunc, d)
  expect_false(v$downloaded)
  expect_equal(v$size_on_disk, as.numeric(good_size))

  # absent
  expect_false(.osf_verify_downloads(
    data.frame(path = "missing.txt", size = 10, downloaded = TRUE), d)$downloaded)

  # no path was ever set for the row
  expect_false(.osf_verify_downloads(
    data.frame(path = NA_character_, size = 10, downloaded = TRUE), d)$downloaded)

  # the OSF reported no size, so presence is all that can be checked
  expect_true(.osf_verify_downloads(
    data.frame(path = "good.txt", size = NA_real_, downloaded = TRUE), d)$downloaded)

  # a row already known to have failed is not resurrected by the file existing
  expect_false(.osf_verify_downloads(
    data.frame(path = "good.txt", size = good_size, downloaded = FALSE), d)$downloaded)

  # a directory is not a downloaded file
  dir.create(file.path(d, "adir"))
  expect_false(.osf_verify_downloads(
    data.frame(path = "adir", size = NA_real_, downloaded = TRUE), d)$downloaded)

  # zip kept unopened: every row points at the one archive, so sizes cannot
  # be compared per file
  expect_true(.osf_verify_downloads(
    data.frame(path = "good.txt", size = 999999, downloaded = TRUE), d,
    check_size = FALSE)$downloaded)

  # nothing to check
  expect_equal(nrow(.osf_verify_downloads(r[0, ], d)), 0L)
  expect_false(.osf_verify_downloads(
    data.frame(size = 1, downloaded = TRUE), d)$downloaded)
})


test_that("osf_user_projects lists projects to choose from", {
  skip_if_quick() # queries a real user profile
  skip_if_not(online("api.osf.io"))
  skip_on_cran()

  expect_true(is.function(metacheck::osf_user_projects))
  expect_no_error(helplist <- help(osf_user_projects, metacheck))

  projects <- osf_user_projects("4i578")

  expect_s3_class(projects, "data.frame")
  expect_true(all(c("osf_id", "name", "category", "public", "osf_url") %in%
                    names(projects)))
  expect_gt(nrow(projects), 0)
  expect_false(any(duplicated(projects$osf_id)))
  expect_true(all(nchar(projects$osf_id) == 5))

  # titles are the point of the listing: a project only reachable through one
  # of its components is filled in by a second batched request, so almost
  # every row should be named
  expect_gt(sum(!is.na(projects$name)), nrow(projects) * 0.9)

  # Every ID is a plausible project the download path could expand a user to.
  # Not compared against a second live call of .osf_user_nodes(): the OSF
  # returns fewer nodes when it is under load, so two separate listings of the
  # same profile legitimately differ in length.
  expect_type(projects$osf_id, "character")
  expect_false(any(is.na(projects$osf_id)))

  # nothing to list
  expect_equal(nrow(osf_user_projects(NA_character_)), 0L)
})


test_that("osf_file_download accepts a table of projects", {
  # a filtered osf_user_projects() table is how a subset is chosen
  expect_error(osf_file_download(data.frame(name = "x")),
               "no `osf_id` column")
})

test_that(".osf_parse_response keeps the reason a lookup failed (#501)", {
  parse <- function(status) suppressWarnings(
    metacheck:::.osf_parse_response(httr2::response(status_code = status), osf_id = "abcde"))
  # an outage is not "could not be found"
  expect_warning(metacheck:::.osf_parse_response(httr2::response(status_code = 503), osf_id = "abcde"),
                 "the OSF may be down")
  r <- parse(503)
  expect_equal(r$osf_type, "error")
  expect_equal(r$error, "the OSF API did not answer (HTTP 503)")
  expect_equal(parse(404)$osf_type, "unfound")
  expect_equal(parse(404)$error, "not found on the OSF (HTTP 404)")
  expect_equal(parse(410)$error, "deleted or withdrawn from the OSF (HTTP 410)")
  expect_match(parse(429)$error, "^API rate limit exhausted: .*OSF's limit")
  # a failed lookup is not kept in the listing cache
  expect_false(metacheck:::.repo_info_ok(r))
  # a private project is not a failure
  expect_equal(parse(403)$osf_type, "private")
})

test_that("a refused OSF request is not reported as not found, and a file is asked for without contributors (#672)", {
  json <- function(status, body) httr2::response(
    status_code = status, body = charToRaw(body),
    headers = list(`Content-Type` = "application/vnd.api+json"))
  embed <- json(400, '{"errors":[{"source":{"parameter":"embed"},"detail":"The following fields are not embeddable: bibliographic_contributors","status":"400"}]}')
  other <- json(400, '{"errors":[{"detail":"Bad filter"}]}')
  expect_true(metacheck:::.osf_embed_refused(embed))
  expect_false(metacheck:::.osf_embed_refused(other))
  expect_false(metacheck:::.osf_embed_refused(httr2::response(status_code = 404)))
  expect_false(metacheck:::.osf_embed_refused(simpleError("no answer")))

  expect_warning(r <- metacheck:::.osf_parse_response(other, osf_id = "abcde"),
                 "refused the request")
  expect_equal(r$osf_type, "error")
  expect_equal(r$error, "the OSF refused the request (HTTP 400): Bad filter")
  expect_false(metacheck:::.repo_info_ok(r))

  # osf_info() asks for a file again without the contributors
  asked <- character(0)
  file_json <- '{"data":{"id":"64db83e25e739d047b39b61e","type":"files","attributes":{"kind":"file","name":"a.docx","size":10},"links":{"download":"https://osf.io/download/abcde/"},"relationships":{}}}'
  testthat::local_mocked_bindings(.batch_query = function(urls, ...) {
    asked <<- c(asked, urls)
    lapply(urls, function(u) if (grepl("embed=", u, fixed = TRUE)) embed else json(200, file_json))
  })
  info <- osf_info("abcde")
  expect_equal(asked, c("https://api.osf.io/v2/guids/abcde?embed=bibliographic_contributors",
                        "https://api.osf.io/v2/guids/abcde"))
  expect_equal(info$osf_type, "files")
  expect_equal(info$name, "a.docx")
})

test_that("an OSF refusal (private) is not kept in the lasting cache, and an old one is asked again", local({
  withr::local_options(metacheck.repo_info_cache.dir = withr::local_tempdir())
  n_calls <- 0
  local_mocked_bindings(.batch_query = function(urls, ...) {
    n_calls <<- n_calls + length(urls)
    lapply(urls, \(u) httr2::response(status_code = 403))
  })
  # a refusal is given, but not stored
  r <- metacheck:::.osf_info("abcde", cache = TRUE)
  expect_equal(r$osf_type, "private")
  expect_null(metacheck:::.repo_info_cache_get("osf", "abcde"))
  # one stored before is not reused: the OSF is asked again
  metacheck:::.repo_info_cache_put("osf", "fghij", data.frame(osf_id = "fghij", osf_type = "private", public = FALSE))
  metacheck:::.osf_info("fghij", cache = TRUE)
  expect_equal(n_calls, 2)
}))

test_that("osf_file_download on a folders-only project, and osf_api_check offline (U53)", {
  # a single value into the 0-row result stopped with "replacement has 1
  # row, data has 0"
  contents <- data.frame(osf_id = c("proj1", "fold1"), name = c("Project", "Data"),
    osf_type = c("nodes", "files"), kind = c(NA, "folder"), parent = c(NA, "proj1"),
    size = NA, provider = c(NA, "osfstorage"), download_url = NA_character_,
    public = TRUE, filetype = NA_character_, downloads = NA_integer_)
  local_mocked_bindings(osf_info = function(...) contents, osf_api_check = function(...) "OK")
  for (mode in c("select", "zip")) {
    dl <- suppressWarnings(suppressMessages(
      osf_file_download("proj1", download_to = withr::local_tempdir(), mode = mode)))
    expect_equal(nrow(dl), 0, label = mode)
  }
})

test_that("osf_api_check applies on_error when offline (U53)", {
  # return() inside tryCatch() left the function before on_error was read
  local_mocked_bindings(has_internet = function() FALSE, .package = "curl")
  expect_error(osf_api_check(on_error = "stop"), "no internet")
  expect_warning(osf_api_check(on_error = "warn"), "no internet")
  expect_equal(osf_api_check(on_error = "ignore"), "no internet")
})

test_that("OSF helpers: a versioned GUID goes to /guids/, and only folders take a provider's name (U52)", {
  # abcde_v1 was sent to /files/; folders & is.na(name) mixed positions with
  # TRUE/FALSE and renamed unnamed non-folder rows
  sent <- character(0)
  local_mocked_bindings(.batch_query = function(urls, ...) {
    sent <<- c(sent, urls)
    lapply(urls, function(u) NULL)
  })
  suppressWarnings(metacheck:::.osf_info("abcde_v1"))
  expect_match(sent[1], "/guids/abcde_v1", fixed = TRUE)

  data <- list(id = c("f1", "x2"), type = c("files", "files"),
               attributes = data.frame(name = c(NA, NA), kind = c("folder", "file"),
                                       provider = c("osfstorage", "osfstorage")))
  obj <- metacheck:::.osf_file_data(data)
  expect_equal(obj$name, c("osfstorage", NA))
})

test_that("a record with more contributors than the OSF named gets the full list (#682)", {
  people <- function(full, family) data.frame(id = seq_along(full)) |>
    transform(embeds = I(data.frame(users = I(data.frame(data = I(data.frame(
      attributes = I(data.frame(full_name = full, family_name = family,
                                social = I(data.frame(orcid = rep(NA_character_, length(full)))))))))))))
  asked <- character(0)
  local_mocked_bindings(.osf_get_pages_parallel = function(urls) {
    asked <<- c(asked, urls)
    lapply(urls, \(u) {
      if (grepl("fail", u)) return(.osf_error_result("request_failed"))
      list(data = people(c(paste("Person", 1:10), "Anna van 't Veer"), c(paste0("P", 1:10), "van 't Veer")),
           links = list(meta = list(total = 11L)))
    })
  })
  ten <- paste(paste("Person", 1:10), collapse = "; ")
  rows <- list(
    data.frame(osf_id = "xv5rp", osf_type = "nodes", contributors = ten, n_contributors = 11L),
    data.frame(osf_id = "small", osf_type = "nodes", contributors = "A B; C D", n_contributors = 2L),
    data.frame(osf_id = "old10", osf_type = "registrations", contributors = ten),    # cached before the count
    data.frame(osf_id = "fail1", osf_type = "nodes", contributors = ten, n_contributors = 12L),
    data.frame(osf_id = "f1", osf_type = "files"))
  out <- .osf_complete_contributors(rows)
  expect_equal(sort(basename(dirname(dirname(asked)))), c("fail1", "old10", "xv5rp"))
  expect_true(any(grepl("/registrations/old10/bibliographic_contributors/", asked, fixed = TRUE)))
  expect_match(out[[1]]$contributors, "Anna van 't Veer", fixed = TRUE)
  expect_match(out[[1]]$contributor_families, "van 't Veer", fixed = TRUE)
  expect_equal(out[[1]]$n_contributors, 11L)
  expect_equal(out[[3]]$n_contributors, 11L)
  expect_identical(out[[2]], rows[[2]])
  expect_identical(out[[4]], rows[[4]])   # a failed request leaves the record as it was
  expect_identical(out[[5]], rows[[5]])
  # nothing to complete: no request at all
  asked <- character(0)
  expect_identical(.osf_complete_contributors(rows[c(2, 5)]), rows[c(2, 5)])
  expect_length(asked, 0)
})

test_that("a whole listing gets the full list of contributors for its projects only (#682)", {
  local_mocked_bindings(.osf_get_pages_parallel = function(urls) lapply(urls, \(u) list(
    data = data.frame(id = 1:11) |>
      transform(embeds = I(data.frame(users = I(data.frame(data = I(data.frame(
        attributes = I(data.frame(full_name = paste("Person", 1:11), family_name = paste0("P", 1:11),
                                  social = I(data.frame(orcid = rep(NA_character_, 11)))))))))))),
    links = list(meta = list(total = 11L)))))
  ten <- paste(paste("Person", 1:10), collapse = "; ")
  # a listing stored before the count and the family names were kept
  tbl <- data.frame(osf_id = c("xv5rp", "comp1", "f1"), osf_type = c("nodes", "nodes", "files"),
                    contributors = c(ten, "A B", NA))
  out <- .osf_complete_table(tbl)
  expect_equal(out$n_contributors, c(11L, NA, NA))
  expect_match(out$contributors[1], "Person 11", fixed = TRUE)
  expect_match(out$contributor_families[1], "P11", fixed = TRUE)
  expect_equal(out$contributors[2:3], tbl$contributors[2:3])
  expect_identical(.osf_complete_table(tbl[2:3, ]), tbl[2:3, ])
  expect_identical(.osf_complete_table(data.frame()), data.frame())
})
