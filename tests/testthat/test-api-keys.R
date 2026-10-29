# The test bodies are wrapped in local(): helper.R's test_that() wrapper
# evaluates a test's code in the test FILE's environment, so a withr
# local_*() call made directly in it would stay in force until the end of the
# file instead of the end of the test.

# A state folder of its own and no keys, for one test
local_no_keys <- function(env = parent.frame()) {
  withr::local_options(list(metacheck.state.dir = withr::local_tempdir(.local_envir = env),
                            metacheck.keys.shared = NULL,
                            metacheck.github.pat = NULL, metacheck.osf.pat = NULL,
                            metacheck.dryad.pat = NULL, metacheck.dryad.client_id = NULL,
                            metacheck.researchbox.token = NULL,
                            metacheck.gitlab.pat = NULL, metacheck.openalex.key = NULL),
                       .local_envir = env)
  withr::local_envvar(GITHUB_PAT = "", OSF_PAT = "", DRYAD_PAT = "",
                      DRYAD_CLIENT_ID = "", DRYAD_CLIENT_SECRET = "",
                      RESEARCHBOX_API_KEY = "", GITLAB_PAT = "",
                      OPENALEX_API_KEY = "", .local_envir = env)
}

test_that("the state folder is the option, else the environment variable", local({
  withr::local_options(metacheck.state.dir = NULL)
  withr::local_envvar(METACHECK_STATE_DIR = "/srv/state")
  expect_equal(.metacheck_state_dir(), "/srv/state")
  withr::local_options(metacheck.state.dir = "/opt/state")
  expect_equal(.metacheck_state_dir(), "/opt/state")
  withr::local_options(metacheck.state.dir = NULL)
  withr::local_envvar(METACHECK_STATE_DIR = "")
  expect_match(.metacheck_state_dir(), "metacheck.*state$")
}))

test_that("whose key is used: a visitor's, the server's, or none", local({
  local_no_keys()
  expect_equal(.key_source("osf"), "none")
  withr::local_envvar(OSF_PAT = "server-key")
  expect_equal(.key_source("osf"), "server")
  withr::local_options(metacheck.osf.pat = "server-key")
  expect_equal(.key_source("osf"), "server")
  withr::local_options(metacheck.osf.pat = "own-key")
  expect_equal(.key_source("osf"), "user")
  # Dryad's server key may be a client id and secret
  withr::local_envvar(DRYAD_CLIENT_ID = "id", DRYAD_CLIENT_SECRET = "secret")
  expect_equal(.key_source("dryad"), "server")
}))

test_that("key problems are kept in the state folder until they end", local({
  local_no_keys()
  withr::local_envvar(GITHUB_PAT = "server-key")
  expect_null(.key_problem("github"))
  .key_problem_record("github", "limit", Sys.time() + 600)
  p <- .key_problem("github")
  expect_equal(p$problem, "limit")
  expect_equal(p$source, "server")
  expect_true(file.exists(file.path(getOption("metacheck.state.dir"), "key_status.rds")))

  # a record about the server's key does not concern a visitor's own key,
  # nor the requests without a key once the server's key is removed
  withr::local_options(metacheck.github.pat = "own-key")
  expect_null(.key_problem("github"))
  withr::local_options(metacheck.github.pat = NULL)
  expect_null(.key_problem("github", source = "none"))

  # a limit that has ended no longer counts
  .key_problem_record("github", "limit", Sys.time() - 1)
  expect_null(.key_problem("github"))

  # a refused key is tried again after an hour
  .key_problem_record("github", "rejected")
  p <- .key_problem("github")
  expect_equal(p$problem, "rejected")
  expect_true(p$until > Sys.time() + 3500)
  .key_problem_clear("github")
  expect_null(.key_problem("github"))

  # problems with a visitor's own key are not recorded
  withr::local_options(metacheck.github.pat = "own-key")
  .key_problem_record("github", "limit", Sys.time() + 600)
  expect_null(.key_status_read()$github)
}))

test_that("the messages name whose key it is and what to do", local({
  until <- as.POSIXct("2026-10-28 15:00:00", tz = "UTC")
  m <- .key_problem_message("dryad", "limit", until, source = "server", shared = TRUE)
  expect_equal(m, paste(
    "The Dryad key provided by metacheck has reached Dryad's limit until",
    "2026-10-28 15:00 UTC. Please provide your own Dryad key to continue",
    "(you can make one at https://datadryad.org/account)"))
  m <- .key_problem_message("github", "limit", until, source = "server", shared = FALSE)
  expect_match(m, "^The GitHub key in GITHUB_PAT has reached GitHub's limit")
  m <- .key_problem_message("osf", "limit", until, source = "none")
  expect_match(m, "^OSF's limit for requests without a key was reached until 2026-10-28 15:00 UTC\\.")
  m <- .key_problem_message("osf", "rejected", source = "user")
  expect_match(m, "^Your OSF key was rejected by OSF.*Check the key, or make a new one at https://osf.io/settings/tokens$")
}))

test_that("api_keys_use sets a visitor's keys and skips empty ones", local({
  local_no_keys()
  old <- api_keys_use(list(github = " gh-key ", osf = "", dryad = NA_character_),
                      shared = TRUE)
  expect_equal(github_pat(), "gh-key")
  expect_equal(osf_pat(), "")
  expect_equal(dryad_pat(), "")
  expect_true(getOption("metacheck.keys.shared"))
  expect_equal(.key_source("github"), "user")
  options(old)
  expect_null(getOption("metacheck.github.pat"))
  expect_null(getOption("metacheck.keys.shared"))

  expect_error(api_keys_use(list(zenodo = "x")), "Name each key by its service")
  expect_error(api_keys_use("x"), "Name each key by its service")
  expect_no_error(api_keys_use())
}))

test_that("api_key_status lists each service with its problem", local({
  local_no_keys()
  local_mocked_bindings(github_pat = function(...) "")
  withr::local_envvar(RESEARCHBOX_API_KEY = "server-key")
  withr::local_options(metacheck.keys.shared = TRUE)
  .key_problem_record("researchbox", "limit", Sys.time() + 600)
  st <- api_key_status()
  expect_equal(st$service, c("github", "osf", "dryad", "researchbox", "gitlab", "openalex"))
  expect_equal(st$key[st$service == "researchbox"], "server")
  expect_equal(st$problem[st$service == "researchbox"], "limit")
  expect_match(st$message[st$service == "researchbox"],
               "^The ResearchBox key provided by metacheck has reached")
  expect_true(all(is.na(st$message[st$service != "researchbox"])))
}))

test_that("a refused key is checked once and then left out", local({
  local_no_keys()
  withr::local_envvar(GITLAB_PAT = "refused-key-for-test")
  n <- 0
  check <- function(key) { n <<- n + 1; FALSE }
  expect_warning(k <- .key_use("gitlab", check = check),
                 "The GitLab key in GITLAB_PAT was rejected by GitLab")
  expect_equal(k, "")
  expect_equal(.key_problem("gitlab")$problem, "rejected")
  expect_equal(.key_use("gitlab", check = check), "")
  expect_equal(n, 1)

  # a key that passes, or could not be checked, is used
  withr::local_envvar(GITLAB_PAT = "good-key-for-test")
  expect_equal(.key_use("gitlab", check = \(key) TRUE), "good-key-for-test")
  expect_null(.key_status_read()$gitlab)
  withr::local_envvar(GITLAB_PAT = "unchecked-key-for-test")
  expect_equal(.key_use("gitlab", check = \(key) NA), "unchecked-key-for-test")
  expect_equal(.key_use("gitlab", key = ""), "")
}))

test_that(".gitlab_config sends a token only when GitLab accepts it", local({
  local_no_keys()
  local_mocked_bindings(.key_check_request = function(url, headers) {
    !identical(headers$`PRIVATE-TOKEN`, "refused-token-for-gitlab-test")
  })
  gitlab_pat("accepted-token-for-gitlab-test")
  h <- .gitlab_config(httr2::request("https://gitlab.com"))
  expect_equal(h$headers[["PRIVATE-TOKEN"]], "accepted-token-for-gitlab-test")
  gitlab_pat("refused-token-for-gitlab-test")
  expect_warning(h <- .gitlab_config(httr2::request("https://gitlab.com")), "rejected")
  expect_null(h$headers[["PRIVATE-TOKEN"]])
}))

test_that("keys go only to the host they belong to", local({
  expect_true(.url_host_is("https://osf.io/download/abc/", "osf.io"))
  expect_true(.url_host_is("https://files.de-1.osf.io/v1/x", "osf.io"))
  expect_false(.url_host_is("https://web.archive.org/web/2020/https://osf.io/abc/", "osf.io"))
  expect_false(.url_host_is("https://example.com/?u=zenodo.org", "zenodo.org"))
  expect_false(.url_host_is("https://osf.io.example.com/x", "osf.io"))
  expect_false(.url_host_is("https://notosf.io/x", "osf.io"))
  expect_false(.url_host_is("not a url", "osf.io"))
  expect_true(.url_host_is("https://dataverse.nl/api/x", c("dataverse.harvard.edu", "dataverse.nl")))

  local_mocked_bindings(osf_pat = function(...) "tok", zenodo_pat = function(...) "ztok")
  req <- .auth_for_url(httr2::request("https://web.archive.org/web/2020/https://osf.io/abc/"))
  expect_null(httr2::req_get_headers(req, "reveal")$Authorization)
  req <- .auth_for_url(httr2::request("https://example.com/files?from=zenodo.org"))
  expect_null(httr2::req_get_headers(req, "reveal")$Authorization)

  expect_equal(.key_service_for_host("api.github.com"), "github")
  expect_equal(.key_service_for_host("datadryad.org"), "dryad")
  expect_equal(.key_service_for_host("files.osf.io"), "osf")
  expect_true(is.na(.key_service_for_host("raw.githubusercontent.com")))
  expect_true(is.na(.key_service_for_host("zenodo.org")))
}))

test_that("GitHub's limit is reported, not read as an empty repository", local({
  local_no_keys()
  # without this, git's credential store may still hold a GitHub token
  local_mocked_bindings(github_pat = function(...) "")
  reset <- as.numeric(Sys.time()) + 1800
  limited <- httr2::response(
    status_code = 403, url = "https://api.github.com/repos/a/b",
    headers = list(`x-ratelimit-remaining` = "0",
                   `x-ratelimit-reset` = as.character(round(reset))))
  m <- .github_limit(limited)
  expect_match(m, "^API rate limit exhausted: GitHub's limit for requests without a key was reached until")
  expect_equal(.key_problem("github")$problem, "limit")
  # a 403 for another reason is not a limit
  expect_null(.github_limit(httr2::response(status_code = 403)))
  expect_null(.github_limit(httr2::response(status_code = 200)))

  local_mocked_bindings(.github_config = function(req) req)
  httr2::local_mocked_responses(function(req) limited)
  r <- github_tree_files("a/b")
  expect_true(r$gated)
  expect_null(r$files)
  expect_match(r$reason, "^API rate limit exhausted: ")
}))

test_that("GitLab's and OpenAlex's limits are recorded", local({
  local_no_keys()
  m <- .gitlab_limit(httr2::response(status_code = 429,
                                     headers = list(`retry-after` = "60")))
  expect_match(m, "^API rate limit exhausted: GitLab's limit")
  expect_null(.gitlab_limit(httr2::response(status_code = 404)))
  expect_null(.gitlab_limit(NULL))

  m <- .openalex_limit(httr2::response(status_code = 429))
  expect_match(m, "^API rate limit exhausted: OpenAlex's limit .* until \\d{4}-\\d\\d-\\d\\d 00:00 UTC")
  expect_null(.openalex_limit(httr2::response(status_code = 200)))
}))

test_that("openalex_key gets and sets the key, sent as a Bearer header", local({
  local_no_keys()
  expect_equal(openalex_key(), "")
  req <- .openalex_headers(httr2::request("https://api.openalex.org/works"))
  expect_null(httr2::req_get_headers(req, "reveal")$Authorization)
  local_mocked_bindings(.key_check_request = function(url, headers) TRUE)
  openalex_key("oa-key-for-test")
  req <- .openalex_headers(httr2::request("https://api.openalex.org/works"))
  expect_equal(httr2::req_get_headers(req, "reveal")$Authorization, "Bearer oa-key-for-test")
  expect_error(openalex_key(1), "single string")
}))

test_that("a visitor's Dryad token comes before the server's client id", local({
  local_no_keys()
  withr::local_envvar(DRYAD_CLIENT_ID = "server-id", DRYAD_CLIENT_SECRET = "server-secret")
  req <- .dryad_headers(httr2::request("https://datadryad.org/api/v2/files/1/download"))
  expect_false(is.null(req$policies$auth_sign))
  dryad_pat("visitor-token")
  req <- .dryad_headers(httr2::request("https://datadryad.org/api/v2/files/1/download"))
  expect_equal(httr2::req_get_headers(req, "reveal")$Authorization, "Bearer visitor-token")
  expect_null(req$policies$auth_sign)
}))

test_that("a refused Dryad key is recorded and named in the error", local({
  local_no_keys()
  withr::local_envvar(DRYAD_PAT = "server-token")
  withr::local_options(metacheck.keys.shared = TRUE)
  hint <- .dryad_401_hint("https://datadryad.org/api/v2/files/1/download", 401L)
  expect_match(hint, "The Dryad key provided by metacheck was rejected by Dryad")
  expect_equal(.key_problem("dryad")$problem, "rejected")
  expect_equal(.dryad_401_hint("https://datadryad.org/x", 404L), "")
}))

test_that("ResearchBox: a refused key and a used-up limit stop later requests", local({
  local_no_keys()
  withr::local_envvar(RESEARCHBOX_API_KEY = "server-key")
  json <- function(status, body, ...) {
    httr2::response(status_code = status, body = charToRaw(body),
                    headers = list(`Content-Type` = "application/json", ...))
  }
  sent <- 0
  reply <- json(401, '{"ok":false,"error":"token invalid"}')
  local_mocked_bindings(req_perform = function(req, ...) { sent <<- sent + 1; reply },
                        .package = "httr2")
  res <- .rbox_api("box_meta", box_id = 1)
  expect_match(res$error, "^HTTP 401: token invalid\\. The ResearchBox key in RESEARCHBOX_API_KEY was rejected")
  expect_equal(.key_problem("researchbox")$problem, "rejected")
  res <- .rbox_api("box_meta", box_id = 2)
  expect_equal(sent, 1)
  expect_match(res$error, "^The ResearchBox key in RESEARCHBOX_API_KEY was rejected")

  # a limit for one box does not stop requests for other boxes; the key's
  # daily limit does
  .key_problem_clear("researchbox")
  reply <- json(429, '{"ok":false,"error":"list_files same-box hourly limit"}', `Retry-After` = "1800")
  expect_match(.rbox_api("list_files", box_id = 3)$error, "same-box hourly limit")
  expect_null(.key_problem("researchbox"))
  reply <- json(429, '{"ok":false,"error":"daily limit"}', `Retry-After` = "7200")
  expect_match(.rbox_api("list_files", box_id = 4)$error,
               "^API rate limit exhausted: daily limit \\(ResearchBox asks to wait 7200 seconds\\)\\. The ResearchBox key")
  n <- sent
  expect_match(.rbox_api("list_files", box_id = 5)$error, "^API rate limit exhausted: The ResearchBox key")
  expect_equal(sent, n)

  # a visitor's own key is not held back by the server key's limit
  withr::local_options(metacheck.researchbox.token = "own-key")
  reply <- json(200, '{"ok":true}')
  expect_true(.rbox_api("list_files", box_id = 5)$ok)
}))
