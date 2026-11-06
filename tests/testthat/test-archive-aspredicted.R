test_that("aspredicted_links", {
  skip_no_psychsci()
  expect_true(is.function(metacheck::aspredicted_links))
  expect_no_error(helplist <- help(aspredicted_links, metacheck))

  expect_error(aspredicted_links(bad_arg))

  links <- aspredicted_links(psychsci)
  expect_in("text_id", names(links))
  expect_true(all(grepl("^https://aspredicted\\.org", links$href, ignore.case = TRUE)))
  # expect_equal(nrow(links), 74)
  expect_equal(links, unique(links))

  sentences <- links |>
    dplyr::rename(text = href) |>
    text_expand(psychsci)

  urls <- c(
    "https://aspredicted.org/abcd1",
    "https://aspredicted.org/abcd2",
    "https://aspredicted.org/abcd3",
    "https://aspredicted.org/blind.php?x=abcd4",
    "https://aspredicted.org/abcd5.pdf",
    "https://aspredicted.org/ABC_DE6"
  )
  paper <- test_paper(url = urls)

  links <- aspredicted_links(paper)
  exp <- c("https://aspredicted.org/abcd1",
           "https://aspredicted.org/abcd2",
           "https://aspredicted.org/abcd3",
           "https://aspredicted.org/blind.php?x=abcd4",
           "https://aspredicted.org/abcd5.pdf",
           "https://aspredicted.org/ABC_DE6")
  expect_equal(links$href, exp)

  # second trailing blind links
  paper <- psychsci[[220]]
  links <- aspredicted_links(paper)
  expect_true(all(links$href != "https://aspredicted.org/blind.php?"))

  # wierd aspredicted> .org
  paper <- psychsci[[88]]
  links <- aspredicted_links(paper)
  expect_true(any(grepl("/vp4rg", links$href)))
  expect_true(any(grepl("/3kq9y", links$href)))
})

# skip online checking in aspredicted_* functions
testthat::local_mocked_bindings(
  online = \(...) TRUE
)

test_that("aspredicted_info blind", {
  withr::local_options(metacheck.researchbox.token = "") # the web page, not the API
  # single blind link
  ap_url <- "https://aspredicted.org/blind.php?x=nq4xa3"
  suppressMessages( info <- aspredicted_info(ap_url) )
  expect_equal(info$ap_url, ap_url)
  exp_auth <- "This pre-registration is currently anonymous to enable blind peer-review.\nIt has one author."
  expect_equal(info$AP_authors, exp_auth)
}, "mock")

test_that("aspredicted_info pdf", {
  withr::local_options(metacheck.researchbox.token = "") # the web page, not the API
  # single pdf
  ap_url <- "https://aspredicted.org/ve2qn.pdf"
  suppressMessages( info <- aspredicted_info(ap_url) )
  expect_equal(info$ap_url, ap_url)
  exp <- "How infants encode unexpected events: a SSVEP study"
  expect_equal(info$AP_title, exp)
}, "mock")

test_that("aspredicted_info multiple", {
  withr::local_options(metacheck.researchbox.token = "") # the web page, not the API
  skip_if_quick() # testing wait time

  # multiple links in a table
  ap_url <- data.frame(link = c(
    "https://aspredicted.org/ve2qn.pdf",
    "https://aspredicted.org/blind.php?x=nq4xa3"
  ))
  suppressMessages( {
    time1 <- system.time( info <- aspredicted_info(ap_url))
    time3 <- system.time( info <- aspredicted_info(ap_url, wait = 3))
  })
  expect_true(time1[["elapsed"]] > 2)
  expect_true(time3[["elapsed"]] > 6)
}, "mock")

test_that(".aspredicted_info proj", {
  withr::local_options(metacheck.researchbox.token = "") # the web page, not the API
  ap_url <- "https://aspredicted.org/Y2F_6B7"
  suppressMessages( info <- .aspredicted_info(ap_url) )
  title <- "Children's prosocial behavior in response to awe-inspiring art"
  authors <- "This pre-registration is currently anonymous to enable blind peer-review.\nIt has 5 authors."

  expect_equal(info$ap_url, ap_url)
  expect_equal(info$AP_title, title)
  expect_equal(info$AP_authors, authors)
}, "mock")


test_that(".aspredicted_info pdf", {
  withr::local_options(metacheck.researchbox.token = "") # the web page, not the API
  ap_url <- "https://aspredicted.org/ve2qn.pdf"
  suppressMessages( info <- .aspredicted_info(ap_url) )
  title <- "How infants encode unexpected events: a SSVEP study"
  authors <- paste(sep = "\n",
    "Moritz Köster (Freie Universität Berlin) - moritz.koester@ur.de",
    "Miriam Langeloh (Max Planck Institute for Human Cognitive and) - langeloh@cbs.mpg.de",
    "Stephanie Höhl (Max Planck Institute for Human Cognitive and) - stefanie.hoehl@univie.ac.at"
  )

  expect_equal(info$ap_url, ap_url)
  expect_equal(info$AP_title, title)
  expect_equal(info$AP_authors, authors)
}, "mock")

test_that(".aspredicted_info blind", {
  withr::local_options(metacheck.researchbox.token = "") # the web page, not the API
  ap_url <- "https://aspredicted.org/blind.php?x=nq4xa3"
  suppressMessages( info <- .aspredicted_info(ap_url) )
  title <- "Depre_ctrl_elicit [3x2] [N800 MT]"
  authors <- "This pre-registration is currently anonymous to enable blind peer-review.\nIt has one author."
  hypo <- "Participants will imagine a depressed person as more likely to be overweight, less likely to take care of themselves, less likely to be successful at work, with a less attractive face, and more introverted compared to an actor that is explicitly described as not depressed and to a control condition."
  key_dv <- "^Seven questions .* extroverted\\?$"
  cond <- "^Participants will be .* quite OK psychologically speaking\\.$"
  outliers <- "Participants who respond .* will be excluded from analyses\\.$"

  expect_equal(info$ap_url, ap_url)
  expect_equal(info$AP_title, title)
  expect_equal(info$AP_authors, authors)
  expect_equal(info$AP_created, "2021/06/14 02:17 (PT)")
  expect_equal(info$AP_data, "No, no data have been collected for this study yet.")
  expect_equal(info$AP_hypotheses, hypo)
  expect_true(grepl(key_dv, info$AP_key_dv))
  expect_true(grepl(cond, info$AP_conditions))
  expect_equal(info$AP_analyses, "We will conduct two-way ANOVAs with gender and condition as IVs and all the seven DVs.")
  expect_true(grepl(outliers, info$AP_outliers))
  expect_equal(info$AP_sample_size, "800 Mturkers.")
  expect_equal(info$AP_anything_else, "")
  expect_equal(info$AP_version, "2.00")
}, "mock")

test_that("an AsPredicted page answering an error, or no connection, is recorded (U54)", {
  withr::local_options(metacheck.researchbox.token = "") # the web page, not the API
  # req_perform() raised on 4xx/5xx before the status check, stopping prereg_check
  local_mocked_bindings(req_perform = function(req, ...) httr2::response(404), .package = "httr2")
  info <- suppressWarnings(.aspredicted_info("https://aspredicted.org/abc.pdf"))
  expect_equal(info$error, "Not Found")
  local_mocked_bindings(req_perform = function(req, ...) stop("timeout"), .package = "httr2")
  info <- suppressWarnings(.aspredicted_info("https://aspredicted.org/abc.pdf"))
  expect_equal(info$error, "the connection failed")
})

test_that("a version 1.05 web page is cut at the headings it has", {
  # each section was cut only at the next heading in the list: the analyses
  # took in questions 6 to 8 and the page's scripts, the sample size the
  # scripts, and the outliers and "anything else" were NA
  withr::local_options(metacheck.researchbox.token = "") # the web page, not the API
  info <- suppressMessages(.aspredicted_info("https://aspredicted.org/ve2qn.pdf"))
  expect_equal(info$AP_version, "1.05")
  expect_match(info$AP_analyses, "Alpha \\(higher \\[less supression\\] for expected\\)\\.$")
  expect_match(info$AP_sample_size, "upper end of the sample size range\\.$")
  expect_true(is.na(info$AP_outliers))
  expect_match(info$AP_secondary, "^3\\. Explorative")
  expect_match(info$AP_anything_else, "^There are usually high drop out rates")
  expect_equal(info$AP_registered, "2017-08-18")
  expect_false(info$AP_anonymous)
}, "mock")

test_that(".aspredicted_info reads ResearchBox's API, each answer placed by its question", {
  withr::local_options(metacheck.researchbox.token = "test-token",
                       metacheck.state.dir = withr::local_tempdir())
  info <- suppressMessages(.aspredicted_info("https://aspredicted.org/ve2qn.pdf"))
  expect_equal(attr(info, "route"), "api")
  expect_null(info$error)
  expect_equal(info$AP_title, "How infants encode unexpected events: a SSVEP study")
  # version 1.05 asks about secondary analyses where 2.00 asks about outliers
  expect_equal(info$AP_version, "1.05")
  expect_true(is.na(info$AP_outliers))
  expect_match(info$AP_secondary, "^3\\. Explorative")
  expect_match(info$AP_analyses, "^For the extraction of the SSVEP signal")
  expect_match(info$AP_anything_else, "^There are usually high drop out rates")
  # the API's "08/18/2017 06:55 AM" as the web page writes it
  expect_equal(info$AP_created, "2017/08/18 06:55 (PT)")
  expect_equal(info$AP_registered, "2017-08-18")
  expect_equal(info$AP_authors, paste(sep = "\n",
    "Moritz Köster (Freie Universität Berlin)",
    "Miriam Langeloh (Max Planck Institute for Human Cognitive and)",
    "Stephanie Höhl (Max Planck Institute for Human Cognitive and)"))
  expect_false(info$AP_anonymous)
  expect_equal(info$AP_number, "5189")
  expect_equal(info$AP_status, "public")
  expect_equal(info$AP_authors_count, 3L)
  expect_equal(info$AP_bundle, "5189")
}, "mock")

test_that("the API and the web page give the same columns and answers", {
  urls <- c("https://aspredicted.org/blind.php?x=nq4xa3",
            "https://aspredicted.org/Y2F_6B7")
  withr::local_options(metacheck.state.dir = withr::local_tempdir())
  page <- withr::with_options(list(metacheck.researchbox.token = ""),
                              suppressMessages(aspredicted_info(urls, wait = 0)))
  # the blind link is sent as its code, which the API takes
  api <- withr::with_options(list(metacheck.researchbox.token = "test-token"),
                             suppressMessages(aspredicted_info(urls)))
  expect_equal(names(api), names(page))
  # line breaks and spaces differ
  same <- \(x) gsub("[[:space:]]+", " ", x)
  for (col in c("AP_title", "AP_authors", "AP_created", "AP_data", "AP_hypotheses",
                "AP_key_dv", "AP_conditions", "AP_analyses", "AP_outliers",
                "AP_secondary", "AP_sample_size", "AP_anything_else", "AP_version",
                "AP_registered", "AP_anonymous")) {
    expect_equal(same(api[[col]]), same(page[[col]]), label = col)
  }
  expect_equal(api$AP_number, c("68347", "67958"))
  expect_equal(api$AP_authors_count, c(1L, 5L))
  expect_true(all(is.na(page$AP_number)))
}, "mock")

test_that(".ap_id takes the identifier the API takes from each link form", {
  # the API refuses a blind.php link but takes its code (checked 2026-11-05)
  links <- c("https://aspredicted.org/ve2qn.pdf", "https://aspredicted.org/3pc3-6tw7.pdf",
             "https://aspredicted.org/Y2F_6B7", "https://aspredicted.org/blind.php?x=nq4xa3",
             "http://www.AsPredicted.org/blind.php?x=JCZ_RNR", "https://aspredicted.org/Y2F_6B7/",
             "https://aspredicted.org/", "http://aspredicted.org", "https://aspredicted",
             "https://aspredicted.org/see_one.php?a=1")
  expect_equal(.ap_id(links),
               c("ve2qn.pdf", "3pc3-6tw7.pdf", "Y2F_6B7", "nq4xa3", "JCZ_RNR", "Y2F_6B7",
                 NA, NA, NA, NA))
})

test_that("a link that names no preregistration is not looked up", {
  withr::local_options(metacheck.researchbox.token = "test-token")
  sent <- 0
  local_mocked_bindings(req_perform = function(req, ...) {
    sent <<- sent + 1
    httr2::response(200)
  }, .package = "httr2")
  for (u in c("https://aspredicted.org/", "http://www.aspredicted.org/", "https://aspredicted")) {
    info <- suppressWarnings(suppressMessages(.aspredicted_info(u)))
    expect_equal(info$error, "the link names no AsPredicted preregistration")
  }
  expect_equal(sent, 0)
})

test_that("a preregistration the API cannot find is recorded, and after 4 no new one is sent", {
  # ResearchBox blocks the key for an hour after 5 within 24 hours
  withr::local_options(metacheck.researchbox.token = "test-token",
                       metacheck.state.dir = withr::local_tempdir())
  sent <- 0
  local_mocked_bindings(req_perform = function(req, ...) {
    sent <<- sent + 1
    httr2::response(404, headers = list(`Content-Type` = "application/json"),
                    body = charToRaw('{"ok":false,"error":"not found or not public"}'))
  }, .package = "httr2")
  info <- .aspredicted_info_api("https://aspredicted.org/abcd1.pdf")
  expect_equal(info$error, "unfound")
  expect_equal(.ap_unfound_ids()$id, "abcd1.pdf")
  # not sent again
  info <- .aspredicted_info_api("https://aspredicted.org/abcd1.pdf")
  expect_equal(info$error, "unfound")
  expect_equal(sent, 1)
  for (id in c("abcd2", "abcd3", "abcd4")) {
    .aspredicted_info_api(paste0("https://aspredicted.org/", id, ".pdf"))
  }
  expect_equal(sent, 4)
  info <- .aspredicted_info_api("https://aspredicted.org/abcd5.pdf")
  expect_equal(sent, 4)
  expect_match(info$error, "^not sent: ResearchBox could not find 4 ")
})

test_that("AsPredicted's daily limit is recorded apart from the limit for boxes", {
  withr::local_options(metacheck.researchbox.token = NULL,
                       metacheck.state.dir = withr::local_tempdir())
  # only a problem with the server's key is recorded
  withr::local_envvar(RESEARCHBOX_API_KEY = "server-key")
  reply <- NULL
  sent <- 0
  local_mocked_bindings(req_perform = function(req, ...) {
    sent <<- sent + 1
    reply
  }, .package = "httr2")
  json <- \(status, body, ...) httr2::response(
    status, headers = list(`Content-Type` = "application/json", ...),
    body = charToRaw(body))

  reply <- json(429, '{"ok":false,"error":"daily limit"}', `Retry-After` = "7200")
  res <- .rbox_api("get_aspredicted", id = "abcd1.pdf", format = "json")
  expect_match(res$error, "^API rate limit exhausted: ")
  expect_match(res$error, "ResearchBox's limit for AsPredicted preregistrations until")
  expect_null(.key_problem("researchbox"))

  # boxes are still asked, AsPredicted is not
  reply <- json(200, '{"ok":true,"box_id":801}')
  sent <- 0
  expect_true(.rbox_api("box_meta", box_id = "801")$ok)
  res <- .rbox_api("get_aspredicted", id = "abcd1.pdf", format = "json")
  expect_match(res$error, "^API rate limit exhausted: ")
  expect_match(res$error, "ResearchBox's limit for AsPredicted preregistrations until")
  expect_equal(sent, 1)

  # a limit for boxes does not stop AsPredicted; a refused key stops both
  .key_problem_clear("researchbox_aspredicted")
  .key_problem_record("researchbox", "limit", Sys.time() + 3600)
  expect_true(.rbox_api("get_aspredicted", id = "abcd1.pdf", format = "json")$ok)
  .key_problem_record("researchbox", "rejected")
  sent <- 0
  expect_false(.rbox_api("get_aspredicted", id = "abcd1.pdf", format = "json")$ok)
  expect_equal(sent, 0)
})

test_that("after AsPredicted's captcha the API is still asked, and no page is read", {
  # the loop stopped at the captcha, and every later preregistration got no
  # information
  withr::local_options(metacheck.researchbox.token = "test-token")
  pages <- 0
  local_mocked_bindings(
    .aspredicted_info_api = function(ap_url) {
      obj <- data.frame(ap_url = ap_url)
      if (grepl("abcd2", ap_url)) obj$AP_title <- "Read through the API" else obj$error <- "unfound"
      obj
    },
    .aspredicted_info_page = function(ap_url) {
      pages <<- pages + 1
      data.frame(ap_url = ap_url, error = "captcha")
    }
  )
  urls <- paste0("https://aspredicted.org/abcd", 1:3, ".pdf")
  info <- suppressMessages(aspredicted_info(urls, wait = 0))
  expect_equal(pages, 1)
  expect_equal(info$error[[1]], "captcha")
  expect_equal(info$AP_title[[2]], "Read through the API")
  expect_true(is.na(info$error[[2]]))
  expect_match(info$error[[3]], "web page not read: AsPredicted asked for a captcha$")
})

test_that("cache = TRUE reuses a preregistration already read", {
  withr::local_options(metacheck.researchbox.token = "test-token",
                       metacheck.repo_info_cache.dir = withr::local_tempdir())
  calls <- 0
  local_mocked_bindings(.aspredicted_info_api = function(ap_url) {
    calls <<- calls + 1
    data.frame(ap_url = ap_url, AP_title = "Study A")
  })
  suppressMessages(aspredicted_info("https://aspredicted.org/abcd1.pdf", cache = TRUE))
  # cached by identifier: another link to it gets its own url
  info <- suppressMessages(aspredicted_info("http://www.aspredicted.org/abcd1.pdf", cache = TRUE))
  expect_equal(calls, 1)
  expect_equal(info$ap_url, "http://www.aspredicted.org/abcd1.pdf")
  expect_equal(info$AP_title, "Study A")
  # off by default
  suppressMessages(aspredicted_info("https://aspredicted.org/abcd1.pdf"))
  expect_equal(calls, 2)
})

test_that("without a ResearchBox key, a message says so once per session", {
  withr::local_options(metacheck.researchbox.token = "",
                       metacheck.aspredicted_key.notified = NULL)
  local_mocked_bindings(
    online = function(...) TRUE,
    .aspredicted_info_page = function(ap_url) data.frame(ap_url = ap_url, AP_title = "A")
  )
  url <- "https://aspredicted.org/abcd1.pdf"
  expect_message(aspredicted_info(url, wait = 0),
                 "No ResearchBox API key is set.*rbox_token\\(\\).*https://researchbox.org/api")
  msgs <- testthat::capture_messages(aspredicted_info(url, wait = 0))
  expect_false(any(grepl("No ResearchBox API key", msgs)))

  # not with a key
  withr::local_options(metacheck.researchbox.token = "test-token",
                       metacheck.aspredicted_key.notified = NULL)
  local_mocked_bindings(.aspredicted_info_api = function(ap_url) data.frame(ap_url = ap_url, AP_title = "A"))
  msgs <- testthat::capture_messages(aspredicted_info(url, wait = 0))
  expect_false(any(grepl("No ResearchBox API key", msgs)))
})
