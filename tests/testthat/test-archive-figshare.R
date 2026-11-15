test_that("figshare_links finds article, project, and share-link urls", {
  expect_true(is.function(metacheck::figshare_links))
  expect_no_error(helplist <- help(figshare_links, metacheck))

  paper <- test_paper(url = c(
    "https://figshare.com/articles/dataset/some_title/18093368",
    "https://figshare.com/projects/PERICLES_-_Heritage_values/133332",
    "https://figshare.com/s/5e01cc0cae4cf3e2e14f",
    "https://osf.io/abcde"
  ))

  links <- figshare_links(paper)

  expect_equal(nrow(links), 3)
  expect_true(all(c(
    "https://figshare.com/articles/dataset/some_title/18093368",
    "https://figshare.com/projects/PERICLES_-_Heritage_values/133332",
    "https://figshare.com/s/5e01cc0cae4cf3e2e14f"
  ) %in% links$href))

  # article url resolves to a real id; project and share-link urls do not
  # (a project bundles multiple articles rather than being one, and a
  # share-link's hash is opaque -- see .figshare_id()'s own comment)
  by_url <- setNames(links$figshare_id, links$href)
  expect_equal(unname(by_url["https://figshare.com/articles/dataset/some_title/18093368"]),
              "18093368")
  expect_true(is.na(by_url["https://figshare.com/projects/PERICLES_-_Heritage_values/133332"]))
  expect_true(is.na(by_url["https://figshare.com/s/5e01cc0cae4cf3e2e14f"]))

  # only the share-link is flagged unsupported -- a project url is NOT
  # unsupported (it is fully resolvable, just via a different mechanism;
  # see figshare_info()'s project-expansion)
  by_flag <- setNames(links$figshare_unsupported, links$href)
  expect_false(by_flag[["https://figshare.com/articles/dataset/some_title/18093368"]])
  expect_false(by_flag[["https://figshare.com/projects/PERICLES_-_Heritage_values/133332"]])
  expect_true(by_flag[["https://figshare.com/s/5e01cc0cae4cf3e2e14f"]])
})


test_that(".figshare_id", {
  expect_true(is.function(metacheck:::.figshare_id))

  figshare_url <- c(
    "18093368",
    "https://figshare.com/articles/dataset/some_title/18093368",
    "https://figshare.com/articles/18093368",
    "https://doi.org/10.6084/m9.figshare.18093368",
    "10.6084/m9.figshare.18093368.v1",
    "https://ndownloader.figshare.com/files/12345",
    "https://figshare.com/projects/some_project/133332",  # project, not an article
    "https://figshare.com/s/5e01cc0cae4cf3e2e14f",         # share link, opaque hash
    "not-a-figshare-url",
    "",
    # No resource-type segment (slug directly followed by id) -- a real,
    # still-common Figshare article URL shape, confirmed live 2026-09-19
    # against https://figshare.com/articles/PxW_dataset/6934484. Used to
    # return NA: the three-segment (type/name/id) pattern above requires a
    # type keyword that isn't there, and the bare-id pattern requires the
    # id immediately after /articles/, which it also isn't.
    "https://figshare.com/articles/PxW_dataset/6934484",
    # An institutional Figshare instance's own DOI prefix (see
    # .figshare_doi_prefix_hosts()) -- confirmed live 2026-09-19 against a
    # real paper citing Monash University's bridges.monash.edu this way,
    # with no host domain anywhere in the URL. Used to return NA: the
    # id-extraction patterns only ever recognised 10.6084.
    "https://doi.org/10.26180/19095317.v1",
    # Same institutional-prefix shape, no version suffix.
    "https://doi.org/10.26188/14122688",
    # An institutional prefix whose suffix inserts a short sub-prefix
    # before the numeric id (ZivaHub/UCT's "uct." -- confirmed live to
    # resolve to article id 14618526, not a literal id "uct.14618526").
    "https://doi.org/10.25375/uct.14618526.v1",
    # An institutional prefix whose suffix chains TWO sub-prefix segments
    # before the numeric id, not just one (University of Auckland's
    # "k6.auckland." -- confirmed live 2026-09-29 to resolve to article id
    # 25808182). Used to return NA (issue #439): the sub-prefix was matched
    # zero-or-one times, so a second segment left the id unmatched.
    "https://doi.org/10.17608/k6.auckland.25808182.v2",
    # Same two-segment shape for a different institution (University of
    # Sheffield/ORDA's "shef.data." -- confirmed live 2026-09-29 to resolve
    # to article id 13712533).
    "https://doi.org/10.15131/shef.data.13712533"
  )

  ids <- .figshare_id(figshare_url)
  expect_equal(unname(ids), c(
    "18093368", "18093368", "18093368", "18093368", "18093368",
    "12345", NA, NA, NA, NA,
    "6934484", "19095317", "14122688", "14618526",
    "25808182", "13712533"
  ))

  # NULL / empty
  expect_equal(.figshare_id(NULL), character(0))
})


test_that(".figshare_id handles the 4 institutional DOI-prefix hosts added in #464 (issue #465)", {
  # UCL (10.5522) separates its sub-prefix from the id with "/" rather than
  # "." -- confirmed live 2026-10-06 that this used to return just the
  # sub-prefix ("04") instead of the real id.
  expect_equal(unname(.figshare_id("10.5522/04/14484084.v1")), "14484084")
  expect_equal(unname(.figshare_id("10.5522/04/23217572.v1")), "23217572")

  # Adelaide (10.25909) mints a plain numeric suffix for most of its DOIs,
  # which IS the real figshare article id directly.
  expect_equal(unname(.figshare_id("10.25909/33113177")), "33113177")

  # Older Adelaide records use an opaque hex-shaped suffix that is NOT the
  # article id (confirmed live: the real id is 6859511, unrelated to any
  # digits in the DOI) -- this must return NA here, not a truncated partial
  # match on the id's leading digit run ("5"), since a wrong id is worse
  # than a correctly-flagged unresolved one. See the
  # "figshare_info resolves ... via DOI lookup" test below for how this
  # case is actually resolved, one level up in figshare_info().
  expect_equal(unname(.figshare_id("10.25909/5b581a5a151da")), NA_character_)

  # VTechData (10.7294) and USDA Ag Data Commons (10.15482) use DOI
  # suffixes that carry no real figshare article id at all -- a
  # handle-style code, or a numeric accession that is a DIFFERENT number
  # from the real id (confirmed live: 10.15482/USDA.ADC/1402049's real id
  # is 24852405, not 1402049). Both must return NA here rather than a
  # silently wrong id.
  expect_equal(unname(.figshare_id("10.7294/WSDX-AJ44")), NA_character_)
  expect_equal(unname(.figshare_id("10.15482/USDA.ADC/1402049")), NA_character_)
})


test_that("figshare_info resolves VTechData/USDA/Adelaide DOIs with no parseable id via DOI lookup (issue #465)", {
  # These three DOIs all return NA from .figshare_id() itself (see the test
  # above); figshare_info() must still resolve each to its real article id
  # via the API's own GET /v2/articles?doi= lookup rather than leaving it
  # unresolved, confirmed live 2026-10-06 against each DOI's real,
  # independently-checked article id.
  info <- figshare_info(c(
    "https://doi.org/10.7294/WSDX-AJ44",
    "10.15482/USDA.ADC/1402049",
    "10.25909/5b581a5a151da"
  ))

  by_url <- setNames(info$figshare_id, info$figshare_url)
  expect_equal(unname(by_url[["https://doi.org/10.7294/WSDX-AJ44"]]), "14096975")
  expect_equal(unname(by_url[["10.15482/USDA.ADC/1402049"]]), "24852405")
  expect_equal(unname(by_url[["10.25909/5b581a5a151da"]]), "6859511")
})


test_that("figshare_links recognises institutional Figshare DOI prefixes with no host domain in the URL", {
  # Regression test: 29 institutional Figshare instances (28 found via
  # DataCite's client registry, plus Monash found separately -- see
  # .figshare_vanity_hosts()'s own header comment) added 2026-09-19, each
  # confirmed live to answer Figshare's own SPA shell (HTTP 202) rather
  # than a real API response, and each DOI prefix confirmed live to
  # resolve to that exact host.
  hosts <- metacheck:::.figshare_doi_prefix_hosts()
  expect_equal(unname(hosts[["10.26180"]]), "bridges.monash.edu")
  expect_equal(unname(hosts[["10.26188"]]), "melbourne.figshare.com")
  expect_equal(unname(hosts[["10.25375"]]), "zivahub.uct.ac.za")
  expect_true("bridges.monash.edu" %in% metacheck:::.figshare_vanity_hosts())

  paper <- test_paper(
    text = "Data from: ... Monash University. Dataset, https://doi.org/10.26180/19095317.v1."
  )
  links <- figshare_links(paper)
  expect_equal(nrow(links), 1)
  expect_equal(unname(links$figshare_id), "19095317")
  expect_false(links$figshare_unsupported)
})


test_that(".figshare_project_id", {
  expect_true(is.function(metacheck:::.figshare_project_id))

  project_url <- c(
    "https://figshare.com/projects/PERICLES_-_Heritage_values/133332",
    "https://figshare.com/projects/A_Project_With-Punctuation.In.It/999",
    "https://figshare.com/articles/dataset/some_title/18093368",  # article, not a project
    "not-a-figshare-url",
    ""
  )

  ids <- .figshare_project_id(project_url)
  expect_equal(unname(ids), c("133332", "999", NA, NA, NA))
})


test_that(".figshare_project_articles lists a real project's articles", {
  expect_true(is.function(metacheck:::.figshare_project_articles))

  # PERICLES Heritage values project, verified live 2026-08-30 to hold 8
  # articles via GET https://api.figshare.com/v2/projects/133332/articles
  article_ids <- .figshare_project_articles("133332")

  expect_true(length(article_ids) >= 1)
  expect_true(all(grepl("^[0-9]+$", article_ids)))
  expect_true(!anyDuplicated(article_ids))
})


test_that("figshare_info expands a project url into one row per article", {
  expect_true(is.function(metacheck::figshare_info))
  expect_no_error(helplist <- help(figshare_info, metacheck))

  info <- figshare_info("https://figshare.com/projects/PERICLES_-_Heritage_values/133332")

  # every row shares the same source url (traceable back to what the paper
  # actually cited) but has its own resolved article id/title/doi
  expect_true(nrow(info) >= 1)
  expect_true(all(info$figshare_url ==
                 "https://figshare.com/projects/PERICLES_-_Heritage_values/133332"))
  expect_false(anyNA(info$figshare_id))
  expect_false(anyNA(info$doi))
  expect_true(!anyDuplicated(info$figshare_id))
})


test_that("figshare_info on a single article still returns exactly one row", {
  # Guards against the project-expansion logic accidentally affecting the
  # plain single-article path (confirmed live before this test existed: an
  # early version of the expansion left a stray NA-id row alongside the
  # real ones for a PROJECT url -- this test covers the non-project path
  # staying unaffected).
  info <- figshare_info("https://doi.org/10.6084/m9.figshare.18093368")
  expect_equal(nrow(info), 1)
  expect_false(is.na(info$figshare_id))
})


test_that(".figshare_file_path joins folder and name, and cannot leave the folder", local({
  expect_equal(.figshare_file_path("a.csv", "Experiment_1"), "Experiment_1/a.csv")
  expect_equal(.figshare_file_path("a.csv", "Experiment_1/raw/"), "Experiment_1/raw/a.csv")
  expect_equal(.figshare_file_path("Readme.pdf", ""), "Readme.pdf")
  expect_equal(.figshare_file_path("Readme.pdf", NULL), "Readme.pdf")
  expect_equal(.figshare_file_path("x.csv", "../../etc"), "etc/x.csv")
  expect_equal(.figshare_file_path("x.csv", ".."), "x.csv")
  expect_true(is.na(.figshare_file_path(NA_character_, "A")))
}))


# A mocked article response in the shape api.figshare.com returns (field
# names as in article 28052888, read live 2026-10-13).
.fs_article <- function(files, size, folders = NULL) {
  rec <- list(id = 1, title = "T", doi = "10.6084/m9.figshare.1",
              size = size, files = files)
  if (!is.null(folders)) rec$folder_structure <- folders
  rec
}
.fs_json_resp <- function(x) {
  httr2::response(status_code = 200,
                  headers = list(`Content-Type` = "application/json"),
                  body = charToRaw(jsonlite::toJSON(x, auto_unbox = TRUE,
                                                    null = "null")))
}
.fs_file <- function(id, name, size) {
  list(id = id, name = name, size = size,
       download_url = paste0("https://ndownloader.figshare.com/files/", id),
       computed_md5 = NA)
}

test_that(".figshare_info gives each file its folder path from folder_structure", local({
  # Regression test (2026-10-13 survey): folder_structure was ignored, so a
  # file in Experiment_1/ looked as if it sat at the top level (article
  # 28052888 uses Experiment_1/ and Experiment_2/).
  urls <- character(0)
  testthat::local_mocked_bindings(
    .batch_query = function(urls_in, ...) {
      urls <<- c(urls, urls_in)
      list(.fs_json_resp(.fs_article(
        files = list(.fs_file(11, "a.csv", 3), .fs_file(12, "a.csv", 4),
                     .fs_file(13, "Readme.pdf", 5)),
        size = 12,
        folders = list(`11` = "Experiment_1", `12` = "Experiment_2", `13` = ""))))
    }
  )

  info <- .figshare_info("1")
  paths <- vapply(info$files[[1]], \(f) f$path, character(1))
  expect_equal(paths, c("Experiment_1/a.csv", "Experiment_2/a.csv", "Readme.pdf"))
  # sizes add up to the article's size: no second request for the file list
  expect_equal(urls, "https://api.figshare.com/v2/articles/1")
}))

test_that(".figshare_info fetches the full file list when the article's list falls short", local({
  # Figshare's spec says the article response lists "up to 10 article files"
  # (it returned all 103 for article 33328980 on 2026-10-13). When the listed
  # sizes add up to less than the article's `size`, the paged
  # /files endpoint is read instead.
  urls <- character(0)
  testthat::local_mocked_bindings(
    .batch_query = function(urls_in, ...) {
      urls <<- c(urls, urls_in)
      if (grepl("/files?", urls_in, fixed = TRUE)) {
        list(.fs_json_resp(list(.fs_file(11, "a.csv", 3), .fs_file(12, "b.csv", 4),
                                .fs_file(13, "c.csv", 5))))
      } else {
        list(.fs_json_resp(.fs_article(files = list(.fs_file(11, "a.csv", 3)),
                                       size = 12)))
      }
    }
  )

  info <- .figshare_info("1")
  keys <- vapply(info$files[[1]], \(f) f$name, character(1))
  expect_equal(keys, c("a.csv", "b.csv", "c.csv"))
  expect_equal(urls, c(
    "https://api.figshare.com/v2/articles/1",
    "https://api.figshare.com/v2/articles/1/files?page=1&page_size=1000"))
}))

test_that(".figshare_article_files follows pages and keeps what it has on failure", local({
  pages <- 0
  testthat::local_mocked_bindings(
    .batch_query = function(urls_in, ...) {
      pages <<- pages + 1
      if (pages == 1) list(.fs_json_resp(list(.fs_file(1, "a", 1), .fs_file(2, "b", 1))))
      else if (pages == 2) list(.fs_json_resp(list(.fs_file(3, "c", 1))))
      else stop("no third page expected")
    }
  )
  out <- .figshare_article_files("1", page_size = 2)
  expect_length(out, 3)
  expect_equal(pages, 2)

  testthat::local_mocked_bindings(
    .batch_query = function(urls_in, ...) list(httr2::response(status_code = 404))
  )
  expect_null(.figshare_article_files("1"))
}))

test_that("figshare_file_download saves files under their Figshare folders", local({
  testthat::local_mocked_bindings(
    .figshare_info = function(figshare_id, host = "api.figshare.com", pb = NULL) {
      files <- list(.fs_file(11, "a.csv", 8), .fs_file(12, "a.csv", 8))
      files[[1]]$path <- "Experiment_1/a.csv"
      files[[2]]$path <- "Experiment_2/a.csv"
      data.frame(figshare_id = as.character(figshare_id), files = I(list(files)))
    }
  )
  testthat::local_mocked_bindings(
    request = function(url) structure(list(url = url), class = "httr2_request"),
    req_headers = function(req, ...) req,
    req_timeout = function(req, seconds) req,
    req_error = function(req, is_error) req,
    req_perform = function(req) structure(
      list(status = 200, body = charToRaw("x,y\n1,2\n")), class = "httr2_response"),
    resp_status = function(resp) resp$status,
    resp_body_raw = function(resp) resp$body,
    .package = "httr2"
  )

  tmp <- withr::local_tempdir()
  dl <- figshare_file_download("4242", download_to = tmp)

  expect_equal(dl$path, c("Experiment_1/a.csv", "Experiment_2/a.csv"))
  expect_true(all(dl$downloaded))
  # two same-named files, both kept: they no longer overwrite each other
  expect_true(file.exists(file.path(tmp, "figshare_4242", "Experiment_1", "a.csv")))
  expect_true(file.exists(file.path(tmp, "figshare_4242", "Experiment_2", "a.csv")))
}))

test_that(".figshare_zip_members passes the listed size to zip_peek()", local({
  # A Figshare download_url redirects to a signed S3 address that answers
  # HEAD with 403 (verified 2026-10-13), so zip_peek() must not need one.
  got_size <- "unset"
  testthat::local_mocked_bindings(
    zip_peek = function(url, ..., size = NULL) {
      got_size <<- size
      NULL
    }
  )
  expect_null(.figshare_zip_members("https://ndownloader.figshare.com/files/1",
                                    dest = tempdir(), size = 4096))
  expect_equal(got_size, 4096)
}))

test_that("figshare_links keeps a '/' inside an institution's DOI (#634)", {
  paper <- test_paper(c("Ag Data Commons. https://doi.org/10.15482/USDA.ADC/1529590.",
                        "UCL data: 10.5522/04/14484084.v1 here."))
  obs <- figshare_links(paper)
  expect_setequal(sub("[.]$", "", obs$href),
                  c("https://doi.org/10.15482/USDA.ADC/1529590", "10.5522/04/14484084.v1"))
})
