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
