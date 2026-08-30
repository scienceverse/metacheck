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
    ""
  )

  ids <- .figshare_id(figshare_url)
  expect_equal(unname(ids), c(
    "18093368", "18093368", "18093368", "18093368", "18093368",
    "12345", NA, NA, NA, NA
  ))

  # NULL / empty
  expect_equal(.figshare_id(NULL), character(0))
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
