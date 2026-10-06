test_that("gitlab_links finds real hyperlinks, subgroup paths, and a bare mention", {
  expect_true(is.function(metacheck::gitlab_links))

  exp <- c(
    "gitlab.com/a/b1",
    "http://gitlab.com/a/b2",
    "https://gitlab.com/a/b3",
    "https://gitlab.com/a/b4.git",
    # a subgroup path (GitLab-specific; unlike GitHub's flat owner/repo)
    "https://gitlab.com/a/b5/subgroup/b6"
  )
  text <- "The gitlab repo is a/b9"
  paper <- test_paper(text)
  paper$url <- data.frame(href = exp, text_id = 2:6)

  obs <- gitlab_links(paper)
  expect_setequal(obs$href, c(exp, "a/b9"))
})


test_that("gitlab_links excludes gitlab.io pages and self-hosted instances", {
  # GitLab Pages (gitlab.io) must not be mistaken for a repo mention near
  # the word "gitlab".
  paper_pages <- test_paper("See the gitlab page at someuser.gitlab.io/project for details")
  expect_equal(nrow(gitlab_links(paper_pages)), 0)

  # A self-hosted instance is, by documented design (file header comment),
  # indistinguishable from an arbitrary institutional website and is never
  # matched -- only gitlab.com is.
  paper_self_hosted <- test_paper(text = "nothing relevant here")
  paper_self_hosted$url <- data.frame(href = "https://gitlab.example.org/a/b", text_id = 1)
  expect_equal(nrow(gitlab_links(paper_self_hosted)), 0)
})


test_that(".gitlab_config adds a User-Agent, and PRIVATE-TOKEN only when a PAT is set", {
  expect_true(is.function(metacheck:::.gitlab_config))

  old <- gitlab_pat()
  on.exit(gitlab_pat(old %||% ""), add = TRUE)

  gitlab_pat("")
  h <- .gitlab_config(httr2::request("https://gitlab.com"))
  expect_s3_class(h, "httr2_request")
  expect_equal(h$headers[["User-Agent"]], "scienceverse/metacheck")
  expect_null(h$headers[["PRIVATE-TOKEN"]])

  gitlab_pat("fake-token-for-test")
  h2 <- .gitlab_config(httr2::request("https://gitlab.com"))
  expect_equal(h2$headers[["PRIVATE-TOKEN"]], "fake-token-for-test")
  gitlab_pat(old %||% "")
})


test_that("gitlab_pat gets and sets the token", {
  old <- gitlab_pat()
  on.exit(gitlab_pat(old %||% ""), add = TRUE)

  gitlab_pat("")
  expect_equal(gitlab_pat(), "")

  gitlab_pat("abc123")
  expect_equal(gitlab_pat(), "abc123")
  gitlab_pat(old %||% "")
})


test_that("gitlab_repo normalizes a real public project's URL/path forms", {
  expect_true(is.function(metacheck::gitlab_repo))

  expect_equal(gitlab_repo("gitlab-org/gitlab-test"), "gitlab-org/gitlab-test")
  expect_equal(
    gitlab_repo("https://gitlab.com/gitlab-org/gitlab-test"),
    "gitlab-org/gitlab-test"
  )
  expect_equal(
    gitlab_repo("https://gitlab.com/gitlab-org/gitlab-test/"),
    "gitlab-org/gitlab-test"
  )
  expect_equal(
    gitlab_repo("https://gitlab.com/gitlab-org/gitlab-test.git"),
    "gitlab-org/gitlab-test"
  )
  # a real public subgroup project (3 path segments), unlike GitHub's
  # exactly-two-segment github_repo()
  expect_equal(
    gitlab_repo("gitlab-org/quality/triage-ops"),
    "gitlab-org/quality/triage-ops"
  )

  # invalid shape, never reaches the network
  expect_null(gitlab_repo("just-one-segment"))

  # length-0 guard, no network call
  expect_null(gitlab_repo(character(0)))

  # a real 404
  expect_null(gitlab_repo("gitlab-org/nonexistent-repo-xyz-should-404"))

  # vectorized
  res <- gitlab_repo(c("gitlab-org/gitlab-test", "just-one-segment"))
  expect_equal(unname(res[[1]]), "gitlab-org/gitlab-test")
  expect_null(res[[2]])
})


test_that("gitlab_tree_files reports gated=TRUE for an invalid repo with no network call", {
  result <- gitlab_tree_files("just-one-segment")
  expect_true(result$gated)
  expect_equal(result$reason, "invalid or inaccessible GitLab repository")
  expect_null(result$files)
  expect_true(is.na(result$default_branch))
  expect_true(is.na(result$license))
})


test_that("gitlab_tree_files lists a real, small, stable public project", {
  # gitlab-org/gitlab-test is GitLab's own long-lived internal test fixture
  # project (used in GitLab's own test suite for years): small (59 entries,
  # single tree page, confirmed live 2026-10-05), extremely unlikely to be
  # deleted or restructured.
  result <- gitlab_tree_files("gitlab-org/gitlab-test")

  expect_false(result$gated)
  expect_true(is.na(result$reason))
  expect_equal(result$license, "mit")
  expect_true("README.md" %in% result$files$path)
  expect_true(result$files$size[result$files$path == "README.md"] > 0)
  expect_equal(
    names(result$files),
    c("repo", "clean_repo", "name", "path", "download_url", "size", "type")
  )
})
