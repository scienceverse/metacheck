test_that("semanticscholar_query takes a record as a preprint only from arXiv", {
  # Semantic Scholar's answers, without the network
  records <- list(
    list(title = "A", year = 2020, venue = "", externalIds = list(ArXiv = "2001.1"),
         authors = list(list(name = "Ann van Dijk"))),
    list(title = "B", year = 2020, venue = "Journal of Examples",
         externalIds = list(ArXiv = "2001.2", DOI = "10.1/b"), authors = list()),
    list(title = "C", year = 2020, venue = "International Conference on Machine Learning",
         externalIds = list(ArXiv = "2001.3"), publicationTypes = list("Conference"),
         authors = list()))
  local_mocked_bindings(
    online = \(...) TRUE,
    .batch_query = \(urls, ...) lapply(seq_along(urls), \(j) httr2::response(
      status_code = 200, headers = list(`Content-Type` = "application/json"),
      body = charToRaw(jsonlite::toJSON(list(data = list(records[[j]])), auto_unbox = TRUE)))))
  s2 <- semanticscholar_query(c("A", "B", "C"))
  expect_equal(s2$type, c("Preprint", NA, "Conference"))
  expect_equal(s2$authors[1], "van Dijk, Ann")
})
