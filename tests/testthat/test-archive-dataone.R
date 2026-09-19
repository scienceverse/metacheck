test_that(".dataone_pid handles a DOI suffix containing a further slash", {
  # Regression test: PISCO (data.piscoweb.org, DOI prefix 10.6085) mints
  # DataONE PIDs whose suffix itself contains a slash
  # (10.6085/AA/marine_ltm.20.1, confirmed live to be a real, resolving
  # DOI). The bare-DOI branch of .dataone_pid() used
  # [A-Za-z0-9._-]+ for the suffix, which cannot match across a "/" and so
  # silently returned NA for this whole class of PID -- found via a real
  # paper citing exactly this DOI with no host domain in the URL at all.
  expect_true(is.function(metacheck:::.dataone_pid))

  expect_equal(
    metacheck:::.dataone_pid("https://doi.org/10.6085/AA/marine_ltm.20.1"),
    "doi:10.6085/AA/marine_ltm.20.1"
  )
  expect_equal(
    metacheck:::.dataone_pid("10.6085/AA/marine_ltm.20.1"),
    "doi:10.6085/AA/marine_ltm.20.1"
  )
  # Existing single-segment-suffix shapes still work (no regression).
  expect_equal(
    metacheck:::.dataone_pid("https://doi.org/10.18739/A2GT5FG86"),
    "doi:10.18739/A2GT5FG86"
  )
  expect_equal(
    metacheck:::.dataone_pid("10.5063/PG1Q4B"),
    "doi:10.5063/PG1Q4B"
  )
  # A "doi:" marker in a landing-page URL still takes priority over the
  # bare-DOI branch, and also allows a slash in its own suffix.
  expect_equal(
    metacheck:::.dataone_pid("https://arcticdata.io/metacat/d1/mn/v2/view/doi:10.18739/A2GT5FG86"),
    "doi:10.18739/A2GT5FG86"
  )
})

test_that("dataone_links recognises the data.piscoweb.org host and its DOI prefix", {
  # Regression test: data.piscoweb.org (PISCO) added to .dataone_hosts()
  # 2026-09-19, confirmed live to run the standard Metacat API at
  # /metacat/d1/mn/v2/ (real DataONE XML node-capabilities response) and
  # to serve real systemMetadata for a real dataset under its own
  # registered DOI prefix, 10.6085.
  expect_true(is.function(metacheck::dataone_links))

  hosts <- metacheck:::.dataone_hosts()
  pisco <- Filter(function(h) identical(h$host, "data.piscoweb.org"), hosts)
  expect_length(pisco, 1)
  expect_equal(pisco[[1]]$doi_prefix, "10.6085")

  paper <- test_paper(
    text = "uploaded to DataONE and are accessible here: https://doi.org/10.6085/AA/marine_ltm.20.1"
  )
  links <- dataone_links(paper)

  expect_equal(nrow(links), 1)
  expect_equal(unname(links$dataone_host), "data.piscoweb.org")
  expect_equal(unname(links$dataone_pid), "doi:10.6085/AA/marine_ltm.20.1")
})
