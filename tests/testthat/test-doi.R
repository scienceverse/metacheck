test_that("doi_lookup", {
  expect_true(is.function(metacheck::doi_lookup))
  expect_no_error(helplist <- help(doi_lookup, metacheck))

  expect_error(doi_lookup(badarg))

  # NULL VALUE
  doi <- NULL
  exp <- data.frame(doi = character(0))
  info <- doi_lookup(doi)
  expect_equal(info, exp)

  # empty vector
  doi <- c()
  exp <- data.frame(doi = character(0))
  info <- doi_lookup(doi)
  expect_equal(info, exp)

  #skip_api("doi.org")

  # one item
  doi <- "10.7717/peerj.4375"
  info <- doi_lookup(doi)
  expect_equal(nrow(info), 1)
  expect_equal(info$title, "The state of OA: a large-scale analysis of the prevalence and impact of Open Access articles")

  # one NA
  doi <- NA
  info <- doi_lookup(doi)
  expect_equal(nrow(info), 1)
  expect_equal(info$title, NA_character_)

  # multiple items
  doi <- c("10.1177/2515245920970949", "10.1037/0003-066x.54.6.408")
  info <- doi_lookup(doi)
  exp <- c("Improving Transparency, Falsifiability, and Rigor by Making Hypothesis Tests Machine-Readable",
           "The origins of sex differences in human behavior: Evolved dispositions versus social roles.")
  expect_equal(info$title, exp)
}, "mock")

test_that("doi_clean", {
  expect_true(is.function(metacheck::doi_clean))
  expect_no_error(helplist <- help(doi_clean, metacheck))

  expect_error(doi_clean(bad_arg))

  exp <- "10.1038/nphys1170"
  doi <- "https://doi.org/10.1038/nphys1170"
  clean <- doi_clean(doi)
  expect_equal(clean, exp)

  doi <- "doi:10.1038/nphys1170"
  clean <- doi_clean(doi)
  expect_equal(clean, exp)

  doi <- "  DOI : 10.1038/nphys1170 "
  clean <- doi_clean(doi)
  expect_equal(clean, exp)

  doi <- "bad.doi"
  clean <- doi_clean(doi)
  expect_equal(clean, doi)

  # multiple
  doi <- c(
    "https://doi.org/10.1038/nphys1170",
    "doi:10.1038/nphys1170",
    "  DOI : 10.1038/nphys1170 ",
    "10.1038/nphys1170",
    "",
    NA
  )
  clean <- doi_clean(doi)
  exp <- rep("10.1038/nphys1170", 4) |> c("", NA)
  expect_equal(clean, exp)

  doi <- list(
    "https://doi.org/10.1038/nphys1170",
    "doi:10.1038/nphys1170",
    "  DOI : 10.1038/nphys1170 ",
    "10.1038/nphys1170",
    "",
    NA
  )
  clean <- doi_clean(doi)
  expect_equal(clean, exp)

  # journal-specific format
  doi <- "http://journals.plos.org/plosone/article?id=10.1371/journal.pone.0004153"
  clean <- doi_clean(doi)
  exp <- "10.1371/journal.pone.0004153"
  expect_equal(clean, exp)

  # make sure the start match at the first 10.###
  doi <- "10.1234/more.10.2345"
  clean <- doi_clean(doi)
  expect_equal(clean, doi)

  # remove post # sections
  doi <- "10.1234/mypaper#section"
  clean <- doi_clean(doi)
  expect_equal(clean, "10.1234/mypaper")

  # remove post /full
  doi <- "10.1234/mypaper/full"
  clean <- doi_clean(doi)
  expect_equal(clean, "10.1234/mypaper")

  # what reading a PDF adds: spaces, punctuation, "%2F", a bracket around a
  # link; a parenthesis in the DOI stays
  doi <- c("10.1037/0003-066x.54.6.4 08", "10.1037/0003-066x.54.6.408.",
           "<https://doi.org/10.1037/0003-066x.54.6.408>.",
           "(https://doi.org/10.1037%2F0003-066x.54.6.408).")
  expect_equal(doi_clean(doi), rep("10.1037/0003-066x.54.6.408", 4))
  expect_equal(doi_clean("10.1016/0010-0277(93)90058-4"), "10.1016/0010-0277(93)90058-4")
}, "mock")

test_that("doi_valid_format", {
  expect_true(is.function(metacheck::doi_valid_format))
  expect_no_error(helplist <- help(doi_valid_format, metacheck))

  expect_error(doi_valid_format(bad_arg))

  doi <- "10.1038/nphys1170"
  v <- doi_valid_format(doi)
  expect_equal(v, T)

  doi <- "no10.1038/nphys1170"
  v <- doi_valid_format(doi)
  expect_equal(v, F)

  doi <- NA
  v <- doi_valid_format(doi)
  expect_equal(v, F)

  doi <- c("10.1038/nphys1170", "bad", NA)
  v <- doi_valid_format(doi)
  expect_equal(v, c(T, F, F))

  # odd format
  doi <- "10.1002/(SICI)1099-1611(200001/02)9:1<11::AID-PON424>3.0.CO;2-Z"
  v <- doi_valid_format(doi)
  expect_equal(v, T)
}, "mock")

test_that("doi_resolves", {
  expect_true(is.function(metacheck::doi_resolves))
  expect_no_error(helplist <- help(doi_resolves, metacheck))

  expect_error(doi_resolves(bad_arg))

  doi <- "10.1038/nphys1170"
  check <- doi_resolves(doi)
  expect_true(check)

  doi <- "10.1234/invalid.doi"
  check <- doi_resolves(doi)
  expect_false(check)

  check <- doi_resolves(NA)
  expect_equal(check, NA)

  check <- doi_resolves("")
  expect_equal(check, NA)

  # invalid format
  check <- doi_resolves("bad.doi")
  expect_false(check)

  # multiple
  doi <- c("10.1038/nphys1170", "10.1234/invalid.doi", "bad.doi", NA)
  check <- doi_resolves(doi)
  expect_equal(check, c(T, F, F, NA))

  # clean DOIs
  doi <- "https://doi.org/10.1038/nphys1170"
  check <- doi_resolves(doi)
  expect_true(check)

  doi <- "doi:10.1038/nphys1170"
  check <- doi_resolves(doi)
  expect_true(check)

  doi <- "  DOI : 10.1038/nphys1170 "
  check <- doi_resolves(doi)
  expect_true(check)

  # try to break it
  doi <- list("10.1038/nphys1170", "10.1234/invalid.doi", NA)
  check <- doi_resolves(doi)
  expect_equal(check, c(T, F, NA))

  # should it do this or break?
  doi <- list(c("10.1038/nphys1170", "10.1234/invalid.doi"),
              c("10.1038/nphys1170", NA))
  check <- doi_resolves(doi)
  expect_equal(check, c(T, F, T, NA))

  # server not available - without_internet not working!
  doi <- c("10.1038/nphys1170", "10.1234/invalid.doi", "bad.doi", NA)
  # httptest2::without_internet({
  #   check <- doi_resolves(doi)
  #   expect_equal(check, c(NA, NA, NA, F))
  # })
}, "mock")


test_that("doi_lookup and openalex_doi keep the other DOIs when one fails (U12)", {
  # return() inside tryCatch() returned from the function: one 404 gave NULL
  ok <- list(DOI = "10.1/ok", title = "Fine",
             author = list(list(family = "Smith", given = "A"), list(name = "The Lab")))
  local_mocked_bindings(.batch_query = function(urls, ...) {
    lapply(urls, function(u) if (grepl("bad", u)) httr2::response(404) else
      httr2::response(200, headers = list(`Content-Type` = "application/json"),
                      body = charToRaw(jsonlite::toJSON(ok, auto_unbox = TRUE))))
  })
  r <- doi_lookup(c("10.1234/bad", "10.1234/ok"))
  expect_equal(nrow(r), 2)
  expect_equal(r$author[2], "Smith, A; The Lab")
  oa <- openalex_doi(c("10.1234/bad", "10.1234/ok"))
  expect_length(oa, 2)
  expect_equal(oa[[1]]$error, "not found")
})

test_that("doi_clean decodes percent codes, dashes and entities safely, and doi_valid_format accepts SICI brackets", {
  # Wiley percent-encoded Unicode hyphen and BioOne/ESA SICI square brackets
  expect_equal(doi_clean("10.1111/j.1461%E2%80%900248.2007.01033.x"),
               "10.1111/j.1461-0248.2007.01033.x")
  sici <- "10.1890/0012-9658(2002)083%5B2352:CARTAP%5D2.0.CO;2"
  expect_equal(doi_clean(sici), "10.1890/0012-9658(2002)083[2352:CARTAP]2.0.CO;2")
  expect_true(doi_valid_format(doi_clean(sici)))
  # HTML entities inside DOIs and dual-issue "&" DOIs
  expect_equal(doi_clean("10.1002/1097-4679%28197901%2935%3A1%26lt%3B60%3A%3Aaid-jclp2270350106%26gt%3B3.0.co%3B2-r"),
               "10.1002/1097-4679(197901)35:1<60::aid-jclp2270350106>3.0.co;2-r")
  expect_equal(doi_clean("10.1128/JVI.75.19.9129&ndash;9141.2001"),
               "10.1128/JVI.75.19.9129-9141.2001")
  expect_equal(doi_clean("10.1207/s15327051hci1901%26amp%3B2%5F7"),
               "10.1207/s15327051hci1901&2_7")
  expect_true(doi_valid_format("10.1207/s15327752jpa5503&4_10"))
  expect_equal(doi_clean("10.1101/109876&x00A0;"), "10.1101/109876")
  # numeric entity &#160; decoded before "#" fragment stripping (does not truncate)
  expect_equal(doi_clean("10.1101/109876&#160;end"), "10.1101/109876end")
  # encoded %23, %20, %252F and balanced %5B/%5D preserved safely and idempotently;
  # unbalanced trailing %5D is stripped so doi_clean() is idempotent
  expect_equal(doi_clean("10.1234/abc%23def#frag"), "10.1234/abc%23def")
  expect_equal(doi_clean(doi_clean("10.1234/abc%23def#frag")), "10.1234/abc%23def")
  expect_equal(doi_clean("10.1000/a%252Fb"), "10.1000/a%252Fb")
  expect_equal(doi_clean(doi_clean("10.1000/a%252Fb")), "10.1000/a%252Fb")
  expect_equal(doi_clean("10.1000/a%20b"), "10.1000/a%20b")
  expect_equal(doi_clean("10.1000/a%5B1%5D"), "10.1000/a[1]")
  expect_equal(doi_clean(doi_clean("10.1000/foo%5D")), doi_clean("10.1000/foo%5D"))
  # SICI check character ";2-#" is preserved (not stripped as a URL fragment)
  sici_hash <- "10.1002/(SICI)1097-0193(1998)6:3<160::AID-HBM5>3.0.CO;2-#"
  expect_equal(doi_clean(sici_hash), sici_hash)
  expect_true(doi_valid_format(sici_hash))
  # Latin-1 input is converted to UTF-8 and cleaned
  lat1 <- "https://doi.org/10.1000/caf\xe9"
  Encoding(lat1) <- "latin1"
  expect_equal(doi_clean(lat1), "10.1000/caf\u00e9")
  # stray % does not stop %2F decoding elsewhere in the DOI
  expect_equal(doi_clean("10.1000/50%off%2Fx"), "10.1000/50%off/x")
  # stray %, NUL (%00), newline (%0A), invalid UTF-8 (%E9), and raw invalid bytes do not error
  expect_equal(doi_clean("10.1/a%zz"), "10.1/a%zz")
  expect_equal(doi_clean("10.1/a%00b"), "10.1/a%00b")
  expect_equal(doi_clean("10.1/a%0Ab"), "10.1/a%0Ab")
  expect_equal(doi_clean("10.1/a%E9b"), "10.1/a%E9b")
  bad_bytes <- "10.1/a\xffb"
  Encoding(bad_bytes) <- "UTF-8"
  expect_equal(doi_clean(bad_bytes), bad_bytes)
  # corrupt DOIs with commas still fail format check
  expect_false(doi_valid_format("10.1016/j,jneumeth.2006.11.017"))
})

test_that("doi_lookup handles scalar JSON body and null published date-parts safely", {
  local_mocked_bindings(.batch_query = function(urls, ...) {
    list(
      httr2::response(200, headers = list(`Content-Type` = "application/json"),
                      body = charToRaw('"scalar-string"')),
      httr2::response(200, headers = list(`Content-Type` = "application/json"),
                      body = charToRaw('{"DOI":"10.1/x","title":"T","published":{"date-parts":[[null]]}}'))
    )
  })
  res <- doi_lookup(c("10.1/scalar", "10.1/x"))
  expect_equal(nrow(res), 2)
  expect_true(is.na(res$title[1]))
  expect_equal(res$title[2], "T")
  expect_true(is.na(res$year[2]))
})

test_that("doi_resolves treats 429 and 5xx as NA and forwards timeout", {
  seen_timeout <- NULL
  local_mocked_bindings(.batch_query = function(urls, timeout_s = NULL, ...) {
    seen_timeout <<- timeout_s
    list(httr2::response(429), httr2::response(503), httr2::response(404))
  })
  res <- doi_resolves(c("10.1038/a", "10.1038/b", "10.1038/c"), timeout = 7)
  expect_equal(seen_timeout, 7)
  expect_equal(res, c(NA, NA, FALSE))
})
