# Tests for the ZIP central-directory parser and the download decision. The
# range-fetch (zip_peek) needs a live HTTP server, so it is covered by the OSF
# acceptance run; here we test the pure parser on a real local zip's bytes and
# the classification decision, which need no network.

test_that(".parse_zip_central_dir reads names and sizes from a real zip tail", {
  # Build a small zip with known contents.
  d <- withr::local_tempdir()
  writeLines(rep("x", 100), file.path(d, "data.csv"))
  writeLines("img", file.path(d, "stim.png"))
  zip <- file.path(d, "test.zip")
  withr::with_dir(d, utils::zip("test.zip", c("data.csv", "stim.png"),
                                flags = "-q"))
  skip_if_not(file.exists(zip), "zip utility unavailable")

  raw <- readBin(zip, "raw", file.size(zip))
  cd <- metacheck:::.parse_zip_central_dir(raw)
  expect_false(is.null(cd))
  expect_true(all(c("data.csv", "stim.png") %in% basename(cd$name)))
  # uncompressed size of data.csv is ~200 bytes (100 lines of "x\n")
  expect_true(cd$size[basename(cd$name) == "data.csv"] > 0)
})

test_that("zip_decision keeps a data zip and links a pure-asset zip", {
  # Stub zip_peek so no network: two synthetic listings.
  local_mocked_bindings(
    zip_peek = function(url, ...) {
      if (grepl("data", url))
        data.frame(name = c("study.csv", "notes.png"), size = c(100, 200))
      else
        data.frame(name = c("a.png", "b.jpg", "readme.txt"), size = c(1, 2, 3))
    }
  )
  d1 <- zip_decision("http://x/data.zip", skip_types = "materials")
  expect_true(d1$worth)                       # has study.csv (data)

  d2 <- zip_decision("http://x/stimuli.zip", skip_types = "materials")
  expect_false(d2$worth)                       # only images + a readme, no data
  expect_match(d2$reason, "link")
})

test_that("zip_decision returns NA when the peek fails", {
  local_mocked_bindings(zip_peek = function(url, ...) NULL)
  d <- zip_decision("http://x/opaque.zip")
  expect_true(is.na(d$worth))
  expect_match(d$reason, "could not peek")
})

test_that(".expand_zip keeps inner data files and drops inner materials", {
  d <- withr::local_tempdir()
  writeLines("id,x\n1,2", file.path(d, "study.csv"))   # data
  writeLines("img", file.path(d, "stim.png"))          # materials
  writeLines("notes", file.path(d, "README.txt"))      # documentation (readme)
  z <- file.path(d, "mixed.zip")
  withr::with_dir(d, utils::zip("mixed.zip",
                                c("study.csv", "stim.png", "README.txt"),
                                flags = "-q"))
  skip_if_not(file.exists(z), "zip utility unavailable")

  zip_row <- data.frame(
    repo_url = "r", file_name = "mixed.zip", file_path = "mixed.zip",
    file_url = "u", file_location = z, file_size = file.size(z),
    file_type = "archive", repo_name = "r", paper_id = "p.1",
    data_type = "unknown", doc_role = NA_character_,
    data_format = "tabular", group = NA_character_,
    stringsAsFactors = FALSE)

  rows <- metacheck:::.expand_zip(z, zip_row, skip_types = "materials")
  expect_setequal(rows$file_name, c("study.csv", "README.txt"))  # png dropped
  expect_false("stim.png" %in% rows$file_name)
  expect_equal(rows$data_type[rows$file_name == "study.csv"], "data")
  expect_equal(rows$data_type[rows$file_name == "README.txt"], "documentation")
  expect_equal(rows$doc_role[rows$file_name == "README.txt"], "readme")
  expect_true(all(file.exists(rows$file_location)))              # extracted
  # inner rows inherit the zip's repo/paper, lose their own URL
  expect_true(all(rows$paper_id == "p.1"))
  expect_true(all(is.na(rows$file_url)))
})
