test_that("file_category", {
  expect_true(is.function(metacheck::file_category))
  expect_no_error(helplist <- help(file_category, metacheck))

  # handle zero results and/or OSF down
  summary <- file_category(data.frame())
  expect_equal(nrow(summary), 0)

  # as vector
  contents <- c("a.csv", "b.R", "codebook.xlsx", "readme.txt", "ambiguous", "file.json")
  summary <- file_category(contents)
  obs <- summary$file_category
  exp <- c("data", "code", "codebook", "readme", NA, NA)
  expect_equal(obs, exp)

  # as data frame
  contents <- data.frame(
    name = c("a.csv", "b.R", "codebook.xlsx", "readme.txt", "ambiguous", "file.json"),
    category = c("code", "data", "data", "code", "code", NA),
    filetype = c("data", "code", "data", "text", "text", "code;data")
  )
  summary <- file_category(contents)
  obs <- summary$file_category
  # not currently categorising from category
  exp <- c("data", "code", "codebook", "readme", NA, NA)
  expect_equal(obs, exp)
})

test_that("data_classify_files recognises genomic sequence formats, compressed or not", {
  # Confirmed against real corpus files (data_availability validation) that
  # were falling through to "unknown", wrongly reading a paper as having no
  # data available when real sequencing data was genuinely present. The bare
  # extensions (.fasta/.fa/.fq/.fastq) are format-locked via .ext_registry
  # (data_classify_files()'s Tier 1), so they must be checked through that
  # function rather than file_category() alone, which does not consult
  # .ext_registry and has no entry for them on its own.
  contents <- c("sample.fasta", "sample.fa", "sample.fq", "sample.fastq",
               "sample.fasta.gz", "sample.fa.gz", "sample.fq.gz", "sample.fastq.gz")
  obs <- data_classify_files(contents)
  expect_equal(obs, rep("data", length(contents)))

  # A real archive with no data extension of its own must NOT be swept into
  # "data" by the same "data;archive" compound-type rule file_category() uses
  # for the .gz-compressed forms above -- that rule only fires when the
  # filename ALSO matches a genuine data extension.
  expect_equal(data_classify_files("random_archive.tar.gz"), "unknown")
  expect_equal(data_classify_files("random.gz"), "unknown")

  # .dat is deliberately left ambiguous/unlocked (too generic on its own,
  # same reasoning as excluding a bare ".info"), so a compressed ".dat.gz"
  # stays unclassified too -- this is a design choice, not a gap this fix
  # addresses.
  expect_equal(data_classify_files("sample.dat.gz"), "unknown")
})

test_that("add_filetype", {
  # edge case classification
  files <- c(
    "datarelease.pdf" = "text",    # pdf cannot be data or code
    "my_r_code.pdf" = "text",
    "data.sas" = "stats",          # sas is always code
    "codebook.sas" = "stats",
    "codebook.pdf" = "text"
  )
  ft <- filetype(names(files))
  expect_equal(ft, files)
})

test_that("edge case summarise", {
  # edge case classification
  # category is from OSF, so can be: analysis, communication, data, hypothesis, instrumentation, methods and measures, procedure, project, software, other, but mostly uncategorized (NA)
  contents <- dplyr::tribble(
    ~name,              ~category, ~classify,
    "datarelease.pdf",  NA,         NA,        # pdf cannot be data or code
    "data.pdf",         "data",     NA,        # what about qual data?
    "my_r_code.pdf",    NA,         NA,
    "readme.xls",       "project",  "readme",    # is an xls file always data?
    "data.sas",         NA,         "code",    # sas is always code
    "codebook.sas",     NA,         "codebook",
    "readme.sas",       NA,         "readme",
    "codebook.pdf",     NA,         "codebook" # not a great format but possible
  )
  contents$filetype <- filetype(contents$name)

  summary <- file_category(contents)
  expect_equal(summary$file_category, contents$classify)
})
