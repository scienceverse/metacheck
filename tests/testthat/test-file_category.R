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
  # .json is "data" in file_types since 2026-10-13 (it was listed as both code
  # and data, which file_category() left undecided)
  exp <- c("data", "code", "codebook", "readme", NA, "data")
  expect_equal(obs, exp)

  # as data frame (an explicit "code;data" filetype is still left undecided)
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

  # A single compressed file is typed as the file inside it (#648): .dat is
  # not format-locked, but the crosswalk makes "sample.dat" data, and so
  # "sample.dat.gz" too.
  expect_equal(data_classify_files("sample.dat.gz"), data_classify_files("sample.dat"))
})

test_that("data_classify_files types build files, notices, Stata help and compressed files (#648)", {
  x <- c("Makefile", "Dockerfile", "Snakefile_inflation", "Makefile.win", "postBuild",
         "NOTICE", "NEWS", "CHANGELOG", "asdoc.sthlp", "estadd.hlp",
         "scan.nii.gz", "trials.csv.gz", "ids.txt.xz",
         # not changed: a type found otherwise, an archive, an unknown format
         "makefile.txt", "news.md", "pkg.tar.gz", "reads.bed.gz", "newsletter")
  expect_equal(data_classify_files(x), c(
    rep("code", 5), rep("documentation", 5), "data", "data", "documentation",
    "documentation", "documentation", "unknown", "unknown", "unknown"))
  # a compressed file in a folder is typed with its path, as the file inside
  expect_equal(data_classify_files("x.gz", "raw/x.gz"), "unknown")
  expect_equal(data_classify_files("trials.txt.gz", "data/trials.txt.gz"),
               data_classify_files("trials.txt", "data/trials.txt"))
  # a file of an unknown type is not a naming problem
  expect_equal(nrow(check_file_naming("model.joblib", data_type = "unknown")), 0L)
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

test_that("README names, including read.me (#487)", {
  readmes <- c("README.md", "README_data.txt", "Read_me.txt", "READ ME.pdf", "read-me.txt",
               "read.me.txt", "READ.ME", "DataReadMe.txt", "27README", "Lange.et.al.2021_readme.txt")
  expect_true(all(.is_readme_name(readmes)))
  expect_true(all(file_category(readmes)$file_category == "readme"))
  # Dryad 10.5061/dryad.dt7bh4b keeps its README as read.me.txt
  expect_equal(.data_doc_role("read.me.txt"), "readme")

  # the dotted form only as a whole word
  not_readmes <- c("spread.means.csv", "thread.metadata.csv", "bread.me", "data.csv")
  expect_false(any(.is_readme_name(not_readmes)))
})

test_that("names that are not valid UTF-8 are classified, not refused (U76)", {
  bad <- c("a.csv", rawToChar(as.raw(c(0x62, 0xff, 0x2e, 0x74, 0x78, 0x74))))  # "b<ff>.txt"
  expect_equal(filetype(bad), setNames(c("data", "text"), bad))  # named as given
  fc <- file_category(bad)
  expect_equal(fc$filetype[[2]], "text")
  expect_identical(fc$name, bad)          # names kept as given
  expect_length(data_classify_files(bad), 2)
  cfn <- check_file_naming(bad)
  expect_true(bad[2] %in% cfn$file_name)  # returned as given, so callers can match
})
