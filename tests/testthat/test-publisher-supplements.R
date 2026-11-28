fmt <- function(name) test_path("fixtures", "formats", name)

test_that("supplement_links and supplement_files, without the web", {
  # a paper read from a PDF has no supplementary files
  g <- read(demofile("xml"))
  expect_equal(nrow(supplement_links(g)), 0)
  expect_equal(nrow(supplement_files(g, lookup = FALSE)), 0)

  # JATS: a file with its own DOI has an address
  p <- read(fmt("jats_cases.xml"))
  l <- supplement_links(p)
  expect_equal(l$href, "https://doi.org/10.1234/jtc.2026.001")
  s <- supplement_files(p, lookup = FALSE)
  expect_equal(s$file_name, "s1_data.csv")
  expect_equal(s$file_url, "https://doi.org/10.1234/jtc.2026.001.s001")
  expect_equal(s$source, "address in the XML")
  expect_equal(s$repo_url, l$href)

  # Wiley: no address for automatic download, and the page a reader uses
  w <- supplement_files(read(fmt("wiley_cases.xml")), lookup = FALSE)
  expect_equal(w$file_name, "tst10101-sup-0001-Appendix.pdf")
  expect_true(is.na(w$file_url))
  expect_equal(w$source, "no address for automatic download")
  expect_equal(w$page_url, paste0(
    "https://onlinelibrary.wiley.com/action/downloadSupplement?doi=10.1111%2Ftst.10101",
    "&file=tst10101-sup-0001-Appendix.pdf"))

  # a paper list: one link per paper with supplementary files
  both <- read(c(fmt("jats_cases.xml"), fmt("wiley_cases.xml")))
  expect_equal(nrow(supplement_links(both)), 2)
  expect_equal(sort(supplement_files(both, lookup = FALSE)$paper_id), c("jats_cases", "wiley_cases"))
})

test_that(".supplement_page", {
  expect_equal(.supplement_page("10.1177/0956797620000000", "sj-pdf-1.pdf"),
               "https://journals.sagepub.com/doi/suppl/10.1177/0956797620000000/suppl_file/sj-pdf-1.pdf")
  expect_equal(.supplement_page("10.1371/journal.pone.0000001", c("a.csv", "b.csv")),
               rep("https://doi.org/10.1371/journal.pone.0000001", 2))
})

test_that(".supplement_copies: a supplementary file that is also in a repository", {
  zip <- "PMC1_supplementary_files.zip"
  supp <- data.frame(paper_id = "a", repo_url = "https://doi.org/10.1/x",
                     file_name = c("Data.csv", "appendix.docx", zip),
                     file_size = c(100, NA, NA))
  repo <- data.frame(paper_id = c("a", "a", "a", "b"), repo_url = "https://osf.io/abcde",
                     file_name = c("data.csv", "script.R", "s1.csv", "appendix.docx"),
                     file_size = c(100, 5, 7, 9))
  listed <- data.frame(paper_id = "a", file_name = zip, holds = "S1.csv; script.R")
  # the same name (whatever its case) and size; another paper's file is no
  # copy; a zip whose listed files are all in the repository is a copy
  expect_equal(.supplement_copies(supp, repo, listed)$copy_of,
               c("https://osf.io/abcde", NA, "https://osf.io/abcde"))
  # another size: another file
  repo$file_size[1] <- 99
  expect_true(is.na(.supplement_copies(supp, repo, listed)$copy_of[1]))
  # a zip with a file the repository lacks is not a copy
  listed$holds <- "S1.csv; other.csv"
  expect_true(is.na(.supplement_copies(supp, repo, listed)$copy_of[3]))
  # without repository files, nothing is a copy
  expect_true(all(is.na(.supplement_copies(supp, repo[0, ])$copy_of)))
})

test_that(".supplement_report", {
  expect_null(.supplement_report(NULL)$summary)
  supp <- data.frame(file_path = c("data.csv", "appendix.docx", "code.R"),
                     file_type = c("data", "text", "code"),
                     file_url = c("https://x.org/data.csv", NA, "https://x.org/code.R"),
                     page_url = "https://doi.org/10.1/x",
                     copy_of = c(NA, NA, "https://osf.io/abcde"))
  r <- .supplement_report(supp)
  expect_match(r$summary, "hosts 3 supplementary files")
  expect_match(r$summary, "1 of them is also in a repository")
  expect_match(r$summary, "1 could not be downloaded automatically")
  expect_equal(r$report[1], "#### Supplementary files hosted by the publisher")
})
