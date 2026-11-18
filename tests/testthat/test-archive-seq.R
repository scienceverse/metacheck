test_that("seq_archive_links finds sequence and expression archive deposits (#660)", {
  p <- test_paper(text = c(
    "RNA-seq data are deposited in GEO (GSE149523) and the SRA (PRJNA770129, SRR16287660).",
    "The single nucleus reads are deposited (SRR7416431 to SRR7416438).",
    "Data are in ArrayExpress under E-MTAB-6487; see E-ISSN 1234-5678.",
    "All reads were deposited in the DDBJ Sequence Read Archive (DRA007363).",
    "We analysed 20 participants."))
  l <- seq_archive_links(p)
  expect_setequal(paste(l$accession, l$repo_type),
                  c("GSE149523 geo", "PRJNA770129 ena", "SRR7416431 ena", "E-MTAB-6487 arrayexpress", "DRA007363 ena"))
  expect_equal(l$href[l$accession == "GSE149523"], "https://www.ncbi.nlm.nih.gov/geo/query/acc.cgi?acc=GSE149523")
  # a run in a sentence that names its study is not a deposit of its own
  expect_false("SRR16287660" %in% l$accession)
  # an accession is found in its sentence (the link itself is not written there)
  expect_equal(.own_needle("https://www.ncbi.nlm.nih.gov/geo/query/acc.cgi?acc=GSE149523"), "gse149523")
  expect_equal(.own_needle("https://osf.io/abcde"), .own_core("https://osf.io/abcde"))
  expect_equal(nrow(seq_archive_links(test_paper(text = "No accession here."))), 0)
  # a paper naming many accessions keeps those in depositing sentences first
  many <- test_paper(text = c(paste("We reanalysed", paste0("GSE", 1001:1030, collapse = ", "), "."),
                              "Our data have been deposited in GEO (GSE999999)."))
  m <- seq_archive_links(many, max_n = 5)
  expect_equal(nrow(m), 5)
  expect_true("GSE999999" %in% m$accession)
})

test_that("an archive record's institute counts as authorship only without people (#660)", {
  aff <- c("Department of Biology, Michigan State University, East Lansing",
           "Fundacion para el Fomento de la Investigacion Sanitaria y Biomedica (FISABIO), Valencia")
  expect_true(.own_org_match("Michigan State University", aff))
  expect_true(.own_org_match("FISABIO", aff))
  expect_false(.own_org_match("University of Groningen", aff))
  expect_true(is.na(.own_org_match("SUB14660361", aff)))
  expect_true(is.na(.own_org_match("Michigan State University", character(0))))

  pf <- list(title = "Host genotype controls ecological change in the leaf fungal microbiome",
             doi = "10.1371/journal.pbio.0000001", family = c("Vanwallendael", "Lowry"),
             full = c("Acer Vanwallendael", "David Lowry"), orcids = character(0),
             affiliations = "Department of Plant Biology, Michigan State University, East Lansing",
             year = 2022, date = as.Date("2022-07-01"), abstract = "")
  own <- function(...) .repo_ownership(data.frame(..., stringsAsFactors = FALSE), pf)
  # deposited, by the authors' institute: the paper's own
  x <- own(source = "text", repo_url = "https://www.ebi.ac.uk/ena/browser/view/PRJNA717293", repo_type = "ena",
           context = "Raw sequence data are available in the Sequence Read Archive as BioProject PRJNA717293.",
           dep_type = "Dataset", dep_org = "Michigan State University", dep_date = "2021-05-10")
  expect_equal(x$ownership, "own_this")
  # taken from elsewhere, and only the institute is shared: others'
  y <- own(source = "text", repo_url = "https://www.ebi.ac.uk/ena/browser/view/PRJEB14199", repo_type = "ena",
           context = "Additional sequences derived from other publications are deposited in the ENA (PRJEB14199).",
           dep_type = "Dataset", dep_org = "Michigan State University", dep_date = "2021-05-10")
  expect_equal(y$ownership_group, "others")
  # a record with people: they decide, not the institute it shares with the paper
  z <- own(source = "text", repo_url = "https://www.ncbi.nlm.nih.gov/geo/query/acc.cgi?acc=GSE157827", repo_type = "geo",
           context = "The single-nucleus data were downloaded from GEO (GSE157827).",
           dep_type = "Dataset", dep_org = "Michigan State University", dep_creators = "Nancy Ip; Shun-Fat Lau",
           dep_date = "2020-09-11")
  expect_equal(z$ownership, "others_data")
})

test_that("seq_archive_info reads GEO, ENA and ArrayExpress records (#660)", {
  skip_if_not(online("www.ebi.ac.uk"))
  skip_if_not(online("www.ncbi.nlm.nih.gov"))
  skip_on_cran()
  i <- seq_archive_info(c("https://www.ncbi.nlm.nih.gov/geo/query/acc.cgi?acc=GSE149523",
                          "https://www.ebi.ac.uk/ena/browser/view/PRJNA770129",
                          "https://www.ebi.ac.uk/ena/browser/view/SRR7416431",
                          "https://www.ebi.ac.uk/biostudies/arrayexpress/studies/E-MTAB-6487"))
  expect_true(all(is.na(i$error)))
  expect_match(i$creators[1], "Walser", fixed = TRUE)
  expect_match(i$related[1], "IsSupplementTo:10.1371/journal.pbio.3001317", fixed = TRUE)
  expect_match(i$organisation[2], "National Institute of Biological Sciences", fixed = TRUE)
  expect_equal(i$study[3], "PRJNA477348")
  expect_match(i$creators[4], "Naldini", fixed = TRUE)
})

test_that("repo_check lists the authors' own archive deposits and does not check them (#660)", {
  skip_if_not(online("www.ebi.ac.uk"))
  skip_if_not(online("www.ncbi.nlm.nih.gov"))
  skip_on_cran()
  p <- test_paper(text = "All RNA-seq raw data generated in this study have been deposited to NCBI under BioProject ID PRJNA770129.")
  mo <- module_run(p, "repo_check")
  expect_equal(mo$summary_table$archive_own_n, 1L)
  expect_equal(mo$summary_table$repo_n, 0L)
  expect_equal(mo$traffic_light, "na")
  rep <- paste(unlist(mo$report), collapse = "\n")
  expect_match(rep, "#### Sequence and expression archives", fixed = TRUE)
  expect_match(rep, "67 runs", fixed = TRUE)
  expect_no_match(rep, "without READMEs", fixed = TRUE)
  # nothing of it in the file table the other modules read
  expect_false(any(grepl("PRJNA770129", mo$table$repo_url %||% character(0))))
})

test_that("code_check names the archive deposit when files the code reads are missing (#660)", {
  skip_if_not(online("www.ebi.ac.uk"))
  skip_if_not(online("www.ncbi.nlm.nih.gov"))
  skip_on_cran()
  p <- test_paper(text = "All RNA-seq raw data generated in this study have been deposited to NCBI under BioProject ID PRJNA770129.")
  mo <- module_run(p, "code_check", local_path = test_path("fixtures", "code_files"))
  rep <- paste(unlist(mo$report), collapse = " ")
  expect_match(rep, "The paper deposited raw data in SRA / BioProject: PRJNA770129. If some of these files",
               fixed = TRUE)
  # without an archive deposit, no such sentence
  q <- module_run(test_paper(), "code_check", local_path = test_path("fixtures", "code_files"))
  expect_no_match(paste(unlist(q$report), collapse = " "), "deposited raw data", fixed = TRUE)
})
