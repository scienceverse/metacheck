# Whose deposit is each link? (#634; R/repo-ownership.R). Offline: every
# test builds the link's text and the deposit's record by hand.

pf <- list(title = "Memory for faces in older adults and their caregivers",
           doi = "10.1234/abcd.5678", family = c("Smith", "Wonder", "Okafor"),
           full = c("Anna Smith", "Ben Wonder", "Chidi Okafor"),
           orcids = "0000-0002-1825-0097",
           affiliations = c("Allen Institute for Neural Dynamics", "University of Glasgow"),
           year = 2023, date = as.Date("2023-07-01"), abstract = "")

own1 <- function(...) .repo_ownership(data.frame(..., stringsAsFactors = FALSE), pf)

test_that("a preprint server's record is a publication, whatever the sentence says (#634)", {
  x <- own1(source = "reference", repo_url = "osf.io/v7xpg", repo_type = "osf",
            ref_text = "Beffara, B. (2019). A blind protocol for sequential analyses. PsyArXiv. https://doi.org/10.31234/osf.io/v7xpg",
            citing = "We used custom-written scripts (Beffara, 2019).")
  expect_equal(x$ownership, "not_data")
  expect_equal(x$ownership_group, "not_data")
  expect_true(.own_preprint_link("osf.io/v7xpg", x$ref_text))
  expect_true(.own_preprint_link("https://doi.org/10.1101/2020.01.01.123456"))
  expect_false(.own_preprint_link("https://osf.io/abcde", "Smith (2020). Data. OSF."))
})

test_that("a deposit that names the paper's DOI is the paper's own (#634)", {
  x <- own1(source = "text", repo_url = "https://doi.org/10.5281/zenodo.1", repo_type = "zenodo",
            context = "Data are at https://doi.org/10.5281/zenodo.1.",
            dep_related = "isSupplementTo:https://doi.org/10.1234/abcd.5678", listed = TRUE, n_files = 4)
  expect_equal(x$ownership, "own_this")
})

test_that("a deposit of another paper, by others, is others' data (#634)", {
  x <- own1(source = "text", repo_url = "https://doi.org/10.5061/dryad.x", repo_type = "dryad",
            context = "We reanalysed the data of a published study (https://doi.org/10.5061/dryad.x).",
            dep_creators = "Maria Garcia; Tom Lee", dep_related = "primary_article:https://doi.org/10.9999/other.1",
            dep_type = "Dataset", listed = TRUE, n_files = 5, has_data = TRUE)
  expect_equal(x$ownership, "others_data")
  expect_equal(x$ownership_group, "others")
})

test_that("a widely used tool by others is others' code; one under the authors' lab account is theirs (#634)", {
  x <- own1(source = "text", repo_url = "https://github.com/crsh/papaja", repo_type = "github",
            context = "Analyses used papaja (https://github.com/crsh/papaja).", gh_stars = "400", listed = TRUE, n_files = 50)
  expect_equal(x$ownership, "others_code")
  y <- own1(source = "text", repo_url = "https://github.com/WonderLab/facemem", repo_type = "github",
            context = "Code is at https://github.com/WonderLab/facemem.", gh_stars = "400", listed = TRUE, n_files = 5)
  expect_equal(y$ownership_group, "own")
  z <- own1(source = "text", repo_url = "https://github.com/AllenNeuralDynamics/voxel", repo_type = "github",
            context = "Hardware was controlled with a library (https://github.com/AllenNeuralDynamics/voxel).",
            gh_stars = "400", listed = TRUE, n_files = 50)
  expect_equal(z$ownership_group, "own")
})

test_that("data 'available from' the cited reference's creator are that creator's (#634)", {
  x <- own1(source = "reference", repo_url = "https://doi.org/10.5281/zenodo.2", repo_type = "zenodo",
            ref_text = "Robert Koch-Institut. 7-Tage-Inzidenz der COVID-19-Faelle. Zenodo https://doi.org/10.5281/zenodo.2 (2024).",
            ref_year = "2024", citing = "The incidence data are available from the Robert Koch Institute [26].",
            dep_creators = "Robert Koch-Institut", dep_type = "Dataset", dep_date = "2024-08-01", listed = TRUE, n_files = 4)
  expect_equal(x$ownership, "others_data")
  # the same sentence without naming the source is the paper's own statement
  y <- own1(source = "reference", repo_url = "https://doi.org/10.5281/zenodo.3", repo_type = "zenodo",
            ref_text = "Smith, A. Data for: memory for faces. Zenodo https://doi.org/10.5281/zenodo.3 (2023).",
            ref_authors = "Smith, A", ref_year = "2023", citing = "All data are available on Zenodo [12].",
            dep_type = "Dataset", listed = TRUE, n_files = 4)
  expect_equal(y$ownership, "own_this")
})

test_that("a paper without a title or DOI can be classified (#634)", {
  # repo_check looks up reference deposits through a paper of links only,
  # whose info table has no rows
  p <- paper("p1")
  p$info <- data.frame(title = character(0), doi = character(0))
  f <- .own_paper_facts(p)
  expect_true(is.na(f$title))
  expect_true(is.na(f$doi))
  x <- .repo_ownership(data.frame(source = "text", repo_url = "https://doi.org/10.5281/zenodo.1",
                                  repo_type = "zenodo", context = "Data are on Zenodo.",
                                  dep_related = "isSupplementTo:10.1/x"), f)
  expect_equal(x$ownership_group, "own")
})

test_that("an empty table gets the ownership columns (#634)", {
  x <- .repo_ownership(data.frame(source = character(0), repo_url = character(0), repo_type = character(0)), pf)
  expect_true(all(c("ownership", "ownership_group", "ownership_reason") %in% names(x)))
  expect_equal(nrow(x), 0)
})

test_that(".own_dep_type reads every host's record type alike (#634)", {
  expect_equal(.own_dep_type(c("zenodo", "zenodo", "zenodo", "figshare", "figshare", "dryad", "github"),
                             c("publication", "software", "dataset", "journal contribution", "dataset", NA, NA),
                             c("preprint", NA, NA, NA, NA, NA, NA)),
               c("Preprint", "Software", "Dataset", "JournalArticle", "Dataset", "Dataset", NA))
})

test_that(".meta_related writes a deposit's related works as relation:identifier (#634)", {
  expect_equal(.meta_related(list(list(relation = "isSupplementTo", identifier = "10.1/a"),
                                  list(relation = "cites", identifier = "")), "relation", "identifier"),
               "isSupplementTo:10.1/a")
  expect_true(is.na(.meta_related(NULL, "relation", "identifier")))
})

test_that(".own_facts counts a deposit's files and reads its record (#634)", {
  links <- data.frame(paper_id = "p1", repo_url = c("https://osf.io/abcde", "https://github.com/a/b"),
                      repo_type = c("osf", "github"))
  meta <- data.frame(repo_url = "https://github.com/a/b", title = "a/b", creators = "a",
                     published = "2020-01-02", stars = 3L, forks = 0L, readme = "# b\ninstall.packages('b')")
  files <- data.frame(repo_url = "https://github.com/a/b", file_name = c("DESCRIPTION", "x.R"),
                      file_path = c("DESCRIPTION", "R/x.R"), data_type = c("other", "code"))
  od <- data.frame(repo_url = "https://osf.io/abcde", title = "Faces", category = "project",
                   osf_type = "nodes", creators = "Anna Smith", published = "2022-05-01")
  p <- paper("p1")
  x <- .own_facts(p, links, meta, files, od)
  expect_equal(x$n_files, c(0L, 2L))
  expect_equal(x$pkg_layout, c(FALSE, TRUE))
  expect_equal(x$has_code, c(FALSE, TRUE))
  expect_equal(x$osf_contributors, c("Anna Smith", NA))
  # a code host's owner is not counted as the deposit's creators
  expect_true(is.na(x$dep_creators[2]))
  expect_equal(x$dep_date, c("2022-05-01", "2020-01-02"))
})

test_that(".repo_readme_from_listing reads the top-level README, README.md first (#634)", {
  asked <- character(0)
  local_mocked_bindings(.repo_readme_text = function(url, n = 20000) { asked <<- c(asked, url); "text" })
  files <- data.frame(path = c("docs/README.md", "README.txt", "README.md", "R/x.R"),
                      download_url = c("u/docs", "u/txt", "u/md", "u/x"))
  expect_equal(.repo_readme_from_listing(files), "text")
  expect_equal(asked, "u/md")
  # GitLab's listing has the same columns; a repository without a README,
  # or without a listing, asks nothing
  expect_true(is.na(.repo_readme_from_listing(files[c(1, 4), ])))
  expect_true(is.na(.repo_readme_from_listing(NULL)))
  expect_equal(asked, "u/md")
})

test_that(".own_reference_links finds deposits cited only in the reference list (#634)", {
  p <- paper("p1")
  p$text <- data.frame(
    text = c("We used the data of Lee (2020).", "Our code is at https://github.com/a/b.",
             "Lee, T. (2020). Survey data. Zenodo. https://doi.org/10.5281/zenodo.123"),
    text_id = 1:3, paragraph_id = 1:3, section_id = c(1L, 1L, 2L), paper_id = "p1")
  p$section <- data.frame(section_id = 1:2, header = c("Methods", "References"),
                          section_type = c("method", "references"))
  p$bib <- data.frame(bib_id = "b1", authors = "Lee, T", year = "2020", title = "Survey data",
                      doi = NA_character_, text_id = 3L)
  p$xref <- data.frame(xref_id = "b1", xref_type = "bibr", contents = "Lee (2020)", text_id = 1L)
  r <- .own_reference_links(p, exclude = "https://github.com/a/b")
  expect_equal(nrow(r), 1)
  expect_equal(r$repo_type, "zenodo")
  expect_equal(r$citing, "We used the data of Lee (2020).")
  expect_equal(r$ref_authors, "Lee, T")
})

test_that(".own_dep_type reads InvenioRDM's combined types (#634, #636)", {
  expect_equal(.own_dep_type(c("zenodo", "zenodo", "zenodo"), c("publication-article", "dataset", "image-figure")),
               c("JournalArticle", "Dataset", "Image"))
})

test_that("a link repo_check ignored as a publication is not a deposit (#634, #637)", {
  out <- own1(source = c("text", "text"), repo_url = c("https://repo.example/handle/1/2", "https://osf.io/abcde"),
              repo_type = c("dspace7", "osf"), context = c("Our earlier paper.", "Data are at osf.io/abcde."))
  m <- .own_mark_publications(out, "https://repo.example/handle/1/2")
  expect_equal(m$ownership, c("not_data", out$ownership[2]))
  expect_equal(m$ownership_reason[1], .own_pub_reason)
  expect_equal(.own_mark_publications(out, character(0)), out)
})

test_that(".own_reference_links finds deposits with repo_check's general finders (#636, #637)", local({
  testthat::local_mocked_bindings(.repo_doi_records = function(doi) data.frame(
    doi = tolower(doi), url = "https://datacompass.lshtm.ac.uk/id/eprint/409", resource_type = "Dataset", in_datacite = TRUE))
  p <- paper("p1")
  p$text <- data.frame(
    text = c("We used survey data (Lee, 2020).",
             "Lee, T. (2020). Survey data. LSHTM Data Compass. https://doi.org/10.17037/DATA.174"),
    text_id = 1:2, paragraph_id = 1:2, section_id = c(1L, 2L), paper_id = "p1")
  p$section <- data.frame(section_id = 1:2, header = c("Methods", "References"), section_type = c("method", "references"))
  p$bib <- data.frame(bib_id = "b1", authors = "Lee, T", year = "2020", title = "Survey data", doi = NA_character_, text_id = 2L)
  p$xref <- data.frame(xref_id = "b1", xref_type = "bibr", contents = "Lee, 2020", text_id = 1L)
  r <- .own_reference_links(p)
  expect_equal(r$repo_type, "reshare")
  expect_equal(r$repo_url, "https://datacompass.lshtm.ac.uk/id/eprint/409")
}))
