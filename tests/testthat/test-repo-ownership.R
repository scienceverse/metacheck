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

test_that("a deposit's DOI written in a reference and given as its link is one deposit (#644)", {
  expect_equal(.own_key(c("10.6084/m9.figshare.29094266", "https://doi.org/10.6084/M9.figshare.29094266",
                          "doi.org/10.6084/m9.figshare.29094266/")),
               rep(.own_key("10.6084/m9.figshare.29094266"), 3))
  expect_false(.own_key("10.6084/m9.figshare.29094266") == .own_key("10.6084/m9.figshare.c.8754103"))
  p <- paper("p1")
  p$text <- data.frame(
    text = c("All scripts are available from Figshare [74].",
             "Yang S. 2025 IGPC. Figshare. (doi:10.6084/m9.figshare.29094266)"),
    text_id = 1:2, paragraph_id = 1:2, section_id = c(1L, 2L), paper_id = "p1")
  p$section <- data.frame(section_id = 1:2, header = c("Data accessibility", "References"),
                          section_type = c("availability", "references"))
  p$bib <- data.frame(bib_id = "b74", authors = "Yang, S", year = "2025", title = "",
                      doi = "10.6084/m9.figshare.29094266", text_id = 2L)
  p$xref <- data.frame(xref_id = "b74", xref_type = "bibr", contents = "[74]", text_id = 1L)
  r <- .own_reference_links(p)
  expect_equal(r$repo_url, "https://doi.org/10.6084/m9.figshare.29094266")
  # the lookup's record, under either form, marks the same row
  out <- own1(source = "reference", repo_url = r$repo_url, repo_type = "figshare",
              context = NA_character_)
  expect_equal(.own_mark_publications(out, "10.6084/m9.figshare.29094266")$ownership, "not_data")
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

test_that("a reference whose link gave nothing is a publication only without a deposit's marks (#634)", {
  # written as a deposit (a repository's name, a version): its authors decide
  x <- own1(source = "reference", repo_url = "https://github.com/smithlab/facetool", repo_type = "github",
            ref_text = "Smith, A. & Lee, T. Facetool: a platform for face stimuli. GitHub repository, smithlab/facetool: v0.0.1 (2023).",
            ref_authors = "Smith, A; Lee, T", ref_year = "2023")
  expect_equal(x$ownership_group, "own")
  y <- own1(source = "reference", repo_url = "https://swh.example/x", repo_type = "ckan",
            ref_text = "Smith, A. 2023. Macroeco. swh:1:rev:3d0e. Software Heritage.",
            ref_authors = "Smith, A", ref_year = "2023")
  expect_equal(y$ownership_group, "own")
  # an article's reference without such a mark is still a publication
  z <- own1(source = "reference", repo_url = "https://data.example.org/dataset/x", repo_type = "ckan",
            ref_text = "Smith, A. (2023). Faces in memory. Journal of Memory, 12(3), 45-60.",
            ref_authors = "Smith, A", ref_year = "2023")
  expect_equal(z$ownership, "not_data")
  # the link itself is no mark: every OSF link names the OSF (osf.io/5qkm3,
  # a preprint's file)
  w <- own1(source = "reference", repo_url = "https://osf.io/5qkm3", repo_type = "osf",
            ref_text = "Smith, A. (2018). Word segmentation from text. Preprint. Retrieved from https://osf.io/5qkm3/",
            ref_authors = "Smith, A", ref_year = "2018")
  expect_equal(w$ownership, "not_data")
  # an OSF project is a deposit whatever its category (osf.io/4g7fv,
  # "instrumentation")
  v <- own1(source = "reference", repo_url = "https://osf.io/4g7fv", repo_type = "osf",
            ref_text = "Smith, A. (2016). Competitive reaction time task. https://doi.org/10.17605/OSF.IO/4G7FV",
            ref_authors = "Smith, A", ref_year = "2016", osf_type = "nodes", osf_category = "instrumentation")
  expect_equal(v$ownership_group, "own")
})

test_that("the paper's own data statement in the reference list: 'data is located in' (#634)", {
  x <- own1(source = "reference", repo_url = "https://doi.org/10.5061/dryad.abc", repo_type = "dryad",
            ref_text = "data is located in a permanent Dryad repository https://doi.org/10.5061/dryad.abc.",
            dep_type = "Dataset", listed = FALSE, n_files = 0)
  expect_equal(x$ownership, "own_this")
  expect_equal(x$ownership_reason, "the reference is the paper's own data statement")
})

test_that("the name a GitHub account gives itself settles whose repository it is (#634)", {
  old <- list(source = "text", repo_url = "https://github.com/AnWier/nanotool", repo_type = "github",
              context = "See more information in https://github.com/AnWier/nanotool.",
              dep_date = "2018-01-01", listed = TRUE, n_files = 40, has_code = TRUE)
  # an account login that names no author: an old repository by others
  expect_equal(do.call(own1, old)$ownership, "others_code")
  # the account's name is a paper author's
  expect_equal(do.call(own1, c(old, gh_owner_name = "Ben Wonder"))$ownership, "own_earlier_code")
  # or a lab named after paper authors
  expect_equal(do.call(own1, c(old, gh_owner_name = "Smith & Okafor Computational Lab"))$ownership_group, "own")
  # a name that is no author's leaves it others'
  expect_equal(do.call(own1, c(old, gh_owner_name = "Maria Garcia"))$ownership, "others_code")
})

test_that(".github_owner_names asks once per account, and keeps no failed answer (#634)", local({
  rm(list = ls(.github_owner_memo), envir = .github_owner_memo)
  withr::defer(rm(list = ls(.github_owner_memo), envir = .github_owner_memo))
  asked <- character(0)
  local_mocked_bindings(req_perform = function(req, ...) {
    asked <<- c(asked, req$url)
    status <- if (grepl("down", req$url)) 503L else if (grepl("gone", req$url)) 404L else 200L
    body <- if (grepl("nameless", req$url)) '{"login":"nameless","name":null}' else '{"login":"x","name":"Anna Wierczeiko"}'
    httr2::response(status_code = status, url = req$url, headers = list(`Content-Type` = "application/json"),
                    body = charToRaw(body))
  }, .package = "httr2")
  got <- .github_owner_names(c("AnWiercze", "anwiercze", NA, "nameless", "gone", "down"))
  expect_equal(got, c("Anna Wierczeiko", "Anna Wierczeiko", NA, NA, NA, NA))
  # one request per account (logins are not case sensitive)
  expect_equal(sum(grepl("users/AnWiercze$", asked)), 1)
  expect_false(any(grepl("users/anwiercze$", asked)))
  # a failed request is asked again; a settled answer is not
  .github_owner_names(c("AnWiercze", "gone", "down"))
  expect_equal(sum(grepl("users/down$", asked)), 2)
  expect_equal(sum(grepl("users/gone$", asked)), 1)
}))

test_that(".own_paper_facts takes the authors from the text when the author table is empty (#634)", {
  p <- paper("p1")
  p$info <- data.frame(title = "Selecting fitted models under epistemic uncertainty", doi = NA_character_)
  p$text <- data.frame(
    text = c("Selecting fitted models under epistemic uncertainty Alexandre René 1,2,3 & André Longtin 3, 4, 5 Fitting models to data is important.",
             "Correspondence and requests for materials should be addressed to Maria Okafor.",
             "We thank the reviewers."),
    text_id = 1:3, paragraph_id = 1:3, section_id = 1L, paper_id = "p1")
  pf2 <- .own_paper_facts(p)
  expect_setequal(pf2$family, c("René", "Longtin", "Okafor"))
  expect_setequal(pf2$full, c("Alexandre René", "André Longtin", "Maria Okafor"))
  # no names in the text: none made up
  p$text$text <- c("Selecting fitted models under epistemic uncertainty Fitting models to data is important.",
                   "Correspondence should be addressed to the corresponding author.", "We thank the reviewers.")
  expect_length(.own_paper_facts(p)$family, 0)
})

test_that(".own_reference_links gives a GitHub name written alone its address (#634)", {
  p <- paper("p1")
  p$text <- data.frame(
    text = c("We used Prop3D [1].",
             "Draizen, E. J. Prop3D: a platform for protein structures. GitHub repository, bouralab/Prop3D: v0.0.1 (2024)."),
    text_id = 1:2, paragraph_id = 1:2, section_id = c(1L, 2L), paper_id = "p1")
  p$section <- data.frame(section_id = 1:2, header = c("Methods", "References"), section_type = c("method", "references"))
  p$bib <- data.frame(bib_id = "b1", authors = "Draizen, E J", year = "2024", title = "Prop3D", doi = NA_character_, text_id = 2L)
  p$xref <- data.frame(xref_id = "b1", xref_type = "bibr", contents = "1", text_id = 1L)
  r <- .own_reference_links(p)
  expect_equal(r$repo_url, "https://github.com/bouralab/Prop3D")
  # the same repository linked in the text is not looked up twice
  expect_equal(nrow(.own_reference_links(p, exclude = "http://github.com/bouralab/Prop3D")), 0)
})

test_that("a family name with a particle matches only that family name, as a whole word (#682)", {
  # the examples of the issue: "van" alone, or "lissa" inside a given name
  expect_false(.own_names_author("Marcel A.L.M. van Assen; Melissa Lewis", family = "Van Lissa"))
  expect_false(.own_names_author("Melissa Lewis", family = "Van Lissa"))
  expect_false(.own_names_author("Marcel A.L.M. van Assen", family = "Van Lissa"))
  expect_false(.own_people_author("Marcel A.L.M. van Assen; Melissa Lewis", "Van Lissa"))
  expect_false(.own_people_author("Melissa Lewis", "Van Lissa"))
  # the family name itself, written either way
  expect_true(.own_people_author("Caspar J. van Lissa", "Van Lissa"))
  expect_true(.own_people_author("Van Lissa, C.; Lee, T.", "Van Lissa"))
  # an account name runs the name together
  expect_true(.own_names_author("cjvanlissa", "Van Lissa", inside = TRUE))
  expect_true(.own_names_author("rbmvanaert", "Van Aert", inside = TRUE))
  # Grobid's family name can hold the given name, or noise
  expect_true(.own_people_author("Heene, M.", "Moritz Heene"))
  expect_true(.own_people_author("Hecker, L.", "Hecker -Orcid"))
  expect_true(.own_people_author("Przybylski, A.", "Przybyl- Ski"))
  expect_equal(.own_fam_keys(c("Van 't Veer", "De Bruin", "Van Der")), c("veer", "bruin", "vander"))
  # a double family name matches either part, but not a given name
  expect_true(.own_people_author("Gabriel García Márquez", "García Márquez"))
  expect_true(.own_people_author("García, G.", "García Márquez"))
  expect_false(.own_people_author("García Lopez", "García Márquez"))
  # a family name followed by initials
  expect_true(.own_people_author("Smith AB; Lee C", "Smith"))
  # the OSF's own family names, where it gives them
  expect_false(.own_people_author("Lissa Brown", "Van Lissa", families = "Brown"))
  # a README may run names together, so a name of five letters or more can
  # be part of a word there ("Chang" in "Changes"), which is why only its
  # opening is read; the name an account gives itself is read word by word
  expect_true(.own_names_author("Changes made", "Chang", inside = TRUE))
  expect_false(.own_names_author("Changes made", "Chang"))
  expect_true(is.na(.own_people_author(NA_character_, "Smith")))
})

test_that("an old OSF project with many contributors, none an author, is others' data (#682)", {
  pf2 <- pf
  pf2$family <- c("Hoijtink", "De Bruin", "Duken", "Flores", "Frankenhuis", "Van Lissa")
  pf2$full <- c("Herbert Hoijtink", "Caspar J. Van Lissa")
  pf2$year <- 2025; pf2$date <- as.Date("2025-07-01")
  ten <- paste("Christopher Jon Anderson; Joanna Anderson; Marcel A.L.M. van Assen; Peter Raymond Attridge;",
               "Angela Attwood; Jordan Axt; Molly Babel; Melissa Lewis; Erica Baranski; Michael Barnett-Cowan")
  row <- list(source = "text", repo_url = "https://osf.io/ezcuj", repo_type = "osf",
              context = "The Reproducibility Project (https://osf.io/ezcuj) replicated 100 studies.",
              osf_type = "nodes", osf_category = "project", dep_date = "2012-10-08", dep_creators = ten,
              osf_contributors = ten, listed = TRUE, n_files = 1756, has_data = TRUE, has_code = TRUE)
  rpp <- function(...)
    .repo_ownership(as.data.frame(utils::modifyList(row, list(...)), stringsAsFactors = FALSE), pf2)
  x <- rpp(dep_n_creators = "270",
           dep_creator_families = "Anderson; van Assen; Attridge; Attwood; Axt; Babel; Lewis; Baranski; Barnett-Cowan")
  expect_equal(x$ownership, "others_data")
  expect_equal(x$ownership_reason, "an OSF project more than 5 years older than the paper, without a paper author")
  # a record stored before the OSF's count was kept: the same
  expect_equal(rpp()$ownership, "others_data")
  # a recent project whose list of contributors was cut short: the 10 it
  # names do not show that the authors are not among the others
  y <- rpp(dep_date = "2025-01-01", dep_n_creators = "30",
           context = "Materials are at https://osf.io/ezcuj.")
  expect_equal(y$ownership, "own_this")
  # the whole list, without an author: made by others
  z <- rpp(dep_date = "2025-01-01", dep_n_creators = "10",
           context = "Materials are at https://osf.io/ezcuj.")
  expect_equal(z$ownership, "others_data")
  # an author among the 10 named settles it
  w <- rpp(dep_n_creators = "270", osf_contributors = paste(ten, "; Caspar J. van Lissa"),
           dep_creators = paste(ten, "; Caspar J. van Lissa"))
  expect_equal(w$ownership_group, "own")
})

test_that(".own_facts keeps the OSF's family names and number of contributors (#682)", {
  links <- data.frame(paper_id = "p1", repo_url = "https://osf.io/abcde", repo_type = "osf")
  od <- data.frame(repo_url = "https://osf.io/abcde", title = "Faces", category = "project", osf_type = "nodes",
                   creators = "Anna van Smith", creator_families = "van Smith", n_creators = "12",
                   published = "2022-05-01")
  x <- .own_facts(paper("p1"), links, NULL, NULL, od)
  expect_equal(x$dep_creator_families, "van Smith")
  expect_equal(x$dep_n_creators, "12")
})
