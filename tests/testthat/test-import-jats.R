fmt <- function(name) test_path("fixtures", "formats", name)

test_that(".xml_format", {
  expect_equal(.xml_format(demofile("xml"))$format, "tei")
  expect_equal(.xml_format(fmt("apa.xml"))$format, "jats")
  expect_equal(.xml_format(fmt("published.cermine.xml"))$format, "jats")
  expect_equal(.xml_format(fmt("jats_cases.xml"))$format, "jats")
  expect_match(.xml_format(fmt("jats_cases.xml"))$doctype, "JATS")
  expect_equal(.xml_format(fmt("elsevier_cases.xml"))$format, "elsevier")
  expect_equal(.xml_format(fmt("wiley_cases.xml"))$format, "wiley")

  other <- withr::local_tempfile(fileext = ".xml")
  writeLines("<?xml version=\"1.0\"?><!-- <article> --><dataset><a/></dataset>", other)
  f <- .xml_format(other)
  expect_equal(f$format, "unknown")
  expect_equal(f$root, "dataset")
  expect_error(.read_xml_fulltext(other), "first element is <dataset>")
  expect_warning(read(other), "not an XML format metacheck reads")
})

test_that("read - JATS", {
  p <- read(fmt("jats_cases.xml"))
  expect_true(.is_paper(p))
  expect_true(paper_validate(p))
  expect_equal(p$paper_id, "jats_cases")

  # metadata, from the front matter
  info <- p$info
  # a named entity (&mdash;) is kept; the subtitle is part of the title
  expect_equal(info$title, "Reading publisher XML\u2014a test: Cases from many publishers")
  expect_equal(info$doi, "10.1234/jtc.2026.001")
  expect_equal(info$journal, "Journal of Test Cases")
  expect_equal(c(info$volume, info$issue, info$first_page, info$last_page),
               c("12", "2", "101", "120"))
  expect_equal(info$published_date, "2026-03-05")
  expect_equal(info$pmid, "12345678")
  expect_equal(info$license_url, "https://creativecommons.org/licenses/by/4.0/")
  expect_equal(info$input_format, "jats")
  expect_equal(info$file_name, "jats_cases.xml")
  expect_equal(unlist(info$keywords), c("JATS", "testing"))
  # the abstract, not the teaser
  expect_equal(info$abstract, "We tested whether p values are read.")

  # statements: in the front matter, the author notes, and a top-level
  # section whose whole heading is a statement heading
  expect_match(info$funding_statement, "Test Foundation")
  expect_match(info$data_availability, "osf.io/abcde")
  expect_match(info$coi_statement, "no competing interests")
  types <- p$section$section_type
  expect_true(all(c("funding", "data_availability", "coi", "author_contributions",
                    "acknowledgment", "references") %in% types))
  # "Funding of Public Goods" is a section of the paper, not a statement
  pg <- p$section$section_id[p$section$header %in% "Funding of Public Goods"]
  expect_false(p$section$section_type[pg] == "funding")

  # authors: affiliations by a link and (APA) by the author's own rid; the
  # corresponding author's e-mail from the author notes; no editors
  expect_equal(p$author$family, c("Smith", "Jones"))
  expect_equal(p$author$email[1], "anna.smith@example.org")
  expect_equal(p$author$orcid[1], "https://orcid.org/0000-0002-1825-0097")
  expect_equal(nrow(p$affiliation), 2)
  expect_match(p$affiliation$text[1], "^Department of Psychology, Test University, Netherlands$")

  # sections: nested sections are flat; a heading-only section is kept
  # without a text row; "1." is written as "1"
  expect_true(all(c("1 Method", "Participants", "Study 1", "Results") %in% p$section$header))
  expect_false(any(grepl("^>|<head", p$text$text)))
  # text outside any section is the introduction
  expect_equal(p$section$section_type[is.na(p$section$header)], "intro")

  # text: an exponent keeps its ^; a list's items are paragraphs
  expect_true(any(grepl("10^\u22125", p$text$text, fixed = TRUE)))
  expect_true("A second point." %in% p$text$text)

  # cross-references point to their targets; several targets are rows
  x <- p$xref
  expect_equal(sum(x$xref_type == "bib"), 6)
  expect_true(all(!is.na(x$target_id)))
  expect_equal(sort(x$target_id[x$contents == "Smith, 2020; Jones et al., 2019"]), 1:2)
  # "[1-3]", linked at 1 and 3, also cites reference 2
  bib <- x[x$xref_type == "bib", ]
  expect_equal(sort(bib$target_id[bib$contents %in% c("1", "3")]), 1:3)
  expect_true(all(c("table", "figure", "foot") %in% x$xref_type))

  # links: a DOI link gets its address, a link's own text is kept
  expect_true("https://doi.org/10.17605/OSF.IO/XYZ12" %in% p$url$href)
  expect_true(all(c("https://osf.io/abcde/", "https://test.example.org/") %in% p$url$href))

  # references: mixed and element citations; the id on the citation (APA);
  # the year of a reference printed as text only
  b <- p$bib
  expect_equal(nrow(b), 3)
  expect_equal(b$doi[1], "10.1234/tj.2020.1")
  expect_equal(c(b$volume[1], b$issue[1], b$first_page[1], b$last_page[1]),
               c("3", "1", "1", "10"))
  expect_equal(b$bib_type[1:2], c("journal_article", "book"))
  expect_equal(b$authors[2], "Jones, C; Brown, D")
  expect_equal(b$year, c(2020L, 2019L, 2018L))
  expect_equal(b$doi[3], "10.1234/sj.2018.4")
  # the reference text keeps its spaces: the PDF repair "1. 5" -> "1.5" is
  # not applied to publisher text
  expect_true(any(grepl("1\u201310. 10.1234/tj.2020.1", p$text$text, fixed = TRUE)))

  # floats: the table's row and column spans are filled in; its footnote
  # joins the caption; the reviewer's figure is not the article's
  expect_equal(nrow(p$table), 1)
  # the header rows the file marks (thead), joined per column; a cell that
  # spans both header rows is named once
  expect_equal(p$table$contents[[1]][[1]], c("Group", "Age / M", "Age / SD"))
  expect_equal(p$table$header_rows, 1L)
  expect_equal(p$table$contents[[1]][[3]], c("Old", "68.0", "5.4"))
  expect_match(p$table$caption, "^Table 1 Participants a One value was missing\\.$")
  expect_equal(nrow(p$figure), 1)
  expect_equal(p$figure$caption, "Figure 1 Mean scores. Error bars show 95% CIs.")
  expect_equal(nrow(p$footnote), 1)
  expect_false(any(grepl("reviewer", p$text$text)))

  # statistics
  expect_true(all(c("t", "p") %in% p$eq$lhs))

  expect_equal(p$extraction$producer$name, "jats")
  expect_equal(p$extraction$producer$version, "1.3")
  codes <- vapply(p$extraction$warnings, `[[`, "", "code")
  expect_false("METACHECK_SOURCE_IS_TEI" %in% codes)
})

test_that("read - JATS variants: APA and CERMINE", {
  # APA's Journal Archive DTD: ids on the citations, affiliations by the
  # author's rid, figures labelled with a number only
  p <- read(fmt("apa.xml"))
  expect_true(paper_validate(p))
  expect_equal(p$info$doi, "10.1037/emo0001047")
  expect_equal(nrow(p$bib), 78)
  expect_gt(nrow(p$xref), 100)
  expect_true(all(!is.na(p$xref$target_id[p$xref$xref_type == "bib"])))
  expect_equal(nrow(p$affiliation), 2)
  expect_match(p$figure$caption[1], "^Figure 1 ")

  # CERMINE writes JATS without a DOCTYPE
  p <- read(fmt("published.cermine.xml"))
  expect_true(paper_validate(p))
  expect_match(p$info$title, "Inferring Benign and Malicious Envy")
  expect_gt(nrow(p$bib), 50)
})

test_that("read - Elsevier", {
  p <- read(fmt("elsevier_cases.xml"))
  expect_true(.is_paper(p))
  expect_true(paper_validate(p))

  info <- p$info
  expect_equal(info$title, "An Elsevier test article")
  expect_equal(info$doi, "10.1016/j.test.2026.100001")
  expect_equal(info$journal, "Journal of Elsevier Tests")
  expect_equal(info$issn, "1234-5678")
  expect_equal(info$published_date, "2026-06-30")
  expect_equal(info$pmid, "87654321")
  expect_equal(info$input_format, "xml")
  # the author abstract, not the highlights
  expect_equal(info$abstract, "We test reading Elsevier XML.")
  expect_equal(unlist(info$keywords), c("Elsevier", "XML"))
  expect_match(info$data_availability, "osf.io/fghij")
  expect_match(info$coi_statement, "no competing interests")
  expect_match(info$funding_statement, "Example Council")

  # both authors of the group have its affiliation
  expect_equal(p$author$family, c("Lee", "Moss"))
  expect_equal(p$author$email[1], "dana.lee@example.org")
  expect_equal(p$affiliation$author_ids[[1]], 1:2)

  expect_true(all(c("1 Introduction", "2 Method", "2.1 Participants") %in% p$section$header))
  expect_true(any(grepl("t(48) = 2.10, p = .041", p$text$text, fixed = TRUE)))

  x <- p$xref
  expect_equal(sort(x$target_id[x$xref_type == "bib"]), c(1L, 1L, 2L))
  expect_true(all(c("table", "figure") %in% x$xref_type))
  expect_equal(p$url$href, "https://osf.io/fghij/")

  # references: a journal article, a chapter in an edited book, a reference
  # given as text; the printed text is the reference text
  b <- p$bib
  expect_equal(b$bib_type[1:2], c("journal_article", "book_chapter"))
  expect_equal(b$doi[1], "10.1234/ej.2020.10")
  expect_equal(b$container[2], "The Edited Book")
  expect_equal(b$editors[2], "Editor, F.")
  expect_equal(b$year, c(2020L, 2021L, 1999L))
  expect_true("Lee, D., 2020. An earlier study. Earlier Journal 10 (2), 11\u201320." %in% p$text$text)

  # the CALS table's spans
  expect_equal(p$table$contents[[1]][[1]], c("Group", "Score / M", "Score / SD"))
  expect_match(p$table$caption, "Scores range from 1 to 5\\.$")
  expect_equal(nrow(p$footnote), 1)
  expect_equal(p$extraction$producer$name, "elsevier")
})

test_that("read - an Elsevier response without the article text", {
  f <- withr::local_tempfile(fileext = ".xml")
  writeLines(paste0(
    "<full-text-retrieval-response xmlns=\"http://www.elsevier.com/xml/svapi/article/dtd\" ",
    "xmlns:prism=\"http://prismstandard.org/namespaces/basic/2.0/\"><coredata>",
    "<prism:doi>10.1016/x</prism:doi></coredata></full-text-retrieval-response>"), f)
  expect_error(.read_xml_fulltext(f), "no article text")
})

test_that("read - Wiley", {
  p <- read(fmt("wiley_cases.xml"))
  expect_true(.is_paper(p))
  expect_true(paper_validate(p))

  info <- p$info
  expect_equal(info$title, "A test of reading Wiley XML")
  expect_equal(info$doi, "10.1111/tst.10101")
  expect_equal(info$journal, "Journal of Wiley Tests")
  expect_equal(info$issn, "1234-5678")
  expect_equal(c(info$volume, info$issue, info$first_page, info$last_page),
               c("12", "3", "301", "312"))
  expect_equal(info$published_date, "2026-02-01")
  expect_equal(info$license_url, "http://creativecommons.org/licenses/by/4.0/")
  expect_equal(info$language, "en")
  expect_equal(info$input_format, "xml")
  expect_equal(unlist(info$keywords), c("Wiley", "in situ testing"))
  # the main abstract (a list: its items are paragraphs), not the graphical one
  expect_equal(info$abstract, "We test whether Wiley XML is read. It is.")

  # statements: typed sections, the ones "Open Research" groups, a heading
  # with a typographic apostrophe, the funding note
  expect_match(info$data_availability, "Dryad")
  expect_match(info$coi_statement, "no conflict of interest")
  expect_match(info$funding_statement, "TRC-1")
  types <- p$section$section_type
  expect_true(all(c("intro", "method", "results", "acknowledgment", "coi",
                    "data_availability", "author_contributions", "funding",
                    "references") %in% types))
  # "contributed equally" is a note about the authors, not a statement
  expect_false(any(grepl("contributed equally", p$text$text)))

  # authors: affiliations by reference, the corresponding author's e-mail;
  # the editor is a contributor, named once although a note names her too
  expect_equal(p$author$family, c("Byron", "van der Berg"))
  expect_equal(p$author$email[1], "ada.byron@example.org")
  expect_equal(p$author$orcid[1], "https://orcid.org/0000-0002-1825-0097")
  expect_equal(p$affiliation$text[1], "Department of Testing, Test University, Eindhoven, The Netherlands")
  expect_equal(p$affiliation$author_ids[[1]], 1:2)
  # the second author's second affiliation is kept
  expect_equal(p$affiliation$text[2], "Other University, Bristol, UK")
  expect_equal(p$affiliation$author_ids[[2]], 2L)
  expect_equal(p$contributor$family, "Handler")
  expect_equal(p$contributor$role[[1]], "Editor")

  # text: program code keeps its lines apart; a hypothesis starts with its
  # name; a formula printed as an image leaves no text; MathML is read
  expect_true("x <- 1 y <- 2" %in% p$text$text)
  expect_true("H1 Tadpoles hide more when predators are near." %in% p$text$text)
  expect_false("1" %in% p$text$text)
  expect_true(any(p$eq$lhs == "F" & p$eq$rhs == "6.25"))
  expect_true(all(c("t", "p") %in% p$eq$lhs))

  # cross-references: Wiley links only the year of a citation; the citation
  # is its names and year, and the sentence keeps its spaces
  x <- p$xref
  expect_equal(x$target_id[x$xref_type == "bib"], 1:3)
  expect_equal(x$contents[x$xref_type == "bib"], c("Smith, 2020", "Jones & Lee, 2019", "Byron, 2026"))
  expect_true("Earlier work tested this (Smith, 2020; Jones & Lee, 2019), as Figure 1 shows." %in% p$text$text)
  expect_true(all(c("figure", "table") %in% x$xref_type))
  expect_equal(nrow(p$footnote), 1)
  expect_true(all(c("https://github.com/example/test", "https://doi.org/10.5061/dryad.test") %in% p$url$href))

  # references: a journal article, a book chapter with its editor, a data
  # citation whose DOI is printed with Wiley's hyphen (U+2010)
  b <- p$bib
  expect_equal(b$bib_type, c("journal_article", "book_chapter", "dataset"))
  expect_equal(b$doi[c(1, 3)], c("10.1234/tj.2020.1", "10.5061/dryad-test"))
  expect_equal(b$editors[2], "Editor, E.")
  expect_equal(b$container[2], "The Book of Tests")
  expect_equal(b$year, c(2020L, 2019L, 2026L))

  # floats: the CALS table's spans, its note in the caption
  expect_equal(p$table$contents[[1]][[1]], c("Treatment", "Hiding (s) / M", "Hiding (s) / SD"))
  expect_equal(p$table$caption, "Table 1 Hiding time by treatment Predator cue added.")
  expect_equal(p$figure$caption, "Figure 1 Mean hiding time of R. dalmatina per treatment.")

  expect_equal(p$extraction$producer$name, "wiley")
})

test_that("read - Wiley: what the file declares beyond the text (bibr 12.6)", {
  p <- read(fmt("wiley_cases.xml"))
  info <- p$info
  expect_equal(info$schema_version, "12.6")
  expect_equal(info$article_type, "research-article")
  expect_equal(c(info$received_date, info$revised_date, info$accepted_date),
               c("2025-09-01", "2025-12-15", "2026-01-10"))
  expect_equal(info$copyright_holder, "The Authors")
  expect_equal(info$copyright_year, 2026L)
  expect_equal(c(info$word_count, info$figure_count, info$table_count, info$page_count),
               c(4000L, 1L, 1L, 12L))
  expect_equal(p$subject$scheme, c("heading", "toc-heading", "subject"))
  expect_equal(p$subject$term, c("RESEARCH ARTICLE", "Test methods", "Test ecology"))
  # the peer review history: its address holds the article's DOI, which is
  # not the review's
  expect_equal(p$related$relation, "peer_review")
  expect_true(is.na(p$related$doi))
  expect_equal(p$review$url, "https://publons.com/publon/10.1111/tst.10101")
  expect_equal(p$supplement$file_name, "tst10101-sup-0001-Appendix.pdf")
  expect_equal(p$supplement$media_type, "application/pdf")
  expect_equal(p$supplement$caption, "Appendix S1. Extra analyses.")
  expect_equal(p$funding$funder, "Test Research Council")
  expect_equal(p$funding$funder_doi, "10.13039/501100000270")
  expect_equal(p$funding$award_ids[[1]], c("TRC-1", "TRC-2"))
})

test_that(".statement_heading", {
  expect_equal(.statement_heading("Declaration of Competing Interest"), "conflict")
  expect_equal(.statement_heading("CRediT authorship contribution statement"), "contribution")
  expect_equal(.statement_heading("AUTHORS' CONTRIBUTIONS"), "contribution")
  expect_equal(.statement_heading("Authors\u2019 contributions"), "contribution")
  expect_equal(.statement_heading("Data availability"), "availability")
  expect_equal(.statement_heading("Funding."), "funding")
  expect_true(is.na(.statement_heading("Funding of public goods")))
  expect_true(is.na(.statement_heading("Results")))
})

test_that("read - JATS: what the file declares beyond the text (bibr 12.6)", {
  p <- read(fmt("jats_cases.xml"))
  expect_equal(p$info$schema_version, "12.6")
  info <- p$info
  expect_equal(info$article_type, "research-article")
  expect_equal(c(info$received_date, info$revised_date, info$accepted_date),
               c("2025-10-01", "2026-01", "2026-02-02"))
  expect_equal(info$copyright_holder, "The Authors")
  expect_equal(info$copyright_year, 2026L)
  expect_equal(c(info$word_count, info$ref_count), c(2500L, 3L))
  # statements by their heading: a box, a footnote, a back section
  expect_equal(info$significance_statement, "This matters for everyone who reads XML.")
  expect_equal(info$open_practices_statement, "All data and materials are on OSF.")
  expect_equal(info$ai_statement, "No generative AI was used.")

  # subject terms, with an open-science badge
  expect_equal(p$subject$scheme, c("heading", "open-science-badge", "research-organism"))
  expect_equal(p$subject$code[2], "open-data")
  expect_equal(p$related$relation, "preprint")
  expect_equal(p$related$doi, "10.31234/osf.io/abcde")

  # editors and reviewers are contributors, not authors
  expect_equal(p$contributor$family, c("Editor", "Reviewer"))
  expect_equal(p$contributor$role[[1]], "Academic Editor")
  expect_false("Editor" %in% p$author$family)

  # peer reviews: their paragraphs are kept apart from the paper's text
  expect_equal(p$review$review_type, c("referee_report", "author_response"))
  expect_equal(p$review$doi[1], "10.1234/jtc.2026.001.r001")
  expect_equal(p$review$contributor_ids[[1]], 2L)
  expect_equal(p$review_text$text, c("The reviewer liked it.", "We thank the reviewer.",
                                     "We changed the analysis."))
  expect_false(any(grepl("reviewer liked", p$text$text)))
  # a translation of the article, printed as a sub-article, is not a review
  # and not part of the article's text
  expect_false(any(grepl("Texto traducido", c(p$review_text$text, p$text$text))))

  expect_equal(p$supplement$file_name, "s1_data.csv")
  expect_equal(p$supplement$media_type, "text/csv")
  expect_equal(p$supplement$doi, "10.1234/jtc.2026.001.s001")

  # funders with their registry DOI, award numbers and recipients
  expect_equal(p$funding$funder, "Test Foundation")
  expect_equal(p$funding$funder_doi, "10.13039/501100001659")
  expect_equal(p$funding$award_ids[[1]], "TF-123")
  expect_equal(p$funding$recipients[[1]], "Anna B. Smith")

  # CRediT roles, ROR IDs, figure text alternatives, header rows, data citations
  expect_equal(p$author$credit_roles[[1]],
               "https://credit.niso.org/contributor-roles/conceptualization/")
  expect_equal(p$affiliation$ror[1], "https://ror.org/02c2kyt77")
  expect_false(grepl("ror.org", p$affiliation$text[1]))
  expect_equal(p$figure$alt_text, "A bar chart of two means.")
  expect_equal(p$table$header_rows, 1L)
  expect_equal(p$bib$relation, c(NA, NA, "supplements"))

  # written as a 12.6 export, and read back the same
  out <- withr::local_tempdir()
  json <- paper_write(p, "jats", out, "12.0")
  x <- jsonlite::read_json(json)
  expect_equal(x$schema_version, "12.6")
  expect_true(all(c("subject", "review", "review_text") %in% names(x)))
  back <- read(json)
  expect_equal(back$review_text$text, p$review_text$text)
  expect_equal(back$funding$funder_doi, p$funding$funder_doi)
})

test_that("read - Elsevier: what the file declares beyond the text (bibr 12.6)", {
  p <- read(fmt("elsevier_cases.xml"))
  expect_equal(p$info$article_type, "research-article")
  expect_equal(c(p$info$received_date, p$info$revised_date, p$info$accepted_date),
               c("2025-07-11", "2026-03-01", "2026-03-28"))
  expect_equal(p$subject$scheme, "jel")
  expect_equal(p$subject$term, "C93")
  expect_equal(p$contributor$literal, "Frank Editor")
  expect_equal(p$author$credit_roles[[1]],
               "https://credit.niso.org/contributor-roles/formal-analysis/")
  expect_equal(p$section$section_type[p$section$header %in% "2 Method"], "method")
})

test_that("paper_write - a Grobid paper in 12.0 form has no 12.6 keys", {
  g <- suppressWarnings(grobid_to_bibr(demofile("xml"), NULL, schema_version = "12.0"))
  out <- withr::local_tempdir()
  x <- jsonlite::read_json(paper_write(g, "g", out, "12.0"))
  expect_equal(x$schema_version, "12.0")
  expect_false(any(.bibr12_v126$tables %in% names(x)))
  expect_false(any(.bibr12_v126$cols$metadata %in% names(x$metadata)))
})

test_that("read - JATS: plain-text links, reference links, MathML statistics", {
  p <- read(fmt("jats_cases.xml"))
  # web addresses printed without a link element; "e.g." and "Fig.1" are none
  expect_true(all(c("https://osf.io/qwert1", "https://www.example.org") %in% p$url$href))
  expect_false(any(grepl("e\\.g|Fig", p$url$href)))
  u <- p$url[p$url$href == "https://osf.io/qwert1", ]
  row <- p$text$text[p$text$text_id == u$text_id]
  expect_equal(substr(row, u$start + 1, u$end), "osf.io/qwert1")

  # a reference's web address, not its DOI link
  expect_equal(p$bib$url, c(NA, "https://books.example.org/tests", NA))

  # the formula's main comparison, not the sign inside the bracket; a comma
  # separates two comparisons
  e <- p$eq[grepl("^P\\(|^N$", p$eq$lhs), ]
  expect_true(any(e$lhs == "P(\u03b2 > 0)" & e$comp == "=" & e$rhs == ".97"))
  expect_true(any(e$lhs == "N" & e$comp == "=" & e$rhs == "40"))
  expect_false(any(p$eq$rhs %in% "0)"))
})
