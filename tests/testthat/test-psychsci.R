# change fancy quotes to straight for text matching with crossref
fix_fancy <- function(x) {
  x |>
    gsub("[\u2018\u2019\u0060]", "'", x = _) |>
    gsub("[\u201C\u201D]", "\"", x = _) |>
    gsub("–", "-", x = _)
}

test_that("psychsci", {
  expect_true(is_paper_list(metacheck::psychsci))

  # check all valid format
  for (paper in psychsci) {
    expect_no_warning(v <- paper_validate(paper))
    expect_true(v)
  }

  # check all tables can be combined ----
  tbls <- names(psychsci[[1]]) |> setdiff("paper_id")
  tables <- sapply(tbls, \(tbl) paper_table(psychsci, tbl))

  # check DOIs ----
  expect_true(all(doi_valid_format(tables$info$doi)))

  # get valid DOIs from file name
  dois <- tables$info$file_name |>
    gsub("\\.pdf", "", x = _) |>
    paste0("10.1177/", x = _)

  expect_equal(dois, tables$info$doi)

  file_path <- test_path("fixtures", "formats", "psychsci_info.csv")
  if (FALSE) {
    # get info from crossref
    cr_info <- crossref_doi(dois, c("title", "abstract", "DOI"))
    cr_info$abstract <- cr_info$abstract |>
      gsub("</jats:p>", "", x = _, fixed = TRUE) |>
      gsub("<jats:p>", "", x = _, fixed = TRUE) |>
      trimws()

    readr::write_csv(cr_info, file_path)
  }
})


test_that("psychsci components", {
  # check titles ----
  cr_info <- readr::read_csv(file_path)

  title_mismatch <- cr_info[, c("DOI", "title")] |>
    dplyr::mutate(bibr_title = tables$info$title,
                  title = fix_fancy(title),
                  bibr_title = fix_fancy(bibr_title)) |>
    tidyr::separate(bibr_title, c("bibr_title", "bibr_subtitle"),
                    sep = ":\\s+", fill = "right") |>
    tidyr::separate(title, c("cr_title", "cr_subtitle"),
                    sep = ":\\s+", fill = "right") |>
    dplyr::filter(tolower(cr_title) != tolower(bibr_title))

  expect_equal( nrow(title_mismatch), 0)
  # most of the title mismatches are spaces after the second - when there are two hyphenated words (check sub vs gsub?)

  # check abstracts ----
  bibr_abst <- search_text(psychsci, section = "abstract", return = "section") |>
    dplyr::select(DOI = paper_id, bibr_abst = text)

  abst_check <- cr_info |>
    dplyr::select(DOI, cr_abst = abstract) |>
    dplyr::left_join(bibr_abst, by = "DOI") |>
    dplyr::mutate(cr_abst = fix_fancy(cr_abst),
                  bibr_abst = fix_fancy(bibr_abst)) |>
    dplyr::rowwise() |>
    dplyr::mutate(
      bibr_same_cr = tolower(cr_abst) == tolower(bibr_abst),
      bibr_contains_cr = grepl(tolower(cr_abst), tolower(bibr_abst), fixed = TRUE)
    )

  expect_true(all(abst_check$bibr_same_cr))
  # so many abstract mismatches! could be due to bad labelling in the text table?

  # get all sentences and check if they're in the CR abstract
  all_text <- search_text(psychsci) |>
    dplyr::select(DOI = paper_id, text, text_id, section_type) |>
    dplyr::filter(nchar(text) > 1) |>
    dplyr::left_join(cr_info, by = "DOI") |>
    dplyr::select(-title) |>
    dplyr::rowwise() |>
    dplyr::mutate(
      text = fix_fancy(text),
      abstract = fix_fancy(abstract),
      in_abstract = grepl(tolower(text), tolower(abstract), fixed = TRUE)
    ) |>
    dplyr::ungroup() |>
    dplyr::filter(in_abstract)

  papers_with_some_abstract_text <- unique(all_text$DOI) |> length()
  expect_equal(papers_with_some_abstract_text, length(psychsci))
})
