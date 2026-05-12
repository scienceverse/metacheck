test_that("psychsci", {
  expect_true(is_paper_list(metacheck::psychsci))

  # check all valid format
  for (paper in psychsci) {
    expect_no_warning(v <- paper_validate(paper))
    expect_true(v)
  }

  # check all tables can be combined ----
  tbls <- names(psychsci[[1]]) |> setdiff("paper_id")
  expect_no_error(
    tables <- sapply(tbls, \(tbl) paper_table(psychsci, tbl))
  )

  skip("Failures expected")

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
      gsub("<\\/?jats[^>]*>", "", x = _) |>
      gsub("\\(\\s+", "(", x = _) |>
      gsub("\\s+", " ", x = _) |>
      gsub("(\\S)- ", "\\1-", x = _) |> # e.g., "1- SD"
      trimws()

    readr::write_csv(cr_info, file_path)
  }
})


test_that("psychsci components", {
  skip("Failures expected")

  # check titles ----
  file_path <- test_path("fixtures", "formats", "psychsci_info.csv")
  cr_info <- readr::read_csv(file_path, show_col_types = FALSE)
  cr_info$paper_id <- gsub("10.1177/", "", cr_info$DOI, fixed = TRUE)

  bibr_info <- paper_table(psychsci, "info")

  bibr_title_mismatch <- cr_info[, c("paper_id", "title")] |>
    dplyr::mutate(bibr_title = bibr_info$title,
                  title = fix_fancy(title),
                  bibr_title = fix_fancy(bibr_title)) |>
    tidyr::separate(bibr_title, c("bibr_title", "bibr_subtitle"),
                    sep = ":\\s+", fill = "right") |>
    tidyr::separate(title, c("cr_title", "cr_subtitle"),
                    sep = ":\\s+", fill = "right") |>
    dplyr::filter(tolower(cr_title) != tolower(bibr_title))

  # check abstracts ----
  bibr_abst <- search_text(psychsci, return = "section") |>
    dplyr::filter(section_type == "abstract") |>
    dplyr::select(paper_id, bibr_abst = text)

  bibr_abst_check <- cr_info |>
    dplyr::select(paper_id, cr_abst = abstract) |>
    dplyr::left_join(bibr_abst, by = "paper_id") |>
    dplyr::mutate(cr_abst = fix_fancy(cr_abst),
                  bibr_abst = fix_fancy(bibr_abst)) |>
    dplyr::rowwise() |>
    dplyr::mutate(
      bibr_same_cr = tolower(cr_abst) == tolower(bibr_abst),
      bibr_contains_cr = grepl(tolower(cr_abst), tolower(bibr_abst), fixed = TRUE)
    )


  # Could be due to bad labelling in the text table?
  # get all sentences and check if they're in the CR abstract
  bibr_all_text <- search_text(psychsci) |>
    dplyr::select(paper_id, text, text_id, section_type) |>
    dplyr::filter(nchar(text) > 1,
                  (section_type %in% "abstract" | text_id < 50)) |>
    dplyr::left_join(cr_info, by = "paper_id") |>
    dplyr::select(-title) |>
    dplyr::rowwise() |>
    dplyr::mutate(
      text = fix_fancy(text) |>
        gsub("-", "", x = _),
      abstract = fix_fancy(abstract) |>
        gsub("-", "", x = _),
      in_abstract = agrepl(text, tolower(abstract), fixed = TRUE)
    ) |>
    dplyr::ungroup() |>
    dplyr::filter(in_abstract)

  bibr_abs_text <- bibr_all_text |>
    dplyr::summarise(text = paste(text, collapse = " "),
                     .by = c("paper_id", "abstract")) |>
    dplyr::mutate(
      abst_identical = text == abstract
    ) |>
    dplyr::rowwise() |>
    dplyr::mutate(
      adist = utils::adist(text, abstract)[[1]]
    )

  # report/test
  cat("\n- bibr title mismatches:", nrow(bibr_title_mismatch),
      "\n- bibr papers with some abstract text:", nrow(bibr_abs_text),
      "\n- bibr papers with all abstract text:", sum(bibr_abs_text$abst_identical),
      "\n- bibr papers with all adist < 10:", sum(bibr_abs_text$adist < 10)

  )

  expect_equal( nrow(bibr_title_mismatch), 0)
  # most of the title mismatches are spaces after the second - when there are two hyphenated words (check sub vs gsub?)
  expect_true(all(bibr_abst_check$bibr_same_cr))
  expect_equal(bibr_some_abs, length(psychsci))
})


test_that("urls", {
  bibr_url <- paper_table(psychsci, "url") |>
    dplyr::mutate(bibr = TRUE,
                  # remove trailing slashes
                  href = gsub("/$", "", href),
                  href = tolower(href))

  # compare extract_urls() function in metacheck with bibr urls
  mc_url <- extract_urls(psychsci) |>
    dplyr::select(paper_id, text_id, href = text) |>
    dplyr::mutate(mc = TRUE,
                  href = gsub("/$", "", href),
                  href = tolower(href))

  url_comp <- mc_url |>
    dplyr::full_join(bibr_url, by = c("paper_id", "text_id", "href"),
                     relationship = "many-to-many") |>
    dplyr::arrange(paper_id, text_id)

  dplyr::count(url_comp, mc, bibr)

  mc_only <- dplyr::filter(url_comp, mc, is.na(bibr)) |>
    dplyr::select(paper_id, text_id, mc = href)
  bibr_only <- dplyr::filter(url_comp, bibr, is.na(mc)) |>
    dplyr::select(paper_id, text_id, bibr = href)

  # mismatches with the same paper_id:text_id
  url_mismatch <- dplyr::inner_join(
    mc_only, bibr_only,
    by = c("paper_id", "text_id"),
    relationship = "many-to-many") |>
    expand_text(psychsci)
})

# psychsci from grobid ----

test_that("psychsci2", {
  skip("Failures expected")

  expect_true(is_paper_list(metacheck::psychsci2))

  # check all valid format
  for (paper in psychsci2) {
    expect_no_warning(v <- paper_validate(paper))
    expect_true(v)
  }

  # check all tables can be combined ----
  tbls <- names(psychsci2[[1]]) |> setdiff("paper_id")
  tables <- sapply(tbls, \(tbl) paper_table(psychsci2, tbl))

  # check DOIs ----
  expect_true(all(doi_valid_format(tables$info$doi)))

  # get valid DOIs from file name
  dois <- tables$info$file_name |>
    gsub("\\.xml", "", x = _) |>
    gsub("data-raw/psychsci/grobid_0.9.0-crf/", "", x = _, fixed = TRUE) |>
    paste0("10.1177/", x = _)

  expect_equal(dois, tables$info$doi)
})


test_that("psychsci2 components", {
  skip("Failures expected")

  # check titles ----
  file_path <- test_path("fixtures", "formats", "psychsci_info.csv")
  cr_info <- readr::read_csv(file_path, show_col_types = FALSE)
  cr_info$paper_id <- gsub("10.1177/", "", cr_info$DOI, fixed = TRUE)

  grobid_info <- paper_table(psychsci2, "info")

  grobid_title_mismatch <- cr_info[, c("paper_id", "title")] |>
    dplyr::mutate(grobid_title = grobid_info$title,
                  title = fix_fancy(title),
                  grobid_title = fix_fancy(grobid_title)) |>
    tidyr::separate(grobid_title, c("grobid_title", "grobid_subtitle"),
                    sep = ":\\s+", fill = "right") |>
    tidyr::separate(title, c("cr_title", "cr_subtitle"),
                    sep = ":\\s+", fill = "right") |>
    dplyr::filter(tolower(cr_title) != tolower(grobid_title))


  # check abstracts ----
  grobid_abst <- search_text(psychsci2, return = "section") |>
    dplyr::filter(section_type == "abstract") |>
    dplyr::select(paper_id, grobid_abst = text)

  abst_check <- cr_info |>
    dplyr::select(paper_id, cr_abst = abstract) |>
    dplyr::left_join(grobid_abst, by = "paper_id") |>
    dplyr::mutate(cr_abst = fix_fancy(cr_abst),
                  grobid_abst = fix_fancy(grobid_abst)) |>
    dplyr::rowwise() |>
    dplyr::mutate(
      grobid_same_cr = tolower(cr_abst) == tolower(grobid_abst),
      grobid_contains_cr = grepl(tolower(cr_abst), tolower(grobid_abst), fixed = TRUE)
    )

  # Could be due to bad labelling in the text table?
  # get all sentences and check if they're in the CR abstract
  grobid_all_text <- search_text(psychsci2) |>
    dplyr::select(paper_id, text, text_id, section_type) |>
    dplyr::filter(nchar(text) > 1,
                  (section_type %in% "abstract" | text_id < 50)) |>
    dplyr::left_join(cr_info, by = "paper_id") |>
    dplyr::select(-title) |>
    dplyr::rowwise() |>
    dplyr::mutate(
      text = fix_fancy(text) |>
        gsub("-", "", x = _),
      abstract = fix_fancy(abstract) |>
        gsub("-", "", x = _),
      in_abstract = agrepl(text, tolower(abstract), fixed = TRUE)
    ) |>
    dplyr::ungroup() |>
    dplyr::filter(in_abstract)

  grobid_abs_text <- grobid_all_text |>
    dplyr::summarise(text = paste(text, collapse = " "),
                     .by = c("paper_id", "abstract")) |>
    dplyr::mutate(
      abst_identical = text == abstract
    ) |>
    dplyr::rowwise() |>
    dplyr::mutate(
      adist = utils::adist(text, abstract)[[1]]
    )

  # report/test
  cat("\n- grobid title mismatches:", nrow(grobid_title_mismatch),
      "\n- grobid papers with some abstract text:", nrow(grobid_abs_text),
      "\n- grobid papers with all abstract text:", sum(grobid_abs_text$abst_identical),
      "\n- grobid papers with all adist < 10:", sum(grobid_abs_text$adist < 10)

  )

  expect_equal( nrow(grobid_title_mismatch), 0)
  # most of the title mismatches are spaces after the second - when there are two hyphenated words (check sub vs gsub?)
  expect_true(all(abst_check$grobid_same_cr))
  expect_equal(grobid_some_abs, length(psychsci2))
})


