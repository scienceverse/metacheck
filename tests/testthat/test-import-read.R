
test_that(".read_bibr", {
  expect_true(is.function(metacheck::.read_bibr))
  expect_no_error(helplist <- help(.read_bibr, metacheck))

  expect_error(.read_bibr(bad_arg))

  # single paper from json
  file_path <- demofile("json")
  paper <- .read_bibr(file_path)

  expect_true(paper_validate(paper))
  #expect_match(paper$paper_id, "^[a-f0-9]{14,16}$")
})

test_that("read", {
  expect_true(is.function(metacheck::read))
  expect_no_error(helplist <- help(read, metacheck))

  expect_error(read(bad_arg))

  # no files
  file_path <- file.path(withr::local_tempdir(), "emptydir")
  dir.create(file_path)
  expect_message(paper <- read(file_path))
  expect_equal(length(paper), 0)

  # no relevant files
  txt_path <- file.path(file_path, "hi.txt")
  write("hi", txt_path)
  expect_message(paper <- read(file_path))
  expect_equal(length(paper), 0)
})

test_that("read - json and xml together", {
  # set up temp
  file_path <- file.path(withr::local_tempdir(), "a")
  dir.create(file_path)
  file.copy(demofile("xml"), file.path(file_path, "demo.xml"))
  file.copy(demofile("json"), file.path(file_path, "demo.json"))

  paper <- read(file_path)
  expect_true(.is_paper(paper))
})

test_that("read - recursive", {
  skip_no_psychsci()
  # set up temp
  a <- file.path(withr::local_tempdir(), "a")
  b1 <- file.path(a, "b1")
  b2 <- file.path(a, "b2")
  dir.create(a)
  dir.create(b1)
  dir.create(b2)

  sink <- paper_write(psychsci[[1]], save_path = a )
  sink <- paper_write(psychsci[[1]], save_path = b1)
  sink <- paper_write(psychsci[[2]], save_path = b1)
  sink <- paper_write(psychsci[[3]], save_path = b1)
  sink <- paper_write(psychsci[[3]], save_path = b2)

  # no recursive ----
  paper <- read(a)
  expect_equal(paper$paper_id, psychsci[[1]]$paper_id)

  paper <- read(b1)
  expect_equal(paper_id(paper), paper_id(psychsci[1:3]))

  # recursive ----
  # the same paper in two folders: the second copy is renamed "~2" (U208)
  expect_warning(paper <- read(a, recursive = TRUE), "shared a paper_id")
  expect_equal(length(paper), 5)
  # read(recursive = TRUE) lists files via list.files(..., recursive = TRUE),
  # which returns paths in alphabetical order -- derive the expectation the
  # same way rather than hardcoding a paper-id-dependent pattern.
  exp <- list.files(a, pattern = "\\.json$", recursive = TRUE, full.names = TRUE) |>
    sort() |>
    basename() |>
    gsub("\\.json$", "", x = _)
  exp <- ifelse(duplicated(exp), paste0(exp, "~2"), exp)
  expect_equal(paper_id(paper), exp)
})

test_that("read - single paper", {
  file_path <- demofile("json")
  paper <- read(file_path)

  expect_s3_class(paper, "scivrs_paper")
  expect_true(paper_validate(paper))

  # check for no urls ending in .
  end_dot <- grepl("\\.$", paper$url$href)
  expect_true(all(!end_dot))
  expect_true(all(is.na(paper$figure$image)))
})

# test_that("read - images", {
#   file_path <- system.file("demos/to_err_is_human.json", package = "metacheck")
#   paper <- read(file_path, include_images = TRUE)
#
#   expect_true(paper_validate(paper))
#   expect_true(all(!is.na(paper$figure$image)))
# })

test_that("read - vector of paths", {
  file_path <- demofile("json")
  paper <- read(file_path)

  file_path <- c(
    demofile("json"),
    test_path("fixtures", "psychsci", "0956797613520608.json")
  )
  papers <- read(file_path)
  expect_s3_class(papers, "scivrs_paperlist")
  expect_true(paper_validate(papers[[1]]))
  expect_true(paper_validate(papers[[2]]))

  expect_equal(paper, papers[[1]])
  expect_equal(names(papers),
               c(papers[[1]]$paper_id, papers[[2]]$paper_id))
})

test_that("read - directory", {
  file_path <- test_path("fixtures", "psychsci")
  papers <- read(file_path)
  expect_s3_class(papers, "scivrs_paperlist")
  expect_true(paper_validate(papers[[1]]))
  expect_true(paper_validate(papers[[2]]))
  expect_true(paper_validate(papers[[3]]))
})

test_that("a JSON file that is not a paper is refused with a clear message (#499)", {
  f <- withr::local_tempfile(fileext = ".json")
  for (txt in c("{}", "[]", '{"foo": 1}', '{"paper_id": ""}')) {
    writeLines(txt, f)
    expect_error(metacheck:::.read_bibr(f), "is not bibr output", fixed = TRUE)
    # read() skips it with a warning instead of returning a paper without an id
    expect_warning(p <- read(f), "is not bibr output")
    expect_length(p, 0)
  }
})

test_that("module_run reports the module's own error for a paper without an id (#499)", {
  p <- paper()
  p$paper_id <- character(0)
  p$info <- data.frame(title = "x")
  # paper_id() itself fails on such a paper
  expect_error(paper_id(p))
  # the module's error is the one reported, not paper_id()'s
  err <- tryCatch(module_run(p, "power"), error = function(e) conditionMessage(e))
  expect_match(err, "Running the module 'power' produced errors", fixed = TRUE)
})

test_that("a legacy bibr file without bib has no references, not its bib_match rows (U25)", {
  # data$bib matched data$bib_match by its prefix
  f <- withr::local_tempfile(fileext = ".json")
  writeLines('{"paper_id": "p1", "info": {"title": "T"}, "bib_match": [{"bib_id": 1, "title": "Matched"}]}', f)
  p <- suppressWarnings(read(f))
  expect_equal(nrow(p$bib), 0)
})

test_that(".repair_links joins links split by a space, for every reader", {
  x <- c(
    "Code is on https://github.com/ seantuck12/MODISTools for all.",
    "See https://github.com/nardus/zoonotic_ rank/tree/main/FigureData and more.",
    "doi 10.6084/m9.figshare. 19518898 and https://doi. org/ 10.1002/ zoo.21834",
    "github. com/ etldrz/phage and https:// doi.org/10.1/x"
  )
  expect_equal(metacheck:::.repair_links(x), c(
    "Code is on https://github.com/seantuck12/MODISTools for all.",
    "See https://github.com/nardus/zoonotic_rank/tree/main/FigureData and more.",
    "doi 10.6084/m9.figshare.19518898 and https://doi.org/10.1002/zoo.21834",
    "github.com/etldrz/phage and https://doi.org/10.1/x"
  ))
})

test_that(".repair_links keeps the letters as written", {
  x <- "Data: https://doi. ORG/ 10.17605/OSF. IO/ CY68W and GITHUB. COM/x/y."
  expect_equal(metacheck:::.repair_links(x),
               "Data: https://doi.ORG/10.17605/OSF.IO/CY68W and GITHUB.COM/x/y.")
})

test_that(".repair_links joins a website's name split before or after any of its dots", {
  x <- c(
    "Pain, 23, 1616-1628. https://doi .org/10.1016/j.jpain.2022.03.1",
    "Materials are at osf .io/abcde and code at github .com/a/b.",
    "Retrieved from http://hdl. handle. net/1911/16324 and http://dx. doi. org/10.1/x",
    "Preregistered (https://aspredicted. org/vp4rg.pdf); https://archive. softwareheritage. org/swh:1:dir:1"
  )
  expect_equal(metacheck:::.repair_links(x), c(
    "Pain, 23, 1616-1628. https://doi.org/10.1016/j.jpain.2022.03.1",
    "Materials are at osf.io/abcde and code at github.com/a/b.",
    "Retrieved from http://hdl.handle.net/1911/16324 and http://dx.doi.org/10.1/x",
    "Preregistered (https://aspredicted.org/vp4rg.pdf); https://archive.softwareheritage.org/swh:1:dir:1"
  ))
})

test_that(".repair_links leaves a website's name without a '/' after it as written", {
  x <- c("Data are available at datadryad. org for production of results.",
         "Software: github. com;2022.",
         "We use the OSF .io file format.")
  expect_equal(metacheck:::.repair_links(x), x)
})

test_that(".repair_links takes GROBID's temporary folder and Check Point wrappers out of links (#634)", {
  x <- c(
    "found in file://localhost/opt/grobid/grobid-home/tmp/10.5281/zenodo.15527338. More",
    "accessed at file://localhost/opt/grobid/grobid-home/tmp/www.osf.io/qknug.",
    "(Study 1: https://protect.checkpoint.com/v2/___https://osf.io/qby5j___.YzJ1OnN0b255YnJvb2s6YzpnOjY5 and"
  )
  expect_equal(metacheck:::.repair_links(x), c(
    "found in 10.5281/zenodo.15527338. More",
    "accessed at www.osf.io/qknug.",
    "(Study 1: https://osf.io/qby5j and"
  ))
})

test_that(".repair_links leaves a space before an ordinary word", {
  x <- c(
    "Data are in Zenodo https://zenodo.org/ with doi: 10.5281/zenodo.3876533.",
    "Available at figshare.com/ upon request and aspredicted.org/ Clinical trials.",
    "The variable my_ var is not a link.",
    NA
  )
  expect_equal(metacheck:::.repair_links(x), x)
})

test_that(".paper_repair_links moves the positions of items after a removed space", {
  p <- paper("t")
  s <- "See https://github.com/ owner/repo and https:// doi.org/ 10.1/x (Smith, 2020)."
  p$text <- data.frame(text = s, text_id = 1L)
  at <- function(x) regexpr(x, s, fixed = TRUE)[[1]] - 1L
  p$url <- data.frame(url_id = 1L, text_id = 1L, start = at("https:// doi"),
                      end = at("10.1/x") + 6L)
  p$xref <- data.frame(xref_id = 1L, text_id = 1L, start = at("Smith"),
                       end = at("Smith") + 11L)
  q <- metacheck:::.paper_repair_links(p)
  expect_equal(q$text$text,
               "See https://github.com/owner/repo and https://doi.org/10.1/x (Smith, 2020).")
  expect_equal(substr(q$text$text, q$url$start + 1, q$url$end), "https://doi.org/10.1/x")
  expect_equal(substr(q$text$text, q$xref$start + 1, q$xref$end), "Smith, 2020")
})

test_that(".repair_bib_dois joins a reference's DOI with the DOI the reader found (#459)", {
  s <- c(
    "Main text: see 10.5281/zenodo.76187 49 here.",
    "Smith (2024). Data. Zenodo. https:// doi. org/ 10. 5281/ ZENODO. 13986711",
    "Lee (2021). Data. https://doi.org/10.5061/dryad.7d7wm 3806",
    "Kim (2020). Text. https://doi.org/10.1016/j.cell.2017.10.041"
  )
  bib <- data.frame(doi = c("10.5281/ZENODO.13986711", "10.5061/dryad.7d7wm3806",
                            "10.1016/j.cell.2017.10.041"), text_id = 2:4)
  expect_equal(metacheck:::.repair_bib_dois(s, 1:4, bib), c(
    s[[1]],
    "Smith (2024). Data. Zenodo. https:// doi. org/ 10.5281/ZENODO.13986711",
    "Lee (2021). Data. https://doi.org/10.5061/dryad.7d7wm3806",
    s[[4]]
  ))
  # a space before a capitalised word stays: the reader's DOI took in the
  # next reference's author; the spaces before it are still removed
  s <- "Lee (1990). Vision Res. 10.1016/0042- 6989(90)90173-I Cao, X. (2001)."
  bib <- data.frame(doi = "10.1016/0042-6989(90)90173-ICao", text_id = 1L)
  expect_equal(metacheck:::.repair_bib_dois(s, 1L, bib),
               "Lee (1990). Vision Res. 10.1016/0042-6989(90)90173-I Cao, X. (2001).")
  # a DOI written twice is left alone
  s <- "Ann (1994). Med Biol Eng. 10.1007/BF025 10705 10.1007/BF02510705"
  bib <- data.frame(doi = "10.1007/BF0251070510.1007/BF02510705", text_id = 1L)
  expect_equal(metacheck:::.repair_bib_dois(s, 1L, bib), s)
  # no bib table, or one without DOIs
  expect_equal(metacheck:::.repair_bib_dois(s, 1L, NULL), s)
  expect_equal(metacheck:::.repair_bib_dois(s, 1L, data.frame(doi = NA, text_id = 1L)), s)
})

test_that(".paper_repair_links joins references' DOIs after the link rules (#459)", {
  p <- paper("t")
  s <- "Lee (2021). Data. https:// doi. org/ 10. 5061/ dryad. 7d7wm 3806 (Lee, 2021)."
  p$text <- data.frame(text = s, text_id = 1L)
  p$bib <- data.frame(bib_id = 1L, doi = "10.5061/dryad.7d7wm3806", text_id = 1L)
  at <- function(x) regexpr(x, s, fixed = TRUE)[[1]] - 1L
  p$url <- data.frame(url_id = 1L, text_id = 1L, start = at("https"),
                      end = at("3806") + 4L)
  p$xref <- data.frame(xref_id = 1L, text_id = 1L, start = at("Lee, 2021"),
                       end = at("Lee, 2021") + 9L)
  q <- metacheck:::.paper_repair_links(p)
  expect_equal(q$text$text,
               "Lee (2021). Data. https://doi.org/10.5061/dryad.7d7wm3806 (Lee, 2021).")
  expect_equal(substr(q$text$text, q$url$start + 1, q$url$end),
               "https://doi.org/10.5061/dryad.7d7wm3806")
  expect_equal(substr(q$text$text, q$xref$start + 1, q$xref$end), "Lee, 2021")
  # a second repair changes nothing
  expect_equal(metacheck:::.paper_repair_links(q)$text$text, q$text$text)
})
