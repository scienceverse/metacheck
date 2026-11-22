test_that(".file_tree_rows", {
  rows <- .file_tree_rows(c("data/raw.csv", "README.md", "data/clean.csv", NA, ""))
  # folders first, then files, each alphabetically
  expect_equal(rows$text, c("├── data/", "│   ├── clean.csv", "│   └── raw.csv",
                            "└── README.md"))
  # each file row points back at its input path; a folder row has none
  expect_equal(rows$leaf_idx, c(NA, 3L, 1L, 2L))

  empty <- .file_tree_rows(character(0))
  expect_equal(nrow(empty), 0)
  expect_equal(names(empty), c("text", "leaf_idx"))

  # a folder given as an input is drawn and sorted as a folder, even when
  # nothing lies below it, and its row points at it
  rows <- .file_tree_rows(c("b.csv", "a_empty", "z_data/x.csv"),
                          is_dir = c(FALSE, TRUE, FALSE))
  expect_equal(rows$text, c("├── a_empty/", "├── z_data/", "│   └── x.csv", "└── b.csv"))
  expect_equal(rows$leaf_idx, c(2L, NA, 3L, 1L))
})

test_that(".file_tree_block", {
  files <- data.frame(
    repo_url = "my_study",
    file_name = c("raw.csv", "README.md", "analysis.R"),
    file_path = c("data/raw.csv", "README.md", "code/analysis.R"),
    data_type = c("data", "documentation", "code"),
    doc_role = c(NA, "readme", NA)
  )
  naming <- data.frame(file_name = "raw.csv", detail = "no date")
  block <- .file_tree_block(files, naming)

  expect_equal(block[[1]], "#### Data Tree")
  expect_match(block[[2]], "and any file-naming issue found by repo_check", fixed = TRUE)
  expect_match(block[[2]], "junk files, hidden files and empty folders are marked", fixed = TRUE)
  expect_equal(block[[3]], "**Repository: my_study**")
  tree <- block[[4]]
  # the tree sits in a scroll box
  expect_match(tree, "^::: \\{style=\"max-height: 40em; overflow: auto;\"\\}\n```\n")
  expect_match(tree, "\n```\n:::$")
  expect_match(tree, "data/", fixed = TRUE)
  expect_match(tree, "raw.csv\\s+\U0001F4CA data\\s+no date")
  expect_match(tree, "README.md\\s+\U0001F4C4 readme")

  expect_null(.file_tree_block(NULL))
  expect_null(.file_tree_block(files[0, ]))
  expect_null(.file_tree_block(data.frame(file_name = "a.csv")))
})

test_that(".file_tree_block marks junk from repo_check for a repository link", {
  files <- data.frame(repo_url = "https://osf.io/abcde", file_name = "raw.csv",
                      file_path = "data/raw.csv", data_type = "data")
  useless <- data.frame(repo_url = "https://osf.io/abcde",
                        file_path = c(".DS_Store", "__MACOSX/._raw.csv", "__MACOSX/._x.csv",
                                      ".git/config"),
                        kind = c("operating system", "operating system", "operating system",
                                 "version control"),
                        severity = c("problem", "problem", "problem", "suggestion"))
  tree <- .file_tree_block(files, NULL, useless)[[4]]
  expect_match(tree, ".DS_Store\\s+junk: remove")
  # a junk folder is one line with the number of files inside
  expect_match(tree, "__MACOSX/\\s+junk: remove \\(2 files\\)")
  expect_match(tree, ".git/\\s+junk: keep only on purpose \\(1 file\\)")
  expect_no_match(tree, "._raw.csv", fixed = TRUE)
})

test_that(".file_tree_block lists a local folder in full", {
  d <- withr::local_tempdir()
  for (x in c("data/raw", "code/__pycache__", ".git/objects/ab", "docs/empty")) {
    dir.create(file.path(d, x), recursive = TRUE)
  }
  writeLines("a,b\n1,2", file.path(d, "data", "raw", "raw data.csv"))
  writeLines("x <- 1", file.path(d, "code", "analysis.R"))
  writeLines("b", file.path(d, "code", "__pycache__", "u.pyc"))
  for (i in 1:3) writeLines("o", file.path(d, ".git", "objects", "ab", paste0("obj", i)))
  writeLines("junk", file.path(d, ".DS_Store"))
  writeLines("*.html", file.path(d, ".gitignore"))
  writeLines("# Study", file.path(d, "README.md"))
  files <- data.frame(repo_url = d,
                      file_name = c("raw data.csv", "analysis.R", "README.md"),
                      file_path = c("data/raw/raw data.csv", "code/analysis.R", "README.md"),
                      data_type = c("data", "code", "documentation"),
                      doc_role = c(NA, NA, "readme"))

  lines <- strsplit(.file_tree_block(files)[[4]], "\n")[[1]]
  lines <- lines[!grepl("^(:::|```)", lines)]
  # junk folders as one line with their number of files, nothing below them
  expect_true(any(grepl("^├── \\.git/\\s+junk: keep only on purpose \\(3 files\\)$", lines)))
  expect_true(any(grepl("__pycache__/\\s+junk: remove \\(1 file\\)$", lines)))
  expect_false(any(grepl("obj1|u\\.pyc", lines)))
  # junk files, hidden files and empty folders are marked
  expect_true(any(grepl("\\.DS_Store\\s+junk: remove$", lines)))
  expect_true(any(grepl("\\.gitignore\\s+hidden file$", lines)))
  expect_true(any(grepl("empty/\\s+empty folder$", lines)))
  # classified files keep their classification
  expect_true(any(grepl("raw data\\.csv\\s+\U0001F4CA data$", lines)))
  # folders (empty ones included) before files, alphabetically
  top <- sub("^[├└]── ", "", lines[grepl("^[├└]── ", lines)])
  top <- sub("\\s.*$", "", top)
  expect_equal(top, c(".git/", "code/", "data/", "docs/", ".DS_Store", ".gitignore", "README.md"))
})

test_that(".file_tree_from_report and .file_tree", {
  report <- c("Intro", "#### Data Tree", "legend", "tree", "#### Data Files", "x")
  expect_equal(.file_tree_from_report(report), c("legend", "tree"))
  expect_null(.file_tree_from_report(c("Intro", "#### Data Files")))
  expect_null(.file_tree_from_report(NULL))

  # data_check's tree first
  mo <- list(data_check = list(report = report))
  expect_equal(.file_tree(mo), c("legend", "tree"))

  # else one drawn from repo_check's files and junk, without its heading
  files <- data.frame(repo_url = "my_study", file_name = "raw.csv",
                      file_path = "data/raw.csv", data_type = "data")
  useless <- data.frame(repo_url = "my_study", file_path = ".DS_Store",
                        kind = "operating system", severity = "problem")
  mo <- list(data_check = list(report = "nothing"),
             repo_check = list(table = files, useless_files = useless))
  tree <- .file_tree(mo)
  expect_false("#### Data Tree" %in% tree)
  expect_equal(tree[[2]], "**Repository: my_study**")
  expect_match(tree[[3]], ".DS_Store\\s+junk: remove")

  expect_null(.file_tree(list()))
})

test_that(".file_tree_drop_from_report", {
  report <- c("Intro", "#### Data Tree", "legend", "tree", "#### Data Files", "x")
  expect_equal(.file_tree_drop_from_report(report), c("Intro", "#### Data Files", "x"))
  # a tree at the end, and a heading with nothing under it
  expect_equal(.file_tree_drop_from_report(c("Intro", "#### Data Tree", "tree")), "Intro")
  expect_equal(.file_tree_drop_from_report(c("Intro", "#### Data Tree")), "Intro")
  # nothing to drop
  expect_equal(.file_tree_drop_from_report(c("Intro", "#### Data Files")),
               c("Intro", "#### Data Files"))
  expect_null(.file_tree_drop_from_report(NULL))
})

test_that(".file_tree_block names a local folder by its own name (Lakens/tue_datamanager_check#5, item 2)", {
  files <- data.frame(
    repo_url = "/data/metacheck_storage/metacheck-api-jobs/1a2b3c/repo/my_study",
    file_name = "raw.csv", file_path = "data/raw.csv", data_type = "data", doc_role = NA)
  block <- .file_tree_block(files)
  expect_equal(block[[3]], "**Repository: my_study**")
  expect_false(any(grepl("metacheck_storage", block, fixed = TRUE)))
  # a link stays a link
  files$repo_url <- "https://osf.io/abcde/"
  expect_equal(.file_tree_block(files)[[3]], "**Repository: https://osf.io/abcde/**")
})

test_that("the tree of a large repository is drawn quickly and cut at .file_tree_max_lines (#702)", {
  n <- 20000
  p <- sprintf("site%03d/plot%02d/day%06d.csv", rep(1:200, length.out = n), rep(1:20, length.out = n), seq_len(n))
  files <- data.frame(repo_url = "https://zenodo.org/records/1", file_path = p,
                      file_name = basename(p), data_type = "data", doc_role = NA)
  t <- system.time(block <- .file_tree_block(files))[["elapsed"]]
  expect_lt(t, 30)
  lines <- strsplit(block[grepl("^:::", block)], "\n", fixed = TRUE)[[1]]
  expect_equal(sum(!grepl("^(:::|```)", lines)), .file_tree_max_lines + 1L)
  expect_match(lines[length(lines) - 2], "^\\(and [0-9,]+ more files and folders, not shown\\)$")
})

test_that("a repository whose files are all inside a junk folder gets a tree (#703)", {
  junk <- data.frame(repo_url = "https://osf.io/abcde",
                     file_path = c("__MACOSX/._a.csv", "__MACOSX/._b.csv"))
  block <- .file_tree_block(NULL, NULL, junk)
  expect_true(any(grepl("__MACOSX/\\s+junk: remove \\(2 files\\)", block)))
})

test_that(".path_ancestors() gives every folder above each path", {
  a <- .path_ancestors(c("a/b/c.csv", "x.csv", "a/", "/r/s"))
  expect_equal(a$i, c(1L, 1L, 4L, 4L))
  expect_equal(a$prefix, c("a", "a/b", "", "/r"))
})
