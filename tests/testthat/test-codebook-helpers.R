# Unit tests for the codebook parsing + column matching helpers (rules path).
# LLM tiers are exercised only in the module and require an LLM, so are not here.

test_that("normalize_varname canonicalises names", {
  expect_equal(normalize_varname("SSS_total"), "sss total")
  expect_equal(normalize_varname("  Age.  "), "age")
  expect_equal(normalize_varname("subj-id"), "subj-id")
})

test_that("normalize_label stems and strips for equivalence", {
  a <- normalize_label("Participants' responses")
  b <- normalize_label("participant response")
  expect_equal(a, b)
})

test_that("parse_codebook reads a structured CSV codebook", {
  d <- tempfile(fileext = ".csv")
  writeLines(c("varname,description",
               "id,participant identifier",
               "score,outcome measure"), d)
  res <- parse_codebook(d)
  expect_s3_class(res, "data.frame")
  expect_true(all(c("codebook_variable", "label") %in% names(res)))
  expect_equal(nrow(res), 2)
  expect_equal(res$parse_method[[1]], "structured")
})

test_that("parse_codebook returns text lines for unstructured files", {
  d <- tempfile(fileext = ".txt")
  writeLines(c("This is a prose readme.", "No variable table here."), d)
  res <- parse_codebook(d)
  # character vector (for the LLM tier), not a data.frame
  expect_type(res, "character")
})

test_that("match_column_labels matches by normalised name", {
  cols <- data.frame(
    paper_id = "p", source_file = "d.csv",
    column_name = c("age", "SSS_total", "unmatched"),
    stringsAsFactors = FALSE)
  cbk <- data.frame(
    codebook_variable = c("age", "sss total"),
    label = c("Age in years", "SSS total score"),
    codebook_source = "cb.csv", group = NA_character_,
    parse_method = "structured", stringsAsFactors = FALSE)
  res <- match_column_labels(cols, cbk)
  expect_equal(res$label_status[res$column_name == "age"], "labelled")
  expect_equal(res$label_status[res$column_name == "SSS_total"], "labelled")
  expect_equal(res$label_status[res$column_name == "unmatched"], "unlabelled")
})

test_that("match_column_labels flags conflicting definitions", {
  cols <- data.frame(paper_id = "p", source_file = "d.csv",
                     column_name = "dv", stringsAsFactors = FALSE)
  cbk <- data.frame(
    codebook_variable = c("dv", "dv"),
    label = c("reaction time", "accuracy"),      # semantically different
    codebook_source = c("a.csv", "b.csv"),
    group = NA_character_, parse_method = "structured",
    stringsAsFactors = FALSE)
  res <- match_column_labels(cols, cbk)
  expect_equal(res$label_status, "conflicting_definition")
})

test_that("match_column_labels returns unlabelled when no codebook", {
  cols <- data.frame(paper_id = "p", source_file = "d.csv",
                     column_name = c("a", "b"), stringsAsFactors = FALSE)
  res <- match_column_labels(cols, .empty_codebook_vars())
  expect_true(all(res$label_status == "unlabelled"))
})

test_that(".infer_group maps experiment context to group codes", {
  expect_equal(.infer_group("Experiment 1"), "ex1")
  expect_equal(.infer_group("Study 2a"), "ex2a")
  expect_equal(.infer_group("Pilot 1"), "pilot1")
  expect_true(is.na(.infer_group("")))
})

test_that("data_group_llm skips grouping when there is nothing analysable", {
  # Only assets -> no LLM call needed, everything defaults to 'shared'.
  f <- data.frame(
    file_name = c("fig1.png", "photo.jpg", "manual.pdf"),
    data_type = c("asset", "asset", "asset"),
    stringsAsFactors = FALSE)
  out <- data_group_llm(f)
  expect_equal(out$group, rep("shared", 3))
})

test_that("data_group_llm returns NULL on empty input", {
  expect_null(data_group_llm(data.frame()))
  expect_null(data_group_llm(NULL))
})

test_that(".llm_classify_batched batches items and maps results by index", {
  # Mock llm() to echo each numbered input line back as index + value, but
  # deliberately return the batch in reverse order to prove index mapping (not
  # position) is what aligns results. The mock also records each batch size.
  batch_sizes <- integer(0)
  fake_llm <- function(text, text_col = "text", ...) {
    listing <- text[[text_col]][1]
    lines <- strsplit(listing, "\n", fixed = TRUE)[[1]]
    n <- length(lines)
    batch_sizes[[length(batch_sizes) + 1L]] <<- n
    # Each line is "i. <item text>"; classify by whether the item text is "num".
    idx <- as.integer(sub("\\..*$", "", lines))
    item <- sub("^[0-9]+\\. ", "", lines)
    val <- ifelse(grepl("num", item), "continuous", "text")
    df <- data.frame(results.index = rev(idx), results.value = rev(val))
    class(df) <- c("metacheck_llm", "data.frame")
    attr(df, "llm") <- list(model = "mock")
    df
  }

  testthat::local_mocked_bindings(llm = fake_llm)
  items <- paste0(rep(c("num", "cat"), length.out = 120))  # 120 items
  res <- metacheck:::.llm_classify_batched(
    items, system_prompt = "classify", value_desc = "type",
    valid = c("continuous", "text"), batch_size = 50)

  expect_length(res, 120)
  # 120 items / 50 per batch -> batches of 50, 50, 20.
  expect_equal(batch_sizes, c(50L, 50L, 20L))
  # Odd items ("num") -> continuous, even ("cat") -> text, correctly index-mapped.
  expect_equal(res[c(1, 3, 5)], rep("continuous", 3))
  expect_equal(res[c(2, 4, 6)], rep("text", 3))
})

test_that(".llm_classify_batched drops values outside the valid set", {
  fake_llm <- function(text, text_col = "text", ...) {
    listing <- text[[text_col]][1]
    n <- length(strsplit(listing, "\n", fixed = TRUE)[[1]])
    df <- data.frame(results.index = seq_len(n),
                     results.value = rep("banana", n))  # not a valid level
    class(df) <- c("metacheck_llm", "data.frame")
    attr(df, "llm") <- list(model = "mock")
    df
  }
  testthat::local_mocked_bindings(llm = fake_llm)
  res <- metacheck:::.llm_classify_batched(
    c("a", "b"), system_prompt = "p", value_desc = "v",
    valid = c("continuous", "text"), batch_size = 50)
  expect_true(all(is.na(res)))   # invalid values rejected -> NA
})

test_that(".strip_llm_wrapper removes the object-wrapper prefix", {
  # Providers that require an object-wrapped array return prefixed columns
  # (assignments.index); the helper restores the bare inner names.
  df <- data.frame(assignments.index = 1:2, assignments.group = c("ex1", "shared"))
  out <- .strip_llm_wrapper(df, "assignments")
  expect_identical(names(out), c("index", "group"))
  # no-op when the prefix is absent, and NULL passes through
  bare <- data.frame(index = 1L, group = "x")
  expect_identical(.strip_llm_wrapper(bare, "assignments"), bare)
  expect_null(.strip_llm_wrapper(NULL, "assignments"))
})
