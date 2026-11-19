# Unit tests for the codebook parsing + column matching helpers (rules path).
# LLM tiers are exercised only in the module and require an LLM, so are not here.
# cbc_dir()/cbc_run(), used by test-module-codebook_check.R too, now live in
# helper.R (see that file's own comment on why).

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

# ── DDI: value labels / code lists, missing scheme, question ──────────────────

test_that("value-label JSON round-trips through encode/decode", {
  s <- metacheck:::.encode_value_labels(c(1, 2, -99),
                                        c("Male", "Female", "Refused"))
  expect_true(grepl("Male", s))
  vl <- metacheck:::.decode_value_labels(s)
  expect_equal(unname(vl[["1"]]), "Male")
  expect_equal(unname(vl[["-99"]]), "Refused")
  expect_null(metacheck:::.decode_value_labels(NA_character_))
})

test_that(".parse_value_label_text parses common coding encodings", {
  expect_equal(
    metacheck:::.decode_value_labels(
      metacheck:::.parse_value_label_text("1 = Male; 2 = Female"))[["2"]],
    "Female")
  expect_equal(
    metacheck:::.decode_value_labels(
      metacheck:::.parse_value_label_text("0: no | 1: yes"))[["1"]],
    "yes")
  # A single pair is not a real mapping.
  expect_true(is.na(metacheck:::.parse_value_label_text("1 = only")))
  expect_true(is.na(metacheck:::.parse_value_label_text("")))
})

test_that(".extract_haven_labels harvests value labels and missing codes", {
  skip_if_not_installed("haven")
  df <- data.frame(x = 1:6)
  df$sex <- haven::labelled_spss(
    c(1, 2, -99, 1, 2, -99), labels = c(Male = 1, Female = 2, Refused = -99),
    na_values = -99, label = "Sex")
  res <- metacheck:::.extract_haven_labels(df, "study.sav")
  srow <- res[res$codebook_variable == "sex", ]
  expect_equal(metacheck:::.decode_value_labels(srow$value_labels)[["1"]], "Male")
  # -99 is a declared missing (and named "Refused") → in the missing scheme.
  expect_false(is.na(srow$missing_values))
  expect_true(grepl("-99", srow$missing_values))
})

test_that(".extract_structured_codebook reads values / question columns", {
  cb <- data.frame(
    variable = c("sex", "age"),
    label    = c("Sex", "Age"),
    values   = c("1 = Male; 2 = Female; -99 = Refused", ""),
    question = c("What is your sex?", "How old are you?"),
    stringsAsFactors = FALSE)
  res <- metacheck:::.extract_structured_codebook(cb, "cb.csv")
  expect_equal(metacheck:::.decode_value_labels(res$value_labels[1])[["1"]], "Male")
  expect_true(is.na(res$value_labels[2]))
  expect_equal(res$question[1], "What is your sex?")
  # "-99 = Refused" contributes to the missing scheme from a text codebook.
  expect_false(is.na(res$missing_values[1]))
  expect_true(grepl("-99", res$missing_values[1]))
})

test_that("match_column_labels carries DDI properties onto data columns", {
  cols <- data.frame(paper_id = "p", source_file = "d.csv",
                     column_name = "sex", stringsAsFactors = FALSE)
  cbk <- data.frame(
    codebook_variable = "sex", label = "Sex", codebook_source = "cb.csv",
    group = NA_character_,
    value_labels = '{"1":"Male","2":"Female"}',
    missing_values = NA_character_,
    question = "What is your sex?",
    coding_instructions = "recoded from raw gender field",
    parse_method = "structured", stringsAsFactors = FALSE)
  res <- match_column_labels(cols, cbk)
  expect_equal(res$value_labels, '{"1":"Male","2":"Female"}')
  expect_equal(res$question, "What is your sex?")
  expect_equal(res$coding_instructions, "recoded from raw gender field")
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

test_that("data_group_llm falls back to a single study when there is no grouping evidence", {
  # Materials files with no repo/path evidence are still sent toward grouping
  # (every file needs a real study — there is no 'shared' bucket), but with no
  # mocked LLM here the call fails gracefully and every file falls back to
  # 'ex1', the single/default study, exactly as it would with llm_use(FALSE).
  f <- data.frame(
    file_name = c("fig1.png", "photo.jpg", "manual.pdf"),
    data_type = c("materials", "materials", "materials"),
    stringsAsFactors = FALSE)
  out <- data_group_llm(f)
  expect_equal(out$group, rep("ex1", 3))
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
  # asked again in halves, then counted as unanswered (#673)
  expect_warning(res <- metacheck:::.llm_classify_batched(
    c("a", "b"), system_prompt = "p", value_desc = "v",
    valid = c("continuous", "text"), batch_size = 50),
    "2 of 2 items got no usable answer")
  expect_true(all(is.na(res)))   # invalid values rejected -> NA
  expect_identical(attr(res, "llm_unanswered"), 1:2)
})

test_that(".llm_classify_batched asks again for a batch whose answer was cut off (#673)", {
  # A batch of more than 25 items is cut off at the default length limit
  # (`limit`), or at any limit (`long`): the batch is asked again with four
  # times the limit, then in halves; only what is still unanswered is counted.
  calls <- list()
  mode <- "limit"
  fake_llm <- function(text, text_col = "text", params = list(), ...) {
    listing <- text[[text_col]][1]
    n <- length(strsplit(listing, "\n", fixed = TRUE)[[1]])
    calls[[length(calls) + 1L]] <<- list(n = n, max_tokens = params$max_tokens %||% NA)
    fail <- switch(mode,
                   limit = (params$max_tokens %||% 4096) <= 4096 && n > 25,
                   long = n > 25,
                   always = TRUE)
    df <- if (fail) {
      warning("Note (not fatal): 1 of 1 LLM extraction failed and was left blank", call. = FALSE)
      data.frame(text = listing, .error = TRUE,
                 .error_msg = "Response was truncated because it hit the `max_tokens` limit.")
    } else {
      data.frame(results.index = seq_len(n), results.value = rep("text", n))
    }
    attr(df, "llm") <- list(model = "mock")
    df
  }
  testthat::local_mocked_bindings(llm = fake_llm)
  withr::local_options(metacheck.llm_max_tokens = NULL)
  items <- rep("item", 120)   # batches of 50, 50 and 20
  classify <- function(...) metacheck:::.llm_classify_batched(
    items, system_prompt = "p", value_desc = "v", valid = "text", batch_size = 50, ...)

  # cut off at the limit: once more with four times it, and llm()'s note is not shown
  expect_no_warning(res <- classify())
  expect_true(all(res == "text"))
  expect_length(attr(res, "llm_unanswered"), 0)
  expect_equal(vapply(calls, `[[`, 0, "n"), c(50, 50, 50, 50, 20))
  expect_equal(vapply(calls, `[[`, 0, "max_tokens"), c(NA, 4 * 4096, NA, 4 * 4096, NA))

  # a caller's own limit is not raised; the halves answer
  calls <- list()
  expect_no_warning(res <- classify(params = list(max_tokens = 2000)))
  expect_true(all(res == "text"))
  expect_true(all(vapply(calls, `[[`, 0, "max_tokens") == 2000))

  # too long at any limit: the halves answer
  mode <- "long"
  calls <- list()
  expect_no_warning(res <- classify())
  expect_true(all(res == "text"))
  expect_equal(vapply(calls, `[[`, 0, "n"), c(50, 50, 25, 25, 50, 50, 25, 25, 20))

  # never answered: every item counted, with one warning
  mode <- "always"
  expect_warning(res <- classify(), "120 of 120 items got no usable answer")
  expect_true(all(is.na(res)))
  expect_identical(attr(res, "llm_unanswered"), 1:120)
})

test_that(".strip_llm_wrapper removes the object-wrapper prefix", {
  # Providers that require an object-wrapped array return prefixed columns
  # (assignments.index); the helper restores the bare inner names.
  df <- data.frame(assignments.index = 1:2, assignments.group = c("ex1", "ex2"))
  out <- .strip_llm_wrapper(df, "assignments")
  expect_identical(names(out), c("index", "group"))
  # no-op when the prefix is absent, and NULL passes through
  bare <- data.frame(index = 1L, group = "x")
  expect_identical(.strip_llm_wrapper(bare, "assignments"), bare)
  expect_null(.strip_llm_wrapper(NULL, "assignments"))
})

test_that("match_column_labels: a codebook row without a label does not label the column (#520, item 1)", {
  cols <- data.frame(paper_id = "p", source_file = "d.csv",
                     column_name = c("age", "cond"), stringsAsFactors = FALSE)
  # .jasp/.omv columns read with haven, without variable labels, next to an
  # .xlsx dictionary that labels age; this stopped codebook_check with
  # "replacement has length zero"
  cbk <- data.frame(
    codebook_variable = c("age", "age", "age", "cond"),
    label = c(NA, NA, "Age in years", NA),
    codebook_source = c("a.jasp", "b.omv", "dict.xlsx", "a.jasp"),
    group = NA_character_,
    value_labels = c(NA, NA, NA, '{"1":"control","2":"treatment"}'),
    parse_method = c("haven", "haven", "structured", "haven"),
    stringsAsFactors = FALSE)
  expect_no_error(res <- match_column_labels(cols, cbk))
  expect_equal(res$label_status, c("labelled", "unlabelled"))
  expect_equal(res$label[1], "Age in years")
  expect_equal(res$label_source[1], "dict.xlsx")
  # no label, but its value labels still count
  expect_true(is.na(res$label[2]))
  expect_equal(res$value_labels[2], '{"1":"control","2":"treatment"}')
})
