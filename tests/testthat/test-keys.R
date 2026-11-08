# Keys in a repository's files (#607). The keys are made at random when the
# tests run, in the form of each provider's keys, so that no file holds one:
# GitHub refuses a push with a key it recognises.
fake_keys <- function(seed = 1) {
  withr::local_seed(seed)
  r <- function(n, chars) paste(sample(strsplit(chars, "")[[1]], n, replace = TRUE), collapse = "")
  hex <- "0123456789abcdef"
  an <- "ABCDEFGHIJKLMNOPQRSTUVWXYZabcdefghijklmnopqrstuvwxyz0123456789"
  list(
    "openrouter-api-key" = paste0("sk-or-v1-", r(64, hex)),
    "github-pat" = paste0("ghp_", r(36, an)),
    "github-fine-grained-pat" = paste0("github_pat_", r(82, paste0(an, "_"))),
    "aws-access-token" = paste0("AKIA", r(16, "ABCDEFGHIJKLMNOPQRSTUVWXYZ234567")),
    "slack-bot-token" = paste0("xoxb-", r(12, "0123456789"), "-", r(12, "0123456789"), "-", r(24, an)),
    "stripe-access-token" = paste0("sk_live_", r(24, an)),
    "anthropic-api-key" = paste0("sk-ant-api03-", r(93, paste0(an, "_-")), "AA"),
    "gcp-api-key" = paste0("AIza", r(35, paste0(an, "_-"))),
    "huggingface-access-token" = paste0("hf_", r(34, "abcdefghijklmnopqrstuvwxyzABCDEFGHIJKLMNOPQRSTUVWXYZ")),
    "openai-api-key" = paste0("sk-proj-", r(58, paste0(an, "_-")), "T3BlbkFJ", r(58, paste0(an, "_-"))),
    "gitlab-pat" = paste0("glpat-", r(20, an)),
    "groq-api-key" = paste0("gsk_", r(52, an))
  )
}
has_any <- function(keys, text) any(vapply(keys, grepl, logical(1), x = text, fixed = TRUE))

test_that("a key of each common type is found, in the forms code and files write it", {
  k <- fake_keys()
  for (n in names(k)) {
    for (s in c(paste("my key is", k[[n]], "for the API."),
                sprintf('api_key <- "%s"', k[[n]]),
                sprintf('"Authorization": "Bearer %s"', k[[n]]),
                sprintf('{"key":"%s","n":1}', k[[n]]))) {
      f <- .key_find(s)
      expect_true(n %in% f$rule, info = paste(n, "in", substr(s, 1, 30)))
      expect_true(k[[n]] %in% f$key, info = n)
    }
  }
})

test_that("placeholders, examples and ordinary text are not keys", {
  for (s in c(paste0("sk-or-v1-", strrep("x", 64)),
              paste0("ghp_", strrep("a", 36)),
              "api_key = 'YOUR_API_KEY'", "token: ${API_KEY}", "key = {{ secrets.KEY }}",
              "AKIAIOSFODNN7EXAMPLE",
              "The participants were asked to rate the stimuli.",
              "KeyError: 'response_time' in file data_cleaning.py",
              "")) {
    expect_equal(nrow(.key_find(s)), 0, info = s)
  }
  expect_equal(nrow(.key_find(NA_character_)), 0)
})

test_that("a key far into a long text is found, at its place", {
  k <- fake_keys(2)[["openrouter-api-key"]]
  s <- paste(strrep("participant,condition,1.234\n", 2000), "key =", k, strrep("x,y\n", 3000))
  f <- .key_find(s)
  expect_equal(f$key, k)
  expect_equal(substr(s, f$start, f$end), k)
})

test_that(".key_remove() replaces each key in a result, in tables and names too", {
  k <- fake_keys(3)
  res <- list(report = c("text", paste("found", k[["openrouter-api-key"]])),
              table = data.frame(a = c("x", k[["github-pat"]]), b = 1:2),
              nested = list(list(z = paste0("key=", k[["aws-access-token"]]))),
              f = factor(c("a", k[["gcp-api-key"]])))
  names(res$table)[1] <- k[["groq-api-key"]]
  out <- .key_remove(res)
  expect_false(has_any(k, paste(c(unlist(lapply(out, as.character)), unlist(out$nested),
                                  names(out$table), levels(out$f)), collapse = " ")))
  expect_equal(attr(out, "keys_removed"), 5L)
  expect_equal(out$report[[2]], "found [key removed]")
  expect_equal(out$table[[2, 1]], "[key removed]")
  expect_equal(names(out$table)[1], "[key removed]")
  expect_true(is.data.frame(out$table))
  expect_type(out$table$b, "integer")
  expect_equal(out$nested[[1]]$z, "key=[key removed]")
  # a result without a keyword is returned as it was
  plain <- list(report = "nothing here", table = data.frame(a = 1:3), x = NULL)
  expect_identical(.key_remove(plain), plain)
})

test_that("module_run() removes a key from any module's result", {
  k <- fake_keys(4)[["openrouter-api-key"]]
  d <- withr::local_tempdir()
  mod <- file.path(d, "leaky.R")
  writeLines(c(
    "#' Leaky", "#'", "#' @description", "#' A module that copies a key.", "#'",
    "#' @keywords general", "#'", "#' @param paper a paper", "#'", "#' @returns a list",
    "leaky <- function(paper) {",
    sprintf("  key <- paste0('%s', '%s')", substr(k, 1, 20), substr(k, 21, nchar(k))),
    "  list(table = data.frame(text = paste('Bearer', key)), report = paste('key:', key),",
    "       summary_text = key, traffic_light = 'info')",
    "}"), mod)
  mo <- module_run(test_paper("x"), mod)
  expect_false(grepl(k, paste(unlist(mo[c("table", "report", "summary_text")]), collapse = " "), fixed = TRUE))
  expect_equal(mo$table$text, "Bearer [key removed]")
  expect_null(attr(mo, "keys_removed"))
})

test_that(".key_scan_files() gives file, line, type and only the beginning of a key", {
  k <- fake_keys(5)
  d <- withr::local_tempdir()
  # Windows line endings, a Latin-1 character, a key on the last line without
  # a line ending
  writeBin(charToRaw(paste0("x <- 1\r\n# caf\xe9\r\ntoken <- '", k[["github-pat"]], "'")),
           file.path(d, "a.R"))
  writeBin(as.raw(c(0, 1, 2, 0, 255)), file.path(d, "blob.bin"))
  writeLines(c("a", "b", paste("k1 =", k[["openrouter-api-key"]], "; k2 =", k[["aws-access-token"]])),
             file.path(d, "two.txt"))
  f <- .key_scan_files(file.path(d, c("a.R", "blob.bin", "two.txt", "missing.txt")),
                       names = c("a.R", "blob.bin", "two.txt", "missing.txt"))
  expect_equal(f$file, c("a.R", "two.txt", "two.txt"))
  expect_equal(f$line, c(3L, 3L, 3L))
  expect_setequal(f$rule, c("github-pat", "openrouter-api-key", "aws-access-token"))
  expect_equal(f$key, 1:3)
  expect_false(has_any(k, paste(unlist(f), collapse = " ")))
  expect_true(all(endsWith(f$shown, "...")))
  expect_equal(f$shown[f$rule == "github-pat"], paste0(substr(k[["github-pat"]], 1, 6), "..."))
})

test_that("data_check warns about keys in an R file, a .qsf file and a .env file, without copying them (#607)", {
  llm_use(FALSE)
  k <- fake_keys(6)
  d <- withr::local_tempdir()
  dir.create(file.path(d, "code"))
  writeLines(c("library(httr)", sprintf('key <- "%s"', k[["openrouter-api-key"]])),
             file.path(d, "code", "analysis.R"))
  # a Qualtrics survey that calls the service, with the key in its header
  qsf <- list(SurveyEntry = list(SurveyName = "Climate"), SurveyElements = list(list(
    Element = "SQ", PrimaryAttribute = "QID1",
    Payload = list(QuestionText = "How worried are you?", DataExportTag = "worry",
                   QuestionType = "MC", Choices = list(`1` = list(Display = "Not"), `2` = list(Display = "Very")),
                   QuestionJS = sprintf('fetch(u, {headers: {"Authorization": "Bearer %s"}})',
                                        k[["anthropic-api-key"]])))))
  writeLines(jsonlite::toJSON(qsf, auto_unbox = TRUE), file.path(d, "survey.qsf"))
  writeLines(c(sprintf("GITHUB_TOKEN=%s", k[["github-pat"]]), "MODE=test"), file.path(d, ".env"))
  utils::write.csv(data.frame(id = 1:20, worry = rep(1:2, 10)), file.path(d, "data.csv"), row.names = FALSE)

  mo <- module_run(test_paper("x"), "data_check", local_path = d, local_only = TRUE)
  expect_equal(mo$traffic_light, "red")
  expect_setequal(mo$keys$file, c("code/analysis.R", "survey.qsf", ".env"))
  expect_setequal(mo$keys$rule, c("openrouter-api-key", "anthropic-api-key", "github-pat"))
  expect_equal(mo$summary_table$key_n, 3)
  expect_match(mo$summary_text, "We found 3 keys", fixed = TRUE)
  rep <- module_report(mo)
  expect_match(rep, "Keys in the repository", fixed = TRUE)
  expect_false(has_any(k, rep))
  expect_false(has_any(k, paste(unlist(mo[setdiff(names(mo), "previews")]), collapse = " ")))
  # codebook_check reads the .qsf file: no key in its result either
  mo2 <- module_run(test_paper("x"), "codebook_check", local_path = d, local_only = TRUE)
  expect_false(has_any(k, paste(unlist(mo2), collapse = " ")))

  # a folder without keys: no warning
  d2 <- withr::local_tempdir()
  utils::write.csv(data.frame(id = 1:5, x = 1:5), file.path(d2, "data.csv"), row.names = FALSE)
  writeLines("x <- read.csv('data.csv')", file.path(d2, "a.R"))
  mo3 <- module_run(test_paper("x"), "data_check", local_path = d2, local_only = TRUE)
  expect_equal(nrow(mo3$keys), 0)
  expect_equal(mo3$summary_table$key_n, 0)
  expect_false(grepl("Keys in the repository", module_report(mo3), fixed = TRUE))
})

test_that("of a data table only the column names and first rows are searched, which hold a survey's embedded key (#607)", {
  k <- fake_keys(8)[["openrouter-api-key"]]
  # a survey's key as embedded data, in every response (as in 6qf9p_v3); the
  # question text has a line break, as Qualtrics' do
  responses <- data.frame(StartDate = sprintf("2026-01-0%d", 1:9), Q1 = "How\nworried?",
                          OpenRouterAPIKey = paste("Bearer", k))
  # a key typed by one participant far down a table is not searched for
  late <- data.frame(id = 1:500, answer = c(rep("ok", 498), paste("my key", k), "ok"))
  seen <- new.env()
  f <- .key_scan_tables(list(responses.csv = responses, late.csv = late), seen = seen)
  expect_equal(f$table, rep("responses.csv", 3))
  expect_equal(f$row, 1:3)
  expect_equal(f$key, rep(1L, 3))
  expect_false(grepl(k, paste(unlist(f), collapse = " "), fixed = TRUE))
  # a key in a column name is found as row 0
  hdr <- data.frame(x = 1); names(hdr) <- k
  expect_equal(.key_scan_tables(list(h.csv = hdr))$row, 0L)
  # data files are left to .key_scan_tables(); other files are searched whole
  d <- withr::local_tempdir()
  writeLines(c("id,answer", sprintf("%d,%s", 1:3, k)), file.path(d, "data.csv"))
  writeLines(c(sprintf("# line %d", 1:498), sprintf("key <- '%s'", k)), file.path(d, "late.R"))
  files <- data.frame(paper_id = "p", repo_url = d, file_name = c("data.csv", "late.R"),
                      file_path = c("data.csv", "late.R"), file_location = file.path(d, c("data.csv", "late.R")),
                      data_type = c("data", "code"))
  g <- .key_scan_repo_files(files, seen = seen)
  expect_equal(g$file, "late.R")
  expect_equal(g$line, 499L)
  # the same key as in the table: the same number
  expect_equal(g$key, 1L)
})

test_that("encoded data (an image in a knitted page) is not searched, and not changed", {
  k <- fake_keys(9)[["github-pat"]]
  withr::local_seed(10)
  b64 <- paste(sample(c(LETTERS, letters, 0:9, "+", "/"), 3000, replace = TRUE), collapse = "")
  # a run that holds what looks like a Brave Search key, as in a corpus page
  b64 <- paste0(substr(b64, 1, 1000), "/BSA", paste(sample(c(LETTERS, letters, 0:9), 30, TRUE), collapse = ""),
                "/", substr(b64, 1001, 3000))
  page <- paste0('<p>Results</p>\n<img src="data:image/png;base64,', b64, '">\n<p>token ', k, '</p>')
  f <- .key_find(page)
  expect_equal(f$rule, "github-pat")
  expect_equal(substr(page, f$start, f$end), k)
  d <- withr::local_tempdir()
  writeLines(page, file.path(d, "report.html"))
  expect_equal(.key_scan_files(file.path(d, "report.html"))$line, 3L)
  # the image passes .key_remove() as it was; the key next to it does not
  out <- .key_remove(list(report = page))
  expect_true(grepl(b64, out$report, fixed = TRUE))
  expect_false(grepl(k, out$report, fixed = TRUE))
})

test_that("a rule made for some files only is tried on those files only", {
  d <- withr::local_tempdir()
  line <- 'password = "x7Kq2-Lm9Pz"'
  writeLines(line, file.path(d, "main.tf"))
  writeLines(line, file.path(d, "Test.java"))
  f <- .key_scan_files(file.path(d, c("main.tf", "Test.java")), names = c("main.tf", "Test.java"))
  expect_equal(f$file[f$rule == "hashicorp-tf-password"], "main.tf")
  # without a file, as in a module's result, such a rule is not tried
  expect_false("hashicorp-tf-password" %in% .key_find(line)$rule)
})

test_that("the rules are betterleaks' rules of high and medium confidence, all readable by R", {
  x <- .key_rules()
  expect_gt(nrow(x$patterns), 400)
  expect_setequal(unique(x$patterns$confidence), c("high", "medium"))
  expect_equal(x$source$licence, "MIT")
  readable <- vapply(x$patterns$regex, function(rx) !inherits(tryCatch(grepl(rx, "", perl = TRUE),
                                                              error = function(e) e), "error"), logical(1))
  expect_true(all(readable))
  expect_true(all(lengths(x$patterns$keywords) > 0))
})
