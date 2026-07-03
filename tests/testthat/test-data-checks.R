# Unit tests for the native data-quality check functions and the file/column
# classification helpers in R/data_check_helpers.R. All deterministic + offline.

# ── Data-quality checks ───────────────────────────────────────────────────────

test_that("data_check_outliers flags Tukey outliers", {
  r <- data_check_outliers(c(1:10, 500))
  expect_true(r$problem)
  expect_true(500 %in% r$values)
  # no outliers in clean symmetric data
  expect_false(data_check_outliers(c(1:20))$problem)
  # too few / constant → no problem
  expect_false(data_check_outliers(c(1, 2))$problem)
  expect_false(data_check_outliers(rep(5, 20))$problem)
  # non-numeric → no problem
  expect_false(data_check_outliers(c("a", "b"))$problem)
})

test_that("data_check_miscoded_missing flags recurring extreme sentinels", {
  x <- c(rnorm(50, 20, 3), -99, -99)
  r <- data_check_miscoded_missing(x)
  expect_true(r$problem)
  expect_true(-99 %in% r$values)
  # a single sentinel is not flagged (ambiguous)
  expect_false(data_check_miscoded_missing(c(rnorm(50, 20, 3), -99))$problem)
  # no sentinels
  expect_false(data_check_miscoded_missing(rnorm(50))$problem)
})

test_that("data_check_constant flags constant and near-constant columns", {
  expect_true(data_check_constant(rep(5, 10))$problem)
  expect_true(data_check_constant(c(rep("a", 99), "b"))$problem)
  expect_false(data_check_constant(c(rep("a", 5), rep("b", 5)))$problem)
})

test_that("data_check_case_issues flags case-only duplicate categories", {
  r <- data_check_case_issues(c("Male", "male", "Female"))
  expect_true(r$problem)
  expect_false(data_check_case_issues(c("Male", "Female"))$problem)
  expect_false(data_check_case_issues(c(1, 2, 3))$problem)
})

test_that("data_check_sparse_levels flags rare categories", {
  r <- data_check_sparse_levels(c(rep("a", 20), "b"))
  expect_true(r$problem)
  expect_true("b" %in% r$values)
  expect_false(data_check_sparse_levels(c(rep("a", 10), rep("b", 10)))$problem)
})

test_that("data_check_whitespace flags padded values", {
  r <- data_check_whitespace(c("Male ", "Male", "Female"))
  expect_true(r$problem)
  expect_false(data_check_whitespace(c("Male", "Female"))$problem)
  expect_false(data_check_whitespace(c(1, 2, 3))$problem)
})

test_that("data_check_numeric_in_text flags contaminated numeric columns", {
  r <- data_check_numeric_in_text(c(as.character(1:20), "n/a", ">100"))
  expect_true(r$problem)
  expect_true("n/a" %in% r$values)
  # clean categorical text → not flagged
  expect_false(data_check_numeric_in_text(c("apple", "pear", "kiwi", "plum", "fig"))$problem)
  # fully numeric text → not flagged (readers auto-type these)
  expect_false(data_check_numeric_in_text(as.character(1:20))$problem)
})

# ── File classification ───────────────────────────────────────────────────────

test_that("data_classify_files classifies by name and extension", {
  cl <- data_classify_files(c("data.csv", "analysis.R", "README.md",
                              "codebook.xlsx", "photo.png"))
  expect_equal(cl[[1]], "data")
  expect_equal(cl[[2]], "code")
  expect_equal(cl[[3]], "readme")
  expect_equal(cl[[5]], "asset")
  expect_equal(length(data_classify_files(character(0))), 0)
})

test_that("data_format separates tabular from raw", {
  expect_equal(data_format("csv"), "tabular")
  expect_equal(data_format("sav"), "tabular")
  expect_equal(data_format("edf"), "raw")
  expect_equal(data_format("mp4"), "raw")
})

test_that("data_is_manifest detects a file-listing masquerading as data", {
  repo <- c("Study 1.r", "Study 1.csv", "notes.txt")
  manifest <- data.frame(
    type = c("code", "data", "doc"),
    file = c("Study 1.r", "Study 1.csv", "notes.txt"),
    stringsAsFactors = FALSE)
  expect_true(data_is_manifest(manifest, repo))
  # a real data frame whose cells are not repo files
  real <- data.frame(id = 1:3, score = c(1.1, 2.2, 3.3))
  expect_false(data_is_manifest(real, repo))
})

# ── Column typing + stats ─────────────────────────────────────────────────────

test_that("data_col_type applies the rule ladder", {
  expect_equal(data_col_type("subject_id", c("s01", "s02", "s03"))$col_type, "id")
  expect_equal(data_col_type("x", rep(1, 10))$col_type, "constant")
  expect_equal(data_col_type("cond", c("a", "b", "a", "b"))$col_type, "binary")
  expect_equal(data_col_type("age", c(23.5, 45.1, 31.9, 29.2, 55.7))$col_type,
               "continuous")
  # empty
  expect_equal(data_col_type("e", c(NA, NA))$col_type, "empty")
})

test_that("data_col_stats returns numeric summaries", {
  s <- data_col_stats(c(1, 2, 3, 4, 5), c(1, 2, 3, 4, 5))
  expect_equal(s$n, 5)
  expect_equal(s$mean, 3)
  expect_equal(s$min, 1)
  expect_equal(s$max, 5)
  # NULL numeric values → empty stats but n/n_missing from raw
  s2 <- data_col_stats(NULL, c("a", "b", NA))
  expect_true(is.na(s2$mean))
  expect_equal(s2$n_missing, 1)
})

# ── PII / disclosure checks ───────────────────────────────────────────────────

test_that("data_check_pii_values detects value patterns without false positives", {
  expect_true(data_check_pii_values(c("a@b.com", "c@d.org", "e@f.net"))$problem)
  expect_true(data_check_pii_values(c("192.168.0.1", "10.0.0.255", "8.8.8.8"))$problem)
  expect_true(data_check_pii_values(c("123-45-6789", "987-65-4321", "222-33-4444"))$problem)
  # Luhn-valid test card numbers.
  expect_true(data_check_pii_values(
    c("4111111111111111", "5500005555555559", "4012888888881881"))$problem)
  # Ordinary numeric / categorical data must not be flagged.
  expect_false(data_check_pii_values(c("1", "2", "3", "4", "5", "3", "2"))$problem)
  expect_false(data_check_pii_values(as.character(seq(10, 30, length.out = 40)))$problem)
  expect_false(data_check_pii_values(c("yes", "no", "maybe", "yes", "no"))$problem)
  # The report names the pattern, not the matching value (no PII leak).
  r <- data_check_pii_values(c("a@b.com", "c@d.org", "e@f.net"))
  expect_false(grepl("@", r$message))
})

test_that("data_check_pii_values flags rare-but-real specific PII (no false negatives)", {
  # A specific identifier is a disclosure even in a small fraction of the column.
  expect_true(data_check_pii_values(c(rep("n/a", 38), "a@b.com", "c@d.com"))$problem)
  expect_true(data_check_pii_values(c(rep("none", 39), "x@y.com"))$problem)   # single email
  expect_true(data_check_pii_values(c(rep("na", 197), "a@b.com", "c@d.org", "e@f.net"))$problem)
})

test_that("data_check_pii_values validates to avoid false positives", {
  # Credit-card shape but fails the Luhn checksum -> not flagged.
  expect_false(data_check_pii_values(rep("1234567890123456", 4))$problem)
  # 16-digit run embedded in a longer alphanumeric id -> not a card.
  expect_false(data_check_pii_values(rep("A1234567890123456Z", 6))$problem)
  # Dates and date-like strings are not flagged.
  expect_false(data_check_pii_values(rep("2021-05-03", 6))$problem)
  expect_false(data_check_pii_values(as.character(sample(1990:2020, 40, TRUE)))$problem)
  # Long integer IDs and prices are not PII.
  expect_false(data_check_pii_values(as.character(sample(1e8:9e8, 40)))$problem)
  expect_false(data_check_pii_values(sprintf("%.2f", runif(40, 1, 100)))$problem)
})

test_that("data_check_pii_values does not flag phone-shaped or timestamp data", {
  # The phone value-pattern was removed: it collided with Qualtrics timestamps
  # (StartDate/EndDate) on essentially every survey export. Datetime strings and
  # phone-shaped strings must therefore NOT be flagged as PII values.
  expect_false(data_check_pii_values(rep("2019-01-15 14:32:07", 10))$problem)  # was the main FP
  expect_false(data_check_pii_values(rep("+31612345678", 5))$problem)          # a real phone: no longer detected
  expect_false(data_check_pii_values(rep("040-247 1234", 8))$problem)
})

test_that("data_check_pii_name flags identifying column names only", {
  expect_true(data_check_pii_name("participant_email")$problem)
  expect_true(data_check_pii_name("DOB")$problem)
  expect_true(data_check_pii_name("home_address")$problem)
  expect_true(data_check_pii_name("latitude")$problem)
  # Specific person-name compounds still flag.
  expect_true(data_check_pii_name("RecipientFirstName")$problem)
  expect_true(data_check_pii_name("PeronalData_fullname")$problem)
  expect_true(data_check_pii_name("IPAddress")$problem)
  # Short tokens must be the whole name, not a substring of an ordinary word.
  expect_false(data_check_pii_name("description")$problem)  # contains "ip"
  expect_false(data_check_pii_name("score")$problem)
  expect_false(data_check_pii_name("reaction_time")$problem)
  # The bare "name" token was removed: names ending in/containing "name" that do
  # not identify a person must NOT be flagged (real false positives from OSF
  # repos: experimentName, trial_name, videoName, fileName, conditionName, ...).
  expect_false(data_check_pii_name("experimentName")$problem)
  expect_false(data_check_pii_name("trial_name")$problem)
  expect_false(data_check_pii_name("videoName")$problem)
  expect_false(data_check_pii_name("fileName")$problem)
  expect_false(data_check_pii_name("Conditionname")$problem)
  expect_false(data_check_pii_name("Rolename")$problem)
})

test_that("data_check_pii_geo requires a geographic name, not just a value range", {
  # Geographic name + in-range values → flagged.
  expect_true(data_check_pii_geo("latitude", c(52.37, 4.89, 51.5))$problem)
  expect_true(data_check_pii_geo("gps", c(1, 2, 3))$problem)
  # Ordinary decimals in coordinate range but a non-geographic name → not flagged.
  expect_false(data_check_pii_geo("x", seq(10, 30, length.out = 40))$problem)
  expect_false(data_check_pii_geo("temperature", c(20.1, 21.5, 19.8))$problem)
})

test_that("data_check_pii_freetext flags real prose but not codes or non-prose", {
  # Genuine open typed responses (multi-word, alphabetic, distinct) are flagged.
  expect_true(data_check_pii_freetext(c(
    "I really enjoyed the study and thought it was interesting overall today.",
    "The instructions were a bit unclear at the start but fine later on here.",
    "My name is Jane and I live in Amsterdam near the central train station.",
    "Great experience overall, I would happily participate again next time.",
    "The room felt cold and I had some trouble focusing on the main tasks."))$problem)

  # Short category codes are not free text.
  expect_false(data_check_pii_freetext(
    c("yes", "no", "yes", "maybe", "no", "yes", "no"))$problem)

  # Non-prose long/varied columns must NOT fire (the previous behaviour flagged
  # these on real repos). They are long and distinct but not typed language:
  # numeric matrices, IDs/hashes, URLs, file paths.
  expect_false(data_check_pii_freetext(
    as.character(round(rnorm(30, 1000, 200), 4)))$problem)                 # numeric matrix column
  expect_false(data_check_pii_freetext(
    paste0("R_", replicate(30, paste(sample(c(0:9, LETTERS), 16, TRUE),
                                     collapse = ""))))$problem)            # Qualtrics response IDs
  expect_false(data_check_pii_freetext(
    paste0("https://example.com/path/", 1:30, "/item?x=", 1:30))$problem)  # URLs
  expect_false(data_check_pii_freetext(
    paste0("C:/data/study/participant_", 1:30, "/trial_data.csv"))$problem) # file paths
})
