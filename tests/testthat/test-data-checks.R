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

test_that("data_col_facets splits type into orthogonal properties", {
  # An identifier: text representation, identifier role, id concept.
  f <- data_col_facets("subject_id", c("s01", "s02", "s03"))
  expect_equal(f$representation, "text")
  expect_equal(f$role, "identifier")
  expect_equal(f$concept, "id")

  # Age: numeric / ratio / age concept / years, independent axes.
  f <- data_col_facets("age", c(23, 45, 31, 29, 55, 19, 67, 40))
  expect_equal(f$representation, "numeric")
  expect_equal(f$measurement_level, "ratio")
  expect_equal(f$concept, "age")
  expect_equal(f$unit, "years")

  # Reaction time: concept + unit inferred from magnitude (ms vs s).
  expect_equal(data_col_facets("RT", c(543, 612, 498, 701, 555))$unit,
               "milliseconds")
  expect_equal(data_col_facets("rt_s", c(0.54, 0.61, 0.49, 0.70, 0.55))$unit,
               "seconds")

  # Likert item: ordinal level, likert concept.
  f <- data_col_facets("panas_1", sample(1:5, 60, TRUE))
  expect_equal(f$measurement_level, "ordinal")
  expect_equal(f$concept, "likert")

  # A comma-decimal column is numeric with a parse note, not a fake type.
  f <- data_col_facets("price", c("1,50", "2,30", "4,10", "5,00", "3,25",
                                  "6,60", "2,10", "1,90"))
  expect_equal(f$representation, "numeric")
  expect_equal(f$parse_note, "comma_decimal")

  # Constant / empty are a quality state, not a type.
  expect_equal(data_col_facets("k", rep(7, 10))$quality, "constant")
  expect_equal(data_col_facets("e", c(NA, NA))$quality, "empty")
})

test_that("data_col_concept detects concepts by name+value agreement", {
  expect_equal(data_col_concept("RT", c(543, 612, 498, 701)), "reaction_time")
  expect_equal(data_col_concept("accuracy", c(1, 0, 1, 1, 0)), "accuracy")
  expect_equal(data_col_concept("correct",
    c("correct", "incorrect", "correct")), "accuracy")
  expect_equal(data_col_concept("condition", c(1, 2, 1, 2)), "condition")
  expect_equal(data_col_concept("age", c(23, 45, 31)), "age")
  # A plain "time" clock column is not a reaction time.
  expect_true(is.na(data_col_concept("time", c("10:00", "10:05", "10:10"))) ||
              data_col_concept("time", c("10:00", "10:05", "10:10")) == "timestamp")
  # An RT-named column with impossible negative values is rejected.
  expect_true(is.na(data_col_concept("rt", rep(-5, 10))))
})

test_that("data_analysis_unit infers the unit of observation", {
  # Person-level: unique id per row.
  persons <- data.frame(subject_id = 1:30, age = sample(18:65, 30, TRUE))
  expect_equal(data_analysis_unit(persons, "subject_id")$unit, "person")

  # Trial-level: repeating id + a trial column.
  trials <- data.frame(subject_id = rep(1:5, each = 8), trial = rep(1:8, 5),
                       rt = runif(40, 300, 900))
  expect_equal(data_analysis_unit(trials, "subject_id")$unit, "trial")

  # Session/repeated-measures: repeating id + a wave column.
  sessions <- data.frame(id = rep(1:10, each = 3), wave = rep(1:3, 10),
                         score = rnorm(30))
  expect_equal(data_analysis_unit(sessions, "id")$unit, "session")

  # Dyad: two identifier columns.
  dyads <- data.frame(actor_id = rep(1:5, 2), partner_id = rep(6:10, 2),
                      liking = rnorm(10))
  expect_equal(data_analysis_unit(dyads, c("actor_id", "partner_id"))$unit, "dyad")

  # id inferred by name when not supplied.
  expect_equal(data_analysis_unit(persons)$unit, "person")

  # All-NA id column: 0/0 unique fraction must not error (regression: an id
  # column with no non-NA values gave frac_unique = NaN and `if (NaN >= 0.98)`
  # threw "missing value where TRUE/FALSE needed").
  na_ids <- data.frame(subject_id = rep(NA_character_, 5), value = 1:5)
  expect_no_error(res <- data_analysis_unit(na_ids, "subject_id"))
  expect_false(identical(res$unit, "person"))
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

# ── Demographic-column detection ──────────────────────────────────────────────

test_that("data_check_demographic detects age with name+value agreement", {
  expect_equal(data_check_demographic("age", c(23, 45, 31, 29, 55, 19, 67, 40)), "age")
  expect_equal(data_check_demographic("Age_years", 18:35), "age")
  expect_equal(data_check_demographic("participant_age", c(34, 29, 41)), "age")
  # A missing-data sentinel in an otherwise valid age column still counts.
  expect_equal(data_check_demographic("age", c(23, 45, 31, 29, 55, 19, 67, 999)), "age")
  # Name looks like age but values are out of human range (birth years) → NA.
  expect_true(is.na(data_check_demographic("age", c(1990, 1985, 2001, 1979, 1995, 2003))))
  # Name looks like age but values are text → NA.
  expect_true(is.na(data_check_demographic("age", c("young", "old", "middle", "young", "old"))))
})

test_that("data_check_demographic does not fire on age false-friend names", {
  # "age" is a substring of many unrelated column names; none must be flagged.
  for (nm in c("percentage", "page", "average_rt", "image_id", "coverage",
               "storage", "language", "damage", "usage", "agent"))
    expect_true(is.na(data_check_demographic(nm, c(10, 20, 30, 40, 50))),
                info = nm)
})

test_that("data_check_demographic detects gender/sex from words or coding", {
  expect_equal(data_check_demographic("gender",
    c("Male", "Female", "Female", "Male", "Non-binary")), "gender")
  expect_equal(data_check_demographic("sex", c(1, 2, 1, 2, 1, 2)), "gender")
  expect_equal(data_check_demographic("Geslacht", c("man", "vrouw", "man")), "gender")
  # Name matches but values are unrelated categories → NA.
  expect_true(is.na(data_check_demographic("gender",
    c("apple", "banana", "cherry", "kiwi", "melon"))))
  # A 1/2 condition code must NOT be read as gender (name does not match).
  expect_true(is.na(data_check_demographic("condition", c(1, 2, 1, 2, 1, 2))))
})

test_that("data_check_demographic detects race/ethnicity categories", {
  expect_equal(data_check_demographic("race",
    c("White", "Black", "Asian", "Hispanic", "Other")), "race")
  expect_equal(data_check_demographic("ethnicity", c("Hispanic", "Non-Hispanic")), "race")
  expect_equal(data_check_demographic("race_ethnicity", c(1, 2, 3, 4, 5)), "race")
  # Long free text under a race-like name is not a race category → NA.
  expect_true(is.na(data_check_demographic("race",
    rep(paste(rep("word", 30), collapse = " "), 5))))
})

test_that("data_check_demographic handles empty / NA names", {
  expect_true(is.na(data_check_demographic("", c(1, 2, 3))))
  expect_true(is.na(data_check_demographic(NA_character_, c(1, 2, 3))))
  expect_true(is.na(data_check_demographic(character(0), c(1, 2, 3))))
})

test_that("data_check tags demographic columns in its table", {
  llm_use(FALSE)
  d <- file.path(tempdir(), "demo_repo"); unlink(d, recursive = TRUE)
  dir.create(file.path(d, "data"), recursive = TRUE)
  utils::write.csv(
    data.frame(
      id     = 1:20,
      age    = sample(18:65, 20, TRUE),
      gender = sample(c("Male", "Female"), 20, TRUE),
      race   = sample(c("White", "Black", "Asian", "Other"), 20, TRUE),
      score  = rnorm(20)),
    file.path(d, "data", "study.csv"), row.names = FALSE)

  mo <- module_run(test_paper("x"), "data_check",
                   local_path = d, local_only = TRUE)
  expect_true(all(c("representation", "measurement_level", "concept", "role")
                  %in% names(mo$table)))
  sem <- setNames(mo$table$concept, mo$table$column_name)
  expect_equal(unname(sem["age"]), "age")
  expect_equal(unname(sem["gender"]), "gender")
  expect_equal(unname(sem["race"]), "race")
  expect_true(is.na(sem[["score"]]))
  # Orthogonal facets: age is a ratio-level numeric measure in years.
  arow <- mo$table[mo$table$column_name == "age", ]
  expect_equal(arow$representation, "numeric")
  expect_equal(arow$measurement_level, "ratio")
  expect_equal(arow$unit, "years")
  # id column → identifier role, id concept.
  irow <- mo$table[mo$table$column_name == "id", ]
  expect_equal(irow$role, "identifier")
})

# ── Qualtrics detection ───────────────────────────────────────────────────────

test_that("data_check_is_qualtrics fires on the metadata column set", {
  q <- data.frame(
    StartDate = "2021-01-01", EndDate = "2021-01-01", Progress = 100,
    `Duration (in seconds)` = 300, Finished = 1, ResponseId = "R_abc123de",
    Q1 = 3, check.names = FALSE)
  expect_true(data_check_is_qualtrics(q))
  # An ordinary data frame that merely has a StartDate must NOT be flagged.
  expect_false(data_check_is_qualtrics(
    data.frame(id = 1:3, StartDate = c("a", "b", "c"), score = 1:3)))
  expect_false(data_check_is_qualtrics(data.frame(a = 1:3, b = 4:6)))
})

test_that("data_check_is_qualtrics corroborates a thin export via ResponseId", {
  # Only two metadata names, but ResponseId values are Qualtrics response ids.
  q <- data.frame(
    StartDate = rep("2021-01-01 10:00:00", 3), Progress = c(100, 100, 50),
    ResponseId = c("R_abc123de", "R_xyz789gh", "R_qwe456rt"),
    Q1 = 1:3, check.names = FALSE)
  expect_true(data_check_is_qualtrics(q))
})

test_that("data_strip_qualtrics_header removes header rows and re-types", {
  df <- data.frame(
    Progress = c("Progress", '{"ImportId":"progress"}', "100", "100"),
    `Duration (in seconds)` = c("Duration (in seconds)",
                                '{"ImportId":"duration"}', "300", "120"),
    ResponseId = c("Response ID", '{"ImportId":"_recordId"}',
                   "R_abc", "R_xyz"),
    check.names = FALSE, stringsAsFactors = FALSE)
  out <- data_strip_qualtrics_header(df)
  expect_equal(nrow(out), 2)
  expect_true(is.numeric(out[["Duration (in seconds)"]]))
  expect_equal(out[["Duration (in seconds)"]], c(300, 120))
  expect_true(is.character(out$ResponseId))  # not numeric, left as text
})

test_that("data_check tags Qualtrics metadata columns in its table", {
  llm_use(FALSE)
  d <- file.path(tempdir(), "q_tag_repo"); unlink(d, recursive = TRUE)
  dir.create(file.path(d, "data"), recursive = TRUE)
  qf <- file.path(d, "data", "survey.csv")
  q <- function(...) paste0('"', c(...), '"', collapse = ",")
  writeLines(c(
    q("StartDate", "Status", "Progress", "Duration (in seconds)", "Finished",
      "ResponseId", "Q1"),
    q("Start Date", "Response Type", "Progress", "Duration (in seconds)",
      "Finished", "Response ID", "How happy?"),
    q('{"ImportId":"startDate"}', '{"ImportId":"status"}',
      '{"ImportId":"progress"}', '{"ImportId":"duration"}',
      '{"ImportId":"finished"}', '{"ImportId":"_recordId"}',
      '{"ImportId":"QID1"}'),
    vapply(1:20, function(i) q(sprintf("2021-05-%02d 10:00:00", i), 0, 100,
      sample(200:900, 1), 1, sprintf("R_abc%05d", i), sample(1:5, 1)),
      character(1))), qf)

  mo <- module_run(test_paper("x"), "data_check",
                   local_path = d, local_only = TRUE)
  sem <- setNames(mo$table$concept, mo$table$column_name)
  expect_equal(unname(sem["Duration (in seconds)"]), "qualtrics_duration")
  expect_equal(unname(sem["Status"]), "qualtrics_status")
  expect_equal(unname(sem["Finished"]), "qualtrics_finished")
  # The substantive question column is not a Qualtrics metadata concept (a 1-5
  # rating column is recognised as a likert item by its values, not qualtrics_*).
  expect_false(grepl("^qualtrics_", sem[["Q1"]] %||% ""))
})

# ── .RData sharing recommendation ─────────────────────────────────────────────

test_that("data_check recommends sharing data when an .RData holds no data", {
  skip_if_not_installed("processx")
  llm_use(FALSE)
  d <- file.path(tempdir(), "rdata_repo"); unlink(d, recursive = TRUE)
  dir.create(file.path(d, "data"), recursive = TRUE)
  # A model-only workspace (no data frame) -> should be flagged.
  a_model <- lm(mpg ~ cyl, mtcars)
  save(a_model, file = file.path(d, "data", "analysis_workspace.RData"))
  # A real CSV so the repo has readable data too.
  utils::write.csv(data.frame(id = 1:5, x = rnorm(5)),
                   file.path(d, "data", "study.csv"), row.names = FALSE)

  mo <- module_run(test_paper("x"), "data_check",
                   local_path = d, local_only = TRUE)
  st <- paste(mo$summary_text, collapse = "\n")
  expect_match(st, "R workspace file")
  expect_match(st, "analysis_workspace.RData")
})

test_that("data_check does not flag an .RData that holds a data frame", {
  skip_if_not_installed("processx")
  llm_use(FALSE)
  d <- file.path(tempdir(), "rdata_ok"); unlink(d, recursive = TRUE)
  dir.create(file.path(d, "data"), recursive = TRUE)
  study_data <- data.frame(id = 1:5, score = rnorm(5))
  save(study_data, file = file.path(d, "data", "clean_data.RData"))

  mo <- module_run(test_paper("x"), "data_check",
                   local_path = d, local_only = TRUE)
  expect_false(grepl("R workspace file", paste(mo$summary_text, collapse = "\n")))
})
