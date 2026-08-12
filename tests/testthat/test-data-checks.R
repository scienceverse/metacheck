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

test_that(".detect_likert_scale infers scale ranges robustly", {
  set.seed(1)
  f <- metacheck:::.detect_likert_scale

  # Clean scales across the target envelope (0-based, 1-based, bipolar).
  expect_equal(f(sample(1:5, 200, TRUE))[c("lo","hi")],  list(lo = 1L, hi = 5L))
  expect_equal(f(sample(1:7, 200, TRUE))[c("lo","hi")],  list(lo = 1L, hi = 7L))
  expect_equal(f(sample(0:10, 300, TRUE))[c("lo","hi")], list(lo = 0L, hi = 10L))
  expect_equal(f(sample(-5:5, 300, TRUE))[c("lo","hi")], list(lo = -5L, hi = 5L))
  expect_equal(f(sample(-11:11, 400, TRUE))[c("lo","hi")], list(lo = -11L, hi = 11L))

  # Sparse coverage: an unobserved floor is inferred down to the natural start
  # (1, or 0 when a 0 is present), and the inference is reported.
  r <- f(sample(2:5, 200, TRUE))
  expect_equal(c(r$lo, r$hi), c(1L, 5L))
  expect_equal(r$floor_inferred, 1L)
  r <- f(sample(3:5, 200, TRUE))
  expect_equal(r$floor_inferred, c(1L, 2L))
  expect_equal(f(sample(c(0,2,3,4,5), 200, TRUE))[c("lo","hi")], list(lo = 0L, hi = 5L))

  # Interior gaps are bridged (a 1-7 scale where nobody picked 3 or 4).
  expect_equal(f(sample(c(1,2,5,6,7), 200, TRUE))[c("lo","hi")], list(lo = 1L, hi = 7L))

  # Contaminants become suspects, not part of the range.
  expect_equal(f(c(sample(1:5, 200, TRUE), 99))$suspects, 99)
  expect_equal(f(c(sample(1:7, 200, TRUE), 33))$suspects, 33)   # mistyped 3->33
  expect_equal(f(c(sample(1:6, 200, TRUE), 8))$suspects, 8)     # gapped overshoot
  expect_equal(f(c(sample(1:5, 200, TRUE), -99, -99))$suspects, -99)

  # A lone ADJACENT overshoot extends the scale (a 6 next to 1-5 means it goes
  # to 6, rarely used) — it is NOT a suspect.
  r <- f(c(sample(1:5, 200, TRUE), 6))
  expect_equal(c(r$lo, r$hi), c(1L, 6L))
  expect_length(r$suspects, 0)

  # Non-scales are rejected.
  expect_null(f(sample(18:65, 200, TRUE)))              # age (wide)
  expect_null(f(round(rlnorm(200, log(650), .35))))     # continuous RT
  expect_null(f(sample(c(10, 20, 30), 200, TRUE)))      # coded groups (gapped, non-consecutive)
  expect_null(f(rpois(200, 8)))                         # count
  expect_null(f(rnorm(200)))                            # non-integer
  expect_null(f(c(1, 2, 3)))                            # too few values
})

test_that("data_check_scale_values flags out-of-scale values and classifies them", {
  set.seed(1)
  # Likert 1-5 with a stray 55 (typo of 5) and -99 (missing code): both flagged,
  # range [1, 5], each classified.
  r <- data_check_scale_values(c(sample(1:5, 200, TRUE), 55, -99))
  expect_true(r$problem)
  expect_setequal(r$values, c(-99, 55))
  expect_equal(c(r$lower, r$upper), c(1, 5))
  expect_equal(r$classes[r$values == -99], "missing")
  expect_equal(r$classes[r$values == 55], "typo:5")

  # A mistyped 33 on a 1-7 scale is recognized as a typo of 3.
  r <- data_check_scale_values(c(sample(1:7, 200, TRUE), 33))
  expect_equal(r$classes, "typo:3")

  # A stray 9 on a 1-7 scale is out of scale but not a digit-typo -> unexplained.
  expect_equal(data_check_scale_values(c(sample(1:7, 200, TRUE), 9))$classes,
               "unexplained")

  # Clean scales, and a lone ADJACENT overshoot (6 next to 1-5 -> scale is 1-6),
  # are not flagged.
  expect_false(data_check_scale_values(sample(1:7, 200, TRUE))$problem)
  expect_false(data_check_scale_values(c(sample(1:5, 200, TRUE), 6))$problem)

  # Non-scale numeric columns have no valid range and are out of scope.
  expect_false(data_check_scale_values(round(rnorm(200, 120, 15)))$problem) # BP
  expect_false(data_check_scale_values(rnorm(200))$problem)                 # non-integer
  expect_false(data_check_scale_values(sample(18:65, 200, TRUE))$problem)   # age
  expect_false(data_check_scale_values(rpois(200, 3))$problem)              # count

  # Ground truth overrides inference.
  expect_true(data_check_scale_values(c(sample(1:5, 200, TRUE), 6),
                                      valid_values = 1:5)$problem)   # 6 flagged
  expect_false(data_check_scale_values(c(sample(1:5, 200, TRUE), 6),
                                       valid_values = 1:6)$problem)  # 6 declared valid
  expect_true(data_check_scale_values(c(sample(1:7, 60, TRUE), 9),
                                      valid_range = c(1, 7))$problem)

  # Declared missing codes (codebook ground truth) are classified as missing,
  # even outside the hardcoded sentinel list (77, -8), and only when present.
  r <- data_check_scale_values(c(sample(1:5, 200, TRUE), 77), declared = 77)
  expect_equal(r$classes, "missing")
  expect_true(data_check_scale_values(c(sample(1:7, 200, TRUE), -8),
                                      declared = -8)$problem)
  expect_false(data_check_scale_values(sample(1:5, 200, TRUE),
                                       declared = 77)$problem)

  # Endpoint-only valid values (e.g., 2 and 8) are interpreted as a bounded
  # contiguous range when interior values are observed; floor anchors to 1.
  r <- data_check_scale_values(sample(2:8, 200, TRUE), valid_values = c(2, 8))
  expect_false(r$problem)
  expect_equal(c(r$lower, r$upper), c(1, 8))

  # Interior observed values are not mislabeled as out-of-scale, while true
  # detached values still are.
  r <- data_check_scale_values(c(sample(2:8, 200, TRUE), 9999),
                               valid_values = c(2, 8))
  expect_true(r$problem)
  expect_setequal(r$values, 9999)
  expect_equal(c(r$lower, r$upper), c(1, 8))

  # Consecutive valid values that start above 1 are also floor-anchored.
  r <- data_check_scale_values(sample(2:8, 200, TRUE), valid_values = 2:8)
  expect_false(r$problem)
  expect_equal(c(r$lower, r$upper), c(1, 8))

  # Guards.
  expect_false(data_check_scale_values(c("a", "b"))$problem)
  expect_false(data_check_scale_values(numeric(0))$problem)
})

test_that("data_check_constant flags constant and near-constant columns", {
  r <- data_check_constant(rep(5, 10))
  expect_true(r$problem)
  expect_false(r$near)
  # Near-constant results are marked as such, so callers can treat them
  # differently (rare outcomes are legitimately 99% one value).
  r <- data_check_constant(c(rep("a", 99), "b"))
  expect_true(r$problem)
  expect_true(r$near)
  expect_false(data_check_constant(c(rep("a", 5), rep("b", 5)))$problem)
})

test_that("data_check_empty flags all-missing columns", {
  expect_true(data_check_empty(rep(NA_real_, 5))$problem)
  # Blank / whitespace-only text counts as missing.
  expect_true(data_check_empty(c(NA, "", "  "))$problem)
  expect_false(data_check_empty(c(NA, 1))$problem)
  expect_false(data_check_empty(numeric(0))$problem)
})

test_that("data_check_design_name detects design/condition variable names", {
  expect_true(data_check_design_name("condition"))
  expect_true(data_check_design_name("exp_cond"))
  expect_true(data_check_design_name("Group"))
  expect_true(data_check_design_name("treatment_arm"))
  expect_true(data_check_design_name("cond1"))
  expect_false(data_check_design_name("age"))
  expect_false(data_check_design_name("charm"))    # 'arm' needs a word boundary
  expect_false(data_check_design_name("response"))
})

test_that("data_check_spss_filter flags SPSS Select Cases filter columns", {
  # Constant at 1: the file was saved after deleting unselected cases.
  r <- data_check_spss_filter("filter_$", rep(1, 10))
  expect_true(r$problem)
  expect_true(grepl("pre-filtered", r$message))
  # Varying: analyses likely used only the selected rows.
  r <- data_check_spss_filter("filter_.", c(1, 1, 1, 0, 0))
  expect_true(r$problem)
  expect_true(grepl("3 of 5", r$message))
  # Only the SPSS default name matches, not arbitrary columns of 0/1.
  expect_false(data_check_spss_filter("excluded", c(1, 1, 0))$problem)
})

test_that("data_check_case_issues flags case-only duplicate categories", {
  r <- data_check_case_issues(c("Male", "male", "Female"))
  expect_true(r$problem)
  expect_false(data_check_case_issues(c("Male", "Female"))$problem)
  expect_false(data_check_case_issues(c(1, 2, 3))$problem)
})

test_that("data_check_whitespace flags padded values", {
  r <- data_check_whitespace(c("Male ", "Male", "Female"))
  expect_true(r$problem)
  expect_false(data_check_whitespace(c("Male", "Female"))$problem)
  expect_false(data_check_whitespace(c(1, 2, 3))$problem)
})

test_that("data_check_colname flags file-illegal, padded, and over-long names", {
  # File-illegal characters (the Emergent .dat header-as-name case).
  emergent <- "_H:\t$Name\t%Input[4:0,0,0,0]<4:1,8,1,1>\t%Input[4:0,1,0,0]"
  r <- data_check_colname(emergent)
  expect_true(r$problem)
  expect_true(":" %in% r$values)
  expect_true("\t" %in% r$values)

  # Each illegal character class flags on its own.
  expect_true(data_check_colname("score:pre")$problem)
  expect_true(data_check_colname("a\tb")$problem)           # control char
  expect_true(data_check_colname('he said "hi"')$problem)   # quote
  expect_true(data_check_colname("ratio a/b")$problem)      # slash

  # Padding flags.
  expect_true(data_check_colname(" score ")$problem)

  # Length: over 64 characters flags (SPSS's maximum variable-name length);
  # 64 or under does not.
  expect_true(data_check_colname(strrep("a", 65))$problem)
  expect_false(data_check_colname(strrep("a", 64))$problem)

  # Ordinary names — including spaces, dots and unicode — are fine.
  expect_false(data_check_colname("reaction_time")$problem)
  expect_false(data_check_colname("orginal hip.go value")$problem)
  expect_false(data_check_colname("âge")$problem)
  expect_false(data_check_colname(NA_character_)$problem)
})

test_that("data_check_colname_collisions finds names that sanitize identically", {
  # IPA phoneme columns: "t'" and "t̪" (t + combining dental diacritic)
  # both sanitize to "t_" (the codebook / make.names collision case).
  r <- data_check_colname_collisions(c("t'", "t̪", "score", "kw"))
  expect_setequal(names(r), c("t'", "t̪"))
  expect_match(r[["t'"]], "cannot tell these columns apart")

  # Identical duplicate names collide too.
  d <- data_check_colname_collisions(c("id", "id", "x"))
  expect_true("id" %in% names(d))
  expect_match(d[["id"]], "identically named column")

  # Distinct-after-sanitization names do not collide; unicode letters are
  # kept, so "k" and "kʷ" (kʷ, modifier letter) stay distinct.
  expect_length(data_check_colname_collisions(c("a", "b", "a_1")), 0)
  expect_length(data_check_colname_collisions(c("k", "kʷ")), 0)
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
  files <- c("data.csv", "analysis.R", "README.md", "codebook.xlsx",
            "photo.png", "experiment.psyexp", "notes.pdf", "archive.zip")
  cl <- data_classify_files(files)
  expect_equal(cl[[1]], "data")
  expect_equal(cl[[2]], "code")
  expect_equal(cl[[3]], "documentation")   # readme folds into documentation
  expect_equal(cl[[4]], "documentation")   # codebook folds into documentation
  expect_equal(cl[[5]], "materials")       # asset folds into materials
  expect_equal(cl[[6]], "materials")       # software (.psyexp) folds into materials
  expect_equal(cl[[7]], "documentation")   # supplemental (PDF) folds into documentation
  expect_equal(cl[[8]], "unknown")         # a never-opened archive has unknown content
  expect_equal(length(data_classify_files(character(0))), 0)

  # doc_role distinguishes WHICH documentation artifact this is.
  roles <- .data_doc_role(files)
  expect_equal(roles[[3]], "readme")
  expect_equal(roles[[4]], "codebook")
  expect_equal(roles[[7]], "supplemental")
  expect_true(is.na(roles[[1]]))    # not documentation at all
  expect_true(is.na(roles[[5]]))

  # ro-crate-metadata.json is treated as a readme role (collection-level).
  expect_equal(.data_doc_role("ro-crate-metadata.json"), "readme")
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

  # Likert is a property of a scale BLOCK, not of one column: whole numbers over
  # a narrow range describe a trial counter (`round`, `block`, a PsychoPy loop
  # index) as readily as a rating item. The caller, which has the whole data
  # frame, passes `in_scale_block`.
  f <- data_col_facets("panas_1", sample(1:5, 60, TRUE), in_scale_block = TRUE)
  expect_equal(f$measurement_level, "ordinal")
  expect_equal(f$concept, "likert")

  # Without block context the same values claim nothing, rather than guessing.
  f <- data_col_facets("panas_1", sample(1:5, 60, TRUE))
  expect_true(is.na(f$concept))

  # A trial counter has identical value characteristics and must NOT be likert.
  f <- data_col_facets("round", sample(1:5, 60, TRUE), in_scale_block = FALSE)
  expect_true(is.na(f$concept))

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

test_that("data_check_pii_geo finds a coordinate inside a compound name", {
  # The name is word-split, so a coordinate is recognised however it is spelled
  # out. `LocationLatitude` is Qualtrics' own column name and was previously
  # missed, because the check only accepted a bare "lat"/"latitude".
  sib <- c("LocationLatitude", "LocationLongitude")
  expect_true(data_check_pii_geo("LocationLatitude",
                                 c(52.37, 4.89, 51.5), sib)$problem)
  expect_true(data_check_pii_geo("gps_lat", c(52.37, 4.89, 51.5),
                                 c("gps_lat", "gps_lon"))$problem)
})

test_that("data_check_pii_geo requires a partner column for lat/lon", {
  # A real coordinate is a PAIR. In psychology data "lat" is as often latency
  # or a lateralisation index, and "lon" a loneliness scale — none of which has
  # the matching sibling column a genuine coordinate always has.
  set.seed(1)
  psych <- c("id", "lat", "rt", "accuracy")          # no lon
  expect_false(data_check_pii_geo("lat", round(runif(200, .3, 3.5), 3),
                                  psych)$problem)    # latency in seconds
  expect_false(data_check_pii_geo("lat", sample(0:1, 200, TRUE),
                                  psych)$problem)    # Latin-square code
  expect_false(data_check_pii_geo("lon", sample(1:7, 200, TRUE),
                                  c("id", "lon", "rt"))$problem)  # loneliness

  # The same column IS flagged once its partner is present.
  expect_true(data_check_pii_geo("lat", round(runif(200, 50, 54), 3),
                                 c("id", "lat", "lon"))$problem)

  # A name that IS a coordinate outright needs no partner.
  expect_true(data_check_pii_geo("gps", runif(20, -10, 10), psych)$problem)

  # Values outside the coordinate range are rejected whatever the siblings say.
  expect_false(data_check_pii_geo("lat", round(rlnorm(200, log(650), .4)),
                                  c("id", "lat", "lon"))$problem)
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

test_that("data_strip_qualtrics_header strips both rows on a long survey", {
  # Regression: the question-text row used to be recognised by the FRACTION of
  # cells matching a Qualtrics metadata label. Qualtrics writes a fixed set of
  # metadata columns however long the questionnaire is, so on a real survey that
  # fraction is tiny (measured: 13 matches in 139 columns = 0.09) and the row was
  # not recognised. Stripping then stopped at row 1 and never reached the
  # `ImportId` row behind it, leaving both junk rows in the data — which made
  # every rating column a character vector of question text.
  meta <- c("StartDate", "EndDate", "Progress", "Duration (in seconds)",
            "Finished", "RecordedDate", "ResponseId")
  items <- paste0("q_", 1:60)                     # a long questionnaire
  nm <- c(meta, items)

  question_row <- c("Start Date", "End Date", "Progress", "Duration (in seconds)",
                    "Finished", "Recorded Date", "Response ID",
                    paste("Question text for item", 1:60))
  import_row <- sprintf('{"ImportId":"%s"}', nm)
  data_rows <- lapply(seq_along(nm), function(i)
    if (i <= length(meta)) rep("x", 3) else as.character(sample(1:7, 3, TRUE)))

  q <- as.data.frame(rbind(question_row, import_row,
                           do.call(cbind, data_rows)),
                     stringsAsFactors = FALSE)
  names(q) <- nm

  expect_true(data_check_is_qualtrics(q))
  stripped <- data_strip_qualtrics_header(q)
  expect_equal(nrow(stripped), nrow(q) - 2L)
  # The item columns are numeric again once the two text rows are gone.
  expect_true(is.numeric(stripped[["q_1"]]))
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

# ── Careless responding ───────────────────────────────────────────────────────

# Build a survey fixture: `n_ok` varied respondents on a Likert scale, plus a
# straightliner (same answer throughout) and an alternating responder, with an
# id column. `seed` keeps the varied respondents reproducible.
make_careless_survey <- function(prefix = "panas", n_items = 10, n_ok = 50,
                                 levels = 2:4, seed = 1) {
  set.seed(seed)
  items <- as.data.frame(matrix(sample(levels, n_ok * n_items, replace = TRUE),
                                nrow = n_ok))
  names(items) <- paste0(prefix, "_", seq_len(n_items))
  straight <- as.data.frame(matrix(rep(median(levels), n_items), nrow = 1))
  alternating <- as.data.frame(matrix(rep(range(levels), length.out = n_items),
                                      nrow = 1))
  names(straight) <- names(alternating) <- names(items)
  items <- rbind(items, straight, alternating)
  cbind(participant_id = seq_len(n_ok + 2), items)
}

test_that("data_check flags a straightliner but not an alternating responder", {
  skip_if_not_installed("careless")
  llm_use(FALSE)
  d <- file.path(tempdir(), "dc_careless"); unlink(d, recursive = TRUE)
  dir.create(file.path(d, "data"), recursive = TRUE)
  utils::write.csv(make_careless_survey(), file.path(d, "data", "survey.csv"),
                   row.names = FALSE)

  mo <- module_run(test_paper("x"), "data_check",
                   local_path = d, local_only = TRUE)
  expect_true("careless" %in% names(mo))
  expect_equal(anyDuplicated(mo$careless$respondent), 0L)

  # The straightliner (id 51) answered identically on all 10 items.
  hit <- mo$careless[mo$careless$respondent == "51", ]
  expect_equal(nrow(hit), 1L)
  expect_equal(hit$max_longstring, 10)
  # The threshold text states the run and the cut it crossed.
  expect_match(hit$threshold, "same answer 10 times in a row")

  # The alternating responder (id 52) has high response variability. High IRV
  # is NOT a flag: it cannot be told apart from an engaged respondent using the
  # whole scale, so only straightlining flags anyone.
  expect_false("52" %in% mo$careless$respondent)
  expect_true(all(mo$careless$reasons == "straightlining"))
})

test_that("data_check does not screen scale blocks below the item minimum", {
  skip_if_not_installed("careless")
  llm_use(FALSE)
  # A 3-item block: identical answers there are ordinary (on 3 items with few
  # levels it happens by chance to a sizeable share of honest respondents), so
  # the block must not be screened at all. Every respondent here straightlines.
  d <- file.path(tempdir(), "dc_shortblock"); unlink(d, recursive = TRUE)
  dir.create(file.path(d, "data"), recursive = TRUE)
  flat <- data.frame(participant_id = 1:40,
                     q_1 = rep(3L, 40), q_2 = rep(3L, 40), q_3 = rep(3L, 40))
  utils::write.csv(flat, file.path(d, "data", "survey.csv"), row.names = FALSE)

  mo <- module_run(test_paper("x"), "data_check",
                   local_path = d, local_only = TRUE)
  expect_equal(nrow(mo$careless), 0)
})

test_that("data_check reports careless coverage limits when nothing is flagged", {
  skip_if_not_installed("careless")
  llm_use(FALSE)
  # A screenable survey in which nobody straightlines: the report must still
  # say what the check cannot see, so a clean result is not read as an
  # all-clear for the whole dataset.
  d <- file.path(tempdir(), "dc_clean"); unlink(d, recursive = TRUE)
  dir.create(file.path(d, "data"), recursive = TRUE)
  set.seed(7)
  items <- as.data.frame(matrix(sample(1:5, 40 * 10, replace = TRUE), nrow = 40))
  names(items) <- paste0("panas_", 1:10)
  # Guarantee no run reaches the 8-of-10 cut.
  items[] <- lapply(items, function(x) x)
  items$panas_1 <- rep(c(1L, 5L), length.out = 40)
  utils::write.csv(cbind(participant_id = 1:40, items),
                   file.path(d, "data", "survey.csv"), row.names = FALSE)

  mo <- module_run(test_paper("x"), "data_check",
                   local_path = d, local_only = TRUE)
  rp <- paste(mo$report, collapse = "\n")
  expect_true(grepl("#### Careless Responding", rp, fixed = TRUE))
  expect_match(rp, "What this does not cover")
  expect_match(rp, "not evidence that a dataset is free of careless responding")
})

test_that("careless scale blocks split by variable-name prefix", {
  skip_if_not_installed("careless")
  llm_use(FALSE)
  d <- file.path(tempdir(), "dc_two_scales"); unlink(d, recursive = TRUE)
  dir.create(file.path(d, "data"), recursive = TRUE)
  # Two adjacent scales on the same 1-5 metric: panas_1..8 then rse_1..6. They
  # must be detected as TWO blocks, not merged, because the prefix changes.
  s1 <- make_careless_survey("panas", n_items = 8, n_ok = 40, levels = 1:5, seed = 2)
  s2 <- make_careless_survey("rse",   n_items = 6, n_ok = 40, levels = 1:5, seed = 3)
  wide <- cbind(s1, s2[, -1, drop = FALSE])  # drop duplicate id from s2
  utils::write.csv(wide, file.path(d, "data", "survey.csv"), row.names = FALSE)

  mo <- module_run(test_paper("x"), "data_check",
                   local_path = d, local_only = TRUE)
  scales <- unique(unlist(strsplit(mo$careless$scales, "; ")))
  expect_true(any(grepl("^panas", scales)))
  expect_true(any(grepl("^rse", scales)))
})

test_that("data_check does not run careless without an id or a scale block", {
  skip_if_not_installed("careless")
  llm_use(FALSE)
  # A survey block but NO identifier column: careless is not actionable, skipped.
  d <- file.path(tempdir(), "dc_noid"); unlink(d, recursive = TRUE)
  dir.create(file.path(d, "data"), recursive = TRUE)
  s <- make_careless_survey()[, -1, drop = FALSE]   # drop participant_id
  utils::write.csv(s, file.path(d, "data", "survey.csv"), row.names = FALSE)
  mo <- module_run(test_paper("x"), "data_check",
                   local_path = d, local_only = TRUE)
  expect_equal(nrow(mo$careless), 0)

  # Non-survey data (no Likert block): careless produces nothing either.
  d2 <- file.path(tempdir(), "dc_nonsurvey"); unlink(d2, recursive = TRUE)
  dir.create(file.path(d2, "data"), recursive = TRUE)
  utils::write.csv(data.frame(id = 1:40, rt = rnorm(40, 500, 50),
                              age = sample(18:65, 40, replace = TRUE)),
                   file.path(d2, "data", "d.csv"), row.names = FALSE)
  mo2 <- module_run(test_paper("x"), "data_check",
                    local_path = d2, local_only = TRUE)
  expect_equal(nrow(mo2$careless), 0)
})

# ── Spreadsheet formatting checks (merged from the former spreadsheet_check) ──
# Flags non-machine-readable spreadsheet formatting (colour coding, merged
# cells, empty rows, empty/unnamed columns) as part of data_check. Runs offline
# against fixture files built in tempdir(); no network, no LLM. Requires
# openxlsx to build the .xlsx fixtures.
#
# Ported from the former data_validate module tests. The findings frame moved
# from that module's `table` to data_check's `findings`; `table` now holds the
# per-column table, so file/sheet-level findings are read from `findings`.

# Build a repository fixture with one "messy" and one "clean" Excel file.
make_excel_repo <- function() {
  d <- file.path(tempdir(), paste0("xl_fix_", as.integer(runif(1, 1, 1e6))))
  dir.create(file.path(d, "data"), recursive = TRUE, showWarnings = FALSE)

  # Messy workbook: colour-coded cells, a merged range, an all-empty column.
  wb <- openxlsx::createWorkbook()
  openxlsx::addWorksheet(wb, "Data")
  df <- data.frame(id = 1:4, grp = c("a", "b", "a", "b"),
                   empty = rep(NA, 4), val = c(10, 20, 30, 40))
  openxlsx::writeData(wb, "Data", df)
  openxlsx::addStyle(wb, "Data", openxlsx::createStyle(fgFill = "#FFCC00"),
                     rows = 2, cols = 4)
  openxlsx::addStyle(wb, "Data", openxlsx::createStyle(fgFill = "#00CCFF"),
                     rows = 3, cols = 4)
  openxlsx::mergeCells(wb, "Data", cols = 1:2, rows = 7)
  openxlsx::saveWorkbook(wb, file.path(d, "data", "messy.xlsx"), overwrite = TRUE)

  # Clean workbook: a plain rectangular table.
  openxlsx::write.xlsx(data.frame(id = 1:3, score = c(1.1, 2.2, 3.3)),
                       file.path(d, "data", "clean.xlsx"))
  d
}

test_that("data_check flags spreadsheet colour, merges and empty columns", {
  skip_if_not_installed("openxlsx")
  llm_use(FALSE)
  d <- make_excel_repo()
  mo <- module_run(test_paper("x"), "data_check",
                   local_path = d, local_only = TRUE)

  expect_equal(mo$traffic_light, "yellow")
  st <- mo$summary_table
  expect_equal(st$spreadsheet_file_n, 2)
  expect_equal(st$spreadsheet_flagged_file_n, 1)   # only messy.xlsx has issues

  # The findings table names each issue type for the messy file, with
  # column = NA (these are file/sheet-level, not column-level, findings).
  sheet_finds <- mo$findings[is.na(mo$findings$column), ]
  expect_true(any(grepl("Colour", sheet_finds$check)))
  expect_true(any(grepl("Merged", sheet_finds$check)))
  expect_true(any(grepl("Empty or unnamed", sheet_finds$check)))
  # Scope is always reported.
  expect_true(any(grepl("examined 2 spreadsheet files", mo$report)))
})

test_that("data_check spreadsheet checks are clean when Excel files are clean", {
  skip_if_not_installed("openxlsx")
  llm_use(FALSE)
  d <- file.path(tempdir(), paste0("xl_clean_", as.integer(runif(1, 1, 1e6))))
  dir.create(file.path(d, "data"), recursive = TRUE, showWarnings = FALSE)
  openxlsx::write.xlsx(data.frame(id = 1:3, score = c(1.1, 2.2, 3.3)),
                       file.path(d, "data", "clean.xlsx"))

  mo <- module_run(test_paper("x"), "data_check",
                   local_path = d, local_only = TRUE)
  expect_equal(mo$summary_table$spreadsheet_file_n, 1)
  expect_equal(mo$summary_table$spreadsheet_flagged_file_n, 0)
  expect_false(any(grepl("Colour|Merged|Empty",
                         mo$findings$check[is.na(mo$findings$column)])))
})

# ── OpenDocument (.ods) ───────────────────────────────────────────────────────
#
# The .ods fixtures are written as raw ODF XML rather than through a writer:
# readODS::write_ods() cannot produce cell fills or merged ranges, which are
# exactly the features under test. Writing the XML also pins the two structures
# that make ODS different from OOXML — implicit cell positions and the
# `number-rows-repeated` / `number-columns-repeated` counters that compress
# blank runs — so a regression in the counter expansion is caught here.
.write_ods_fixture <- function(path, content_xml) {
  build <- file.path(tempdir(), paste0("odsb_", as.integer(runif(1, 1, 1e9))))
  dir.create(file.path(build, "META-INF"), recursive = TRUE,
             showWarnings = FALSE)
  writeLines("application/vnd.oasis.opendocument.spreadsheet",
             file.path(build, "mimetype"), sep = "")
  writeLines(paste0(
    '<?xml version="1.0" encoding="UTF-8"?>',
    '<manifest:manifest xmlns:manifest="urn:oasis:names:tc:opendocument:xmlns:manifest:1.0" manifest:version="1.2">',
    '<manifest:file-entry manifest:full-path="/" manifest:media-type="application/vnd.oasis.opendocument.spreadsheet"/>',
    '<manifest:file-entry manifest:full-path="content.xml" manifest:media-type="text/xml"/>',
    '</manifest:manifest>'), file.path(build, "META-INF", "manifest.xml"))
  writeLines(content_xml, file.path(build, "content.xml"))

  wd <- setwd(build)
  on.exit(setwd(wd), add = TRUE)
  utils::zip(path, c("mimetype", "META-INF/manifest.xml", "content.xml"),
             flags = "-r9Xq")
  path
}

.ods_header <- paste0(
  '<?xml version="1.0" encoding="UTF-8"?>',
  '<office:document-content',
  ' xmlns:office="urn:oasis:names:tc:opendocument:xmlns:office:1.0"',
  ' xmlns:table="urn:oasis:names:tc:opendocument:xmlns:table:1.0"',
  ' xmlns:text="urn:oasis:names:tc:opendocument:xmlns:text:1.0"',
  ' xmlns:style="urn:oasis:names:tc:opendocument:xmlns:style:1.0"',
  ' xmlns:fo="urn:oasis:names:tc:opendocument:xmlns:xsl-fo-compatible:1.0"',
  ' office:version="1.2">')

test_that("data_check flags colour and merges in .ods files", {
  llm_use(FALSE)
  skip_if_not_installed("readODS")

  d <- file.path(tempdir(), paste0("ods_fix_", as.integer(runif(1, 1, 1e6))))
  dir.create(file.path(d, "data"), recursive = TRUE, showWarnings = FALSE)

  # A merged banner (A1:B1), one red cell, and three blank rows written as a
  # SINGLE row element with number-rows-repeated="3".
  messy <- paste0(.ods_header,
    '<office:automatic-styles>',
    '<style:style style:name="ceRed" style:family="table-cell">',
    '<style:table-cell-properties fo:background-color="#ff0000"/></style:style>',
    # white/transparent must NOT count as colour coding
    '<style:style style:name="ceWhite" style:family="table-cell">',
    '<style:table-cell-properties fo:background-color="#ffffff"/></style:style>',
    '</office:automatic-styles>',
    '<office:body><office:spreadsheet>',
    '<table:table table:name="Data">',
    '<table:table-row>',
    '<table:table-cell table:number-columns-spanned="2" table:number-rows-spanned="1" office:value-type="string"><text:p>banner</text:p></table:table-cell>',
    '<table:covered-table-cell/>',
    '<table:table-cell office:value-type="string"><text:p>val</text:p></table:table-cell>',
    '</table:table-row>',
    '<table:table-row>',
    '<table:table-cell table:style-name="ceRed" office:value-type="float" office:value="1"><text:p>1</text:p></table:table-cell>',
    '<table:table-cell table:style-name="ceWhite" office:value-type="float" office:value="2"><text:p>2</text:p></table:table-cell>',
    '<table:table-cell office:value-type="float" office:value="3"><text:p>3</text:p></table:table-cell>',
    '</table:table-row>',
    '<table:table-row table:number-rows-repeated="3"><table:table-cell table:number-columns-repeated="3"/></table:table-row>',
    '<table:table-row>',
    '<table:table-cell office:value-type="float" office:value="9"><text:p>9</text:p></table:table-cell>',
    '<table:table-cell table:number-columns-repeated="2"/>',
    '</table:table-row>',
    '</table:table></office:spreadsheet></office:body></office:document-content>')
  .write_ods_fixture(file.path(d, "data", "messy.ods"), messy)

  mo <- module_run(test_paper("x"), "data_check",
                   local_path = d, local_only = TRUE)

  expect_equal(mo$traffic_light, "yellow")
  st <- mo$summary_table
  expect_equal(st$spreadsheet_file_n, 1)
  expect_equal(st$spreadsheet_flagged_file_n, 1)

  sheet_finds <- mo$findings[is.na(mo$findings$column), ]
  expect_true(any(grepl("Colour", sheet_finds$check)))
  expect_true(any(grepl("Merged", sheet_finds$check)))
  # The merge range is synthesised into the same A1:B1 form the xlsx path uses.
  expect_true(any(grepl("A1:B1", sheet_finds$detail)))
  expect_true(any(grepl("examined 1 spreadsheet file\\b", mo$report)))
})

test_that("data_check spreadsheet checks are clean for a clean .ods file", {
  llm_use(FALSE)
  skip_if_not_installed("readODS")

  d <- file.path(tempdir(), paste0("ods_clean_", as.integer(runif(1, 1, 1e6))))
  dir.create(file.path(d, "data"), recursive = TRUE, showWarnings = FALSE)

  clean <- paste0(.ods_header,
    '<office:body><office:spreadsheet><table:table table:name="Data">',
    '<table:table-row>',
    '<table:table-cell office:value-type="string"><text:p>id</text:p></table:table-cell>',
    '<table:table-cell office:value-type="string"><text:p>score</text:p></table:table-cell>',
    '</table:table-row>',
    '<table:table-row>',
    '<table:table-cell office:value-type="float" office:value="1"><text:p>1</text:p></table:table-cell>',
    '<table:table-cell office:value-type="float" office:value="1.1"><text:p>1.1</text:p></table:table-cell>',
    '</table:table-row>',
    '<table:table-row>',
    '<table:table-cell office:value-type="float" office:value="2"><text:p>2</text:p></table:table-cell>',
    '<table:table-cell office:value-type="float" office:value="2.2"><text:p>2.2</text:p></table:table-cell>',
    '</table:table-row>',
    '</table:table></office:spreadsheet></office:body></office:document-content>')
  .write_ods_fixture(file.path(d, "data", "clean.ods"), clean)

  mo <- module_run(test_paper("x"), "data_check",
                   local_path = d, local_only = TRUE)
  expect_equal(mo$summary_table$spreadsheet_flagged_file_n, 0)
})
