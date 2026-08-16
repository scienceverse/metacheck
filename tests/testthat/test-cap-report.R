# Tests for the shared count-cap message builder (report-helpers.R). This is a
# pure string builder: NULL when the work fits, otherwise a message naming the
# parameter, its current value, and the value needed to proceed.

test_that("cap_gate_count returns NULL within the cap and a message over it", {
  expect_null(cap_gate_count(30, "codebook_max_calls", 30, "text block",
                             context = "cb.csv", action = "parse"))
  msg <- cap_gate_count(71, "codebook_max_calls", 30, "text block",
                        context = "cb.csv", action = "parse")
  expect_match(msg, "codebook_max_calls` cap of 30")
  expect_match(msg, "codebook_max_calls >= 71")
  expect_match(msg, "parse them")
  expect_match(msg, "cb.csv")
})
