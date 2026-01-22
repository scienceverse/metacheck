test_that("json_expand", {
  table <- data.frame(
    id = 1:5,
    answer = c(
      '{"number": 1, "letter": "A", "bool": true}',
      '{"number": 2, "letter": "B", "bool": "FALSE"}',
      '{"number": 3, "letter": "", "bool": null}',
      'oh no, the LLM misunderstood',
      '{"number": 5, "letter": ["E", "F"], "bool": false}'
    )
  )

  expanded <- json_expand(table)
  expanded2 <- json_expand(table$answer)
  expect_equal(expanded[, 3:6], expanded2[2:5])
  expect_equal(names(expanded)[[2]], "answer")
  expect_equal(names(expanded2)[[1]], "json")

  expect_equal(names(expanded), c("id", "answer", "number", "letter", "bool", "error"))
  expect_equal(typeof(expanded$number), "integer")
  expect_equal(typeof(expanded$letter), "character")
  expect_equal(typeof(expanded$bool), "logical")
  expect_equal(expanded$letter, c("A", "B", "", NA, "E;F"))
  expect_equal(expanded$bool, c(TRUE, FALSE, NA, NA, FALSE))
})

test_that("json_expand nulls", {
  table <- data.frame(
    id = 1:9,
    answer = c(
      '{"number": 1, "letter": "A", "bool": true}',
      '{null}',
      '[]',
      '[{}]',
      '{}',
      'null',
      '""',
      '"Hi"',
      ''
    )
  )

  expanded <- json_expand(table)
  expect_equal(expanded$error[c(1, 3:6)],
               rep(NA_character_, 5))
  expect_equal(expanded$error[c(7:8)],
               rep("not a list", 2))
  expect_equal(expanded$error[c(2, 9)],
               rep("parsing error", 2))
})

test_that("json_expand name conflict", {
  table <- data.frame(
    id = 1:5,
    number = 1:5,
    answer = c(
      '{"number": 1, "letter": "A", "bool": true}',
      '{"number": 2, "letter": "B", "bool": "FALSE"}',
      '{"number": 3, "letter": "", "bool": null}',
      'oh no, the LLM misunderstood',
      '{"number": 5, "letter": ["E", "F"], "bool": false}'
    )
  )

  expanded <- json_expand(table)
  expect_contains(names(expanded), c("number", "number.json"))

  expanded <- json_expand(table, suffix = c("_orig", "_x"))
  expect_contains(names(expanded), c("number_orig", "number_x"))
})

test_that("json_expand multi-line", {
  table <- data.frame(
    id = 1:3,
    answer = c(
      '[
        {"number": 1, "letter": "A", "bool": true},
        {"number": 2, "letter": "B", "bool": null}
      ]',
      '[{"number": 3, "letter": "C", "bool": false}]',
      '[]'
    )
  )

  exp <- data.frame(
    number = c(1L, 2L, 3L, NA),
    letter = c("A", "B", "C", NA),
    bool = c(TRUE, NA, FALSE, NA)
  )

  expanded <- json_expand(table)

  expect_equal(expanded[, 3:5], exp)
})

test_that("json_expand remove ```json", {
  table <- data.frame(
    id = 1:3,
    answer = c(
      '```json
      [
        {"number": 1, "letter": "A", "bool": true},
        {"number": 2, "letter": "B", "bool": null}
      ]
      ```',
      'Some gibber
      ```json
      [{"number": 3, "letter": "C", "bool": false}]
      ```
      more gibber',
      '```json
      ```'
    )
  )

  exp <- data.frame(
    number = c(1L, 2L, 3L, NA),
    letter = c("A", "B", "C", NA),
    bool = c(TRUE, NA, FALSE, NA)
  )

  expanded <- json_expand(table)

  expect_equal(expanded[, 3:5], exp)
})
