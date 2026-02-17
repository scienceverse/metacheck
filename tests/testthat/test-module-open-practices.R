test_that("open_practices", {
  module <- "open_practices"
  mods <- module_list()
  expect_true(module %in% mods$name)

  # single paper
  paper <- demopaper()
  mo <- module_run(paper, module)

  expect_equal(mo$traffic_light, "green")
  expect_equal(mo$table$data_open, TRUE)
  expect_equal(mo$table$data_category, "general-purpose repository")
  expect_equal(mo$table$data_reuse, FALSE)
  expect_equal(mo$table$code_open, TRUE)
  expect_equal(mo$table$code_supplement, FALSE)
  expect_equal(mo$table$code_reuse, FALSE)
  expect_equal(mo$table$data_das, FALSE)
  expect_equal(mo$table$code_cas, FALSE)
  expect_equal(mo$table$das, "")
  expect_equal(mo$table$cas, "")
  expect_equal(mo$table$data_statements, "Data is also available from https://osf.io/5tbm9 and code is also available from https://osf.io/629bx.")
  expect_equal(mo$table$code_statements, "Data and analysis code is available on GitHub from https://github.com/Lakens/to_err_is_human and from https://researchbox.org/4377.")
})

test_that("open_practices paperlist", {
  module <- "open_practices"
  paper <- paperlist(
    test_paper("Open code is available at https://github.com/repo/mine."),
    test_paper("Data available upon request.")
  )
  mo <- module_run(paper, module)
  expect_equal(nrow(mo$table), length(paper))
  expect_equal(mo$table$id, mo$summary_table$id)
  expect_equal(mo$table$data_open, c(F, F))
  expect_equal(mo$table$data_category, c("", "upon request"))
  expect_equal(mo$table$data_reuse, c(F, F))
  expect_equal(mo$table$code_open, c(T, F))
  expect_equal(mo$table$code_supplement, c(F, F))
  expect_equal(mo$table$code_reuse, c(F, F))
})


test_that("open_practices only open data", {
  # examples from https://authorservices.taylorandfrancis.com/data-sharing-policies/open-data/
  module <- "open_practices"
  paper <- test_paper("Data for all experiments have been made publicly available on OSF at https://osf.io/hk4yq/.")
  mo <- module_run(paper, module)
  expect_equal(mo$table$data_open, TRUE)
  expect_equal(mo$table$data_category, "general-purpose repository")
  expect_equal(mo$table$data_reuse, FALSE)
  expect_equal(mo$table$code_open, FALSE)
  expect_equal(mo$table$code_supplement, FALSE)
  expect_equal(mo$table$code_reuse, FALSE)
})

test_that("open_practices only open code", {
  statements <- c("The computer code for the analyses reported here can be accessed at the Open Science Framework (https://osf.io/geq9x/).",
                  "All analysis code for this study has been made publicly available via the Open Science Framework and can be accessed at https://osf.io/geq9x/.")

  # just statements
  module <- "open_practices"
  paper <- test_paper(statements)
  mo <- module_run(paper, module)
  expect_equal(mo$table$data_open, FALSE)
  expect_equal(mo$table$code_open, TRUE)
  expect_equal(mo$table$code_statements, paste(statements, collapse = "\n\n"))
})

test_that("open_practices both open data and code", {
  # examples from https://authorservices.taylorandfrancis.com/data-sharing-policies/open-data/
  module <- "open_practices"
  paper <- test_paper("The data and code to reproduce the findings of this study are available at the Open Science Framework at https://osf.io/abcde.")
  mo <- module_run(paper, module)
  expect_equal(mo$table$data_open, TRUE)
  expect_equal(mo$table$data_category, "general-purpose repository")
  expect_equal(mo$table$data_reuse, FALSE)
  expect_equal(mo$table$code_open, TRUE)
  expect_equal(mo$table$code_supplement, FALSE)
  expect_equal(mo$table$code_reuse, FALSE)
})

