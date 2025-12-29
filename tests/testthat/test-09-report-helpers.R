test_that("scroll_table", {
  expect_true(is.function(metacheck::scroll_table))
  expect_no_error(helplist <- help(scroll_table, metacheck))

  table <- data.frame(uc = LETTERS,
                      lc = letters)
  obs <- scroll_table(table)
  expect_true(grepl("```{r}", obs, fixed = TRUE))
  expect_true(grepl("escape = FALSE", obs, fixed = TRUE))

  obs <- scroll_table(table, escape = TRUE)
  expect_true(grepl("escape = TRUE", obs, fixed = TRUE))

  # vector vs unnamed table version
  table <- data.frame(table = LETTERS)
  colnames(table) <- ""
  obs_table <- scroll_table(table)
  obs_vec <- scroll_table(LETTERS)
  expect_equal(obs_table, obs_vec)

  # set paginate after maxrows
  obs_scroll <- scroll_table(1:10)
  obs_no <- scroll_table(1:2)
  obs_no10 <- scroll_table(1:10, maxrows = 10)

  expect_true(grepl("dom = \"<'top' p>\"", obs_scroll, fixed = TRUE))
  expect_true(grepl('dom = "t",', obs_no, fixed = TRUE))
  expect_true(grepl('dom = "t",', obs_no10, fixed = TRUE))
  expect_true(grepl('pageLength = 2,', obs_scroll, fixed = TRUE))
  expect_true(grepl('pageLength = 2,', obs_no, fixed = TRUE))
  expect_true(grepl('pageLength = 10,', obs_no10, fixed = TRUE))

  # colwidths
  obs <- scroll_table(data.frame(a = 1, b = 2), c(.3, .7))
  expect_true(grepl('targets = 0, width = "30%"', obs))
  expect_true(grepl('targets = 1, width = "70%"', obs))

  obs <- scroll_table(data.frame(a = 1, b = 2, c = 3, d = 4), c(.1, .4))
  expect_true(grepl('targets = 0, width = "10%"', obs))
  expect_true(grepl('targets = 1, width = "40%"', obs))

  obs <- scroll_table(data.frame(a = 1, b = 2, c = 3, d = 4), c(NA, 200, NA, NA))
  expect_false(grepl('targets = 0', obs))
  expect_true(grepl('targets = 1, width = "200px"', obs))
  expect_false(grepl('targets = 2', obs))
  expect_false(grepl('targets = 3', obs))
})

test_that("collapse_section", {
  expect_true(is.function(metacheck::collapse_section))
  expect_no_error(helplist <- help(collapse_section, metacheck))

  expect_error(collapse_section())
  expect_error(collapse_section("a", callout = "d"))

  text <- "hello"
  obs <- collapse_section(text)
  expect_true(grepl("callout-tip", obs))

  obs <- collapse_section(text, callout = "warning")
  expect_true(grepl("callout-warning", obs))
})

test_that("plural", {
  expect_true(is.function(metacheck::plural))
  expect_no_error(helplist <- help(plural, metacheck))

  s0 <- plural(0)
  expect_equal(s0, "s")
  s1 <- plural(1)
  expect_equal(s1, "")
  s2 <- plural(2)
  expect_equal(s2, "s")

  s0 <- plural(0, "is", "are")
  expect_equal(s0, "are")
  s1 <- plural(1, "is", "are")
  expect_equal(s1, "is")
  s2 <- plural(2, "is", "are")
  expect_equal(s2, "are")
})

test_that("link", {
  expect_true(is.function(metacheck::link))
  expect_no_error(helplist <- help(link, metacheck))

  obs <- link("https://google.com")
  exp <- "<a href='https://google.com' target='_blank'>google.com</a>"
  expect_equal(obs, exp)

  obs <- link("http://google.com")
  exp <- "<a href='http://google.com' target='_blank'>google.com</a>"
  expect_equal(obs, exp)

  obs <- link("https://google.com", "Google")
  exp <- "<a href='https://google.com' target='_blank'>Google</a>"
  expect_equal(obs, exp)

  obs <- link("https://google.com", "Google", FALSE)
  exp <- "<a href='https://google.com'>Google</a>"
  expect_equal(obs, exp)

  url <- c("https://google.com", "https://scienceverse.org")
  text <- c("Google", "Scienceverse")
  obs <- link(url, text, FALSE)
  exp <- c("<a href='https://google.com'>Google</a>",
           "<a href='https://scienceverse.org'>Scienceverse</a>")
  expect_equal(obs, exp)

  url <- c(NA, "https://scienceverse.org")
  text <- c("Google", "Scienceverse")
  obs <- link(url, text, FALSE)
  exp <- c(NA,
           "<a href='https://scienceverse.org'>Scienceverse</a>")
  expect_equal(obs, exp)
})

test_that("format_ref", {
  expect_true(is.function(metacheck::format_ref))
  expect_no_error(helplist <- help(format_ref, metacheck))

  a <- bibentry(
    bibtype = "Article",
    title = "Trustworthy but not lust-worthy: Context-specific effects of facial resemblance",
    author = person(c("L.", "M."), "DeBruine"),
    journal = "Proceedings of the Royal Society B: Biological Sciences",
    year = 2005,
    volume = 272,
    number = 1566,
    pages = "919--922",
    doi = "10.1098/rspb.2004.3003"
  )

  b <- bibentry(
    bibtype = "Article",
    title = "Improving transparency, falsifiability, and rigor by making hypothesis tests machine-readable",
    author = c(
      person("D.", "Lakens"),
      person(c("L.", "M."), "DeBruine")
    ),
    journal = "Advances in Methods and Practices in Psychological Science",
    year = 2021,
    volume = 4,
    number = 2,
    pages = "2515245920970949",
    doi = "10.1177/2515245920970949"
  )

  exp_a <- "DeBruine LM (2005). &ldquo;Trustworthy but not lust-worthy: Context-specific effects of facial resemblance.&rdquo; <em>Proceedings of the Royal Society B: Biological Sciences</em>, <b>272</b>(1566), 919&ndash;922. <a href=\"https://doi.org/10.1098/rspb.2004.3003\">doi:10.1098/rspb.2004.3003</a>."
  exp_b <- "Lakens D, DeBruine LM (2021). &ldquo;Improving transparency, falsifiability, and rigor by making hypothesis tests machine-readable.&rdquo; <em>Advances in Methods and Practices in Psychological Science</em>, <b>4</b>(2), 2515245920970949. <a href=\"https://doi.org/10.1177/2515245920970949\">doi:10.1177/2515245920970949</a>."

  # NOTE: when you run this manually,
  # you get a mismatch with the obs having fancy quotes!

  obs_a <- format_ref(a)
  expect_equal(exp_a, obs_a)
  obs_b <- format_ref(b)
  expect_equal(exp_b, obs_b)

  bib <- c(a, b)
  obs <- format_ref(bib)
  exp <- c(exp_a, exp_b)
  expect_equal(obs, exp)

  ## handles bibtex
  bib <- toBibtex(a)
  obs <- format_ref(bib)
  expect_equal(obs, exp_a)

  bib <- toBibtex(c(a, b))
  obs <- format_ref(bib)
  expect_equal(obs, exp)

  # handles bibtex text
  bib <- toBibtex(a) |> as.character() |> paste(collapse = "\n")
  obs <- format_ref(bib)
  expect_equal(obs, exp_a)

  # non-bibtex text
  bib <- exp_a
  obs <- format_ref(bib)
  expect_equal(obs, exp_a)

  bib <- c("help", "me")
  obs <- format_ref(bib)
  expect_equal(obs, bib)

  # from paper
  paper <- read(demoxml())
  bib <- paper$bib$ref
  obs <- format_ref(bib)
  exp <- c("Gangestad SW, Thornhill R (1998). &ldquo;Menstrual cycle variation in women's preferences for the scent of symmetrical men.&rdquo; <em>Proceedings Biological Sciences</em>, <b>22</b>, 927-933. <a href=\"https://doi.org/10.1098/rspb.1998.0380\">doi:10.1098/rspb.1998.0380</a>.",
           "Gino F, Wiltermuth SS (2014). &ldquo;Evil Genius? How Dishonesty Can Lead to Greater Creativity.&rdquo; <em>Psychological Science</em>, <b>25</b>(4), 973-981. <a href=\"https://doi.org/10.1177/0956797614520714\">doi:10.1177/0956797614520714</a>.",
           "Smith F (2021). &ldquo;Human error is a symptom of a poor design.&rdquo; <em>Journal of Journals</em>, <b>0</b>(0), 0. <a href=\"https://doi.org/10.0000/0123456789\">doi:10.0000/0123456789</a>.",
           "Lakens D (2018). &ldquo;Equivalence testing for psychological research.&rdquo; <em>Advances in Methods and Practices in Psychological Science</em>, <b>1</b>, 259-270."
  )
  expect_equal(obs, exp)
})
