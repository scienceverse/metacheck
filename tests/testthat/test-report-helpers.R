test_that("report_type", {
  expect_true(is.function(metacheck::report_type))
  expect_no_error(helplist <- help(report_type, metacheck))

  # Explicit reset at both ends, not withr::local_options()/on.exit(): this
  # setting is a bare session option report_table()/collapse_section()/the
  # module tabset helpers all read directly, and relying on automatic
  # end-of-test restoration was confirmed to leak "simple" into whichever
  # test happened to run next under this project's actual test runner
  # (devtools::test()) -- reproduced with the plainest possible on.exit()
  # marker, so every test that cares about report_type() now sets it
  # explicitly itself instead of depending on cleanup from the one before it.
  report_type("full")

  expect_equal(report_type(), "full")

  obs <- report_type("simple")
  expect_equal(obs, "simple")
  expect_equal(report_type(), "simple")

  # case-insensitive, and only the first element of a longer vector is used
  obs <- report_type(c("FULL", "simple"))
  expect_equal(obs, "full")
  expect_equal(report_type(), "full")

  # "brief" and "simple_brief" are independent of "simple" (rendering
  # mechanics) -- see report_type()'s own docs for why these are two axes.
  obs <- report_type("brief")
  expect_equal(obs, "brief")
  expect_equal(report_type(), "brief")

  obs <- report_type("simple_brief")
  expect_equal(obs, "simple_brief")
  expect_equal(report_type(), "simple_brief")

  expect_error(report_type("fancy"), "full.*brief.*simple.*simple_brief")

  # a bad value never changes the current setting
  report_type("simple")
  expect_error(report_type("fancy"))
  expect_equal(report_type(), "simple")

  report_type("full")
})

test_that(".report_is_static and .report_is_brief", {
  expect_false(.report_is_static("full"))
  expect_false(.report_is_static("brief"))
  expect_true(.report_is_static("simple"))
  expect_true(.report_is_static("simple_brief"))

  expect_false(.report_is_brief("full"))
  expect_true(.report_is_brief("brief"))
  expect_false(.report_is_brief("simple"))
  expect_true(.report_is_brief("simple_brief"))
})

test_that(".report_flagged_bullets", {
  # no report field at all
  expect_equal(.report_flagged_bullets(list(report = NULL)), character(0))

  # report is plain prose with no scroll_table() chunk
  expect_equal(
    .report_flagged_bullets(list(report = "Nothing to see here.")),
    character(0)
  )

  # report contains a scroll_table()-built chunk: the deparsed table is
  # pulled back out and rendered as one bullet per row, one clause per column
  tbl <- data.frame(Text = c("sentence one", "sentence two"),
                    Section = c("Intro", "Results"))
  chunk <- scroll_table(tbl)
  bullets <- .report_flagged_bullets(list(report = c("Some prose.", chunk)))
  expect_length(bullets, 2)
  expect_match(bullets[1], "sentence one", fixed = TRUE)
  expect_match(bullets[1], "Intro", fixed = TRUE)
  expect_match(bullets[2], "sentence two", fixed = TRUE)
  expect_match(bullets[2], "Results", fixed = TRUE)
})

test_that("scroll_table", {
  expect_true(is.function(metacheck::scroll_table))
  expect_no_error(helplist <- help(scroll_table, metacheck))

  table <- data.frame(uc = LETTERS,
                      lc = letters)
  obs <- scroll_table(table)
  expect_true(grepl("```{r}", obs, fixed = TRUE))
  exp <- "metacheck::report_table(table, \"auto\", 2, FALSE)"
  expect_true(grepl(exp, obs, fixed = TRUE))

  obs <- scroll_table(table, escape = TRUE)
  exp <- "metacheck::report_table(table, \"auto\", 2, TRUE)"
  expect_true(grepl(exp, obs, fixed = TRUE))

  # vector vs unnamed table version
  table <- data.frame(table = LETTERS)
  colnames(table) <- ""
  obs_table <- scroll_table(table)
  obs_vec <- scroll_table(LETTERS)
  expect_equal(obs_table, obs_vec)

  # set paginate after maxrows
  obs_2 <- scroll_table(1:10)
  obs_10 <- scroll_table(1:10, maxrows = 10)
  exp_2 <- "metacheck::report_table(table, \"auto\", 2, FALSE)"
  exp_10 <- "metacheck::report_table(table, \"auto\", 10, FALSE)"
  expect_true(grepl(exp_2, obs_2, fixed = TRUE))
  expect_true(grepl(exp_10, obs_10, fixed = TRUE))

  # colwidths
  obs <- scroll_table(data.frame(a = 1, b = 2), c(.3, .7))
  exp <- "metacheck::report_table(table, c(0.3, 0.7), 2, FALSE)"
  expect_true(grepl(exp, obs, fixed = TRUE))

  obs <- scroll_table(data.frame(a = 1, b = 2, c = 3, d = 4), c(.1, .4))
  exp <- "metacheck::report_table(table, c(0.1, 0.4), 2, FALSE)"
  expect_true(grepl(exp, obs, fixed = TRUE))

  obs <- scroll_table(data.frame(a = 1, b = 2, c = 3, d = 4),
                      c(NA, 200, NA, NA))
  exp <- "metacheck::report_table(table, c(NA, 200, NA, NA), 2, FALSE)"
  expect_true(grepl(exp, obs, fixed = TRUE))
})

test_that("report_table", {
  expect_true(is.function(metacheck::report_table))
  expect_no_error(helplist <- help(report_table, metacheck))

  # explicit, not relied-on cleanup -- see the "report_type" test's own comment
  report_type("full")

  expect_error(report_table(bad_arg))

  # one row
  table <- data.frame(a = 1, b = 2, c = 3, d = 4)
  obs <- report_table(table)
  expect_s3_class(obs, "datatables")
  expect_equal(obs$x$data, table)
  expect_equal(obs$x$options$pageLength, 2)
  expect_equal(obs$x$options$dom, "t")

  # 10 rows, show 2
  table <- data.frame(a = 1:10, b = 21:30)
  obs <- report_table(table)
  expect_equal(obs$x$data, table)
  expect_equal(obs$x$options$pageLength, 2)
  expect_equal(obs$x$options$dom, "<'top' p>")

  # 10 rows, show 10
  table <- data.frame(a = 1:10, b = 21:30)
  obs <- report_table(table, maxrows = 10)
  expect_equal(obs$x$data, table)
  expect_equal(obs$x$options$pageLength, 10)
  expect_equal(obs$x$options$dom, "t")

  # colwidths
  table <- data.frame(a = 1:10, b = 21:30)
  obs <- report_table(table, c(.5, .5))
  expect_equal(obs$x$options$columnDefs[[1]]$width, "50%")
  expect_equal(obs$x$options$columnDefs[[2]]$width, "50%")

  table <- data.frame(a = 1:10, b = 21:30)
  obs <- report_table(table, c(20, 50))
  expect_equal(obs$x$options$columnDefs[[1]]$width, "20px")
  expect_equal(obs$x$options$columnDefs[[2]]$width, "50px")

  table <- data.frame(a = 1:10, b = 21:30)
  colwidths <- c(NA, "4em")
  obs <- report_table(table, colwidths)
  expect_equal(obs$x$options$columnDefs[[1]]$targets, 1)
  expect_equal(obs$x$options$columnDefs[[1]]$width, "4em")
})

test_that("report_table in simple mode returns a static table with no JS widget", {
  # explicit, not relied-on cleanup -- see the "report_type" test's own comment
  report_type("simple")

  table <- data.frame(a = 1, b = 2, c = 3, d = 4)
  obs <- report_table(table)
  # knitr::asis_output(), not a DT htmlwidget: no JS dependency at all.
  expect_s3_class(obs, "knit_asis")
  expect_false(inherits(obs, "htmlwidget"))
  txt <- as.character(obs)
  expect_true(grepl("dt-static", txt, fixed = TRUE))
  expect_false(grepl("<script", txt, fixed = TRUE))
  expect_false(grepl("html-widget", txt, fixed = TRUE))

  # escape = FALSE: raw HTML in a cell passes through unescaped
  df_html <- data.frame(x = "a<br>b", stringsAsFactors = FALSE)
  obs <- report_table(df_html, escape = FALSE)
  expect_true(grepl("a<br>b", as.character(obs), fixed = TRUE))

  # escape = TRUE: special characters are escaped, never executed as markup
  df_unsafe <- data.frame(x = "<script>alert(1)</script>", stringsAsFactors = FALSE)
  obs <- report_table(df_unsafe, escape = TRUE)
  txt <- as.character(obs)
  expect_true(grepl("&lt;script&gt;", txt, fixed = TRUE))
  expect_false(grepl("<script>alert", txt, fixed = TRUE))

  # colwidths applied as a <colgroup>, not DT columnDefs
  table <- data.frame(a = 1:3, b = 4:6)
  obs <- report_table(table, colwidths = c(100, 0.5))
  txt <- as.character(obs)
  expect_true(grepl('<col style="width:100px">', txt, fixed = TRUE))
  expect_true(grepl('<col style="width:50%">', txt, fixed = TRUE))

  # a table within maxrows renders with no scroll box at all
  small <- data.frame(x = 1:3)
  obs <- report_table(small, maxrows = 10)
  expect_false(grepl("max-height", as.character(obs), fixed = TRUE))

  # a table over maxrows gets a scroll box, but every row is still present
  # (unlike the full-report widget, nothing is hidden behind JS pagination)
  big <- data.frame(x = 1:50)
  obs <- report_table(big, maxrows = 10)
  txt <- as.character(obs)
  expect_true(grepl("max-height", txt, fixed = TRUE))
  expect_equal(lengths(regmatches(txt, gregexpr("<tr>", txt))), 51) # 50 rows + header

  # a pathologically large table is truncated with a visible note, so one
  # huge table cannot make the report unboundedly large
  max_rows <- metacheck:::.report_table_simple_max_rows
  huge <- data.frame(x = seq_len(max_rows + 100))
  obs <- report_table(huge, maxrows = 10)
  txt <- as.character(obs)
  expect_true(grepl(
    sprintf("Showing the first %d of %d rows", max_rows, max_rows + 100),
    txt
  ))
  expect_equal(lengths(regmatches(txt, gregexpr("<tr>", txt))), max_rows + 1)

  report_type("full")
})

test_that("collapse_section", {
  expect_true(is.function(metacheck::collapse_section))
  expect_no_error(helplist <- help(collapse_section, metacheck))

  # explicit, not relied-on cleanup -- see the "report_type" test's own comment
  report_type("full")

  expect_error(collapse_section())
  expect_error(collapse_section("a", callout = "d"))

  text <- "hello"
  obs <- collapse_section(text)
  expect_true(grepl("callout-tip", obs))

  obs <- collapse_section(text, callout = "warning")
  expect_true(grepl("callout-warning", obs))
})

test_that("collapse_section in simple mode renders the title as visible text, never collapsed", {
  # explicit, not relied-on cleanup -- see the "report_type" test's own comment
  report_type("simple")

  obs <- collapse_section("hello", title = "How It Works")
  # Plain Pandoc has no concept of a fenced-div "title" attribute -- it would
  # pass title="..." through as an invisible HTML attribute rather than
  # content, so simple mode must render it as real text instead.
  expect_false(grepl('title="How It Works"', obs, fixed = TRUE))
  expect_true(grepl("**How It Works**", obs, fixed = TRUE))
  expect_true(grepl("callout-tip", obs, fixed = TRUE))

  # collapse is always forced FALSE: expand/collapse needs Bootstrap JS a
  # simple report never includes, so collapse="true" would leave the
  # content permanently hidden behind a dead toggle.
  obs <- collapse_section("hello", collapse = TRUE)
  expect_false(grepl('collapse="true"', obs, fixed = TRUE))

  obs <- collapse_section("hello", callout = "warning")
  expect_true(grepl("callout-warning", obs, fixed = TRUE))

  report_type("full")
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
})
