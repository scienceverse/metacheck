# A module output, as module_run() returns it, for the checklist tests.
checklist_mo <- function(module, title, traffic_light = "info", ...) {
  structure(list(module = module, title = title, traffic_light = traffic_light, ...),
            class = "metacheck_module_output")
}

test_that("report_type checklist", {
  report_type("full")
  obs <- report_type("checklist")
  expect_equal(obs, "checklist")
  expect_equal(report_type(), "checklist")
  expect_true(.report_is_static("checklist"))
  expect_false(.report_is_brief("checklist"))
  expect_error(report_type("fancy"), "checklist")
  report_type("full")
})

test_that(".checklist_checks: a module with checks", {
  mo <- checklist_mo("my_check", "My Check", "red", checks = data.frame(
    status = c("fix", "fine", "improve"),
    title = c("A", "B", "C"),
    found = c("Found A.", "Found B.", NA),
    todo = c("Do A.", "", "Do C.")
  ))
  checks <- .checklist_checks(list(my_check = mo))
  expect_equal(checks$status, c("fix", "fine", "improve"))
  expect_equal(checks$found, c("Found A.", "Found B.", ""))
  expect_equal(checks$module, rep("my_check", 3))
  # every column, "" when the module left it out
  expect_true(all(c("why", "note", "section", "details", "details_title") %in% names(checks)))
  expect_equal(checks$why, rep("", 3))

  # an unknown status is shown for information, with a warning
  mo$checks$status[[2]] <- "great"
  expect_warning(checks <- .checklist_checks(list(my_check = mo)), "unknown status")
  expect_equal(checks$status[[2]], "info")
})

test_that(".checklist_checks: a module without checks is one check", {
  mos <- list(
    a = checklist_mo("a", "Red", "red", summary_text = "Something to fix.", report = "The details."),
    b = checklist_mo("b", "Yellow", "yellow", summary_text = "Something.", report = "Something."),
    c = checklist_mo("c", "Green", "green", summary_text = "Fine."),
    d = checklist_mo("d", "Not applicable", "na", summary_text = "Nothing to check."),
    e = checklist_mo("e", "Failed", "fail", summary_text = "This module failed to run", report = "an error")
  )
  checks <- .checklist_checks(mos)
  expect_equal(checks$status, c("fix", "improve", "fine", "na", "fix"))
  expect_equal(checks$title, c("Red", "Yellow", "Green", "Not applicable", "Failed"))
  expect_equal(checks$found[[1]], "Something to fix.")
  # the report becomes the details, unless it is the summary itself
  expect_equal(checks$details[[1]], "The details.")
  expect_equal(checks$details[[2]], "")
  expect_equal(checks$found[[5]], "This module failed to run: an error")

  expect_equal(nrow(.checklist_checks(list())), 0)
})

test_that(".report_checklist_qmd", {
  report_type("checklist")
  logo <- system.file("app/www/images/logo.png", package = "metacheck")
  mo <- checklist_mo("my_check", "My \"Check\"", "red",
    intro = "This report checks a package.",
    logo = logo,
    checks = data.frame(
      status = c("fix", "fine", "na"),
      title = c("No junk", "Size", "ORCIDs"),
      section = c("2 Files", "1.1", ""),
      found = c("2 junk files.", "1 MB.", "No ORCID."),
      todo = c("Remove them.", "", ""),
      details = c("The junk table.", "The size table.", ""),
      details_title = c("Show the files", "", "")
    ))
  paper <- test_paper()
  paper$info$title <- "my_package"
  txt <- .report_checklist_qmd(list(my_check = mo), paper)

  # the template's tokens are all filled in
  expect_false(grepl("{{", txt, fixed = TRUE))
  # the module's title (one module), escaped in the page title and the HTML
  expect_match(txt, 'pagetitle: "My \\"Check\\": my_package"', fixed = TRUE)
  expect_match(txt, "<h1>My &quot;Check&quot;</h1>|<h1>My \"Check\"</h1>")
  expect_match(txt, "This report checks a package.", fixed = TRUE)
  # counts, with links to the sections that have checks
  expect_match(txt, '<a class="count fix" href="#fix"><b>1</b>', fixed = TRUE)
  expect_match(txt, '<span class="count improve zero"><b>0</b>', fixed = TRUE)
  expect_match(txt, "## To fix [(1)]{.n} {#fix}", fixed = TRUE)
  expect_false(grepl("## To improve", txt, fixed = TRUE))
  # what was found and what to do on separate lines
  expect_match(txt, "::: {.label}\nWhat we found\n:::\n\n::: {.text}\n2 junk files.\n:::", fixed = TRUE)
  expect_match(txt, "::: {.text .todo}\nRemove them.\n:::", fixed = TRUE)
  expect_match(txt, "[2 Files]{.where}", fixed = TRUE)
  # the details of a check that needs work fold under it; of a fine one,
  # under "More detail", by its title
  expect_match(txt, "<details><summary>Show the files</summary>", fixed = TRUE)
  expect_match(txt, "## More detail", fixed = TRUE)
  expect_match(txt, "<details><summary>Size</summary>", fixed = TRUE)
  # fenced divs are balanced
  opens <- lengths(regmatches(txt, gregexpr("(?m)^::: \\{", txt, perl = TRUE)))
  closes <- lengths(regmatches(txt, gregexpr("(?m)^:::$", txt, perl = TRUE)))
  expect_equal(opens, closes)

  # several modules: metacheck's title
  mo2 <- checklist_mo("other", "Other", "green", summary_text = "Fine.")
  txt <- .report_checklist_qmd(list(my_check = mo, other = mo2), paper)
  expect_match(txt, "<h1>MetaCheck Report</h1>", fixed = TRUE)
  report_type("full")
})

test_that(".report_checklist_qmd: the file tree once, after the module sections", {
  report_type("checklist")
  dc <- checklist_mo("data_check", "Data Check", "yellow",
    summary_text = "Some data files.",
    report = c("The data files.", "#### Data Tree", "Where the files sit.", "TREE-LINES"))
  rp <- checklist_mo("reproducibility_check", "Reproducibility Check", "green",
    summary_text = "The code ran.", report = "Ran.", checklist_section = "The scripts.")
  other <- checklist_mo("other", "Other", "green", summary_text = "Fine.", report = "Other details.")
  mos <- list(data_check = dc, reproducibility_check = rp, other = other)
  txt <- .report_checklist_qmd(mos, test_paper())

  # in a section of its own, after the reproducibility section, before "More detail"
  tree_at <- regexpr("## Files in the repository {#files}", txt, fixed = TRUE)
  repro_at <- regexpr("## Reproducibility Check {#section-reproducibility_check}", txt, fixed = TRUE)
  more_at <- regexpr("## More detail", txt, fixed = TRUE)
  expect_true(repro_at > 0 && tree_at > repro_at && more_at > tree_at)
  expect_match(txt, "Where the files sit.", fixed = TRUE)
  # shown once: data_check's folded details keep the rest of its report only
  expect_equal(lengths(gregexpr("TREE-LINES", txt, fixed = TRUE)), 1)
  expect_false(grepl("#### Data Tree", txt, fixed = TRUE))
  expect_match(txt, "The data files.", fixed = TRUE)

  # without a repository, no section
  txt <- .report_checklist_qmd(list(reproducibility_check = rp, other = other), test_paper())
  expect_false(grepl("Files in the repository", txt, fixed = TRUE))
  report_type("full")
})

test_that("collapse_section folds in a checklist report", {
  report_type("checklist")
  obs <- collapse_section(c("One.", "Two."), "How It Works")
  expect_equal(obs, "<details><summary>How It Works</summary>\n\nOne.\n\nTwo.\n\n</details>\n")
  report_type("full")
})

test_that("a static table cell that starts like a list item stays text", {
  report_type("simple")
  tbl <- data.frame(Section = c("1. General information", "- a dash", "2) two", "1.5 cm"))
  html <- as.character(.report_table_static(tbl))
  expect_match(html, "1\\. General information", fixed = TRUE)
  expect_match(html, "\\- a dash", fixed = TRUE)
  expect_match(html, "2\\) two", fixed = TRUE)
  expect_match(html, "> 1.5 cm <", fixed = TRUE)
  report_type("full")
})

test_that("render html report_type = 'checklist'", {
  skip_if_quick()
  skip_on_ci()
  skip_on_cran()
  skip_if_not_installed("rmarkdown")

  llm_use(FALSE)
  paper <- test_paper("Example text with p = .03 and p = .5.")
  modules <- c("stat_p_exact", "stat_p_nonsig")
  output_file <- withr::local_tempfile(fileext = ".html")

  prev_report_type <- report_type()
  paper_report <- report(paper, modules, output_file, "html", report_type = "checklist")
  expect_equal(report_type(), prev_report_type)

  txt <- paste(readLines(attr(paper_report, "save_path"), warn = FALSE), collapse = "\n")
  # static, light only, in the website's font
  expect_false(grepl("html-widget", txt, fixed = TRUE))
  expect_false(grepl("prefers-color-scheme", txt, fixed = TRUE))
  expect_match(txt, "font-family: \"Cabin\"", fixed = TRUE)
  expect_match(txt, "data:font/ttf;base64,", fixed = TRUE)
  expect_match(txt, 'class="counts"', fixed = TRUE)
  expect_match(txt, "Exact", fixed = TRUE)
  # every <div> is closed exactly once
  expect_equal(lengths(gregexpr("<div\\b", txt)), lengths(gregexpr("</div>", txt)))
})

test_that("a failed module's error message keeps its angle brackets in the checklist", {
  # as markdown, "<haven_labelled>" was taken as an HTML tag and left out:
  # the page said "Can't convert x to ." (PsyArXiv preprint jev3u_v1)
  mo <- list(module = "data_check", title = "data_check", traffic_light = "fail",
             report = "Can't convert `x` <haven_labelled> to <character>.")
  found <- .checklist_checks(list(mo))$found
  expect_match(found, "&lt;haven_labelled&gt; to &lt;character&gt;", fixed = TRUE)
})
