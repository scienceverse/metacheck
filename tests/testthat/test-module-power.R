# Helpers ----

# Mocked ellmer::chat() constructor for the STRUCTURED path: chat_structured()
# is looked up against a table of known input texts (matching on a fixed
# substring so paragraph_id splits of the same source text still match) and
# returns the power_analyses array a real provider would for that text. Used
# by every "power, with LLM (structured)" scenario below in place of the old
# HTTP-recorded fixtures, since those fixtures only ever captured the
# UNSTRUCTURED (prompt-fenced) request/response shape -- the structured
# request body (response_format/json_schema) never matches them.
.mock_structured_chat <- function(lookup) {
  function(...) structure(list(
    chat_structured = function(text, type) {
      hit <- Filter(function(pat) grepl(pat, text, fixed = TRUE), names(lookup))
      if (length(hit) == 0) return(list(power_analyses = list()))
      lookup[[hit[[1]]]]
    }
  ), class = "Chat")
}

# Tests ----

test_that("power, no LLM", {
  module <- "power"
  mods <- module_list()
  expect_true(module %in% mods$name)

  llm_use(FALSE)

  # no relevant text
  paper <- test_paper("I love to power pose.")
  mo <- module_run(paper, module)
  expect_equal(mo$traffic_light, "na")
  expect_equal(nrow(mo$table), 0)
  expect_equal(nrow(mo$summary_table), 1)
  expect_equal(mo$summary_table$power_n, 0)
  expect_equal(mo$summary_table$power_complete, NA_integer_)

  # several power sentences in one paragraph
  power_text <- c(
    "An a priori power analysis for an independent samples t-test, conducted using the pwr.t.test function from pwr (Champely, 2020), indicated that for a Cohen's d = 0.5, an alpha level of 0.05, and a desired power level of 80% required at least 64 participants in each group.",
    "A sensitivity power analysis for an independent samples t-test, conducted using the pwr.t.test function from pwr (Champely, 2020), indicated that with 64 participants in each group, and an alpha level of 0.05, a desired power level of 80% was reached for an effect size of d = 0.5."
  )
  paper <- test_paper(power_text)
  mo <- module_run(paper, module)
  expect_equal(mo$traffic_light, "yellow")
  expect_equal(nrow(mo$table), 1)
  expect_equal(mo$table$power_type, c("apriori"))
  expect_equal(nrow(mo$summary_table), 1)
  expect_equal(mo$summary_table$power_n, 1)
  expect_equal(mo$summary_table$power_complete, NA_integer_)

  # multiple paragraphs
  paper$text$paragraph_id <- 0:1
  mo <- module_run(paper, module)
  expect_equal(mo$traffic_light, "yellow")
  expect_equal(nrow(mo$table), 2)
  expect_equal(mo$table$power_type, c("apriori", "sensitivity"))
  expect_equal(nrow(mo$summary_table), 1)
  expect_equal(mo$summary_table$power_n, 2)
  expect_equal(mo$summary_table$power_complete, NA_integer_)

  # multiple papers
  paper <- paperlist(
    test_paper(power_text[[1]]),
    test_paper(power_text[[2]])
  )
  mo <- module_run(paper, module)
  expect_equal(mo$traffic_light, "yellow")
  expect_equal(nrow(mo$table), 2)
  expect_equal(mo$table$power_type, c("apriori", "sensitivity"))
  expect_equal(nrow(mo$summary_table), 2)
  expect_equal(mo$summary_table$power_n, c(1, 1))
  expect_equal(mo$summary_table$power_complete, rep(NA_integer_, 2))

  # only false positives
  paper <- test_paper(text = "Our 12 participants have a lot of power to detect a moth.")
  mo <- module_run(paper, module)
  expect_equal(mo$traffic_light, "yellow")
  expect_equal(nrow(mo$table), 1)
  expect_equal(mo$summary_table$power_n, 1)
})

test_that("power, with LLM (structured)", {
  module <- "power"
  llm_use(TRUE)
  llm_model("groq/llama-3.3-70b-versatile")
  withr::local_options(metacheck.llm.cache = FALSE)

  # only false positives
  paper <- test_paper(text = "Our 12 participants have a lot of power to detect a moth.")
  testthat::local_mocked_bindings(
    chat = .mock_structured_chat(list()), .package = "ellmer")
  mo <- module_run(paper, module)
  expect_equal(mo$traffic_light, "na")
  expect_equal(nrow(mo$table), 0)
  expect_equal(mo$summary_table$power_n, 0)

  # only some info
  paper <- test_paper(text = "The a priori power analysis determined a sample size of 15 in each group for 80% power with a medium effect size.")
  testthat::local_mocked_bindings(
    chat = .mock_structured_chat(list(
      "sample size of 15" = list(power_analyses = list(list(
        power_type = "apriori", sample_size = 30, power = 0.8
      )))
    )), .package = "ellmer")
  mo <- module_run(paper, module)
  expect_equal(mo$traffic_light, "yellow")   # yellow, not red (#695, point 2)
  expect_equal(nrow(mo$table), 1)
  expect_equal(mo$table$sample_size, 30)
  expect_equal(mo$table$power, 0.8)
  expect_equal(mo$table$effect_size, NA)
  expect_equal(mo$table$alpha_level, NA)
  expect_equal(mo$table$complete, FALSE)

  # the example from the prompt: two power analyses in one paragraph
  paper <- test_paper(text = "An a priori power analysis was conducted to estimate the sample size required to achieve 80% power to detect a Cohen's d of 0.2 using an unpaired t-test at an alpha level of 0.05. This required a total sample size of 300 participants. A second a priori power analysis was conducted to estimate the required sample size for a secondary outcome. To achieve 80% power to detect a Cohen's f of 0.1 using a one-way ANOVA, a sample size of 350 was required. The a priori power analyses were conducted with G*Power.")
  testthat::local_mocked_bindings(
    chat = .mock_structured_chat(list(
      "A second a priori power analysis" = list(power_analyses = list(
        list(power_type = "apriori", statistical_test = "unpaired t-test",
             sample_size = 300, alpha_level = 0.05, power = 0.8,
             effect_size = 0.2, effect_size_metric = "Cohen's d",
             software = "G*Power"),
        list(power_type = "apriori", statistical_test = "1-way ANOVA",
             sample_size = 350, power = 0.8,
             effect_size = 0.1, effect_size_metric = "Cohen's f",
             software = "G*Power")
      ))
    )), .package = "ellmer")
  mo <- module_run(paper, module)
  expect_equal(mo$traffic_light, "yellow")   # yellow, not red (#695, point 2)
  expect_equal(nrow(mo$table), 2)
  expect_equal(mo$table$statistical_test,
               c("unpaired t-test", "1-way ANOVA"))
  expect_equal(mo$table$sample_size, c(300, 350))
  expect_equal(mo$table$alpha_level, c(0.05, NA))
  expect_equal(mo$table$power, c(0.8, 0.8))
  expect_equal(mo$table$effect_size, c(0.2, 0.1))
  expect_equal(mo$table$effect_size_metric, c("Cohen's d", "Cohen's f"))
  expect_equal(mo$table$software, c("G*Power", "G*Power"))
  expect_equal(mo$table$complete, c(T, F))

  # no relevant text
  paper <- test_paper("I love to power pose.")
  mo <- module_run(paper, module)
  expect_equal(mo$traffic_light, "na")
  expect_equal(nrow(mo$table), 0)
  expect_equal(nrow(mo$summary_table), 1)
  expect_equal(mo$summary_table$power_n, 0)
  expect_equal(mo$summary_table$power_complete, NA_integer_)

  # several power sentences in one paragraph
  power_text <- c(
    "An a priori power analysis for an independent samples t-test, conducted using the pwr.t.test function from pwr (Champely, 2020), indicated that for a Cohen's d = 0.5, an alpha level of 0.05, and a desired power level of 80% required at least 64 participants in each group.",
    "A sensitivity power analysis for an independent samples t-test, conducted using G*Power, indicated that with 64 participants in each group, and an alpha level of 0.05, power of 0.91 was reached for an effect size of d = 0.5."
  )
  paper <- test_paper(power_text)
  # both sentences land in the SAME paragraph (test_paper() defaults every
  # string to paragraph_id 0, per the "no LLM" test's own "multiple
  # paragraphs" section above) -- one LLM call, whose array response holds
  # both power analyses.
  testthat::local_mocked_bindings(
    chat = .mock_structured_chat(list(
      "pwr.t.test function from pwr" = list(power_analyses = list(
        list(power_type = "apriori", statistical_test = "unpaired t-test",
             sample_size = 128, alpha_level = 0.05, power = 0.8,
             effect_size = 0.5, effect_size_metric = "Cohen's d", software = "pwr"),
        list(power_type = "sensitivity", statistical_test = "unpaired t-test",
             sample_size = 128, alpha_level = 0.05, power = 0.91,
             effect_size = 0.5, effect_size_metric = "Cohen's d", software = "G*Power")
      ))
    )), .package = "ellmer")
  mo <- module_run(paper, module)
  expect_equal(mo$traffic_light, "green")
  expect_equal(nrow(mo$table), 2)
  expect_equal(mo$table$power_type, c("apriori", "sensitivity"))
  expect_equal(mo$table$statistical_test, c("unpaired t-test", "unpaired t-test"))
  expect_equal(mo$table$sample_size, c(128, 128))
  expect_equal(mo$table$alpha_level, c(0.05, 0.05))
  expect_equal(mo$table$power, c(0.8, 0.91))
  expect_equal(mo$table$effect_size, c(0.5, 0.5))
  expect_equal(mo$table$effect_size_metric, c("Cohen's d", "Cohen's d"))
  expect_equal(mo$table$software, c("pwr", "G*Power"))
  expect_equal(nrow(mo$summary_table), 1)
  expect_equal(mo$summary_table$power_n, 2)
  expect_equal(mo$summary_table$power_complete, 2)

  # incomplete power
  power_text <- c(
    "An a priori power analysis for an independent samples t-test, conducted using the pwr.t.test function from pwr (Champely, 2020), indicated that for a Cohen's d = 0.5, and a desired power level of 80% required at least 64 participants in each group.",
    "A sensitivity power analysis for a paired samples t-test, conducted using G-Power, indicated that with 64 participants, an adequate power was reached for an effect size of d = 0.5."
  )
  paper <- test_paper(power_text)
  apriori_incomplete <- list(power_type = "apriori", statistical_test = "unpaired t-test",
                             sample_size = 128, power = 0.8, effect_size = 0.5,
                             effect_size_metric = "Cohen's d", software = "pwr")
  sensitivity_incomplete <- list(power_type = "sensitivity", statistical_test = "paired t-test",
                                 sample_size = 64, effect_size = 0.5,
                                 effect_size_metric = "Cohen's d", software = "G*Power")
  # single paper: both sentences share paragraph_id 0 (test_paper() default),
  # so this is ONE call whose array holds both objects.
  testthat::local_mocked_bindings(
    chat = .mock_structured_chat(list(
      "for a Cohen's d = 0.5, and a desired power" = list(
        power_analyses = list(apriori_incomplete, sensitivity_incomplete))
    )), .package = "ellmer")
  mo <- module_run(paper, module)
  expect_equal(mo$traffic_light, "yellow")   # yellow, not red (#695, point 2)
  expect_equal(nrow(mo$table), 2)
  expect_equal(mo$table$power_type, c("apriori", "sensitivity"))
  expect_equal(mo$table$statistical_test, c("unpaired t-test", "paired t-test"))
  expect_equal(mo$table$sample_size, c(128, 64))
  expect_equal(mo$table$alpha_level, c(NA, NA))
  expect_equal(mo$table$power, c(0.8, NA))
  expect_equal(mo$table$effect_size, c(0.5, 0.5))
  expect_equal(mo$table$effect_size_metric, c("Cohen's d", "Cohen's d"))
  expect_equal(mo$table$software, c("pwr", "G*Power"))
  expect_equal(nrow(mo$summary_table), 1)
  expect_equal(mo$summary_table$power_n, 2)
  expect_equal(mo$summary_table$power_complete, 0)

  # multiple papers: each paper's text is now its own call
  paper <- paperlist(
    test_paper(power_text[[1]]),
    test_paper(power_text[[2]])
  )
  testthat::local_mocked_bindings(
    chat = .mock_structured_chat(list(
      "for a Cohen's d = 0.5, and a desired power" = list(power_analyses = list(apriori_incomplete)),
      "paired samples t-test" = list(power_analyses = list(sensitivity_incomplete))
    )), .package = "ellmer")
  mo <- module_run(paper, module)
  expect_equal(mo$traffic_light, "yellow")   # yellow, not red (#695, point 2)
  expect_equal(nrow(mo$table), 2)
  expect_equal(mo$table$power_type, c("apriori", "sensitivity"))
  expect_equal(mo$table$statistical_test, c("unpaired t-test", "paired t-test"))
  expect_equal(mo$table$sample_size, c(128, 64))
  expect_equal(mo$table$alpha_level, c(NA, NA))
  expect_equal(mo$table$power, c(0.8, NA))
  expect_equal(mo$table$effect_size, c(0.5, 0.5))
  expect_equal(mo$table$effect_size_metric, c("Cohen's d", "Cohen's d"))
  expect_equal(mo$table$software, c("pwr", "G*Power"))
  expect_equal(nrow(mo$summary_table), 2)
  expect_equal(mo$summary_table$power_n, c(1, 1))
  expect_equal(mo$summary_table$power_complete, c(0,0))
})

test_that("power, with LLM (structured), model attribution and fallback notice", {
  llm_use(TRUE)
  test_model <- "groq/llama-3.3-70b-versatile" # any valid-shaped model id; not called live
  llm_model(test_model)
  withr::local_options(metacheck.llm.cache = FALSE)

  paper <- test_paper(text = "An a priori power analysis indicated 64 participants per group, d = 0.5, alpha = .05, power = 80%, using an unpaired t-test, run with pwr.")
  testthat::local_mocked_bindings(
    chat = .mock_structured_chat(list(
      "64 participants per group" = list(power_analyses = list(list(
        power_type = "apriori", statistical_test = "unpaired t-test",
        sample_size = 128, alpha_level = 0.05, power = 0.8,
        effect_size = 0.5, effect_size_metric = "Cohen's d", software = "pwr"
      )))
    )), .package = "ellmer")
  mo <- module_run(paper, "power")

  expect_equal(nrow(mo$table), 1)
  # report attributes the run to whatever model was actually used, not a
  # specific hardcoded model id (which providers deprecate over time)
  expect_match(mo$report, test_model, fixed = TRUE, all = FALSE)
  # structured mode succeeded, so the report should NOT mention the fallback
  expect_false(any(grepl("prompt-based extraction", mo$report, fixed = TRUE)))
})

test_that("power falls back to prompt-fenced extraction when structured output is rejected outright", {
  # Simulates a provider whose structured-output validator rejects the power
  # schema (e.g. Groq 400ing on openai/gpt-oss-20b, per issue #323) -- every
  # structured call fails, so the module must fall back to the original
  # prompt-instructed + json_expand() approach rather than losing the check.
  llm_use(TRUE)
  llm_model("groq/llama-3.3-70b-versatile")
  withr::local_options(metacheck.llm.cache = FALSE)

  paper <- test_paper(text = "An a priori power analysis indicated 64 participants per group, d = 0.5, alpha = .05, power = 80%, using an unpaired t-test, run with pwr.")

  testthat::local_mocked_bindings(
    chat = function(...) structure(list(
      chat_structured = function(text, type)
        stop("HTTP 400: json_validate_failed"),
      chat = function(text, echo = FALSE) paste0(
        "```json\n[{\"power_type\":\"apriori\",\"statistical_test\":\"unpaired t-test\",",
        "\"statistical_test_other\":null,\"sample_size\":128,\"alpha_level\":0.05,",
        "\"power\":0.8,\"effect_size\":0.5,\"effect_size_metric\":\"Cohen's d\",",
        "\"effect_size_metric_other\":null,\"software\":\"pwr\"}]\n```"
      )
    ), class = "Chat"), .package = "ellmer")

  mo <- suppressWarnings(module_run(paper, "power"))
  expect_equal(nrow(mo$table), 1)
  expect_equal(mo$table$power_type, "apriori")
  expect_equal(mo$table$statistical_test, "unpaired t-test")
  expect_equal(mo$table$sample_size, 128)
  expect_equal(mo$table$power, 0.8)
  expect_true(any(grepl("prompt-based extraction", mo$report, fixed = TRUE)))
})

test_that("power reports a failed LLM check distinctly from a genuine negative result", {
  # Both structured AND the prompt-fenced fallback fail outright (e.g. the
  # provider is unreachable) -- the module must say the check failed to run,
  # not "no power analyses were detected", which would misrepresent a failure
  # as a real negative finding.
  llm_use(TRUE)
  llm_model("groq/llama-3.3-70b-versatile")
  withr::local_options(metacheck.llm.cache = FALSE)

  paper <- test_paper(text = "An a priori power analysis determined a sample size of 30 for 80% power with a medium effect size, using an unpaired t-test, run with pwr.")

  testthat::local_mocked_bindings(
    chat = function(...) structure(list(
      chat_structured = function(text, type) stop("HTTP 400: json_validate_failed"),
      chat = function(text, echo = FALSE) stop("connection refused")
    ), class = "Chat"), .package = "ellmer")

  mo <- suppressWarnings(module_run(paper, "power"))
  expect_equal(nrow(mo$table), 0)
  expect_equal(mo$traffic_light, "na")
  expect_match(mo$summary_text, "failed to run", fixed = TRUE)
  expect_no_match(mo$summary_text, "No power analyses were detected", fixed = TRUE)
})

test_that("power still reports a genuine negative result as such, not as a failure", {
  # Contrast case for the test above: structured mode succeeds and legitimately
  # finds nothing (an empty power_analyses array) -- this is a real result, not
  # a failure, so the ordinary "no power analyses were detected" message must
  # still be shown.
  llm_use(TRUE)
  llm_model("groq/llama-3.3-70b-versatile")
  withr::local_options(metacheck.llm.cache = FALSE)

  paper <- test_paper(text = "Our 12 participants have a lot of power to detect a moth.")
  testthat::local_mocked_bindings(
    chat = function(...) structure(list(
      chat_structured = function(text, type) list(power_analyses = list())
    ), class = "Chat"), .package = "ellmer")

  mo <- module_run(paper, "power")
  expect_equal(nrow(mo$table), 0)
  expect_equal(mo$summary_text, "No power analyses were detected.")
})

test_that("power structured extraction handles an empty result across all paragraphs", {
  llm_use(TRUE)
  llm_model("groq/llama-3.3-70b-versatile")
  withr::local_options(metacheck.llm.cache = FALSE)

  paper <- test_paper(text = "Our 12 participants have a lot of power to detect a moth.")
  testthat::local_mocked_bindings(
    chat = function(...) structure(list(
      chat_structured = function(text, type) list(power_analyses = list())
    ), class = "Chat"), .package = "ellmer")

  mo <- module_run(paper, "power")
  expect_equal(mo$traffic_light, "na")
  expect_equal(nrow(mo$table), 0)
  expect_equal(mo$summary_table$power_n, 0)
})

test_that("power structured extraction unwraps a tibble-shaped power_analyses array (real ellmer shape)", {
  # Confirmed LIVE against Groq (groq/openai/gpt-oss-20b): ellmer's
  # chat_structured() returns a type_array()'s items as an already-built
  # tibble, not a plain nested list -- so list(power_analyses = list(list(...)))
  # (every mock elsewhere in this file) is NOT representative of what a real
  # provider response looks like. .unnest_result()'s "unwrap a single-field
  # array" fast path only fires for is.list(inner) && !is.data.frame(inner),
  # so a tibble inner value falls through to R's default as.data.frame()
  # flattening instead, producing dotted "power_analyses.field" column names.
  # Reproduces that exact shape (rather than only the live call) so this stays
  # covered without needing network access.
  llm_use(TRUE)
  llm_model("groq/llama-3.3-70b-versatile")
  withr::local_options(metacheck.llm.cache = FALSE)

  paper <- test_paper(text = "An a priori power analysis using G*Power indicated 64 participants per group were needed to detect a Cohen's d = 0.5 with 80% power at an alpha level of .05, using an independent samples t-test.")
  # Any data.frame (not specifically a tibble) triggers .unnest_result()'s
  # fall-through: the "unwrap a single-field array" fast path requires
  # is.list(inner) && !is.data.frame(inner), which a data.frame fails.
  tibble_result <- list(power_analyses = data.frame(
    power_type = factor("apriori", levels = c("apriori", "sensitivity", "posthoc", "unknown")),
    statistical_test = factor("unpaired t-test", levels = c("paired t-test", "unpaired t-test")),
    statistical_test_other = NA_character_,
    sample_size = 64, alpha_level = 0.05, power = 0.8, effect_size = 0.5,
    effect_size_metric = factor("Cohen's d", levels = c("Cohen's d", "other")),
    effect_size_metric_other = NA_character_,
    software = factor("G*Power", levels = c("G*Power", "pwr"))
  ))
  testthat::local_mocked_bindings(
    chat = function(...) structure(list(
      chat_structured = function(text, type) tibble_result
    ), class = "Chat"), .package = "ellmer")

  mo <- module_run(paper, "power")
  expect_equal(nrow(mo$table), 1)
  expect_equal(mo$table$power_type, "apriori")
  expect_equal(mo$table$statistical_test, "unpaired t-test")
  expect_equal(mo$table$sample_size, 64)
  expect_equal(mo$table$alpha_level, 0.05)
  expect_equal(mo$table$power, 0.8)
  expect_equal(mo$table$effect_size, 0.5)
  expect_equal(mo$table$effect_size_metric, "Cohen's d")
  expect_equal(mo$table$software, "G*Power")
  expect_true(mo$table$complete)
  # every enum-typed column must come back as plain character, not factor --
  # a factor leaking through would print as raw structure() internals in the
  # report's embedded table and could break string comparisons downstream
  expect_true(is.character(mo$table$power_type))
  expect_true(is.character(mo$table$statistical_test))
  expect_true(is.character(mo$table$effect_size_metric))
  expect_true(is.character(mo$table$software))
})

test_that("power structured extraction treats an omitted optional key as not-extracted, not as complete", {
  # A provider may legally omit an optional (required = FALSE) key entirely
  # rather than emitting it as null, since ellmer's schema only lists
  # required fields -- confirm the column still ends up NA (not silently
  # dropped from the completeness check).
  llm_use(TRUE)
  llm_model("groq/llama-3.3-70b-versatile")
  withr::local_options(metacheck.llm.cache = FALSE)

  paper <- test_paper(text = "An a priori power analysis determined a sample size of 30 for 80% power with a medium effect size, using an unpaired t-test, run with pwr.")
  testthat::local_mocked_bindings(
    chat = function(...) structure(list(
      chat_structured = function(text, type) list(power_analyses = list(list(
        power_type = "apriori", statistical_test = "unpaired t-test",
        sample_size = 30, power = 0.8, effect_size = 0.5,
        effect_size_metric = "Cohen's d", software = "pwr"
        # alpha_level key entirely absent, not null
      )))
    ), class = "Chat"), .package = "ellmer")

  mo <- module_run(paper, "power")
  expect_true("alpha_level" %in% names(mo$table))
  expect_true(is.na(mo$table$alpha_level))
  expect_false(mo$table$complete)
  expect_equal(mo$traffic_light, "yellow")   # yellow, not red (#695, point 2)
})

test_that("power structured extraction keeps rows from a partially-failing call", {
  # Only some rows of a multi-paragraph call fail structured extraction (not
  # a systemic rejection) -- llm()'s own retry already covers transient
  # failures, so this should NOT trigger the module's fallback; the failed
  # paragraph is simply treated as "no power analysis found" there, same as
  # a genuinely empty array.
  llm_use(TRUE)
  llm_model("groq/llama-3.3-70b-versatile")
  withr::local_options(metacheck.llm.cache = FALSE)

  power_text <- c(
    "An a priori power analysis indicated 64 participants per group, d = 0.5, alpha = .05, power = 80%, using an unpaired t-test, run with pwr.",
    "A different paragraph that always errors during structured extraction."
  )
  paper <- test_paper(power_text)
  paper$text$paragraph_id <- 0:1

  testthat::local_mocked_bindings(
    chat = function(...) structure(list(chat_structured = function(text, type) {
      if (grepl("64 participants per group", text, fixed = TRUE)) {
        list(power_analyses = list(list(
          power_type = "apriori", statistical_test = "unpaired t-test",
          sample_size = 128, alpha_level = 0.05, power = 0.8,
          effect_size = 0.5, effect_size_metric = "Cohen's d", software = "pwr"
        )))
      } else {
        stop("HTTP 400: json_validate_failed")
      }
    }), class = "Chat"), .package = "ellmer")

  mo <- suppressWarnings(module_run(paper, "power"))
  expect_equal(nrow(mo$table), 1)
  expect_equal(mo$table$power_type, "apriori")
  expect_false(any(grepl("prompt-based extraction", mo$report, fixed = TRUE)))
})

test_that("power, with Ollama (structured)", {
  # The old HTTP-recorded fixtures (localhost-11434/api/chat-*) only ever
  # captured Ollama's NATIVE /api/chat endpoint, used for unstructured calls;
  # structured mode routes through the OpenAI-compatible /v1/ endpoint
  # instead (see llm()'s use_ollama_native <- ... && !structured), which those
  # recordings never exercised. This is a structural check (mocked, like the
  # Groq scenarios above) rather than a live-model-quality regression test --
  # the previous version of this test recorded genuine qwen2.5:3b imprecision
  # (see git history), which a mock cannot represent.
  module <- "power"
  llm_use(TRUE)
  withr::defer(llm_use(FALSE))
  withr::local_options(metacheck.llm.cache = FALSE)
  llm_model("ollama/qwen2.5:3b")

  power_text <- c(
    "An a priori power analysis for an independent samples t-test, conducted using the pwr.t.test function from pwr (Champely, 2020), indicated that for a Cohen's d = 0.5, and a desired power level of 80% required at least 64 participants in each group.",
    "A sensitivity power analysis for a paired samples t-test, conducted using G-Power, indicated that with 64 participants, an adequate power was reached for an effect size of d = 0.5."
  )

  paper <- paperlist(
    test_paper(power_text[[1]]),
    test_paper(power_text[[2]])
  )
  # Structured Ollama calls go through ellmer::chat_vllm() on Ollama's /v1
  # address since #494, so that is replaced as well as ellmer::chat().
  mock_chat <- .mock_structured_chat(list(
    "for a Cohen's d = 0.5, and a desired power" = list(power_analyses = list(list(
      power_type = "apriori", statistical_test = "unpaired t-test",
      sample_size = 128, power = 0.8,
      effect_size = 0.5, effect_size_metric = "Cohen's d", software = "pwr"
    ))),
    "paired samples t-test" = list(power_analyses = list(list(
      power_type = "sensitivity", statistical_test = "paired t-test",
      sample_size = 64,
      effect_size = 0.5, effect_size_metric = "Cohen's d", software = "G*Power"
    )))
  ))
  testthat::local_mocked_bindings(chat = mock_chat, chat_vllm = mock_chat,
                                  .package = "ellmer")
  mo <- module_run(paper, module)
  expect_equal(mo$traffic_light, "yellow")   # yellow, not red (#695, point 2)
  expect_equal(nrow(mo$table), 2)
  expect_equal(mo$table$power_type, c("apriori", "sensitivity"))
  expect_equal(mo$table$statistical_test, c("unpaired t-test", "paired t-test"))
  expect_equal(mo$table$sample_size, c(128, 64))
  expect_equal(mo$table$alpha_level, c(NA, NA))
  expect_equal(mo$table$power, c(0.8, NA))
  expect_equal(mo$table$effect_size, c(0.5, 0.5))
  expect_equal(mo$table$effect_size_metric, c("Cohen's d", "Cohen's d"))
  expect_equal(mo$table$software, c("pwr", "G*Power"))
  expect_equal(nrow(mo$summary_table), 2)
  expect_equal(mo$summary_table$power_n, c(1, 1))
  expect_equal(mo$summary_table$power_complete, c(0,0))
}, "mock") # "mock" activates the HTTP fixtures llm()'s own ollama_up / model-
           # exists pre-checks need (localhost-11434/api/version.json,
           # api/tags.json) -- the actual chat call is still mocked above via
           # local_mocked_bindings(), since those fixtures only cover the
           # native /api/chat endpoint, not structured mode's /v1/ one.


# The module's internal helpers are not in the package namespace, so source the
# module into an environment to reach them directly.
.pw <- new.env()
sys.source(metacheck:::module_find("power"), envir = .pw)

test_that("power maps free-text answers onto the known values (#475)", {
  sw <- .pw$.power_canonical(
    c("G*Power 3.1", "GPower", "POWSIM v4.1", "pwr.t.test in R", "the pwrss package",
      "simulation using simr", "Monte Carlo simulation", "pass.lme", "NCSS PASS",
      "R", "none", NA),
    .pw$.power_software_rules())
  expect_equal(sw, c("G*Power", "G*Power", "POWSIM", "pwr", "pwrss", "simr",
                     "simulation", "pass.lme", "PASS", "other", NA, NA))
  # every listed R package maps onto itself
  pkgs <- .pw$.power_software_packages()
  expect_equal(.pw$.power_canonical(pkgs, .pw$.power_software_rules()), pkgs)

  tests <- .pw$.power_canonical(
    c("independent samples t-test", "paired-samples t test", "one-sample t-test",
      "one-way ANOVA", "2 x 2 ANOVA", "2 x 3 x 2 ANOVA", "MANOVA", "logistic regression",
      "chi-square test", "Pearson correlation", "t-test", "linear mixed model"),
    .pw$.power_test_rules())
  expect_equal(tests, c("unpaired t-test", "paired t-test", "one-sample t-test",
                        "1-way ANOVA", "2-way ANOVA", "3-way ANOVA", "MANOVA",
                        "regression", "chi-square", "correlation", "other", "other"))

  es <- .pw$.power_canonical(
    c("Cohen's d", "Cohen\u2019s dz", "d", "Hedges' g", "Cohen's f", "Cohen's f2",
      "partial eta squared", "\u03b7p\u00b2", "eta squared", "raw mean difference",
      "standardized mean difference", "odds ratio"),
    .pw$.power_es_rules())
  expect_equal(es, c("Cohen's d", "Cohen's d", "Cohen's d", "Hedges' g", "Cohen's f",
                     "other", "partial eta squared", "partial eta squared",
                     "eta squared", "unstandardised", "other", "other"))

  pt <- .pw$.power_canonical(c("a priori", "Sensitivity", "post-hoc", "observed power",
                               "compromise", "none"),
                             .pw$.power_type_rules(), other = "unknown")
  expect_equal(pt, c("apriori", "sensitivity", "posthoc", "posthoc", "unknown", NA))

  # an answer that becomes "other" is kept in the *_other column
  tbl <- .pw$.power_canonicalise(data.frame(
    power_type = "a priori", software = c("POWSIM", "a custom R script"),
    statistical_test = c("unpaired t-test", "linear mixed model")))
  expect_equal(tbl$software, c("POWSIM", "other"))
  expect_equal(tbl$software_other, c(NA, "a custom R script"))
  expect_equal(tbl$statistical_test_other, c(NA, "linear mixed model"))

  # the schema has no enum left that an answer could fail
  spec <- .pw$.power_type_spec()
  item <- spec@properties$power_analyses@items@properties
  expect_false(any(vapply(item, \(p) inherits(p, "ellmer::TypeEnum"), logical(1))))
})

test_that("power reports paragraphs the LLM could not check (#475)", {
  llm_use(TRUE)
  llm_model("groq/llama-3.3-70b-versatile")
  withr::local_options(metacheck.llm.cache = FALSE)

  power_text <- c(
    "We used POWSIM v4.1 to estimate power: with 40 loci, an alpha level of 0.05 and Cohen's d = 0.5, an a priori power analysis for an unpaired t-test gave 80% power with 128 samples.",
    "A sensitivity power analysis for a paired samples t-test indicated that with 64 participants, an adequate power was reached for an effect size of d = 0.5."
  )
  paper <- paperlist(test_paper(power_text[[1]]), test_paper(power_text[[2]]))
  # the first paragraph's answer names software the old fixed list lacked;
  # the second paragraph's call fails, as Groq's did for issue #475
  mock <- function(...) structure(list(
    chat_structured = function(text, type) {
      if (grepl("paired samples", text, fixed = TRUE))
        stop("HTTP 400 Bad Request. Generated JSON does not match the expected schema")
      list(power_analyses = list(list(
        power_type = "a priori", statistical_test = "independent samples t-test",
        sample_size = 128, alpha_level = 0.05, power = 0.8, effect_size = 0.5,
        effect_size_metric = "Cohen's d", software = "POWSIM v4.1")))
    }
  ), class = "Chat")
  testthat::local_mocked_bindings(chat = mock, .package = "ellmer")
  mo <- suppressWarnings(module_run(paper, "power"))

  expect_equal(nrow(mo$table), 1)
  expect_equal(mo$table$power_type, "apriori")
  expect_equal(mo$table$statistical_test, "unpaired t-test")
  expect_equal(mo$table$software, "POWSIM")
  expect_true(mo$table$complete)
  # the failed paragraph is reported, and turns a green result yellow
  expect_match(mo$summary_text, "1 of 2 paragraphs could not be checked by the LLM",
               fixed = TRUE)
  expect_true(any(grepl("could not be checked", mo$report, fixed = TRUE)))
  expect_equal(mo$traffic_light, "yellow")
  expect_equal(mo$summary_table$power_unchecked[order(mo$summary_table$paper_id)],
               c(0, 1)[order(paper_id(paper))])
})

test_that("power's fallback uses the module's own schema, and a field no answer gives is missing (U108)", {
  # the fallback downloaded its schema from scienceverse.org (failing
  # offline); a field missing from every answer was skipped by any_of(),
  # which could mark the analysis complete
  llm_use(TRUE)
  llm_model("groq/llama-3.3-70b-versatile")
  withr::local_options(metacheck.llm.cache = FALSE)
  paper <- test_paper(text = "An a priori power analysis indicated 64 participants per group, d = 0.5.")
  prompts <- character(0)
  testthat::local_mocked_bindings(
    chat = function(..., system_prompt = NULL) {
      prompts <<- c(prompts, system_prompt %||% "")
      structure(list(
        chat_structured = function(text, type) stop("HTTP 400: json_validate_failed"),
        chat = function(text, echo = FALSE) paste0(
          "```json\n[{\"power_type\":\"apriori\",\"statistical_test\":\"unpaired t-test\",",
          "\"sample_size\":128,\"effect_size\":0.5,\"effect_size_metric\":\"Cohen's d\"}]\n```")
      ), class = "Chat")
    }, .package = "ellmer")
  mo <- suppressWarnings(module_run(paper, "power"))
  expect_equal(mo$table$power_type, "apriori")
  expect_false(isTRUE(mo$table$complete))    # alpha, power and software were not given
  expect_true(any(grepl("\"title\": \"Power Analyses\"", prompts, fixed = TRUE)))
})
