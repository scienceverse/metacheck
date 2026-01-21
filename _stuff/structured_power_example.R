# https://ellmer.tidyverse.org/articles/structured-data.html

# does not work
type_power <-  ellmer::type_array(
  ellmer::type_from_schema(path = "../schema/power.json")
)

model <- "openai/gpt-4.1" # works
model <- "groq/openai/gpt-oss-120b"
prompt <-"Identify and classify power analyses from exerpts of scientific manuscripts. Use null when information is missing, do not invent values. Only use 'other' if a value not in the enumerated options can be identified. There may be no power analysis in the text."

chat <- ellmer::chat(model, system_prompt = prompt)

text <- "An a priori power analysis was conducted to estimate the sample size required to achieve 80% power to detect a Cohen's d of 0.2 using an unpaired t-test at an alpha level of 0.05. This required a total sample size of 300 participants. A second a priori power analysis was conducted to estimate the required sample size for a secondary outcome. To achieve 80% power to detect a Cohen's f of 0.1 using a one-way ANOVA, a sample size of 350 was required. The a priori power analyses were conducted with G*Power."

data <- chat$chat_structured(text, type = type_power)



# works
type_power <- ellmer::type_from_schema(
  path = "../schema/power_array.json"
)

model <- "groq/openai/gpt-oss-20b"
prompt <-"Identify and classify power analyses from exerpts of scientific manuscripts. Use null when information is missing, do not invent values. Only use 'other' if a value not in the enumerated options can be identified. There may be no power analysis in the text."

chat <- ellmer::chat(model, system_prompt = prompt)

text <- "An a priori power analysis was conducted to estimate the sample size required to achieve 80% power to detect a Cohen's d of 0.2 using an unpaired t-test at an alpha level of 0.05. This required a total sample size of 300 participants. A second a priori power analysis was conducted to estimate the required sample size for a secondary outcome. To achieve 80% power to detect a Cohen's f of 0.1 using a one-way ANOVA, a sample size of 350 was required. The a priori power analyses were conducted with G*Power."

data <- chat$chat_structured(text, type = type_power)

library(testthat)

exp1 <- list(
  text = "An a priori power analysis was conducted to estimate the sample size required to achieve 80% power to detect a Cohen's d of 0.2 using an unpaired t-test at an alpha level of 0.05. This required a total sample size of 300 participants.",
  power_type = "apriori",
  statistical_test = "unpaired t-test",
  statistical_test_other = NULL,
  sample_size = 300L,
  alpha_level = 0.05,
  power = 0.8,
  effect_size = 0.2,
  effect_size_metric = "Cohen's d",
  effect_size_metric_other = NULL,
  software = "G*Power"
)

exp2 <- list(
  text = "A second a priori power analysis was conducted to estimate the required sample size for a secondary outcome. To achieve 80% power to detect a Cohen's f of 0.1 using a one-way ANOVA, a sample size of 350 was required.",
  power_type = "apriori",
  statistical_test = "1-way ANOVA",
  statistical_test_other = NULL,
  sample_size = 350L,
  alpha_level = NULL,
  power = 0.8,
  effect_size = 0.1,
  effect_size_metric = "Cohen's f",
  effect_size_metric_other = NULL,
  software = "G*Power"
)

expect_equal(data$power_analyses[[1]], exp1)

# second alpha might be 0.05 or NULL
a <- data$power_analyses[[2]]$alpha_level
expect_true(is.null(a) || a == 0.05)
data$power_analyses[[2]]$alpha_level <- NULL
exp2$alpha_level <- NULL
expect_equal(data$power_analyses[[2]], exp2)


text <- "A power analysis showed that 242 participants in each of 2 groups was required for 80% power."

chat <- ellmer::chat(model, system_prompt = prompt)
data <- chat$chat_structured(text, type = type_power)

exp <- list(
  text = text,
  power_type = "apriori",
  statistical_test = NULL,
  statistical_test_other = NULL,
  sample_size = 484L,
  alpha_level = NULL,
  power = 0.8,
  effect_size = NULL,
  effect_size_metric = NULL,
  effect_size_metric_other = NULL,
  software = NULL
)

expect_equal(length(data$power_analyses), 1)
expect_equal(data$power_analyses[[1]], exp)


# parallel prompts
type_power <- ellmer::type_from_schema(
  path = "../schema/power_array.json"
)

# get some text
metacheck::llm_use(FALSE)
x <- module_run(psychsci[1:10], "power")
text <- x$table$text

# set up chat
chat <- ellmer::chat(model, system_prompt = prompt)
# data <- chat$chat_structured(text, type = type_power)
# tbl <- do.call(dplyr::bind_rows, data$power_analyses)

data <- ellmer::parallel_chat_structured(chat, text, type_power)

