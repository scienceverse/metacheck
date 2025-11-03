library(dplyr)
library(metacheck)

psychsci_all_urls <- module_run(psychsci, "all-urls")
expanded_urls <- expand_text(psychsci_all_urls$table, psychsci) |>
  filter(!grepl("osf.io/tvyxz", expanded, fixed = TRUE),
         !grepl("content/by/supplemental", expanded, fixed = TRUE),
         !grepl("psychologicalscience.org/publications/badges", expanded, fixed = TRUE),
         !grepl("http://pss.sagepub.com/content/25/1/3.full", expanded, fixed = TRUE))

llm_answers <- llm(expanded_urls, "Does this sentence contain a reference to shared data, code, materials and/or (pre)registration? Return your answer in JSON format like: {data: TRUE, code: TRUE, materials: FALSE, reg: TRUE}", "expanded")

llm_answers <- llm_answers |>
  tidyr::separate(answer, c(NA, NA, "llm_data", NA, "llm_code", NA, "llm_materials", NA, "llm_reg", NA))

z <- mutate(llm_answers,
            auto_data = grepl("data", expanded, ignore.case = TRUE),
            auto_code = grepl("code|analys|script", expanded, ignore.case = TRUE),
            auto_materials = grepl("material|stimul", expanded, ignore.case = TRUE),
            auto_reg = grepl("registra", expanded, ignore.case = TRUE),
            data = "",
            code = "",
            materials = "",
            reg = "",
            other = ""
)

data_same <- mean(z$llm_data == z$auto_data)
code_same <- mean(z$llm_code == z$auto_code)
mat_same <- mean(z$llm_materials == z$auto_materials)
reg_same <- mean(z$llm_reg == z$auto_reg)

readr::write_csv(z, "URLS.csv")

# human coding!

url <- readr::read_csv("URLS.csv")
