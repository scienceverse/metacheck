library(tidyverse)

#-------------------------------------------------------------------------------
# read in raw data
files <- list.files("predictability survey/Hoogeveen/Data/Raw_Data", ".csv", full.names = TRUE)
names(files) <- str_extract(files, "Data_.+_AN") %>% str_sub(6,-4)
raw_data <- map_dfr(files, read_csv, .id = "dataset")

replication_results <- 
  read_csv('predictability survey/Hoogeveen/Data/Processed_Data/DescriptionAndStatisticsPerStudy.csv') %>%
  select(item = `Study number`, StudyID, study_outcome = `RS outcome`, correct_answer = `correct answer`, Project_name)

#-------------------------------------------------------------------------------
# reshape to long, clean up coding

responses_long <- 
  raw_data %>%
  mutate(
    ID = row_number(),
  ) %>%
  filter(
    phd == 2, # No PhD in psychology,
    knowledge == 5, # Not already familiar with ML2 or SSRP
  ) %>%
  select(dataset, ID, `Duration (in seconds)`, RecordedDate,language, psych, preorder, sequence, Conditie, matches("[0-9]+_(des|bf)")) %>%
  pivot_longer(
    cols = matches("[0-9]+_(des|bf)"),
    names_to = c("item",".value"),
    names_pattern = "(.+)\\.(.+)"
  ) %>%
  mutate(
    item_language = if_else(is.na(belief), "Dutch", "English"),
    understanding = if_else(is.na(belief), `0NL`, `0`),
    confidence = if_else(is.na(belief), confNL_1, conf_1),
    belief = if_else(is.na(belief), beliefNL, belief)
  ) %>%
  select(-`0`, -`0NL`, -conf_1, -confNL_1, -beliefNL) %>%
  filter(!is.na(belief)) %>%
  mutate(
    language = recode(language, `1` = "English", `2` = "Dutch"),
    understanding = recode(understanding, `1` = "did not understand", .missing = "understood"),
    belief = recode(belief, `1` = "Will replicate", `2` = "Will not replicate"),
    condition = str_extract(item, "[a-z]+$"),
    item = as.numeric(str_extract(item, "^[1-9]+"))
  )

#-------------------------------------------------------------------------------
# Apply exclusion criteria

passed_attention_check <- 
  responses_long %>%
  group_by(ID) %>%
  filter(
    item == 28,
    belief == "Will not replicate",
    confidence <= 80, confidence >= 70
  ) %>%
  select(ID)

responses_clean <- 
  responses_long %>%
  
  # exclude respondents not passing attention check
  semi_join(passed_attention_check, by = "ID") %>%
  
  # exclude studies not understood by at least half of participants
  group_by(item) %>%
  mutate(study_pct_understood = mean(understanding == "understood")) %>%
  filter(study_pct_understood >= 0.5) %>%
  select(-study_pct_understood) %>%
  
  # exclude respondents who understood less than half of studies
  group_by(ID) %>%
  mutate(respondent_pct_understood = mean(understanding == "understood")) %>%
  filter(respondent_pct_understood >= 0.5) %>%
  select(-respondent_pct_understood) %>%
  ungroup() %>%
  
  # exclude item responses not understood
  filter(understanding == "understood") %>%
  
  # exclude attention check study
  filter(item != 28)

responses_clean %>%
  group_by(condition, ID) %>%
  summarise() %>%
  count()

#-------------------------------------------------------------------------------
# merge actual replication results

responses_with_results <- 
  responses_clean %>%
  left_join(replication_results, by = "item") %>%
  mutate(
    ID = factor(ID),
    correct_answer = recode(correct_answer, `1` = "Will replicate", `2` = "Will not replicate"),
    correct = belief == correct_answer,
    condition = recode(condition, des = "Description only", bf = "Bayes factor")
  )

saveRDS(responses_with_results, "predictability survey/Hoogeveen/cleaned response data.rds")

just_correct <- 
  responses_with_results %>%
  select(ID, StudyID, correct)

library(lme4)
NO_1PL <- glmer(correct ~ (1 | ID) + (1 | StudyID),
                data = responses_with_results,
                family = binomial(link = "probit"))
summary(NO_1PL)
beta <- fixef(NO_1PL)[["(Intercept)"]]
tau_gamma <- getME(NO_1PL, "theta")[["StudyID.(Intercept)"]]
sigma <- getME(NO_1PL, "theta")[["ID.(Intercept)"]]
dpi_dgamma <- dnorm(beta / sqrt(1 + sigma^2)) / sqrt(1 + sigma^2)
tau <- tau_gamma * dpi_dgamma
  
library(metafor)
library(clubSandwich)
study_prob_fit <- lm(correct ~ 0 + StudyID, data = responses_with_results)
summary(coef(study_prob_fit))
V_prob <- as.matrix(vcovCR(study_prob_fit, cluster = responses_with_results$ID, type = "CR2"))
prob_dat <- data.frame(study = names(coef(study_prob_fit)), pr = coef(study_prob_fit))
rma.mv(yi = coef(study_prob_fit), V = V_prob, random = ~ 1 | study, data = prob_dat)
