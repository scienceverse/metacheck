suppressMessages(library(metacheck)); suppressMessages(library(dplyr))
llm_use(TRUE); llm_model("ollama/qwen2.5:3b"); llm_max_calls(20)

sys <- paste(
  "You are screening flagged statistics from psychology papers.",
  "For each TARGET mean reported in the SENTENCE, decide whether the",
  "values that were averaged to produce it are whole numbers (integers).",
  "Integer data: Likert/rating scale responses, counts, number of",
  "items/errors/trials, age in whole years.",
  "Non-integer data: reaction or response times, durations, EEG/ERP",
  "amplitudes (uV/mV), difference scores, proportions or percentages of",
  "trials, physical measurements, money, scores already averaged across",
  "multiple scale items.",
  "Answer with a JSON object {\"integer_data\": true} or",
  "{\"integer_data\": false}. If you cannot tell, answer true",
  "(only suppress a flag when you are confident the data are non-integer).")

cases <- c(
  "TARGET MEAN: M = 2.63\n\nSENTENCE: Girls (M = 3.03, SD = 0.64, n = 22) scored higher on working memory than boys did (M = 2.63, SD = 0.74, n = 31), t(51) = -2.06, p =.045.",
  "TARGET MEAN: mean age = 23.94 years\n\nSENTENCE: Fifty-nine healthy volunteers (24 males, 35 females; mean age = 23.94 years, age range 19-35) were recruited.",
  "TARGET MEAN: M = -1.28\n\nSENTENCE: repeated displays elicited an increased negative amplitude (M = -1.28 uV) relative to nonrepeated displays (M = -0.46 uV), t(15) = ...")

r <- llm(text = cases, system_prompt = sys, params = list(seed = 8675309))
cat("raw answer column:\n"); print(r$answer)
