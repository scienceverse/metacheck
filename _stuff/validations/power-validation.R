power <- readRDS("_stuff/validations/power.Rds")

View(power$table)

options(scipen=999)

paired_t <- power$table |>
  filter(grepl("cohen", es_metric, T)) |>
  filter(grepl("t[ -]test", test, T)) |>
  filter(grepl("pair", test, T)) |>
  select(expanded, type:es_metric) |>
  mutate(alpha = ifelse(is.na(alpha) | alpha == "NA", 0.05, alpha),
         es = as.numeric(es),
         alpha = as.numeric(alpha),
         power = as.numeric(power),
         sample = as.numeric(sample),
         alt = ifelse(grepl("one[- ]tail", test, T), "greater", "two.sided")) |>
  rowwise() |>
  mutate(ss_calc = pwr::pwr.t.test(d = es,
                                   sig.level = alpha,
                                   power = power,
                                   type = "paired",
                                   alternative = alt)$n |> ceiling())

