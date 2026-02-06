devtools::load_all(".")
library(tidyverse)

tl_ok <- c("red", "yellow", "green", "na", "info")

ps_errors <- read_csv("_stuff/psychsci errors.csv") |>
  select(-ends_with(".time")) |>
  pivot_longer(-c(id, doi), names_to = "module", values_to = "tl") |>
  filter(!tl %in% tl_ok)

i = 1
id <- ps_errors$id[[i]]
paper <- psychsci[[i]]
module <- ps_errors$module[[i]]

mo <- module_run(paper, module)
