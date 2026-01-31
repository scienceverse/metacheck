devtools::load_all("~/rproj/scienceverse/metacheck/")
library(tidyverse)
source("_stuff/mod_compare.R")

# read in all RDS files
rds <- list.files("../_papers", "\\.rds", full.names = TRUE)

# funding check ----
mod1 <-  "funding_check"
mod2 <- "funding_check_oi"

funding_table <- mod_compare(rds, mod1, mod2)
write_csv(funding_table, "_stuff/funding_mismatch.csv")

# coi_check ----

mod1 <- "coi_check"
mod2 <- "coi_check_oi"

coi_table <- mod_compare(rds, mod1, mod2)
write_csv(coi_table, "_stuff/coi_mismatch.csv")
