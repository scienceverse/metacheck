# The authors run this script from the top of the project.
# Markers: KEEP = the line must stay as written; RESOLVES = the path in the
# line must lead to the file named after the id, from where the script runs.
library(here)
RAW <- "data/raw"
d01 <- read.csv("data/raw/raw_lab.csv") # KEEP r01
d02 <- read.csv("./data/raw/raw_lab.csv") # KEEP r02
d03 <- read.csv("raw_lab.csv") # RESOLVES r03 data/raw/raw_lab.csv
d04 <- read.csv("C:/Users/me/project/data/raw/raw_lab.csv") # RESOLVES r04 data/raw/raw_lab.csv
d05 <- read.csv("~/project/raw_online.csv") # RESOLVES r05 data/raw/raw_online.csv
d06 <- read.csv("data\\raw\\raw_lab.csv") # RESOLVES r06 data/raw/raw_lab.csv
d07 <- read.csv(file.path("data", "raw", "raw_lab.csv")) # KEEP r07
d08 <- read.csv(file.path(RAW, "raw_lab.csv")) # KEEP r08
d09 <- read.csv(paste0(RAW, "/raw_lab.csv")) # KEEP r09
d10 <- read.csv(paste(RAW, "raw_lab.csv", sep = "/")) # KEEP r10
d11 <- read.csv(stringr::str_c(RAW, "/raw_online.csv")) # KEEP r11
d12 <- read.csv(here::here("data", "raw", "raw_lab.csv")) # KEEP r12
d13 <- read.csv(here("data/raw/raw_lab.csv")) # KEEP r13
d14 <- read.csv(here( # KEEP r14a
  "data", # KEEP r14b
  "raw", # KEEP r14c
  "raw_lab.csv" # KEEP r14d
))
source(here::here("scripts", "helper.R")) # KEEP r15
f16 <- "raw_lab.csv" # RESOLVES r16 data/raw/raw_lab.csv
d16 <- read.csv(f16)
files17 <- c(lab = "raw_lab.csv", online = "raw_online.csv") # KEEP r17
d17 <- lapply(files17, function(x) read.csv(file.path(RAW, x)))
d18 <- read.csv(sprintf("data/raw/%s.csv", "raw_lab")) # KEEP r18
f19 <- list.files("data/raw", pattern = "\\.csv$") # KEEP r19
f20 <- Sys.glob("data/raw/*.csv") # KEEP r20
f21 <- sub("^raw_(.*)\\.csv$", "clean_\\1.csv", f19) # KEEP r21
d22 <- jsonlite::read_json("config/settings.json") # KEEP r22
d23 <- read.csv("data/raw/not_deposited.csv") # KEEP r23
REF <- file.path("data", "reference") # KEEP r24a
d24 <- read.csv(file.path(REF, "ref.csv")) # KEEP r24b
d25 <- read.csv(paste0("data/processed/", "trials.csv")) # KEEP r25
# d26 <- read.csv(here("data", "raw", "raw_lab.csv")) # KEEP r26
raw_path <- function(f) file.path(RAW, f)
d27 <- read.csv(raw_path("raw_lab.csv")) # KEEP r27
d28 <- read.csv(here("data", "raw", "Raw_Lab.csv")) # NAMES r28 raw_lab.csv
OLD_DIR <- "C:/Users/me/project/data/raw/" # RESOLVES r29 data/raw
d29 <- read.csv(file.path(OLD_DIR, "raw_online.csv"))
has_raw <- function(f) file.exists(raw_path(f))
d30 <- if (has_raw("raw_lab.csv")) 1 # KEEP r30
