# The authors run this script from its own folder, analysis/.
ROOT <- ".." # KEEP a00
d01 <- read.csv("../data/raw/raw_lab.csv") # KEEP a01
source("../scripts/helper.R") # KEEP a02
d03 <- read.csv(file.path("..", "data", "reference", "ref.csv")) # KEEP a03
d04 <- read.csv(file.path(ROOT, "data", "raw", "raw_online.csv")) # KEEP a04
RAW_DIR <- file.path(ROOT, "data", "raw") # KEEP a05a
RAW_FILES <- c(Lab = "raw_lab.csv", Online = "raw_online.csv") # KEEP a05b
d05 <- lapply(RAW_FILES, function(x) read.csv(file.path(RAW_DIR, x)))
