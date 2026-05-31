library(dplyr)

dat <- read.csv("missing_file.csv")

result <- lm(y ~ x, data = dat)
