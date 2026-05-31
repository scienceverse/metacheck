# Analysis script
library(dplyr)
library(ggplot2)

# Load the data
dat <- read.csv("data.csv")

# Compute summary
result <- dat |> dplyr::summarise(mean_x = mean(x))
