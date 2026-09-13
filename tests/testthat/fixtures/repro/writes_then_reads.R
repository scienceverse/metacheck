dat <- read.csv("data.csv")
summ <- data.frame(mean_x = mean(dat$x))
write.csv(summ, "intermediate.csv", row.names = FALSE)
