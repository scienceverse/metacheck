dat <- read.csv("data.csv")
m <- lm(x ~ g, data = dat)
anova(m)
s <- summary(m)
s$coefficients[2, , drop = FALSE]
