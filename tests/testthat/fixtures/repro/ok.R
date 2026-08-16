dat <- read.csv("data.csv")
t.test(dat$x ~ dat$g)
