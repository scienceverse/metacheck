# install.packages(c("plumber", "logger"), repos = "https://cloud.r-project.org/") # getting the necessary packages to run the API
library(plumber)
pr <- plumb("api.R")
pr$run(host = "0.0.0.0", port = 2005)
