library(tidyverse) # for data wrangling

# load in the file
orig <- read_delim("folder/file.txt")

# then do some stuff
df <- orig |>
  select(x = 1) |>
  mutate(y = rnorm(nrow(orig)))
  
# do a t-test
t.test(x ~ y, data = df)
