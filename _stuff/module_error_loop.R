# check if there are errors in a paper/module

library(metacheck)
library(readr)
library(dplyr)

# list all modules and papers you want to check
papers <- psychsci[1:10]
csv <- "module_error.csv" # change this for each dataset
modules <- "code_check"

# set up table (adds new modules or papers to an existing table)
if (!file.exists(csv)) {
  df <- info_table(papers, "doi")
  df[modules] <- NA
  write_csv(df, csv)
} else {
  df_old <- read_csv(csv)
  df_new <- info_table(papers, "doi") |>
    anti_join(df_old, by = "id")
  df <- bind_rows(df_old, df_new)

  new_modules <- setdiff(modules, names(df))
  if (length(new_modules)) {
    df[new_modules] <- NA
  }
}

# put in a sensible order
df <- select(df, any_of(c("id", "doi")), any_of(modules), everything())

# run each module on each paper and record traffic light or error message
for (module in modules) {
  for (paper in papers) {
    tryCatch({
      idx <- which(paper$id == df$id)

      # only run if not already in the df
      if (is.na(df[[module]][[idx]])) {
        message(paper$id, ": ", module)
        output <- tryCatch({
          time <- system.time( mo <- module_run(paper, module) )
          # for (n in names(mo$summary_table)) {
          #   if (n != "id") {
          #     col <- paste0(module, ".", n)
          #     df[[col]][[idx]] <- as.character(mo$summary_table[[n]])
          #   }
          # }
          mo$traffic_light
        },
        error = \(e) return(e$message))

        df[[module]][[idx]] <- output

        timing <- paste0(module, ".time")
        df[[timing]][[idx]] <- time[[3]]
        write_csv(df, csv) # write to file at each step
      }
    }, error = \(e) { message(e$message)})
  }
}
