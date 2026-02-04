mod_compare <- function(rds, mod1, mod2) {
  mm_tables <- map(rds, \(filename) {
    message(filename)

    tryCatch({
      # choose or sample and merge
      paper <- filename |>
        lapply(readRDS) |>
        unlist(recursive = FALSE) |>
        paperlist()

      # run and time version1
      t1 <- system.time(
        mo1 <- module_run(paper, mod1)
      )
      message(mod1, " time: ", round(t1[[3]], 1))

      # run and time the overinclusive version
      t2 <- system.time(
        mo2 <- module_run(paper, mod2)
      )
      message(mod2, " time: ", round(t2[[3]], 1))

      # find mismatches
      mismatch1 <- setdiff(mo1$summary_table,
                           mo2$summary_table)
      mismatch2 <- setdiff(mo2$summary_table,
                           mo1$summary_table)
      mismatch <- bind_rows(mismatch1, mismatch2)

      # get exact values for mismatches
      table1 <- mo1$table |>
        filter(id %in% mismatch$id) |>
        select(id, text1 = text)
      table2 <- mo2$table |>
        filter(id %in% mismatch$id) |>
        select(id, text2 = text)
      mismatch_table <- full_join(table1, table2, by = "id")

      mismatch_table$filename <- filename

      mismatch_table
    }, error = \(e) {
      data.frame(filename = filename, error = e$message)
    })
  })

  mm_table <- list_rbind(mm_tables)
  names(mm_table) <- c("id", mod1, mod2, "paper_set")

  mm_table
}
