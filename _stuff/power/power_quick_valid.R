llm_use(TRUE)
llm_max_calls(500)
power <- module_run(psychsci, "power")


# check power_words
llm_use(FALSE)
power_words <- module_run(psychsci, "power") # narrowing to power_words
power_no <- module_run(psychsci, "power") # no power_words

# check omitted text
table <- dplyr::setdiff(power_no$table, power_words$table)[, 1:7]
llm_use(TRUE)
llm_max_calls(nrow(table))
paper <- paper()
paper$full_text <- table
power_check <- module_run(paper, "power")

# wrongly omitted text
power_check$table$text



