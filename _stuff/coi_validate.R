coi_text <- readLines("_stuff/coi.txt")
paper <- paper()
paper$text <- data.frame(
  paper_id = paper$paper_id,
  text = coi_text
)
