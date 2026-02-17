coi_text <- readLines("_stuff/coi.txt")
paper <- paper()
paper$text <- data.frame(
  id = paper$id,
  text = coi_text
)
