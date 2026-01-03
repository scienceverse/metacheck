coi_text <- readLines("_stuff/coi.txt")
paper <- paper()
paper$full_text <- data.frame(
  id = paper$id,
  text = coi_text
)
