if (online("scienceverse.org")) {
  url <- "https://scienceverse.org/schema/paper.json"
  destfile <- "inst/schema/paper.json"
  download.file(url, destfile, quiet = TRUE)
}
