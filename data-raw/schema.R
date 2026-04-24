if (online("scienceverse.org")) {
  url <- "https://scienceverse.org/schema/paper.json"
  destfile <- "inst/schema/paper.json"

  int_hash <- tools::md5sum(destfile)
  download.file(url, destfile, quiet = TRUE)
  ext_hash <- tools::md5sum(destfile)

  if (ext_hash != int_hash) message("schema/paper.json changed")
}
