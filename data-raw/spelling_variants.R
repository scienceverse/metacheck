# British, Canadian and Australian spellings and their American forms
#
# ref_accuracy compares titles and journal names after folding spelling
# variants ("behaviour" and "behavior", "paediatric" and "pediatric",
# "analyse" and "analyze"), so that a citation in another spelling is not a
# mismatch. The variants come from VarCon, part of SCOWL
# (http://wordlist.aspell.net/, MIT-like licence, see inst/COPYRIGHTS). Each
# spelling on a VarCon line is mapped to the line's American spelling.
# Spellings that VarCon marks as improper ("x", a misspelling) or as only
# possible ("-") are left out, so that misspellings are not folded
# away. A spelling that would map to two American forms is left out too.

release <- "rel-2020.12.07"
url <- sprintf("https://raw.githubusercontent.com/en-wl/wordlist/%s/varcon/varcon.txt", release)
# the file is in Latin-1; a note after "|" ("<N>" for a noun) is dropped
lines <- iconv(readLines(url), "latin1", "UTF-8")
lines <- trimws(sub("\\|.*$", "", lines))
lines <- lines[grepl("^[A-Z_]", lines) & grepl(":", lines, fixed = TRUE)]

pairs <- lapply(strsplit(lines, " / ", fixed = TRUE), \(parts) {
  tags <- sub(":.*$", "", parts)
  words <- tolower(trimws(sub("^[^:]*:\\s*", "", parts)))
  tag_list <- strsplit(tags, " ", fixed = TRUE)
  # the preferred American spelling: a tag "A" without a variant mark
  us <- words[vapply(tag_list, \(t) "A" %in% t, logical(1))]
  if (length(us) != 1) return(NULL)
  # a spelling is kept when it has a tag that is not improper or doubtful
  ok <- vapply(tag_list, \(t) any(!grepl("[x-]$", t) & !grepl("^_", t)), logical(1))
  keep <- ok & words != us & !grepl("[^a-z]", words) & !grepl("[^a-z]", us)
  if (!any(keep)) return(NULL)
  data.frame(variant = words[keep], us = us)
})
pairs <- unique(do.call(rbind, pairs))
# a spelling that is itself an American form, or maps to two, is left out
pairs <- pairs[!pairs$variant %in% pairs$us, ]
twice <- pairs$variant[duplicated(pairs$variant)]
pairs <- pairs[!pairs$variant %in% twice, ]
variants <- stats::setNames(pairs$us, pairs$variant)
attr(variants, "source") <- url

saveRDS(variants, "inst/databases/spelling_variants.Rds", compress = "xz")
