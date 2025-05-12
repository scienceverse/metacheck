p <- module_run(paper, "all-p-values")$table
p$p_comp <- gsub("p-?(value)?\\s*|\\s*\\d?\\.\\d+e?-?\\d*", "",
                 p$text, ignore.case = TRUE)
p$p_value <- gsub("^p-?(value)?\\s*[<>=≤≥]{1,2}\\s*", "",
                  p$text, ignore.case = TRUE)
p$p_value <- suppressWarnings(as.numeric(p$p_value))
p$imprecise <- p$p_comp == "<" & p$p_value > .001
p$imprecise <- p$imprecise | p$p_comp == ">"
p$imprecise <- p$imprecise | is.na(p$p_value)
cols <- c("text", "p_comp", "p_value", "section", "header", "div", "p", "s", "id")

if (nrow(p) == 0) {
  tl <- "na"
} else if (any(p$imprecise)) {
  tl <- "red"
} else if (!all(p$imprecise)) {
  tl <- "green"
} else {
  tl <- "yellow"
}

list(
  table = p[p$imprecise, cols],
  traffic_light = tl
)
