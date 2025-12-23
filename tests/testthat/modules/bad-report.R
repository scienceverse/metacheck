#' Bad Report
bad_report <- function(paper) {
  list(
    summary_text = "This should make the report fail to render",
    report =  "```{r}\nbad code\n```\n\n `r 1 + `"
  )
}
