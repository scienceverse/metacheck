refs <- concat_tables(paper, "references")
cites <- concat_tables(paper, "citations")

if (nrow(refs) == 0 & nrow(cites) == 0) {
  table <- data.frame()
  traffic_light <- "na"
} else {
  missing_cites <- dplyr::anti_join(refs, cites, by = c("id", "bib_id"))
  if (nrow(missing_cites)) missing_cites$missing <- "citation"
  missing_refs <- dplyr::anti_join(cites, refs,  by = c("id", "bib_id"))
  if (nrow(missing_refs)) missing_refs$missing <- "reference"
  names(missing_refs) <- names(missing_refs) |> sub("text", "ref", x = _)

  table <- dplyr::bind_rows(missing_cites, missing_refs) |>
    dplyr::arrange(id, bib_id)

  if (nrow(table) == 0) {
    traffic_light <- "green"
  } else {
    traffic_light <- "red"
  }
}

# return
list(
  table = table,
  traffic_light = traffic_light
)
