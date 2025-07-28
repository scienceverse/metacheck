refs <- concat_tables(paper, "bib")
cites <- concat_tables(paper, "xrefs")

missing_cites <- dplyr::anti_join(refs, cites, by = c("id", "ref_id"))
if (nrow(missing_cites)) missing_cites$missing <- "citation"
missing_refs <- dplyr::anti_join(cites, refs,  by = c("id", "ref_id"))
if (nrow(missing_refs)) missing_refs$missing <- "reference"
names(missing_refs) <- names(missing_refs) |> sub("text", "ref", x = _)

table <- dplyr::bind_rows(missing_cites, missing_refs) |>
  dplyr::arrange(id, ref_id)

tl_table <- info_table(paper, c()) |>
  dplyr::left_join(dplyr::count(refs, id, name = "nrefs"), by = "id") |>
  dplyr::left_join(dplyr::count(table, id), by = "id")

traffic_light <- dplyr::case_when(
  is.na(tl_table$nrefs) ~ "na",
  is.na(tl_table$n) ~ "green",
  tl_table$n > 0 ~ "red"
)

if (is_paper_list(paper)) {
  names(traffic_light) <- names(paper)
}

# return
list(
  table = table,
  traffic_light = traffic_light
)
