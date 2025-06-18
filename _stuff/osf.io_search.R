library(papercheck)
library(dplyr)

urls <- module_run(psychsci, "all_urls")
osf <- urls$table |>
  search_text("osf")
utable <- osf |>
  select(url = text) |>
  unique() |>
  mutate(five = sub("https://", "", url) |>
           sub("osf\\.io/", "", x = _)) |>
  mutate(osf_id = osf_check_id(url))

invalid <- filter(utable, is.na(osf_id))
not_five <- utable |>
  filter(!is.na(osf_id), osf_id != five)


osfio <- psychsci |>
  search_text("\\bosf\\s*\\.\\s*io\\s*/\\s*[a-z0-9]{5}\\b", return = "match")

utableio <- osfio |>
  select(url = text) |>
  unique() |>
  mutate(five = sub("https://", "", url) |>
           sub("osf\\.io/", "", x = _, ignore.case = TRUE) |>
           tolower()) |>
  mutate(osf_id = osf_check_id(url))

invalidio <- filter(utableio, is.na(osf_id))
not_fiveio <- utableio |>
  filter(!is.na(osf_id), osf_id != five)

not_fiveio$url |> paste(collapse = "\n") |> cat()

# worked out code

OSF_RGX <- "\\bosf\\s*\\.\\s*io\\s*/\\s*[a-z0-9]{5}\\b"
osfio <- psychsci |>
  search_text(OSF_RGX, return = "match") |>
  mutate(osf_id = osf_check_id(text)) |>
  count(text, osf_id)
