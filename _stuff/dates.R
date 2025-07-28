dates <- c(
  "2024-12-15",           # ISO 8601 (YYYY-MM-DD)
  "15/12/2024",           # European (DD/MM/YYYY)
  "12/15/2024",           # US (MM/DD/YYYY)
  "15-Dec-2024",          # Day-Month abbreviation-Year
  "December 15, 2024",    # Full month, US order
  "15 December 2024",     # Full month, European order
  "2024/12/15",           # ISO-like with slashes
  "15.12.2024",           # Dots as separators (common in parts of Europe)
  "20241215",             # Compact ISO (YYYYMMDD)
  "15 Dec 2024",          # Day AbbrevMonth Year
  "Dec 15, 2024",         # AbbrevMonth Day, Year (US)
  "Sunday, December 15, 2024", # Full weekday and date (US)
  "2024.12.15",           # Dots with ISO order
  "5 February, 2025",     # Day Month, Year with comma
  "Feb 5, 2025",          # AbbrevMonth Day, Year
  "2025-Feb-05",          # ISO-ish with month abbrev
  "05-Feb-2025",          # Day-MonthAbbrev-Year
  "Wed, 05 Feb 2025",     # Weekday Abbrev + European date
)

months <- c(
  lubridate::month(1:12, label = TRUE),
  lubridate::month(1:12, label = TRUE, abbr = FALSE)
) |>
  as.character() |> unique() |> paste(collapse = "|")
pattern <- paste0(
  "(", months, "|\\d{1,4})",
  "[- /\\.](", months, "|\\d{1,2}),?",
  "[- /\\.]\\d{2,4}"
)
m <- gregexpr(pattern, dates, ignore.case = TRUE)
mdates <- regmatches(dates, m)
parsed_dates <- anytime::anydate(dates)
