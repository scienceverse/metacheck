pattern_fund <- c(
  "funder",
  "funded",
  "funding",
  "financed",
  "supported by",
  "support from",
  "grant",
  "award",
  "fellowship",
  "scholarship",
  "stipend"
)

fund_text <- psychsci |>
  search_text(pattern_fund)

readr::write_csv(fund_text, "_stuff/fund_text.csv")
