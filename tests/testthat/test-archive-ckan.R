# CKAN data portals (#637; R/archive-ckan.R). Offline: the portal's answer
# and DataCite's records are stood in for.

test_that(".ckan_id reads every CKAN dataset address", {
  expect_true(all(c("data.cnra.ca.gov", "envidat.ch", "data.nhm.ac.uk", "open.canada.ca") %in% .ckan_hosts()))
  ids <- .ckan_id(c(
    "https://data.cnra.ca.gov/dataset/statewide-crop-mapping",
    "https://www.envidat.ch/#/metadata/chelsa-climatologies",
    "http://data.nhm.ac.uk/dataset/8c67d782-c01d-4807-8913-f57d0daabf35",
    "https://open.canada.ca/data/en/dataset/0c0a6e7b-4c1d-4b6f-9b1e-1a2b3c4d5e6f",
    "https://data.cnra.ca.gov/dataset/6c3d65e3/resource/1",
    "https://data.cnra.ca.gov/organization/cdfa",
    "https://www.example.org/dataset/x"
  ))
  expect_equal(ids, c("data.cnra.ca.gov/statewide-crop-mapping", "www.envidat.ch/chelsa-climatologies",
                      "data.nhm.ac.uk/8c67d782-c01d-4807-8913-f57d0daabf35",
                      "open.canada.ca/0c0a6e7b-4c1d-4b6f-9b1e-1a2b3c4d5e6f",
                      "data.cnra.ca.gov/6c3d65e3", NA, NA))
})

test_that(".ckan_info lists the portal's own files, not links to other websites", local({
  asked <- character(0)
  body <- list(result = list(
    title = "Statewide Crop Mapping", license_title = "Creative Commons Attribution", doi = "10.1/x",
    author = "California DWR", resources = list(
      list(name = "Crop map 2024", format = "ZIP", size = 176115282, url_type = "upload",
           url = "https://data.cnra.ca.gov/dataset/6c3d/resource/1/download/i15_crop_2024.zip"),
      list(name = "Map service", format = "Data", url_type = "",
           url = "https://www.arcgis.com/home/item.html?id=39b6"),
      list(name = "Readme", format = "PDF", size = 2000, url_type = "",
           url = "https://data.cnra.ca.gov/dataset/6c3d/resource/2/download"))))
  testthat::local_mocked_bindings(.batch_query = function(url, ...) {
    asked <<- c(asked, url)
    list(httr2::response_json(body = body))
  })
  x <- ckan_info("https://data.cnra.ca.gov/dataset/statewide-crop-mapping")
  expect_equal(asked, "https://data.cnra.ca.gov/api/3/action/package_show?id=statewide-crop-mapping")
  expect_equal(x$title, "Statewide Crop Mapping")
  expect_equal(x$license, "Creative Commons Attribution")
  expect_true(is.na(x$error))
  f <- x$files[[1]]
  # the ArcGIS page is left out; a file without a name in its address is
  # named after the resource and its format
  expect_equal(f$file_name, c("i15_crop_2024.zip", "Readme.pdf"))
  expect_equal(f$file_size, c(176115282, 2000))
}))

test_that(".ckan_info marks a dataset the portal does not have", local({
  testthat::local_mocked_bindings(.batch_query = function(url, ...) list(httr2::response(status_code = 404)))
  x <- ckan_info("https://data.cnra.ca.gov/dataset/none")
  expect_equal(x$error, "unfound")
}))

test_that("ckan_links finds dataset links and DOIs of CKAN portals", local({
  asked <- character(0)
  testthat::local_mocked_bindings(.repo_doi_records = function(doi) {
    asked <<- c(asked, doi)
    data.frame(doi = tolower(doi), url = "https://www.envidat.ch/#/metadata/chelsa-climatologies",
               resource_type = "Dataset", in_datacite = TRUE)
  })
  paper <- test_paper(
    text = c("Crops were mapped from data.cnra.ca.gov/dataset/statewide-crop-mapping.",
             "Climate data: doi:10.16904/envidat.228 and 10.1016/j.cognition.2020.104."))
  links <- ckan_links(paper)
  expect_setequal(links$ckan_id, c("data.cnra.ca.gov/statewide-crop-mapping", "www.envidat.ch/chelsa-climatologies"))
  expect_equal(asked, "10.16904/envidat.228")
  # a paper without CKAN links gives an empty table with the columns
  # repo_check takes
  none <- ckan_links(test_paper(text = "No data."))
  expect_equal(nrow(none), 0)
  expect_true(all(c("paper_id", "href") %in% names(none)))
}))
