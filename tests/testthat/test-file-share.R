test_that("file_share_links finds links to file-sharing services (#660)", {
  p <- test_paper(text = c(
    "Data are on https://drive.google.com/drive/folders/1AbC?usp=sharing.",
    "Code: www.dropbox.com/s/abc123/code.zip?dl=0 and https://1drv.ms/u/s!AbC",
    "The survey is at https://docs.google.com/forms/d/e/1FA/viewform",
    "Box: https://app.box.com/s/xyz9 ; SURF: https://surfdrive.surf.nl/files/index.php/s/AbCd",
    "We used Dropbox to share drafts among the authors."))
  fs <- file_share_links(p)
  expect_equal(fs$service, c("Google Drive", "Dropbox", "OneDrive", "Box", "SURFdrive"))
  # the full stop that ends the sentence is not part of the link
  expect_equal(fs$href[1], "https://drive.google.com/drive/folders/1AbC?usp=sharing")
  # a hyperlink without the address in the text
  h <- file_share_links(test_paper(url = "https://www.dropbox.com/s/abc123/data.csv?dl=0"))
  expect_equal(h$service, "Dropbox")
  # nothing to find
  expect_equal(nrow(file_share_links(test_paper(text = "Data are at https://osf.io/abcde."))), 0)
  # an archived copy lasts: no warning
  a <- test_paper(text = "See the list at http://web.archive.org/web/20220203164905/https://docs.google.com/spreadsheets/d/1LNF5/edit.")
  expect_equal(nrow(file_share_links(a)), 0)
  # one link in full and shortened is one link
  b <- test_paper(text = "The set (https://nihcc.app.box.com/v/ChestXray-NIHCC) is public.",
                  url = "app.box.com/v/ChestXray-NIHCC")
  expect_equal(file_share_links(b)$href, "https://nihcc.app.box.com/v/ChestXray-NIHCC")
})

test_that("repo_check warns for a file-sharing link and makes the light yellow (#660)", {
  p <- test_paper(text = "All data and code are available at https://www.dropbox.com/s/abc123/project.zip?dl=0.")
  mo <- module_run(p, "repo_check")
  expect_equal(mo$traffic_light, "yellow")
  expect_equal(mo$summary_table$file_share_n, 1L)
  rep <- paste(unlist(mo$report), collapse = "\n")
  expect_match(rep, "File-sharing services", fixed = TRUE)
  expect_match(rep, "Dropbox: https://www.dropbox.com/s/abc123/project.zip?dl=0", fixed = TRUE)
  expect_match(rep, "Zenodo", fixed = TRUE)
  # without such a link, no warning
  q <- test_paper(text = "We report all measures.")
  mq <- module_run(q, "repo_check")
  expect_equal(mq$traffic_light, "na")
  expect_equal(mq$summary_table$file_share_n, 0L)
})
