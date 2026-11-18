test_that("a resolved Handle's sentence is found through the Handle (#660)", {
  assign(.own_core("https://datastorre.stir.ac.uk/handle/11667/999"), "http://hdl.handle.net/11667/999",
         envir = .repo_link_alias)
  expect_equal(.own_needle("https://datastorre.stir.ac.uk/handle/11667/999"), "hdl.handle.net/11667/999")
  expect_equal(.own_needle("https://osf.io/abcde"), .own_core("https://osf.io/abcde"))
  expect_equal(nrow(handle_links(test_paper(text = "No Handle here."))), 0)
})

test_that("handle_links follows a Handle to a repository repo_check reads (#660, #466)", {
  skip_if_not(online("hdl.handle.net"))
  skip_on_cran()
  p <- test_paper(text = c("All data are available at http://hdl.handle.net/11667/195.",
                           "See the thesis at http://hdl.handle.net/1721.1/114075."))
  h <- handle_links(p)
  # the data deposit on Stirling's DataSTORRE (DSpace), not the thesis on a
  # repository metacheck does not read
  expect_equal(h$repo_type, "dspace")
  expect_match(h$href, "datastorre.stir.ac.uk", fixed = TRUE)
  expect_equal(h$handle, "http://hdl.handle.net/11667/195")
  mo <- module_run(p, "repo_check")
  expect_equal(mo$summary_table$repo_n, 1L)
  expect_equal(mo$ownership$context, "All data are available at http://hdl.handle.net/11667/195.")
})
