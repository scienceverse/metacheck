test_that(".repo_link_joined gives the joined form only before a link's part", {
  # a Dryad DOI cut in the middle, a name cut inside a word
  expect_equal(.repo_link_joined("10.5061/dryad.4mw6m",
                                 "Data are at 10.5061/dryad.4mw6m 906h."),
               "10.5061/dryad.4mw6m906h")
  expect_equal(.repo_link_joined("https://github.com/kun-ecolo",
                                 "Code: https://github.com/kun-ecolo gy/SER and more."),
               "https://github.com/kun-ecology/SER")
  # an ordinary word after the link gives no other form
  expect_length(.repo_link_joined("https://github.com/J0vid/",
                                  "see https://github.com/J0vid/ Color images"), 0)
  expect_length(.repo_link_joined("https://github.com/x/y",
                                  "see https://github.com/x/y for The code"), 0)
  # a link found without the "/" that ends it in the text
  expect_equal(.repo_link_joined("github.com/zhoupc",
                                 "Analyses: https://github.com/zhoupc/ OASIS_matlab here."),
               "github.com/zhoupc/OASIS_matlab")
  # after a hyphen, the form without it is tried first
  expect_equal(.repo_link_joined("https://github.com/group/ionic-",
                                 "at https://github.com/group/ionic- glass-formers."),
               c("https://github.com/group/ionicglass-formers",
                 "https://github.com/group/ionic-glass-formers"))
})

test_that(".repo_link_without_hyphen changes the owner and repository only", {
  expect_equal(.repo_link_without_hyphen("github.com/Betha-nyL/DeepKoopman"),
               "github.com/BethanyL/DeepKoopman")
  expect_equal(.repo_link_without_hyphen("https://gitlab.com/a-b/c-d"),
               c("https://gitlab.com/ab/c-d", "https://gitlab.com/a-b/cd"))
  expect_length(.repo_link_without_hyphen("https://github.com/owner/repo"), 0)
  expect_length(.repo_link_without_hyphen("https://zenodo.org/records/1"), 0)
})

test_that(".repo_links_joined keeps a link that exists as written", {
  # no request is made for a link without another form
  repos <- data.frame(paper_id = "p", repo_url = "https://github.com/x/y",
                      repo_type = "github")
  paper <- test_paper("The code is at https://github.com/x/y for The analyses.")
  paper$paper_id <- "p"
  expect_equal(.repo_links_joined(repos, paper), repos)
})

test_that(".repo_links_joined replaces a link that exists only joined", {
  skip_on_cran()
  skip_if_not(online("github.com"))
  skip_if_not(online("doi.org"))
  repos <- data.frame(paper_id = "p",
                      repo_url = c("10.5061/dryad.4mw6m", "https://github.com/kun-ecolo"),
                      repo_type = c("dryad", "github"))
  paper <- test_paper(c("Data are at 10.5061/dryad.4mw6m 906h.",
                        "Code: https://github.com/kun-ecolo gy/SER and more."))
  paper$paper_id <- "p"
  obs <- .repo_links_joined(repos, paper)
  expect_equal(obs$repo_url, c("10.5061/dryad.4mw6m906h",
                               "https://github.com/kun-ecology/SER"))
  expect_equal(.repo_link_unhyphenated("github.com/Betha-nyL/DeepKoopman", github_repo),
               "github.com/BethanyL/DeepKoopman")
})
