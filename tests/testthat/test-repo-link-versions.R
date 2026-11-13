test_that(".repo_link_joined gives the joined form only before a link's part", {
  # a Dryad DOI cut in the middle, a name cut inside a word
  expect_equal(.repo_link_joined("10.5061/dryad.4mw6m",
                                 "Data are at 10.5061/dryad.4mw6m 906h."),
               "10.5061/dryad.4mw6m906h")
  expect_equal(.repo_link_joined("https://github.com/kun-ecolo",
                                 "Code: https://github.com/kun-ecolo gy/SER and more."),
               "https://github.com/kun-ecology/SER")
  # an ordinary word after a repository's link gives no other form
  expect_length(.repo_link_joined("https://github.com/x/y",
                                  "see https://github.com/x/y for The code"), 0)
  # after an account, any word is tried, as a space can fall anywhere in its
  # name; the account stays unless the joined form exists (#634)
  expect_equal(.repo_link_joined("https://github.com/J0vid/",
                                 "see https://github.com/J0vid/ Color images"),
               "https://github.com/J0vid/Color")
  expect_equal(.repo_link_joined("github.com/crs", "Aust (2022). https://github.com/crs h/papaja"),
               "github.com/crsh/papaja")
  # up to three words, the longest form first
  expect_equal(.repo_link_joined("github.com/keras",
                                 "Keras Tuner. https://github.com/keras -team/keras -tuner"),
               c("github.com/keras-team/keras-tuner", "github.com/keras-team/keras"))
  expect_equal(.repo_link_joined("github.com/matthewmoscou",
                                 "Available from: https://github.com/matthewmoscou/ QKphylogeny"),
               "github.com/matthewmoscou/QKphylogeny")
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

test_that(".repo_links_tidy repairs damage from reading the PDF (#634)", {
  repos <- data.frame(paper_id = "p", repo_type = "x", repo_url = c(
    # security wrappers
    "https://nam04.safelinks.protection.outlook.com/?url=https%3A%2F%2Fdatadryad.org%2Fstash%2Fshare%2Fabc&data=05",
    "https://urldefense.proofpoint.com/v2/url?u=https-3A__osf.io_ab12c&d=DwM",
    "https://protect.checkpoint.com/v2/___https://researchbox.org/3218___.YzJ1OnN0",
    # two links run together
    "https://doi.org/10.48329/zrvz-6a47.org/10.1186/s12916-025-04514-z",
    "http://doi.org/10.1525/collabra.48.pr10.1525/collabra.48.pr",
    "github.com/gabehassler/PhylogeneticFactorAnalysis.https",
    # home pages and a DOI's prefix alone
    "https://zenodo.org/", "http://osf.io", "doi.org/10.5281",
    # a link cut inside a name beside its longer form; an account beside its repository
    "10.6084/m9", "10.6084/m9.figshare.27262470",
    "github.com/XiaoyangSu", "https://github.com/XiaoyangSu/AccuCor",
    # a digit after it may be another record: both stay
    "10.5281/zenodo.12345", "10.5281/zenodo.123456"))
  paper <- test_paper("No text needed here.")
  paper$paper_id <- "p"
  expect_equal(.repo_links_tidy(repos, paper)$repo_url, c(
    "https://datadryad.org/stash/share/abc",
    "https://osf.io/ab12c",
    "https://researchbox.org/3218",
    "https://doi.org/10.48329/zrvz-6a47",
    "http://doi.org/10.1525/collabra.48.pr",
    "github.com/gabehassler/PhylogeneticFactorAnalysis",
    # a shorter link that may be whole is left to .repo_links_joined()
    "10.6084/m9", "10.6084/m9.figshare.27262470",
    "https://github.com/XiaoyangSu/AccuCor",
    "10.5281/zenodo.12345", "10.5281/zenodo.123456"))
  # names that share a beginning are two repositories
  repos <- data.frame(paper_id = "p", repo_type = "github",
                      repo_url = c("https://github.com/mastodon-sc/mastodon",
                                   "https://github.com/mastodon-sc/mastodon-averoflab",
                                   "https://github.com/x/Raina-", "https://github.com/x/Raina-lab"))
  expect_equal(.repo_links_tidy(repos, paper)$repo_url, repos$repo_url[c(1, 2, 4)])
  # Proofpoint's newer wrapper writes "https:/"
  expect_equal(.repo_link_unwrapped("https://urldefense.com/v3/__https:/github.com/marhabaie/dmel-nanos-targets__;!!KGKeukY!zR8"),
               "https://github.com/marhabaie/dmel-nanos-targets")
  # a DOI written right after the link in the text, where the link stopped
  repos <- data.frame(paper_id = "p", repo_type = "dryad",
                      repo_url = "doi.org/10.5061/dryad.w0vt4b8sh10")
  paper <- test_paper("Life Sci. 2008. http://dx.doi.org/10.5061/dryad.w0vt4b8sh10.1007/s00018-008-8182-3")
  paper$paper_id <- "p"
  expect_equal(.repo_links_tidy(repos, paper)$repo_url, "doi.org/10.5061/dryad.w0vt4b8sh")
  # a link with nothing wrong is kept as it is
  repos <- data.frame(paper_id = "p", repo_type = "osf", repo_url = "https://osf.io/ab12c")
  expect_equal(.repo_links_tidy(repos, paper), repos)
  # an OSF identifier written twice or run into the next; a DataONE
  # identifier without "/"; the rest of a separator after a cut
  repos <- data.frame(paper_id = "p", repo_type = c("osf", "osf", "osf", "osf", "dataone", "osf"),
                      repo_url = c("https://osf.io/3bh67", "http://osf.io/3bh673bh67",
                                   "http://osf.io/3bh67c9pag", "https://osf.io/c9pag", "knb.341.1",
                                   "https://osf.io/j7kt2/;%20%20https://rr.peercommunityin.org/x"))
  expect_equal(.repo_links_tidy(repos, paper)$repo_url,
               c("https://osf.io/3bh67", "http://osf.io/3bh67", "https://osf.io/c9pag",
                 "knb.341.1", "https://osf.io/j7kt2/"))
  # an OSF link cut inside its identifier, beside its whole form
  repos <- data.frame(paper_id = "p", repo_type = "osf",
                      repo_url = c("https://doi.org/10.17605/osf.io/zry5", "https://doi.org/10.17605/osf.io/zry5b"))
  expect_equal(.repo_links_tidy(repos, paper)$repo_url, "https://doi.org/10.17605/osf.io/zry5b")
  # a longer form that repeats the link's end is the reader writing it twice
  repos <- data.frame(paper_id = "p", repo_type = "github",
                      repo_url = c("https://github.com/o/Vaccine-Policy", "https://github.com/o/Vaccine-PolicyPolicy"))
  expect_equal(.repo_links_tidy(repos, paper)$repo_url, repos$repo_url)
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
  # a link cut short beside its longer form that exists is left out; two
  # repositories whose names share a beginning both stay (#634)
  repos <- data.frame(paper_id = "p", repo_type = c("figshare", "figshare", "github", "github"),
                      repo_url = c("10.6084/m9", "10.6084/m9.figshare.27262470",
                                   "https://github.com/mastodon-sc/mastodon",
                                   "https://github.com/mastodon-sc/mastodon-averoflab"))
  paper <- test_paper("No split links here.")
  paper$paper_id <- "p"
  expect_equal(.repo_links_joined(repos, paper)$repo_url, repos$repo_url[2:4])
  expect_equal(.repo_link_unhyphenated("github.com/Betha-nyL/DeepKoopman", github_repo),
               "github.com/BethanyL/DeepKoopman")
})
