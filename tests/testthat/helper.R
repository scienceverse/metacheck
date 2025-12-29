# always executed by load_all() and at the beginning of automated testing
# https://r-pkgs.org/testing-design.html#testthat-helper-files

email("metacheck@scienceverse.org")

httptest::.mockPaths(NULL)
httptest::.mockPaths("tests/testthat/apis")
httptest::.mockPaths("apis")
