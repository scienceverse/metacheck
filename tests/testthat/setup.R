# code tailored for test execution in non-interactive environments
# not executed by load_all()
# https://r-pkgs.org/testing-design.html#testthat-setup-files

verbose(FALSE)

# teardown code
withr::defer(verbose(TRUE))
