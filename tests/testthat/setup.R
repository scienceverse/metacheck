# code tailored for test execution in non-interactive environments
# not executed by load_all()
# https://r-pkgs.org/testing-design.html#testthat-setup-files

verbose(FALSE)

# Disable the session OSF listing cache during tests: it would let one test
# reuse another's listing and skip the httptest2-mocked request, breaking mock
# expectations. Production default stays TRUE (set in zzz.R).
options(metacheck.osf.cache = FALSE)

# teardown code
withr::defer(verbose(TRUE))
withr::defer(options(metacheck.osf.cache = TRUE))
