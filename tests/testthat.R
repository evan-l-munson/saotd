library(testthat)
library(saotd)

Sys.setenv(R_TESTS="")
testthat::test_check("saotd")
