Sys.setenv(R_TESTS="")

library(testthat)
library(saotd)

testthat::test_check("saotd")
