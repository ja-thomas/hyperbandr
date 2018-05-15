Sys.setenv("R_TESTS" = "")
library(testthat)
library(checkmate)
library(hyperbandr)

test_check("hyperbandr")
