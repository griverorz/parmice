Sys.setenv("R_TESTS" = "") 

library(parallel)
library(doParallel)
library(testthat)
library(parmice)

test_check("parmice")
