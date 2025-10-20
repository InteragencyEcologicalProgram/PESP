library(testthat)
library(tidyverse)
library(rlang)

source('admin/global_functions/global_funcs.R', local = FALSE)
testthat::test_dir('tests/testthat/')
