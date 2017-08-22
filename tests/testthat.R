options(stringsAsFactors = FALSE)

library(testthat)
library(basejump)

testDataURL <- file.path(
    "https://raw.githubusercontent.com",
    "steinbaugh",
    "basejump",
    "dev",
    "docs",
    "tests")

test_check("basejump")
