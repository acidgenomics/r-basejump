library(testthat)
library(patrick)
library(basejump)
test_check("basejump")
if (isTRUE(getOption("basejump.tests.extra"))) {
    test_dir(file.path("tests", "testthat-extra"))
}
