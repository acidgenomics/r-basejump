library(testthat)
library(patrick)
library(basejump)
test_check("basejump")

# Run the resource intensive unit tests less frequently (e.g. per month).
# Currently, I'm setting this in `~/.Rsecrets` for supported machines.
if (isTRUE(getOption("basejump.test.extra"))) {
    # Disable our useful interactive flag for quick CI tests (see Rprofile).
    options(basejump.test = FALSE)
    test_dir(file.path("tests", "testthat-extra"))
}
