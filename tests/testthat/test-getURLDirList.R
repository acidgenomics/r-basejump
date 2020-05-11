context("getURLDirList")

skip_on_appveyor()
skip_on_docker()

test_that("NCBI FTP", {
    url <- "ftp://ftp.ncbi.nlm.nih.gov/genomes/"
    skip_if_not(hasInternet(url))
    x <- getURLDirList(url, pattern = "^refseq$")
    expect_is(x, "character")
})
