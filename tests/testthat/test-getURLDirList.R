context("getURLDirList")

skip_if_not(hasInternet())
skip_on_appveyor()
skip_on_docker()

test_that("NCBI FTP", {
    url <- "ftp://ftp.ncbi.nlm.nih.gov/genomes/Homo_sapiens/current"
    x <- getURLDirList(url, pattern = "^GCF_")
    expect_is(x, "character")
})
