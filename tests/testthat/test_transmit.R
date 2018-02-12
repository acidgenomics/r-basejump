context("transmit")

test_that("transmit", {
    ensembl <- "ftp://ftp.ensembl.org/pub/release-89"
    expect_identical(
        transmit(
            ensembl,
            pattern = "README",
            compress = FALSE,
            quiet = TRUE) %>%
            .[["README"]] %>%
            .[[1L]],
        file.path(getwd(), "README")
    )
    unlink("README")
    expect_identical(
        transmit(
            ensembl,
            pattern = "README",
            rename = "ensembl_readme.txt",
            compress = TRUE,
            quiet = TRUE) %>%
            .[["README"]] %>%
            .[[1L]],
        file.path(getwd(), "ensembl_readme.txt.gz")
    )
    unlink("ensembl_readme.txt.gz")
    expect_error(
        transmit("http://steinbaugh.com", pattern = "README"),
        "is_matching_regex : remoteDir"
    )
    expect_error(
        transmit("ftp://ftp.wormbase.org/pub/", pattern = "README"),
        "is_non_empty : remoteFileList"
    )
    expect_error(
        transmit(ensembl, pattern = "XXX"),
        "is_non_empty : remoteFileName"
    )
    expect_error(
        transmit(
            ensembl,
            pattern = "README",
            rename = c("XXX", "YYY")),
        "is_a_string"
    )
})
