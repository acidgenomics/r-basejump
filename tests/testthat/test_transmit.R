context("transmit")

test_that("transmit", {
    ensembl <- "ftp://ftp.ensembl.org/pub/release-89"
    expect_equal(
        transmit(ensembl,
                 pattern = "README",
                 compress = FALSE,
                 quiet = TRUE) %>%
            .[["README"]] %>%
            .[[1L]],
        "data-raw/README"
    )
    expect_equal(
        transmit(ensembl,
                 pattern = "README",
                 rename = "ensembl_readme.txt",
                 compress = TRUE,
                 quiet = TRUE) %>%
            .[["README"]] %>%
            .[[1L]],
        "data-raw/ensembl_readme.txt.gz"
    )
    expect_error(
        transmit("http://steinbaugh.com",
                 pattern = "README"),
        "FTP protocol not detected"
    )
    expect_error(
        transmit("ftp://ftp.wormbase.org/pub/",
                 pattern = "README"),
        "No files listed on remote server"
    )
    expect_error(
        transmit(ensembl,
                 pattern = "XXX"),
        "Pattern didn't match any files"
    )
    expect_error(
        transmit(ensembl,
                 pattern = "README",
                 rename = c("XXX", "YYY")),
        "Rename vector doesn't match the number of remote files"
    )
})

unlink("data-raw", recursive = TRUE)
